;;; racket-tests.el

;; Copyright (c) 2013-2019 by Greg Hendershott.

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

(require 'ert)
(require 'edmacro)
(require 'faceup)
(require 'paredit)
(require 'racket-mode)
(require 'racket-xp)
(require 'racket-repl)
(require 'racket-edit)
(require 'racket-debug)
(require 'racket-xp)
(require 'racket-common)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-smart-open)
(require 'racket-stepper)

(defconst racket-tests/here-dir (faceup-this-file-directory)
  "The directory this file is located in.")

;;; Utility functions for "integration" testing

(defconst ci-p (or (getenv "TRAVIS_CI")
                   (getenv "CI"))
  "Is there an environment variable saying we're running on CI?")

(defconst racket-tests/timeout (if ci-p (* 5 60) 10))

(defun racket-tests/type (typing)
  (let ((blink-matching-paren nil)) ;suppress "Matches " messages
    (execute-kbd-macro (string-to-vector typing))
    (redisplay)))

(defun racket-tests/press (binding)
  (racket-tests/type (edmacro-parse-keys binding)))

(defun racket-tests/type&press (typing binding)
  (racket-tests/type typing)
  (racket-tests/press binding))

(defun racket-tests/call-until-true (thunk)
  (with-timeout (racket-tests/timeout nil)
    (while (not (funcall thunk))
      (accept-process-output)
      (sit-for 0.25))
    t))

(defmacro racket-tests/eventually (&rest body)
  `(racket-tests/call-until-true (lambda () ,@body)))

(defmacro racket-tests/should-eventually (&rest body)
  "Aside from avoiding some nesting, an advantage of this vs.
composing `should` and eventually is that the `should` macro will
be able to look up an ert-explainer property on the symbol
supplied to it."
  `(progn
     (racket-tests/call-until-true (lambda () ,@body))
     (should (progn ,@body))))

(defun racket-tests/see-back-rx (rx)
  (racket-tests/eventually (looking-back rx (point-min))))

(defun racket-tests/see-forward-rx (rx)
  (racket-tests/eventually (looking-at rx)))

(defun racket-tests/see-back (str)
  (racket-tests/see-back-rx (regexp-quote str)))

(defun racket-tests/see-forward (str)
  (racket-tests/see-forward-rx (regexp-quote str)))

(defun racket-tests/see-char-property (pos name val)
  (racket-tests/eventually
   (equal (get-char-property pos name) val)))

(defun racket-tests/explain-see (_str &optional _dir)
  `(actual . ,(buffer-substring-no-properties
               (point-min)
               (point))))

(dolist (sym '(racket-tests/see-back-rx
               racket-tests/see-forward-rx
               racket-tests/see-back
               racket-tests/see-forward))
  (put sym 'ert-explainer #'racket-tests/explain-see))

(defun racket-tests/call-with-back-end-settings (thunk)
  (let ((racket-command-port (+ racket-command-port 2)) ;skip default cmd & logger ports
        (racket-command-timeout racket-tests/timeout))
    (funcall thunk)))

(defmacro racket-tests/with-back-end-settings (&rest body)
  (declare (indent 0) (debug t))
  `(racket-tests/call-with-back-end-settings (lambda () ,@body)))

;;; REPL

(ert-deftest racket-tests/repl ()
  "Start/exercise/stop REPL without any racket-run."
  (message "racket-tests/repl")
  (racket-tests/with-back-end-settings
    (racket-repl)
    (racket-tests/should-eventually (get-buffer racket-repl-buffer-name))
    (racket-tests/should-eventually (racket--repl-live-p))
    (with-racket-repl-buffer
      (should (racket-tests/see-back-rx
               "Welcome to Racket v?[0-9.]+\\(?: \\[cs\\].\\)?[\n]\\(?:;.*[\n]\\)*> "))

      ;; Completion
      (racket-tests/should-eventually (member "current-output-port"
                                              racket--repl-namespace-symbols))
      (racket-tests/type "current-out")
      (completion-at-point)
      (should (racket-tests/see-back "current-output-port"))
      (racket-tests/press "RET")
      (should (racket-tests/see-back "#<procedure:current-output-port>\n> "))

      ;; Multiline expression indent
      (racket-tests/type&press "(if 1" "C-j")
      (should (racket-tests/see-back "(if 1\n      "))
      (racket-tests/type&press "2" "C-j")
      (should (racket-tests/see-back "2\n      "))
      (racket-tests/type&press "3)" "RET")
      (should (racket-tests/see-back "3)\n2\n> "))

      ;; Smart open bracket
      (let ((typing   "[cond [[values 1] #t] [else #f]]")
            (expected "(cond [(values 1) #t] [else #f])\n#t\n> "))
        (racket-smart-open-bracket-mode 1)
        (mapc (lambda (modes)
                (electric-pair-mode (if (car modes) 1 -1))
                (if (cdr modes) (enable-paredit-mode) (disable-paredit-mode))
                (racket-tests/type&press typing "RET")
                (should (racket-tests/see-back expected)))
              (list (cons nil nil)
                    (cons t   nil)
                    (cons nil t))))

      ;; Exit
      (racket-tests/type&press "(exit)" "RET")
      (should (racket-tests/see-back
               "Process *Racket REPL* connection broken by remote peer\n"))
      (kill-buffer))))

;;; Multi REPLs

(ert-deftest racket-test/unique-repls ()
  "Excercise one unique REPL per racket-mode buffer.
Create file a.rkt with (define a \"a\") -- and so on for b.rkt,
c.rkt. Visit each file, racket-run, and check as expected."
  (message "racket-test/unique-repls")
  (racket-tests/with-back-end-settings
   (let* ((racket-repl-buffer-name-function #'racket-repl-buffer-name-unique)
          (names '("a" "b" "c"))
          (rkt-buffers (dolist (name names)
                         (let ((file (make-temp-file name nil ".rkt"))
                               (code (format "#lang racket/base\n(define %s \"%s\")\n"
                                             name name)))
                           (write-region code nil file nil 'no-wrote-file-message)
                           (find-file file)
                           (current-buffer)))))
     (dolist (rkt-buffer rkt-buffers)
       (let* ((var (with-current-buffer rkt-buffer
                     (goto-char 27)
                     (buffer-substring-no-properties (point) (1+ (point)))))
              (val (concat ?\" var ?\")))
         (with-current-buffer rkt-buffer
           (should (equal racket-repl-buffer-name
                          (racket-repl-buffer-name-unique)))
           (racket-run))
         (with-racket-repl-buffer
           (should (equal (buffer-name)
                          (with-current-buffer rkt-buffer
                            (racket-repl-buffer-name-unique))))
           (racket-tests/type&press var "RET")
           (should (racket-tests/see-back (concat val
                                                  ?\n
                                                  var ".rkt> ")))
           (kill-buffer)))
       (delete-file (buffer-file-name))
       (kill-buffer)))))

;;; Run

(ert-deftest racket-tests/run ()
  "Start the REPL via a racket-run command."
  (message "racket-tests/run")
  (racket-tests/with-back-end-settings
    (let* ((pathname (make-temp-file "test" nil ".rkt"))
           (name     (file-name-nondirectory pathname))
           (code "#lang racket/base\n(define foobar 42)\nfoobar\n"))
      (write-region code nil pathname nil 'no-wrote-file-message)
      (find-file pathname)
      (racket-run)
      (racket-tests/should-eventually (get-buffer racket-repl-buffer-name))
      (racket-tests/should-eventually (racket--repl-live-p))
      (with-racket-repl-buffer
        (should (racket-tests/see-back (concat "\n" name "> ")))
        (racket-repl-exit)
        (should (racket-tests/see-back
                 "Process *Racket REPL* connection broken by remote peer\n"))
        (kill-buffer))
      (kill-buffer)
      (delete-file pathname))))

;;; racket-xp-mode

(ert-deftest racket-tests/xp ()
  (message "racket-tests/xp")
  (racket-tests/with-back-end-settings
    (let* ((pathname (make-temp-file "test" nil ".rkt"))
           (name     (file-name-nondirectory pathname))
           (code     "#lang racket/base\n(define foobar 42)\nfoobar\n"))
      (write-region code nil pathname nil 'no-wrote-file-message)
      (find-file pathname)
      ;; In case running test interactively in Emacs where the config
      ;; loads `racket-xp-mode' automatically, disable it first.
      (racket-xp-mode 0)
      (racket-xp-mode 1)
      (should racket-xp-mode)
      (racket-tests/should-eventually
       (goto-char (point-min))
       (racket-xp-next-definition)
       (racket-tests/see-forward "racket/base"))
      (racket-xp-next-definition)
      (should (racket-tests/see-forward "foobar"))
      (should (equal (get-text-property (point) 'help-echo) "1 bound occurrence"))
      (racket-xp-next-use)
      (should (racket-tests/see-forward "foobar"))
      (should (equal (get-text-property (point) 'help-echo) "Defined locally"))
      (goto-char (point-max))
      (insert "foo")
      (completion-at-point)
      (should (racket-tests/see-back "foobar"))
      (racket-xp-mode 0)
      (kill-buffer)
      (delete-file pathname))))

;;; Debugger

(ert-deftest racket-tests/debugger ()
  (message "racket-tests/debugger")
  (racket-tests/with-back-end-settings
    (let* ((pathname (make-temp-file "test" nil ".rkt"))
           (name     (file-name-nondirectory pathname))
           (code     "#lang racket/base\n(define (f x) (+ 1 x))\n(f 41)\n"))
      (write-region code nil pathname nil 'no-wrote-file-message)
      (find-file pathname)
      (should (eq major-mode 'racket-mode))
      (racket-run `(16))
      (should (racket-tests/eventually (get-buffer racket-repl-buffer-name)))
      (should (racket-tests/eventually (racket--repl-live-p)))
      (should (racket-tests/eventually racket-debug-mode))

      (with-racket-repl-buffer
        (should (racket-tests/see-back (concat "\n[" name ":42]> ")))) ;debugger prompt
      (should (racket-tests/see-char-property (point) 'face
                                              racket-debug-break-face))

      (racket-debug-step)
      (with-racket-repl-buffer
        (should (racket-tests/see-back (concat "\n[" name ":33]> "))))
      (should (racket-tests/see-char-property (point) 'face
                                              racket-debug-break-face))
      (should (racket-tests/see-char-property  (- (point) 3) 'after-string
                                               (propertize "41" 'face racket-debug-locals-face)))

      (racket-debug-step)
      (with-racket-repl-buffer
        (should (racket-tests/see-back (concat "\n[" name ":47]> "))))
      (should (racket-tests/see-char-property  (point) 'after-string
                                               (propertize "⇒ (values 42)" 'face racket-debug-result-face)))

      (racket-debug-step)               ;no more debug breaks left
      (with-racket-repl-buffer
        (should (racket-tests/see-back (concat "\n" name "> "))))
      (should-not (racket-tests/see-char-property (point) 'after-string
                                                  racket-debug-break-face))
      (should (racket-tests/see-char-property (point) 'after-string
                                              nil))
      (should-not racket-debug-mode)
      (with-racket-repl-buffer
        (racket-repl-exit)
        (should (racket-tests/see-back
                 "Process *Racket REPL* connection broken by remote peer\n"))
        (kill-buffer))

      (kill-buffer)
      (delete-file pathname))))

;;; Macro stepper

(defconst racket-tests/expand-mod-name "foo")

(defconst racket-tests/expand-shallow-0
  "«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

")

(defconst racket-tests/expand-shallow-1
  "«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:Final»

")

(ert-deftest racket-tests/expand-file-shallow ()
  (message "racket-tests/expand-file-shallow")
  (racket-tests/with-back-end-settings
    (let* ((dir  (make-temp-file "test" t))
           (path (concat dir "/" racket-tests/expand-mod-name ".rkt"))
           (code "#lang racket/base\n(define x 42)\nx"))
      (write-region code nil path nil 'no-wrote-file-message)
      (find-file path)
      (racket-expand-file)
      (set-buffer "*Racket Stepper*")
      (should (eq major-mode 'racket-stepper-mode))
      (should (equal header-line-format "Press RET to step. C-h m to see help."))
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-shallow-0))
      (racket-tests/press "RET")
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-shallow-1))
      (kill-buffer)
      (delete-file path))))

(defconst racket-tests/expand-deep-0
  "«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

")

(defconst racket-tests/expand-deep-1
  "«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:1: Macro transformation»
«x:@@ -1 +1,11 @@»
«:diff-removed:-(module foo racket/base (#%module-begin (define x 42) x))»
«:diff-added:+(module»
«:diff-added:+ foo»
«:diff-added:+ racket/base»
«:diff-added:+ (printing:module-begin:1»
«:diff-added:+  (module:1»
«:diff-added:+   configure-runtime:1»
«:diff-added:+   '#%kernel:1»
«:diff-added:+   (#%require:1 racket/runtime-config:1)»
«:diff-added:+   (configure:1 #f))»
«:diff-added:+  (define x 42)»
«:diff-added:+  x))»

")

(defconst racket-tests/expand-deep-2
  "«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:1: Macro transformation»
«x:@@ -1 +1,11 @@»
«:diff-removed:-(module foo racket/base (#%module-begin (define x 42) x))»
«:diff-added:+(module»
«:diff-added:+ foo»
«:diff-added:+ racket/base»
«:diff-added:+ (printing:module-begin:1»
«:diff-added:+  (module:1»
«:diff-added:+   configure-runtime:1»
«:diff-added:+   '#%kernel:1»
«:diff-added:+   (#%require:1 racket/runtime-config:1)»
«:diff-added:+   (configure:1 #f))»
«:diff-added:+  (define x 42)»
«:diff-added:+  x))»

«f:2: Macro transformation»
«x:@@ -1,11 +1,13 @@»
 (module
  foo
  racket/base
«:diff-removed:- (printing:module-begin:1»
«:diff-removed:-  (module:1»
«:diff-removed:-   configure-runtime:1»
«:diff-removed:-   '#%kernel:1»
«:diff-removed:-   (#%require:1 racket/runtime-config:1)»
«:diff-removed:-   (configure:1 #f))»
«:diff-removed:-  (define x 42)»
«:diff-removed:-  x))»
«:diff-added:+ (#%module-begin:2»
«:diff-added:+  (do-wrapping-module-begin:2»
«:diff-added:+   print-result:2»
«:diff-added:+   (module:1»
«:diff-added:+    configure-runtime:1»
«:diff-added:+    '#%kernel:1»
«:diff-added:+    (#%require:1 racket/runtime-config:1)»
«:diff-added:+    (configure:1 #f)))»
«:diff-added:+  (do-wrapping-module-begin:2 print-result:2 (define x 42))»
«:diff-added:+  (do-wrapping-module-begin:2 print-result:2 x)))»

")

(ert-deftest racket-tests/expand-file-deep ()
  (message "racket-tests/expand-file-deep")
  (racket-tests/with-back-end-settings
    (let* ((dir  (make-temp-file "test" t))
           (path (concat dir "/" racket-tests/expand-mod-name ".rkt"))
           (code "#lang racket/base\n(define x 42)\nx"))
      (write-region code nil path nil 'no-wrote-file-message)
      (find-file path)
      (racket-expand-file 4) ;; i.e. C-u prefix
      (set-buffer "*Racket Stepper*")
      (should (eq major-mode 'racket-stepper-mode))
      (should (equal header-line-format "Press RET to step. C-h m to see help."))
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-deep-0))
      (racket-tests/press "RET")
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-deep-1))
      (racket-tests/press "RET")
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-deep-2))
      (kill-buffer)
      (delete-file path))))

;;; Indentation

(defun racket-tests/same-indent (file)
  (with-current-buffer (find-file (expand-file-name file
                                                    racket-tests/here-dir))
    (indent-region (point-min) (point-max))
    (let ((ok (not (buffer-modified-p))))
      (revert-buffer t t t)  ;revert in case running ERT interactively
      ok)))

(ert-deftest racket-tests/indent-rkt ()
  "Indentation of example/*.rkt shouldn't change."
  (should (racket-tests/same-indent "racket/example/example.rkt"))
  (should (racket-tests/same-indent "racket/example/indent.rkt")))

;;; Font-lock

(defun racket-tests/same-faceup (file)
  "Test that FILE is fontified as the .faceup file describes.
FILE is interpreted as relative to this source directory."
  (let ((font-lock-maximum-decoration t))
    (faceup-test-font-lock-file 'racket-mode
                                (expand-file-name file
                                                  racket-tests/here-dir))))

(faceup-defexplainer racket-tests/same-faceup)

(ert-deftest racket-tests/font-lock ()
  "Font-lock of example/*.rkt shouldn't change."
  (should (racket-tests/same-faceup "racket/example/indent.rkt"))
  (should (racket-tests/same-faceup "racket/example/example.rkt")))

(provide 'racket-tests)

;;; racket-tests.el ends here
