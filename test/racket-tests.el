;;; racket-tests.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2023 by Greg Hendershott.

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
(require 'compile)
(require 'cl-macs)
(require 'edmacro)
(require 'faceup)
(require 'paredit)
(require 'racket-mode)
(require 'racket-hash-lang)
(require 'racket-xp)
(require 'racket-cmd)
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

(defconst ci-p (getenv "CI")
  "Is there an environment variable saying we're running on CI?")

(defconst racket-tests/timeout (if ci-p 60 10))

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

(defmacro racket-tests/should-eventually (expr)
  "Aside from avoiding some nesting, an advantage of this vs.
composing `should` and eventually is that the `should` macro will
be able to look up an ert-explainer property on the symbol
supplied to it."
  `(progn
     (racket-tests/call-until-true (lambda () ,expr))
     (should ,expr)))

(defun racket-tests/see-back-rx (rx)
  (racket-tests/eventually (looking-back rx (point-min))))

(defun racket-tests/see-forward-rx (rx)
  (racket-tests/eventually (looking-at-p rx)))

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
  (let ((racket-command-timeout racket-tests/timeout))
    (unwind-protect
        (funcall thunk)
      (racket-stop-back-end))))

(defmacro racket-tests/with-back-end-settings (&rest body)
  (declare (indent 0) (debug t))
  `(racket-tests/call-with-back-end-settings (lambda () ,@body)))

;;; REPL

(ert-deftest racket-tests/repl ()
  "Start/exercise/stop REPL without any racket-run."
  (message "racket-tests/repl")
  (racket-tests/with-back-end-settings
    (racket-repl)
    (racket-tests/eventually (racket--repl-session-id))
    (with-racket-repl-buffer
      (should
       (racket-tests/see-back-rx
        (rx "Welcome to Racket v" (+ (any digit ".")) (? " [" (or "bc" "cs") "]") ".\n"
            (? "Warning: GUI programs might not work correctly because\n"
               "your version of Racket lacks `current-get-interaction-evt`,\n"
               "which was added in Racket 8.4.\n")
            "> ")))

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

      ;;; This behavior no longer expected
      ;; ;; Multiple expressions at one prompt should produce multiple
      ;; ;; values, one per line.
      ;; (racket-tests/type&press "1 2 3" "RET")
      ;; (should (racket-tests/see-back "1\n2\n3\n> "))
      ;; (racket-tests/type&press "(+ 1) (+ 2) (+ 3)" "RET")
      ;; (should (racket-tests/see-back "1\n2\n3\n> "))
      ;; (racket-tests/type&press "\"1\" '2 #(3)" "RET")
      ;; (should (racket-tests/see-back "\"1\"\n2\n'#(3)\n> "))

      ;; A trailing space should not cause a hang until another
      ;; expression is entered.
      (racket-tests/type&press "1 " "RET")
      (should (racket-tests/see-back "1\n> "))

      ;; `racket-smart-open-bracket-mode'.
      ;;
      ;; For `paredit-mode' we test using the configuration we
      ;; document in doc/racket-mode.org; see #647.
      (dolist (k '("RET" "C-m" "C-j"))
        (define-key paredit-mode-map (kbd k) nil))
      (let ((typing   "[cond [[values 1] #t] [else #f]]")
            (expected "(cond [(values 1) #t] [else #f])\n#t\n> "))
        (mapc (lambda (modes)
                (racket-smart-open-bracket-mode -1)
                (electric-pair-mode (if (car modes) 1 -1))
                (if (cdr modes) (enable-paredit-mode) (disable-paredit-mode))
                ;; Enable /after/ enabling other mode, as our doc
                ;; string tells users to do.
                (racket-smart-open-bracket-mode 1)
                (racket-tests/type&press typing "RET")
                (should (racket-tests/see-back expected)))
              (list (cons t   nil)      ;just `electric-pair-mode'
                    (cons nil t)        ;just `paredit-mode'
                    (cons nil nil))))   ;neither/plain

      ;; Exit
      (racket-tests/type&press "(exit)" "RET")
      (should (racket-tests/see-back
               "REPL session ended\n"))
      (kill-buffer))))

;;; Multi REPLs

(ert-deftest racket-test/unique-repls ()
  "Exercise one unique REPL per racket-mode buffer.
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
  (dolist (edit-mode (list #'racket-mode #'racket-hash-lang-mode))
    (racket-tests/with-back-end-settings
     (let* ((path (make-temp-file "test" nil ".rkt"))
            (name (file-name-nondirectory path))
            (code "#lang racket/base\n(define foobar 42)\nfoobar\n"))
       (write-region code nil path nil 'no-wrote-file-message)
       (find-file path)
       (funcall edit-mode)
       ;; On older Rackets racket-hash-lang-mode may fail gracefully
       ;; down to prog-mode, but only eventually, after back end
       ;; responds.
       (racket-tests/should-eventually
        (cl-case major-mode
          ((racket-hash-lang-mode) (not buffer-read-only)) ;keep waiting
          ((racket-mode prog-mode) t)))
       (unless (eq major-mode 'prog-mode)
         (racket-run)
         (racket-tests/should-eventually (racket--repl-session-id))
         (with-racket-repl-buffer
           (should (racket-tests/see-back (concat "\n" name "> ")))
           (racket-repl-exit)
           (should (racket-tests/see-back
                    "REPL session ended\n"))
           (kill-buffer)))
       (kill-buffer)
       (delete-file path)))))

;;; Profile

(ert-deftest racket-tests/profile ()
  "Exercise `racket-profile'."
  (skip-unless (not ci-p))
  (message "racket-tests/profile")
  (racket-tests/with-back-end-settings
    (let* ((path (make-temp-file "test" nil ".rkt"))
           (name (file-name-nondirectory path))
           (code "#lang racket/base\n(define foobar 42)\nfoobar\n"))
      (write-region code nil path nil 'no-wrote-file-message)
      (find-file path)
      (racket-tests/should-eventually (equal buffer-file-name path))
      (racket-tests/should-eventually (equal major-mode 'racket-mode))
      ;; Capture racket-repl-buffer-name from the `racket-mode' buffer,
      ;; before `racket-profile' protentially creates/displays/selects
      ;; the `racket-profile-mode' buffer.
      (let ((repl-name racket-repl-buffer-name))
        (racket-profile)
        (racket-tests/should-eventually (get-buffer repl-name))
        (with-current-buffer repl-name
          (racket-tests/should-eventually
           (racket-tests/see-back (concat "\n" name "> "))))

        (racket-tests/should-eventually (get-buffer "*Racket Profile </>*"))
        (with-current-buffer (get-buffer "*Racket Profile </>*")
          (racket-tests/should-eventually (eq major-mode 'racket-profile-mode))
          (kill-buffer))

        (with-current-buffer repl-name
          (racket-repl-exit)
          (should (racket-tests/see-back
                   "REPL session ended\n"))
          (kill-buffer))
        (kill-buffer)
        (delete-file path)))))

;;; racket-xp-mode

(ert-deftest racket-tests/xp ()
  (message "racket-tests/xp")
  (racket-tests/with-back-end-settings
    (let* ((path (make-temp-file "test" nil ".rkt"))
           (code "#lang racket/base\n(define foobar 42)\nfoobar\n"))
      (write-region code nil path nil 'no-wrote-file-message)
      (find-file path)
      ;; In case running test interactively in Emacs where the config
      ;; loads `racket-xp-mode' automatically, disable it first.
      (racket-xp-mode 0)
      (racket-xp-mode 1)
      (should racket-xp-mode)
      (racket-tests/should-eventually
       (progn (goto-char (point-min))
              (racket-xp-next-definition)
              (looking-at-p (regexp-quote "racket/base"))))
      (racket-xp-next-definition)
      (should (racket-tests/see-forward "foobar"))
      (should (equal (get-text-property (point) 'help-echo) "1 bound occurrence"))
      (racket-xp-next-use)
      (should (racket-tests/see-forward "foobar"))
      (should (equal (get-text-property (point) 'help-echo) "defined locally"))
      (goto-char (point-max))
      (insert "foo")
      (completion-at-point)
      (should (racket-tests/see-back "foobar"))
      (racket-xp-mode 0)
      (kill-buffer)
      (delete-file path))))

;;; Debugger

(ert-deftest racket-tests/debugger ()
  (message "racket-tests/debugger")
  (dolist (edit-mode (list #'racket-mode #'racket-hash-lang-mode))
    (racket-tests/with-back-end-settings
     (let* ((path (make-temp-file "test" nil ".rkt"))
            (name (file-name-nondirectory path))
            (code "#lang racket/base\n(define (f x) (+ 1 x))\n(f 41)\n"))
       (write-region code nil path nil 'no-wrote-file-message)
       (find-file path)
       (funcall edit-mode)
       ;; On older Rackets racket-hash-lang-mode may fail gracefully
       ;; down to prog-mode, but only eventually, after back end
       ;; responds.
       (racket-tests/should-eventually
        (cl-case major-mode
          ((racket-hash-lang-mode) (not buffer-read-only)) ;keep waiting
          ((racket-mode prog-mode) t)))
       (unless (eq major-mode 'prog-mode)
         (racket-run `(16))
         (racket-tests/should-eventually (get-buffer racket-repl-buffer-name))
         (racket-tests/should-eventually (racket--repl-session-id))
         (racket-tests/should-eventually racket-debug-mode)

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

         (racket-debug-step)              ;no more debug breaks left
         (with-racket-repl-buffer
           (should (racket-tests/see-back (concat "\n" name "> "))))
         (should (racket-tests/see-char-property (point) 'after-string
                                                 nil))
         (should-not racket-debug-mode)
         (with-racket-repl-buffer
           (racket-repl-exit)
           (should (racket-tests/see-back
                    "REPL session ended\n"))
           (kill-buffer)))
       (kill-buffer)
       (delete-file path)))))

;;; For both "shallow" and "deep" macro stepper tests

(defun racket-tests/racket-7.6-p ()
  "Is `racket-program' \"7.6\" or  \"7.6 [cs]\"?"
  (string-match-p
   (rx bos "Welcome to Racket v7.6" (? " [cs]") ".")
   (with-temp-buffer
     (call-process racket-program nil t nil "--version")
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun racket-tests/expected-result-for-expand-file-p (result)
  "Test expected to fail because macro-debugger broken in Racket 7.6.
For use with :expected-result '(satisfies PRED). This matters
because ert-deftest is a macro evaluated at compile time, and we
want to use the value of `racket-program' at run time."
  (if (racket-tests/racket-7.6-p)
      (ert-test-failed-p result)
    (ert-test-passed-p result)))

(defun racket-tests/racket-8.10-or-newer-p ()
  (zerop
   (call-process
    racket-program nil nil nil
    "-e" "(require version/utils) (unless (version<=? \"8.10\" (version)) (exit 255))")))

(defun racket-tests/expected-result-for-expand-expression-p (result)
  "Test expected to fail because expansion differs in older Racket."
  (if (racket-tests/racket-8.10-or-newer-p)
      (ert-test-passed-p result)
    (ert-test-failed-p result)))

;;; Macro stepper: File "shallow"

(defconst racket-tests/expand-mod-name "foo")

(defconst racket-tests/expand-shallow-0
  "«:button:racket-expand-hiding»: standard

«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

")

(defconst racket-tests/expand-shallow-1
  "«:button:racket-expand-hiding»: standard

«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:Final»
(module foo racket/base (#%module-begin (define x 42) x))
")

(ert-deftest racket-tests/expand-file-shallow ()
  :expected-result '(satisfies racket-tests/expected-result-for-expand-file-p)
  (message "racket-tests/expand-file-shallow")
  (racket-tests/with-back-end-settings
    (let* ((dir  (make-temp-file "test" t))
           (path (concat dir "/" racket-tests/expand-mod-name ".rkt"))
           (code "#lang racket/base\n(define x 42)\nx"))
      (write-region code nil path nil 'no-wrote-file-message)
      (find-file path)
      (let ((racket-expand-hiding 'standard))
        (racket-expand-file))
      (set-buffer "*Racket Stepper </>*")
      (should (eq major-mode 'racket-stepper-mode))
      (should (equal header-line-format "Press RET to step. C-u RET to step all. C-h m to see help."))
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-shallow-0))
      (racket-tests/press "RET")
      (racket-tests/should-eventually
       (faceup-test-font-lock-buffer nil racket-tests/expand-shallow-1))
      (kill-buffer)
      (delete-file path))))

;;; Macro stepper: File "deep"

(defconst racket-tests/expand-deep-0
  "«f:macro hiding disabled by command prefix»

«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

")

(defconst racket-tests/expand-deep-1
  "«f:macro hiding disabled by command prefix»

«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:1: Macro transformation»
«x:@@ -1 +1,9 @@»
«:diff-removed:-(module foo racket/base (#%module-begin (define x 42) x))»
«:diff-added:+(module foo racket/base»
«:diff-added:+  (printing:module-begin:1»
«:diff-added:+   (module:1»
«:diff-added:+    configure-runtime:1»
«:diff-added:+    '#%kernel:1»
«:diff-added:+    (#%require:1 racket/runtime-config:1)»
«:diff-added:+    (configure:1 #f))»
«:diff-added:+   (define x 42)»
«:diff-added:+   x))»

")

(defconst racket-tests/expand-deep-2
  "«f:macro hiding disabled by command prefix»

«f:Original»
(module foo racket/base (#%module-begin (define x 42) x))

«f:1: Macro transformation»
«x:@@ -1 +1,9 @@»
«:diff-removed:-(module foo racket/base (#%module-begin (define x 42) x))»
«:diff-added:+(module foo racket/base»
«:diff-added:+  (printing:module-begin:1»
«:diff-added:+   (module:1»
«:diff-added:+    configure-runtime:1»
«:diff-added:+    '#%kernel:1»
«:diff-added:+    (#%require:1 racket/runtime-config:1)»
«:diff-added:+    (configure:1 #f))»
«:diff-added:+   (define x 42)»
«:diff-added:+   x))»

«f:2: Macro transformation»
«x:@@ -1,9 +1,11 @@»
 (module foo racket/base
«:diff-removed:-  (printing:module-begin:1»
«:diff-removed:-   (module:1»
«:diff-removed:-    configure-runtime:1»
«:diff-removed:-    '#%kernel:1»
«:diff-removed:-    (#%require:1 racket/runtime-config:1)»
«:diff-removed:-    (configure:1 #f))»
«:diff-removed:-   (define x 42)»
«:diff-removed:-   x))»
«:diff-added:+  (#%module-begin:2»
«:diff-added:+   (do-wrapping-module-begin:2»
«:diff-added:+    print-result:2»
«:diff-added:+    (module:1»
«:diff-added:+     configure-runtime:1»
«:diff-added:+     '#%kernel:1»
«:diff-added:+     (#%require:1 racket/runtime-config:1)»
«:diff-added:+     (configure:1 #f)))»
«:diff-added:+   (do-wrapping-module-begin:2 print-result:2 (define x 42))»
«:diff-added:+   (do-wrapping-module-begin:2 print-result:2 x)))»

")

(unless (eq system-type 'windows-nt)    ;requires `diff` program
  (ert-deftest racket-tests/expand-file-deep ()
    :expected-result '(satisfies racket-tests/expected-result-for-expand-file-p)
    (message "racket-tests/expand-file-deep")
    (racket-tests/with-back-end-settings
      (let* ((dir  (make-temp-file "test" t))
             (path (concat dir "/" racket-tests/expand-mod-name ".rkt"))
             (code "#lang racket/base\n(define x 42)\nx"))
        (write-region code nil path nil 'no-wrote-file-message)
        (find-file path)
        (racket-expand-file 4) ;; i.e. C-u prefix
        (set-buffer "*Racket Stepper </>*")
        (should (eq major-mode 'racket-stepper-mode))
        (should (equal header-line-format "Press RET to step. C-u RET to step all. C-h m to see help."))
        (racket-tests/should-eventually
         (faceup-test-font-lock-buffer nil racket-tests/expand-deep-0))
        (racket-tests/press "RET")
        (racket-tests/should-eventually
         (faceup-test-font-lock-buffer nil racket-tests/expand-deep-1))
        (racket-tests/press "RET")
        (racket-tests/should-eventually
         (faceup-test-font-lock-buffer nil racket-tests/expand-deep-2))
        (kill-buffer)
        (delete-file path)))))

;;; Macro stepper: Expression

(defconst racket-tests/expand-expression-original
  "«f:macro hiding disabled by command prefix»

«f:Original»
(cond ((< 1 2) #t) (else #f))

")

(defconst racket-tests/expand-expression-final
  "«f:macro hiding disabled by command prefix»

«f:Original»
(cond ((< 1 2) #t) (else #f))

«f:1: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(cond ((< 1 2) #t) (else #f))»
«:diff-added:+(if:1 (< 1 2) (let-values:1 () #t) (let-values:1 () #f))»

«f:2: Add explicit #%app»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (< 1 2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app < 1 2) (let-values:1 () #t) (let-values:1 () #f))»

«f:3: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app < 1 2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < 1 2) (let-values:1 () #t) (let-values:1 () #f))»

«f:4: Add explicit #%datum»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < 1 2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < (#%datum . 1) 2) (let-values:1 () #t) (let-values:1 () #f))»

«f:5: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < (#%datum . 1) 2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 2) (let-values:1 () #t) (let-values:1 () #f))»

«f:6: Add explicit #%datum»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 (#%datum . 2)) (let-values:1 () #t) (let-values:1 () #f))»

«f:7: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 (#%datum . 2)) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 '2) (let-values:1 () #t) (let-values:1 () #f))»

«f:8: Add explicit #%datum»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 '2) (let-values:1 () #t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 '2) (let-values:1 () (#%datum . #t)) (let-values:1 () #f))»

«f:9: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 '2) (let-values:1 () (#%datum . #t)) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () #f))»

«f:10: Add explicit #%datum»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () #f))»
«:diff-added:+(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () (#%datum . #f)))»

«f:11: Macro transformation»
«x:@@ -1 +1 @@»
«:diff-removed:-(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () (#%datum . #f)))»
«:diff-added:+(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () '#f))»

«f:Final»
(if:1 (#%app:2 < '1 '2) (let-values:1 () '#t) (let-values:1 () '#f))
")

(unless (eq system-type 'windows-nt)    ;requires `diff` program
  (ert-deftest racket-tests/expand-expression ()
    :expected-result '(satisfies racket-tests/expected-result-for-expand-expression-p)
    (message "racket-tests/expand-expression")
    (racket-tests/with-back-end-settings
      (let* ((path (make-temp-file "test" nil ".rkt"))
             (name (file-name-nondirectory path))
             (code "#lang racket/base\n(cond [(< 1 2) #t] [else #f])\n"))
        (write-region code nil path nil 'no-wrote-file-message)
        (find-file path)

        (racket-run)
        (racket-tests/should-eventually (get-buffer racket-repl-buffer-name))
        (racket-tests/should-eventually (racket--repl-session-id))
        (with-racket-repl-buffer
          (should (racket-tests/see-back (concat "\n" name "> "))))

        (goto-char (point-max))         ;after the cond expression
        (racket-expand-last-sexp 4) ;; i.e. C-u prefix
        (set-buffer "*Racket Stepper </>*")
        (should (eq major-mode 'racket-stepper-mode))
        (should (equal header-line-format "Press RET to step. C-u RET to step all. C-h m to see help."))

        (racket-tests/should-eventually
         (faceup-test-font-lock-buffer nil racket-tests/expand-expression-original))

        (racket-tests/press "C-u RET")
        (racket-tests/should-eventually
         (faceup-test-font-lock-buffer nil racket-tests/expand-expression-final))

        (quit-window)
        (with-racket-repl-buffer
          (racket-repl-exit)
          (should (racket-tests/see-back
                   "REPL session ended\n"))
          (kill-buffer))

        (kill-buffer)
        (delete-file path)))))

;;; Indentation correctness

(defun racket-tests/indent-region ()
  "Indent entire current buffer, suppressing progress-reporter messages."
  (cl-letf (((symbol-function #'make-progress-reporter)   #'ignore)
            ((symbol-function #'progress-reporter-update) #'ignore)
            ((symbol-function #'progress-reporter-done)   #'ignore))
    (indent-region (point-min) (point-max))))

(defun racket-tests/same-indent (file)
  (with-current-buffer (find-file (expand-file-name file racket-tests/here-dir))
    (racket-tests/indent-region)
    (let ((ok (not (buffer-modified-p))))
      (revert-buffer t t t)  ;revert in case running ERT interactively
      ok)))

(ert-deftest racket-tests/indent-same ()
  "Indentation of example/*.rkt shouldn't change."
  (should (racket-tests/same-indent "example/example.rkt"))
  (should (racket-tests/same-indent "example/indent.rkt")))

;;; Indentation speed

;; To measure the performance of `racket-indent-line', these tests
;; call `indent-region' on some examples of unusually large files.
;; Re-indenting huge files isn't a priority use case for Racket Mode.
;; We don't even bother to supply an `indent-region-function'. Indeed
;; these tests rely on the fact that `indent-region' will therefore
;; call `racket-indent-line' for every one of the thousands of lines
;; in these files. The huge example files are just a convenient way to
;; get a large, varied amount of test data.

(defun racket-tests/indent-time (file)
  (with-current-buffer (find-file (expand-file-name file racket-tests/here-dir))
    (racket-mode)
    (let* ((start (float-time (get-internal-run-time)))
           (_ (racket-tests/indent-region))
           (finish (float-time (get-internal-run-time)))
           (dur (- finish start)))
      (message "indent %s %f seconds cpu"
               file dur)
      (revert-buffer t t t)
      dur)))

(ert-deftest racket-tests/indent-speed-1 ()
  (skip-unless (not ci-p))
  (should (< (racket-tests/indent-time "example/class-internal.rkt") 10)))

(ert-deftest racket-tests/indent-speed-2 ()
  (skip-unless (not ci-p))
  (should (< (racket-tests/indent-time "example/core.scm") 10)))

;;; Font-lock

(defun racket-tests/same-faceup (file)
  "Test that FILE is fontified as the .faceup file describes.
FILE is interpreted as relative to this source directory."
  (let ((font-lock-maximum-decoration t)
        (faceup-properties '(face syntax-table)))
    (faceup-test-font-lock-file 'racket-mode
                                (expand-file-name file racket-tests/here-dir))))

(faceup-defexplainer racket-tests/same-faceup)

(ert-deftest racket-tests/font-lock ()
  "Font-lock of example/*.rkt shouldn't change."
  (should (racket-tests/same-faceup "example/indent.rkt"))
  (should (racket-tests/same-faceup "example/example.rkt")))

;;; fill-paragraph comment issue 437

(ert-deftest racket-tests/fill-paragraph-comment ()
  (with-current-buffer (get-buffer-create "issue-437.rkt")
    (racket-mode)
    (insert ";; blah blah blah\n")
    (insert ";; blah blah blah\n")
    (forward-line -1)
    (fill-paragraph)
    (goto-char (point-max))
    (should (racket-tests/see-back
             ";; blah blah blah blah blah blah\n"))
    (kill-buffer (current-buffer))))

(ert-deftest racket-tests/cmd-read ()
  "Exercise `racket--cmd-read' with randomly generated and chunked sexprs."
  (dotimes (_ 10)
    (with-temp-buffer
      (let* ((num-top-level-sexprs 3000)
             (orig (mapcar (lambda (_)
                             (mapcar (lambda (_)
                                       (if (zerop (random 2))
                                           (random 1000)
                                         (make-string (+ 5 (random 10))
                                                      (+ ?a (random 26)))))
                                     (make-list (+ 10 (random 90)) nil)))
                           (make-list num-top-level-sexprs nil)))
             (orig-as-str (apply #'concat
                                 (mapcar (lambda (s) (concat s "\n"))
                                         (mapcar #'prin1-to-string
                                                 orig))))
             (results nil))
        ;; Simulate the process filter getting the text in arbitrary
        ;; chunks, inserting into the buffer, and calling
        ;; `racket--cmd-read' to read any available complete top-level
        ;; sexprs.
        (cl-flet ((add/read (str)
                            (goto-char (point-max))
                            (insert str)
                            (racket--cmd-read (lambda (v) (push v results)))))
          (while (< 0 (length orig-as-str))
            (let ((len (max 1 (random (length orig-as-str)))))
              (add/read (substring orig-as-str 0 len))
              (setq orig-as-str (substring orig-as-str len))))
          (setq results (reverse results))
          (should (equal orig results)))))))

(provide 'racket-tests)

;;; racket-tests.el ends here
