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
(require 'racket-xp)
(require 'racket-common)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-smart-open)

(defconst racket-tests/here-dir (faceup-this-file-directory)
  "The directory this file is located in.")

;;; Utility functions for "integration" testing

(defconst ci-p (or (getenv "TRAVIS_CI")
                   (getenv "CI"))
  "Is there an environment variable saying we're running on CI?")

(defconst racket-tests/command-timeout (if ci-p (* 5 60) 30)
  "Timeout for synchronous commands. Very long when running on CI.")

(defun racket-tests/type (typing)
  (let ((blink-matching-paren nil)) ;suppress "Matches " messages
    (execute-kbd-macro (string-to-vector typing))
    (redisplay)))

(defun racket-tests/press (binding)
  (racket-tests/type (edmacro-parse-keys binding)))

(defun racket-tests/type&press (typing binding)
  (racket-tests/type typing)
  (racket-tests/press binding))

(defun racket-tests/eventually (proc &rest args)
  (with-timeout (racket-tests/command-timeout nil)
    (while (not (apply proc args))
      (accept-process-output)
      (sit-for 1))
    t))

(defun racket-tests/see-back-rx (rx)
  (racket-tests/eventually #'looking-back rx (point-min)))

(defun racket-tests/see-forward-rx (rx)
  (racket-tests/eventually #'looking-at rx))

(defun racket-tests/see-back (str)
  (racket-tests/see-back-rx (regexp-quote str)))

(defun racket-tests/see-forward (str)
  (racket-tests/see-forward-rx (regexp-quote str)))

(defun racket-tests/explain-see (_str &optional _dir)
  `(actual . ,(buffer-substring-no-properties
               (point-min)
               (point))))

(dolist (sym '(racket-tests/see-back-rx
               racket-tests/see-forward-rx
               racket-tests/see-back
               racket-tests/see-forward))
  (put sym 'ert-explainer #'racket-tests/explain-see))

(defun racket-tests/next-free-port ()
  ;; (1+ racket-command-port) is used by the logging server so skip
  ;; that, too!
  (+ racket-command-port 2))

(defun racket-tests/wait-for-command-server ()
  (racket-tests/eventually #'racket--cmd-open-p))

;;; REPL

(ert-deftest racket-tests/repl ()
  "Start REPL. Confirm we get Welcome message and prompt. Exit REPL."
  (let ((racket-command-port (racket-tests/next-free-port))
        (racket-command-timeout racket-tests/command-timeout))
    (racket-repl)
    (racket-tests/eventually #'get-buffer racket--repl-buffer-name)
    (with-racket-repl-buffer
      (should (racket-tests/see-back-rx
               "Welcome to Racket v?[0-9.]+\\(?: \\[cs\\].\\)?[\n]\\(?:;.*[\n]\\)*> "))

      ;; Completion
      (should
       (racket-tests/eventually #'member
                                "current-output-port"
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
      (should (racket-tests/see-back "Process *Racket REPL* connection broken by remote peer\n")))))

;;; Run

(ert-deftest racket-tests/run ()
  (let* ((racket-command-port (racket-tests/next-free-port))
         (racket-command-timeout racket-tests/command-timeout)
         (pathname (make-temp-file "test" nil ".rkt"))
         (name     (file-name-nondirectory pathname))
         (code "#lang racket/base\n(define foobar 42)\nfoobar\n"))
    (write-region code nil pathname nil 'no-wrote-file-message)
    (find-file pathname)
    (racket-run)
    (racket-tests/eventually #'get-buffer racket--repl-buffer-name)
    (with-racket-repl-buffer
      (should (racket-tests/see-back (concat "\n" name "> ")))
      (racket-repl-exit)
      (should (racket-tests/see-back "Process *Racket REPL* connection broken by remote peer\n")))
    (delete-file pathname)))

;;; racket-xp-mode

(ert-deftest racket-tests/xp ()
  (let* ((racket-command-port (racket-tests/next-free-port))
         (racket-command-timeout racket-tests/command-timeout)
         (pathname (make-temp-file "test" nil ".rkt"))
         (name     (file-name-nondirectory pathname))
         (code     "#lang racket/base\n(define foobar 42)\nfoobar\n"))
    (write-region code nil pathname nil 'no-wrote-file-message)
    (find-file pathname)
    ;; In case running test interactively in Emacs where the config
    ;; loads `racket-xp-mode' automatically, disable it first.
    (racket-xp-mode 0)
    (racket-xp-mode 1)
    (should racket-xp-mode)
    (racket-tests/wait-for-command-server) ;should start automatically
    (sit-for (if ci-p 30.0 3.0)) ;wait for annotations
    (goto-char (point-min))
    (racket-xp-next-definition)
    (should (racket-tests/see-forward "racket/base"))
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
    (delete-file pathname)))

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
