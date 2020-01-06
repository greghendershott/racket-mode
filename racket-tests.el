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
(require 'racket-repl)
(require 'racket-edit)
(require 'racket-check-syntax)
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

(defconst racket-tests/connect-attempts (if ci-p (* 15 60) (* 2 60))
  "Attempts to connect to command server. Very long when running on CI.")

(defconst racket-tests/connect-timeout (if ci-p (* 15 60) (* 2 60))
  "Timeout waiting for connect to command server. Very long when running on CI.")

(defconst racket-tests/command-timeout (if ci-p (* 15 60) 30)
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

(defun racket-tests/see-rx (rx)
  (with-timeout (racket-tests/command-timeout nil)
    (while (not (looking-back rx (point-min)))
      (sit-for 1))
    t))

(defun racket-tests/see (str)
  (racket-tests/see-rx (regexp-quote str)))

(defun racket-tests/explain-see (_str)
  `(actual . ,(buffer-substring-no-properties
               (point-min)
               (point))))
(put 'racket-tests/see-rx 'ert-explainer #'racket-tests/explain-see)
(put 'racket-tests/see    'ert-explainer #'racket-tests/explain-see)

(defun racket-tests/next-free-port ()
  ;; (1+ racket-command-port) is used by the logging server so skip
  ;; that, too!
  (+ racket-command-port 2))

(defun racket-tests/wait-for-command-server ()
  (with-timeout (racket-tests/connect-timeout)
    (while (not (racket--cmd-open-p)) (sit-for 1))))

;;; REPL

(ert-deftest racket-tests/repl ()
  "Start REPL. Confirm we get Welcome message and prompt. Exit REPL."
  (let ((tab-always-indent 'complete)
        (racket--cmd-connect-attempts racket-tests/connect-attempts)
        (racket-command-port (racket-tests/next-free-port))
        (racket-command-timeout racket-tests/command-timeout))
    (racket-repl)
    (with-racket-repl-buffer
      (should (racket-tests/see-rx
               "Welcome to Racket v?[0-9.]+\\(?: \\[cs\\].\\)?[\n]\\(?:;.*[\n]\\)*> "))
      (racket-tests/wait-for-command-server)

      ;; Completion
      (racket-tests/type&press "with-inp" "TAB")
      (should (racket-tests/see "with-input-from-file"))
      (racket-tests/press "RET")
      (should (racket-tests/see "#<procedure:with-input-from-file>\n> "))

      ;; Multiline expression indent
      (racket-tests/type&press "(if 1" "C-j")
      (should (racket-tests/see "(if 1\n      "))
      (racket-tests/type&press "2" "C-j")
      (should (racket-tests/see "2\n      "))
      (racket-tests/type&press "3)" "RET")
      (should (racket-tests/see "3)\n2\n> "))

      ;; Smart open bracket
      (let ((typing   "[cond [[values 1] #t] [else #f]]")
            (expected "(cond [(values 1) #t] [else #f])\n#t\n> "))
        (racket-smart-open-bracket-mode 1)
        (mapc (lambda (modes)
                (electric-pair-mode (if (car modes) 1 -1))
                (if (cdr modes) (enable-paredit-mode) (disable-paredit-mode))
                (racket-tests/type&press typing "RET")
                (should (racket-tests/see expected)))
              (list (cons nil nil)
                    (cons t   nil)
                    (cons nil t))))

      ;; Exit
      (racket-tests/type&press "(exit)" "RET")
      (should (racket-tests/see "Process Racket REPL finished\n")))))

;;; Run

(ert-deftest racket-tests/run ()
  (let* ((racket--cmd-connect-attempts racket-tests/connect-attempts)
         (racket-command-port (racket-tests/next-free-port))
         (racket-command-timeout racket-tests/command-timeout)
         (pathname (make-temp-file "test" nil ".rkt"))
         (name     (file-name-nondirectory pathname))
         (code "#lang racket/base\n(define x 42)\nx\n"))
    (write-region code nil pathname nil 'no-wrote-file-message)
    (find-file pathname)
    (racket-run)
    ;; see expected prompt
    (with-racket-repl-buffer
      (should (racket-tests/see (concat "\n" name "> "))))
    (racket-tests/wait-for-command-server)
    ;; racket-check-syntax-mode
    (when (version<= "6.2" (racket--version))
      (racket-check-syntax-mode 1)
      ;; check-syntax-mode sets header-line-format, so wait for that:
      (with-timeout (racket-tests/command-timeout)
        (while (not header-line-format) (sit-for 1)))
      (goto-char (point-min))
      (racket-check-syntax-next-def)
      (should (looking-at "racket/base"))
      (racket-check-syntax-next-use)
      (should (looking-at "define"))
      (racket-check-syntax-mode 0))
    ;; Exit
    ;; (with-racket-repl-buffer
    ;;   (racket-tests/type&press "(exit)" "RET"))
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
