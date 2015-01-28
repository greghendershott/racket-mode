(require 'racket-mode)

;; Ensure the faceup package is installed, e.g. on Travis CI.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'faceup)
  (condition-case ()
      (package-install 'faceup)
    (error (package-refresh-contents)
           (package-install 'faceup))))
(require 'faceup)

(defconst racket-tests/here-dir (faceup-this-file-directory)
  "The directory this file is located in.")

;;; REPL

(ert-deftest racket-tests/repl ()
  "Start REPL. Confirm we get Welcome message and prompt. Exit REPL."
  (racket-repl)
  (dotimes (_ 5) (accept-process-output nil 1))
  (with-current-buffer (get-buffer "*Racket REPL*")
    (should (racket-tests/match "^Welcome to Racket v[0-9.]+\n≺≻ "))
    (insert "(exit)")
    (racket-repl-eval-or-newline-and-indent)
    (dotimes (_ 5) (accept-process-output nil 1))
    (should (racket-tests/match "Process Racket REPL finished\n$"))
    (kill-buffer "*Racket REPL*")))

(defun racket-tests/match (regexp)
  "Return `string-match' of regexp on entire non-property buffer text."
  (string-match regexp
                (buffer-substring-no-properties (point-min)
                                                (point-max))))

;;; Indentation

(defun racket-tests/same-indent (file)
  (with-current-buffer (find-file (concat racket-tests/here-dir file))
    (indent-region (point-min) (point-max))
    (not (buffer-modified-p))))

(ert-deftest racket-tests/indent-rkt ()
  "Indentation of example/*.rkt shouldn't change."
  (should (racket-tests/same-indent "example/example.rkt"))
  (should (racket-tests/same-indent "example/indent.rkt")))

;;; Font-lock

(defun racket-tests/same-faceup (file)
  "Test that FILE is fontified as the .faceup file describes.
FILE is interpreted as relative to this source directory."
  (faceup-test-font-lock-file 'racket-mode
                              (concat racket-tests/here-dir file)))

(faceup-defexplainer racket-tests/same-faceup)

(ert-deftest racket-tests/font-lock ()
  "Font-lock of example/*.rkt shouldn't change."
  (should (racket-tests/same-faceup "example/indent.rkt"))
  (should (racket-tests/same-faceup "example/example.rkt")))

;;; Smart open bracket

(ert-deftest racket-tests/smart-open-bracket ()
  "Just a simple test the `cond` form with smart open bracket
disabled vs. enabled. Currently this is really just a regression
test for bug #81. This could be expanded into a series of
exhaustive tests of all the special forms it handles."
  (let ((input "[cond [[number? x] #t]\n[else #f]]"))
    ;; When smart open bracket mode is disabled, result should be
    ;; exactly what was input.
    (with-temp-buffer
      (racket-mode)
      (let ((racket-smart-open-bracket-enable nil))
        (insert input)
        (equal (buffer-substring-no-properties (point-min) (point-max))
               input)))
    ;; When smart open bracket mode is enabled, result should be
    ;; be the adjusted string below.
    (with-temp-buffer
      (racket-mode)
      (let ((racket-smart-open-bracket-enable nil))
        (insert input)
        (equal (buffer-substring-no-properties (point-min) (point-max))
               "(cond [(number? x) #t]\n[else #f])")))))

(provide 'racket-tests)
