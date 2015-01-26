(require 'racket-mode)

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

(ert-deftest racket-tests/indent/example-rkt ()
  "Indentation of example/example.rkt shouldn't change."
  (with-current-buffer (find-file "example/example.rkt")
    (indent-region (point-min) (point-max))
    (should-not (buffer-modified-p))))

(ert-deftest racket-tests/indent/indent-rkt ()
  "Indentation of example/indent.rkt shouldn't change."
  (with-current-buffer (find-file "example/indent.rkt")
    (indent-region (point-min) (point-max))
    (should-not (buffer-modified-p))))

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
