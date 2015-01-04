(require 'racket-mode)

(ert-deftest racket-tests/repl ()
    "Start REPL. Confirm we get Welcome message and prompt. Exit REPL."
  (racket-repl)
  (dotimes (_ 5) (accept-process-output nil 1))
  (with-current-buffer (get-buffer "*Racket REPL*")
    (should (racket-tests/match "^Welcome to Racket v[0-9.]+\n≺≻ "))
    (insert "(exit)")
    (racket-repl-eval-or-newline-and-indent)
    (dotimes (_ 1) (accept-process-output nil 1))
    (should (racket-tests/match "Process Racket REPL finished\n$"))
    (kill-buffer "*Racket REPL*")))

(defun racket-tests/match (regexp)
  "Return `string-match' of regexp on entire non-property buffer text."
  (string-match regexp
                (buffer-substring-no-properties (point-min)
                                                (point-max))))

(provide 'racket-tests)
