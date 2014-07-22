;;; racket-eval.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

(require 'racket-repl)

(defun racket--eval (str)
  (racket-repl)
  (racket--repl-forget-errors)
  (comint-send-string (racket--get-repl-buffer-process) str)
  (racket--repl-show-and-move-to-end))

(defun racket--eval/buffer (expression)
  "Eval EXPRESSION in the *Racket REPL* buffer, but redirect the
resulting output to a temporary output buffer, and return that
buffer's name."
  (cond ((racket--get-repl-buffer-process)
         (let ((output-buffer "*Racket REPL Redirected Output*"))
           (with-current-buffer (get-buffer-create output-buffer)
             (erase-buffer)
             (comint-redirect-send-command-to-process
              expression
              output-buffer
              (racket--get-repl-buffer-process)
              nil ;echo?
              t)  ;no-display?
             ;; Wait for the process to complete
             (set-buffer (process-buffer (racket--get-repl-buffer-process)))
             (while (null comint-redirect-completed)
               (accept-process-output nil 1))
             output-buffer)))
        (t (message "Need to start REPL"))))

(defun racket--eval/string (expression)
  "Eval EXPRESSION in the *Racket REPL* buffer, but redirect the
resulting output to a temporary output buffer, and return that
output as a string."
  (let ((output-buffer (racket--eval/buffer expression)))
    (with-current-buffer output-buffer
      (goto-char (point-min))
      ;; Skip past the expression, if it was echoed
      (and (looking-at expression)
           (forward-line))
      (buffer-substring (point) (point-max)))))

(defun racket--eval/sexpr (expression)
  "Eval EXPRESSION in the *Racket REPL* buffer, but redirect the
resulting output to a temporary output buffer, and return that
output as a sexpr."
  (eval (read (racket--eval/string expression))))

(defun racket--shell (cmd)
  (let ((w (selected-window)))
    (save-buffer)
    (let ((rw (get-buffer-window "*shell*")))
      (if rw
          (select-window rw)
        (other-window -1)))
    (message (concat cmd "..."))
    (shell)
    (racket-pop-to-buffer-same-window "*shell*")
    (comint-send-string "*shell*" (concat cmd "\n"))
    (select-window w)
    (sit-for 3)
    (message nil)))

(provide 'racket-eval)

;; racket-eval.el ends here
