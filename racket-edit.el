;;; racket-edit.el

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

;; racket-mode per se, i.e. the .rkt file buffers

(require 'racket-common)
(require 'racket-complete)
(require 'racket-eval)
(require 'hideshow)

(defun racket-run ()
  "Save and evaluate the buffer in REPL, like DrRacket's Run."
  (interactive)
  (save-buffer)
  (racket--invalidate-completion-cache)
  (racket--invalidate-type-cache)
  (racket--eval (format ",run %s\n" (buffer-file-name))))

(defun racket-racket ()
  "Do `racket <file>` in *shell* buffer."
  (interactive)
  (racket--shell (concat racket-program
                         " "
                         (shell-quote-argument (buffer-file-name)))))

(defun racket-test ()
  "Do (require (submod \".\" test)) in *racket* buffer."
  (interactive)
  (racket-run) ;start fresh, so (require) will have an effect
  (racket--eval
   (format "%S\n"
           `(begin
             (displayln "Running tests...")
             (require (submod "." test))
             (flush-output (current-output-port))))))

(defun racket-raco-test ()
  "Do `raco test -x <file>` in *shell* buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket--shell (concat raco-program
                         " test -x "
                         (shell-quote-argument (buffer-file-name)))))

(defun racket-visit-definition (&optional prefix)
  "Visit definition of symbol at point.

Only works if you've `racket-run' the buffer so that its
namespace is active."
  (interactive "P")
  (let ((sym (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")))
    (when sym
      (racket--do-visit-def-or-mod "def" sym))))

(defun racket--do-visit-def-or-mod (cmd sym)
  "CMD must be \"def\" or \"mod\". SYM must be `symbolp`."
  (let ((result (racket--eval/sexpr (format ",%s %s\n\n" cmd sym))))
    (cond ((and (listp result) (= (length result) 3))
           (racket--push-loc)
           (cl-destructuring-bind (path line col) result
             (find-file path)
             (goto-char (point-min))
             (forward-line (1- line))
             (forward-char col)))
          ((eq result 'kernel)
           (message "`%s' defined in #%%kernel -- source not available." sym))
          ((y-or-n-p "Not found. Run current buffer and try again? ")
           (racket--eval/buffer (format ",run %s\n" (buffer-file-name)))
           (racket--do-visit-def-or-mod cmd sym)))))

(defun racket--get-def-file+line (sym)
  "For use by company-mode 'location option."
  (let ((result (racket--eval/sexpr (format ",def %s\n\n" sym))))
    (cond ((and (listp result) (= (length result) 3))
           (cl-destructuring-bind (path line col) result
             (cons path line)))
          (t nil))))

(defun racket-visit-module (&optional prefix)
  "Visit definition of module at point, e.g. net/url or \"file.rkt\".

Only works if you've `racket-run' the buffer so that its
namespace is active."
  (interactive "P")
  (let* ((v (thing-at-point 'filename)) ;matches both net/url and "file.rkt"
         (v (and v (substring-no-properties v)))
         (v (if (or prefix (not v))
                (read-from-minibuffer "Visit module: " (or v ""))
              v)))
    (racket--do-visit-def-or-mod "mod" v)))

(defun racket-doc (&optional prefix)
  "Find something in Racket's documentation."
  (interactive "P")
  (let ((sym (racket--symbol-at-point-or-prompt prefix "Racket help for: ")))
    (when sym
      (shell-command (concat raco-program
                             " doc "
                             (shell-quote-argument (format "%s" sym)))))))

(defun racket--symbol-at-point-or-prompt (prefix prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or prefix is true."
  (let ((sap (symbol-at-point)))
    (if (or prefix (not sap))
        (read-from-minibuffer prompt (if sap (symbol-name sap) ""))
      sap)))

(defvar racket--loc-stack '())

(defun racket--push-loc ()
  (push (cons (current-buffer) (point))
        racket--loc-stack))

(defun racket-unvisit ()
  "Return from previous `racket-visit-definition' or `racket-visit-module'."
  (interactive)
  (if racket--loc-stack
      (cl-destructuring-bind (buffer . pt) (pop racket--loc-stack)
        (racket-pop-to-buffer-same-window buffer)
        (goto-char pt))
    (message "Stack empty.")))

;;; code folding

;;;###autoload
(add-to-list 'hs-special-modes-alist
             '(racket-mode "(" ")" ";" nil nil))

(defun racket--for-all-tests (verb f)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^(module[+*]? test" (point-max) t)
        (funcall f)
        (cl-incf n)
        (goto-char (match-end 0)))
      (message "%s %d test submodules" verb n))))

(defun racket-fold-all-tests ()
  "Fold (hide) all test submodules."
  (interactive)
  (racket--for-all-tests "Folded" 'hs-hide-block))

(defun racket-unfold-all-tests ()
  "Unfold (show) all test submodules."
  (interactive)
  (racket--for-all-tests "Unfolded" 'hs-show-block))


;;; macro expansion

(defun racket-expand-region (start end &optional prefix)
  "Like `racket-send-region', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "rP")
  (if (region-active-p)
      (progn
        (racket--repl-send-expand-command prefix)
        (racket--send-region-to-repl start end))
    (beep)
    (message "No region.")))

(defun racket-expand-definition (&optional prefix)
  "Like `racket-send-definition', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-definition))

(defun racket-expand-last-sexp (&optional prefix)
  "Like `racket-send-last-sexp', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-last-sexp))

(defun racket--repl-send-expand-command (prefix)
  (comint-send-string (racket--get-repl-buffer-process)
                      (if prefix ",exp!" ",exp ")))

(defun racket-expand-again ()
  "Macro expand again the previous expansion done by one of:
- `racket-expand-region'
- `racket-expand-definition'
- `racket-expand-last-sexp'
- `racket-expand-again'"
  (interactive)
  (comint-send-string (racket--get-repl-buffer-process) ",exp+\n"))

(defun racket-gui-macro-stepper ()
  "Run the DrRacket GUI macro stepper.

Runs on the active region, if any, else the entire buffer.

EXPERIMENTAL: May be changed or removed.

BUGGY: The first-ever invocation might not display a GUI window.
If so, try again."
  (interactive)
  (save-buffer)
  (racket--eval
   (format "%S\n"
           `(begin
             (require macro-debugger/stepper racket/port)
             ,(if (region-active-p)
                  `(expand/step
                    (with-input-from-string ,(buffer-substring-no-properties
                                              (region-beginning)
                                              (region-end))
                                            read-syntax))
                `(expand-module/step
                  (string->path
                   ,(substring-no-properties (buffer-file-name)))))))))

(provide 'racket-edit)

;; racket-edit.el ends here
