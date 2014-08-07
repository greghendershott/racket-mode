;;; racket-repl.el

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

(require 'racket-common)
(require 'comint)
(require 'compile)

(defconst racket--repl-buffer-name/raw
  "Racket REPL"
  "The base buffer name, NOT surrounded in *stars*")
(defconst racket--repl-buffer-name
  (concat "*" racket--repl-buffer-name/raw "*")
  "The actual buffer name as created by comint-mode")
(defun racket--get-repl-buffer-process ()
  (get-buffer-process racket--repl-buffer-name))

(defvar racket-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("RET"     racket-repl-cr)
            (")"       racket-insert-closing-paren)
            ("]"       racket-insert-closing-bracket)
            ("}"       racket-insert-closing-brace)
            ("C-c C-p" racket-cycle-paren-shapes)
            ("M-C-y"   racket-insert-lambda)
            ("C-c C-d" racket-doc)
            ("M-."     racket-visit-definition)
            ("C-M-."   racket-visit-module)))
    m)
  "Keymap for Racket REPL mode.")

(defcustom racket-repl-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :tag "History filter regexp"
  :type 'regexp
  :group 'racket)

(defun racket--input-filter (str)
  "Don't save anything matching `racket-repl-filter-regexp'."
  (not (string-match racket-repl-filter-regexp str)))

(defun racket--get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;; I don't want comint-mode clobbering our font-lock with
;; comint-highlight-input face. (Changing that *face* not to be bold
;; isn't enough).
;;
;; So far, the least-pukey way I can figure out how to do this is to
;; copy-pasta much of comint-send-input, and modify the one tiny
;; offending bit.  Blech. If anyone reading this knows a better way,
;; please let me know!
;;
;; Meanwhile I have slimmed down the copy -- deleted the `no-newline`
;; and `artificial` args we don't use, and the code that could only
;; execute if they were non-nil.
(defun racket--comint-send-input ()
  "Like `comint-send-input` but doesn't use face `comint-highlight-input'."
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send (end-of-line))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        ;; This is messy 'cos ultimately the original
                        ;; functions used do insertion, rather than return
                        ;; strings.  We have to expand, then insert back.
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))
        (insert ?\n)
        (comint-add-to-input-history history)
        (run-hook-with-args 'comint-input-filter-functions
                            (concat input "\n"))
        (let ((beg (marker-position pmark))
              (end (1- (point)))
              (inhibit-modification-hooks t))
          (when (> end beg)
            ;;;; The bit from comint-send-input that we DON'T want:
            ;; (add-text-properties beg end
            ;;                      '(front-sticky t
            ;;                        font-lock-face comint-highlight-input))
            (unless comint-use-prompt-regexp
              ;; Give old user input a field property of `input', to
              ;; distinguish it from both process output and unsent
              ;; input.  The terminating newline is put into a special
              ;; `boundary' field to make cursor movement between input
              ;; and output fields smoother.
              (add-text-properties
               beg end
               '(mouse-face highlight
                 help-echo "mouse-2: insert after prompt as new input"))))
          (unless comint-use-prompt-regexp
            ;; Cover the terminating newline
            (add-text-properties end (1+ end)
                                 '(rear-nonsticky t
                                   field boundary
                                   inhibit-line-move-field-capture t))))
        (comint-snapshot-last-prompt)
        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (funcall comint-input-sender proc input)
        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (run-hook-with-args 'comint-output-filter-functions "")))))

(defun racket-repl-cr ()
  "If complete sexpr, eval. Else do `racket-cr'."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (user-error "Current buffer has no process")
      (condition-case nil
          (progn
            (save-excursion
              (goto-char (process-mark proc))
              (forward-list)) ;will error unless complete sexpr
            (racket--comint-send-input))
        (error (racket-cr))))))

(defvar racket-run-rkt
  (let ((elisp-dir
         (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name "run.rkt" elisp-dir))
  "Path to run.rkt")

;;;###autoload
(defun racket-repl ()
  "Run a Racket REPL in a comint buffer.
Runs the hook `racket-repl-mode-hook' \(after the `comint-mode-hook'
is run)."
  (interactive)
  (let ((original-window (selected-window)))
    ;; If REPL process already visible in a window, use that window.
    (let ((rw (get-buffer-window racket--repl-buffer-name)))
      (if rw
          (select-window rw)
        (other-window 1)))
    (unless (comint-check-proc racket--repl-buffer-name)
      (set-buffer (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                               racket-program
                               nil
                               racket-run-rkt))
      (racket-repl-mode))
    (select-window original-window)))

(defun racket--send-region-to-repl (start end)
  "Internal function to send the region to the Racket REPL.
Calls `racket--repl-forget-errors' beforehand and
`racket--repl-show-and-move-to-end' afterwars."
  (when (and start end)
    (racket--repl-forget-errors)
    (comint-send-region (racket--get-repl-buffer-process) start end)
    (comint-send-string (racket--get-repl-buffer-process) "\n")
    (racket--repl-show-and-move-to-end)))

(defun racket-send-region (start end)
  "Send the current region (if any) to the Racket REPL."
  (interactive "r")
  (if (region-active-p)
      (racket--send-region-to-repl start end)
    (beep)
    (message "No region.")))

(defun racket-send-definition ()
  "Send the current definition to the Racket REPL."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (racket--send-region-to-repl (point) end))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the Racket REPL."
  (interactive)
  (racket--send-region-to-repl (save-excursion (backward-sexp) (point))
                               (point)))

(defun racket--repl-forget-errors ()
  "Forget existing compilation mode errors in the REPL.
Although they remain clickable, `next-error' and `previous-error'
will ignore them."
  (with-current-buffer racket--repl-buffer-name
    (compilation-forget-errors)))

(defun racket--repl-show-and-move-to-end ()
  "Make the Racket REPL visible, and move point to end.
Keep original window selected."
  (let ((w (selected-window)))
    (pop-to-buffer racket--repl-buffer-name t)
    (select-window (get-buffer-window racket--repl-buffer-name))
    (with-current-buffer racket--repl-buffer-name
      (goto-char (point-max)))
    (select-window w)))

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.
\\{racket-repl-mode-map}"
  (racket--variables-for-both-modes)
  (setq-local comint-prompt-regexp "^[^<>#\"\n]*> +")
  ;; (setq-local comint-use-prompt-regexp t)
  ;; (setq-local comint-prompt-read-only t)
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter (function racket--input-filter))
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   '(("^;?[ ]*\\([^ :]+\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" 1 2 3) ;errs, defns
     ("^;?[ ]*at:[ ]+\\([^ :]+\\):\\([0-9]+\\)[.]\\([0-9]+\\)$" 1 2 3) ;contract
     ("#<path:\\([^>]+\\)> \\([0-9]+\\) \\([0-9]+\\)" 1 2 3)   ;rackunit
     ("#<path:\\([^>]+\\)>" 1 nil nil 0)                       ;path struct
     ))
  (setq-local comint-get-old-input (function racket--get-old-input))
  ;; The following is needed to make e.g. λ work when pasted into the
  ;; comint-buffer, both directly by the user and via the racket--eval
  ;; functions. This seems like a global Emacs-wide setting, so I'm
  ;; not 100% confident I should do this here. But if I don't, and
  ;; e.g. people use eldoc within (λ () ...), the Racket reader will
  ;; hang because it gets ",type <eof>" not ",type λ<eof>". Even when
  ;; people paste manually or via C-x C-r, although it doesn't hang,
  ;; the resulting error message is not very clear.
  (set-terminal-coding-system 'utf-8))

(provide 'racket-repl)

;; racket-repl.el ends here
