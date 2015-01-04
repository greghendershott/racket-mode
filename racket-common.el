;;; racket-common.el

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

;; Things used by both racket-mode and racket-repl-mode

(require 'thingatpt)
(require 'racket-emacs-compat)
(require 'racket-keywords-and-builtins)
(require 'racket-font-lock)
(require 'racket-indent)

(defgroup racket nil
  "A mode for Racket"
  :prefix "racket-"
  :group 'languages
  :link '(url-link :tag "README on GitHub" "https://github.com/greghendershott/racket-mode/blob/master/README.md")
  :link '(emacs-commentary-link :tag "Commentary" "racket-mode"))

(defcustom racket-program "racket"
  "Pathname of the racket executable."
  :tag "/path/to/racket"
  :type '(file :must-match t)
  :group 'racket)

(defcustom raco-program "raco"
  "Pathname of the raco executable."
  :tag "/path/to/raco"
  :type '(file :must-match t)
  :group 'racket)

(defvar racket-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)

    ;; ;; Make # and | symbol constituents.
    ;; (modify-syntax-entry ?# "_ p14bn" racket-mode-syntax-table)
    ;; (modify-syntax-entry ?| "_ 23bn"  racket-mode-syntax-table)

    st))

(defvar racket-mode-abbrev-table nil)
(define-abbrev-table 'racket-mode-abbrev-table ())

(defconst racket-sexp-comment-syntax-table
  (let ((st (make-syntax-table racket-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(defun racket-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table racket-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

(defconst racket-syntax-propertize-function
  (syntax-propertize-rules
   ;; Treat #px"" and #rx"" as single sexpr for navigation and indent.
   ((rx (group (or "#px" "#rx"))
        (group "\"")
        (group (zero-or-more (not (any "\""))))
        (group "\""))
    (1 "'")
    (2 "\"")
    (3 (ignore))
    (4 "\""))))

(defun racket--variables-for-both-modes ()
  ;; Set many things explicitly. We wouldn't need to set most of these
  ;; if our editing major mode, `racket-mode`, were derived from
  ;; `scheme-mode` instead of from `prog-mode`. So why do it this way?
  ;; Because of our `racket-repl-mode`. That needs to derive from
  ;; `comint-mode`, therefore it needs to set them explicitly. Setting them
  ;; all here ensures consistency. And in that case, racket-mode need not
  ;; derive from scheme-mode, it can derive from just prog-mode.
  (set-syntax-table racket-mode-syntax-table)
  (setq-local syntax-propertize-function racket-syntax-propertize-function)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'racket-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local comment-start ";")
  (setq-local comment-add 1)            ;default to `;;' in comment-region
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning:
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  ;; Font lock mode uses this only when it KNOWS a comment is starting:
  (setq-local font-lock-comment-start-skip ";+ *")
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'racket-indent-function)
  (racket--set-indentation)
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults
              `(,racket-font-lock-keywords     ;keywords
                nil                            ;keywords-only?
                nil                            ;case-fold?
                (("+-*/.<>=!?$%_&~^:" . "w")   ;syntax-alist
                 (?# . "w 14"))
                beginning-of-defun             ;syntax-begin
                ;; Additional variables:
                (font-lock-mark-block-function . mark-defun)
                (font-lock-syntactic-face-function
                 . racket-font-lock-syntactic-face-function)
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props syntax-table)))
  (setq-local completion-at-point-functions '(racket-complete-at-point))
  (setq-local eldoc-documentation-function 'racket-eldoc-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  (interactive)
  (insert-char racket-lambda-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically insert matching \?) \?] or \?}

(defvar racket-matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket--insert-closing (prefix char)
  (insert char)
  (unless prefix
    (let ((open-pt (condition-case nil
                       (scan-sexps (point) -1)
                     (error (beep) nil))))
      (when open-pt
        (let* ((open-char
                (aref (buffer-substring-no-properties open-pt (1+ open-pt)) 0))
               (close-pair (assoc open-char racket-matching-parens)))
          (when close-pair
            (let ((close-char (cdr close-pair)))
              (when (not (= close-char char))
                (call-interactively 'delete-backward-char)
                (insert close-char))))))))
  (when blink-paren-function (funcall blink-paren-function)))

(defun racket-insert-closing-paren (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\)))

(defun racket-insert-closing-bracket (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\]))

(defun racket-insert-closing-brace (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\}))

(defun racket-cycle-paren-shapes ()
  "In an s-expression, move to the opening, and cycle the shape among () [] {}"
  (interactive)
  (save-excursion
    (unless (looking-at-p (rx (any "([{")))
      (backward-up-list))
    (let ((pt (point))
          (new (cond ((looking-at-p (rx "(")) (cons "[" "]"))
                     ((looking-at-p (rx "[")) (cons "{" "}"))
                     ((looking-at-p (rx "{")) (cons "(" ")"))
                     (t (beep) nil))))
      (when new
        (forward-sexp)
        (backward-delete-char 1)
        (insert (cdr new))
        (goto-char pt)
        (delete-char 1)
        (insert (car new))))))

(defun racket-newline-and-indent ()
  "Do `newline' and `lisp-indent-for-line'."
  (interactive)
  (newline)
  (lisp-indent-line))

(defun racket-indent-or-complete ()
  "Try `indent-for-tab-command' then `completion-at-point'.

Call `indent-for-tab-command'. See if it did anything (changed
the indentation, or moved point to the first non-whitespace on
the line). If not, call `completion-at-point'."
  (interactive)
  ;; As far as I can tell it's sufficient to see whether point moved.
  ;; `indent-for-tab-command` will move point as a side-effect of
  ;; changing indentation, and also obviously when point is moved to
  ;; the starting non-whitespace.
  (let ((pt (point)))
    (indent-for-tab-command)
    (when (equal pt (point))
      (completion-at-point))))

(defun racket-backward-up-list ()
  "Like `backward-up-list' but works when point is in a string literal."
  (interactive)
  (while (in-string-p)
    (backward-char))
  (backward-up-list))

(provide 'racket-common)

;; racket-common.el ends here
