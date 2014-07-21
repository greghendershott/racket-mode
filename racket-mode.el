;;; racket-mode.el --- Major mode for Racket language.

;; Copyright (c) 2013-2014 by Greg Hendershott.

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

;;; Commentary:

;; Goals:
;; - Focus on Racket (not various Schemes).
;; - Fontify all Racket keywords, builtins, and so on.
;; - Fontify variations of define for functions and variables.
;; - Indent Racket forms (even `for/fold` and `for*/fold`).
;; - Follow DrRacket concepts where applicable.
;; - Compatible with Emacs 24.2+.
;;
;; Acknowledgements:
;;
;; - Obviously the existing Emacs Scheme mode and Inferior Scheme mode.
;;
;; - The source code for Neil Van Dyke's Quack provided a model for
;;   many of the scheme-indent-function settings, smart paren closing,
;;   and pretty lambda.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(defconst racket-mode-copyright
  "Copyright (c) 2013-2014 by Greg Hendershott. Portions Copyright (c) Free Software Foundation and Copyright (c) 2002-2012 Neil Van Dyke.")

(defconst racket-mode-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  See
http://www.gnu.org/licenses/ for details.")

(defconst racket-mode-version "0.4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things used by both racket-mode and racket-repl-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'racket-keywords-and-builtins)
(require 'racket-font-lock)
(require 'racket-indent)
(require 'comint)

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

(defun racket--variables-for-both-modes ()
  ;; Set many things explicitly. We wouldn't need to set most of these
  ;; if our editing major mode, `racket-mode`, were derived from
  ;; `scheme-mode` instead of from `prog-mode`. So why do it this way?
  ;; Because of our `racket-repl-mode`. That needs to derive from
  ;; `comint-mode`, therefore it needs to set them explicitly. Setting them
  ;; all here ensures consistency. And in that case, racket-mode need not
  ;; derive from scheme-mode, it can derive from just prog-mode.
  (set-syntax-table racket-mode-syntax-table)
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
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
                 (?#. "w 14"))
                beginning-of-defun             ;syntax-begin
                ;; Additional variables:
                (font-lock-mark-block-function . mark-defun)
                (font-lock-syntactic-face-function
                 . racket-font-lock-syntactic-face-function)
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props syntax-table)))
  (setq-local completion-at-point-functions '(racket-complete-at-point)))

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
    (unless (looking-at-p "[([{]")
      (backward-up-list))
    (let ((pt (point))
          (new (cond ((looking-at-p "(")   (cons "[" "]"))
                     ((looking-at-p "\\[") (cons "{" "}"))
                     ((looking-at-p "{")   (cons "(" ")"))
                     (t (beep) nil))))
      (when new
        (forward-sexp)
        (backward-delete-char 1)
        (insert (cdr new))
        (goto-char pt)
        (delete-char 1)
        (insert (car new))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Racket mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)
(require 'hideshow)

(defun racket-run ()
  "Save and evaluate the buffer in REPL, like DrRacket's Run."
  (interactive)
  (save-buffer)
  (racket--invalidate-completion-cache)
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

;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------

(defun racket--eval (str)
  (racket-repl)
  (racket--repl-forget-errors)
  (comint-send-string (racket--get-repl-buffer-process) str)
  (racket--repl-show-and-move-to-end))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cr = cr + indent

(defun racket-cr ()
  "Insert a newline and indent."
  (interactive)
  (newline)
  (lisp-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code folding

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completions

(make-variable-buffer-local
 (defvar racket--namespace-symbols nil))

(defun racket--invalidate-completion-cache ()
  (setq racket--namespace-symbols nil))

(defun racket--get-namespace-symbols ()
  (unless racket--namespace-symbols
    (setq racket--namespace-symbols
          (racket--eval/sexpr
           (format "%S"
                   `(map symbol->string (namespace-mapped-symbols))))))
  racket--namespace-symbols)

(defun racket--complete-prefix (prefix)
  (all-completions prefix (racket--get-namespace-symbols)))

;; (defun racket--complete-prefix (prefix)
;;   (racket--eval/sexpr
;;    (format "%S"
;;            `(let ([rx (regexp ,(concat "^" prefix))])
;;               (filter-map (lambda (sym)
;;                             (define str (symbol->string sym))
;;                             (cond [(regexp-match? rx str) str]
;;                                   [else false]))
;;                           (namespace-mapped-symbols))))))

(defun racket--complete-prefix-begin ()
  (save-excursion (skip-syntax-backward "^-()>")
                  (point)))

(defun racket--complete-prefix-end (beg)
  (unless (or (eq beg (point-max))
              (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
    (let ((pos (point)))
      (condition-case nil
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (when (>= (point) pos)
              (point)))
        (scan-error pos)))))

(defun racket-complete-at-point (&optional predicate)
  (with-syntax-table racket-mode-syntax-table ;probably don't need this??
    (let* ((beg (racket--complete-prefix-begin))
           (end (or (racket--complete-prefix-end beg) beg))
           (prefix (and (> end beg) (buffer-substring-no-properties beg end)))
           ;; (prefix (and prefix
           ;;              (if (string-match "\\([^-]+\\)-" prefix)
           ;;                  (match-string 1 prefix)
           ;;                prefix)))
           (cmps (and prefix (racket--complete-prefix prefix))))
      (and cmps (list beg end cmps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode

(eval-after-load "company"
  '(progn
     (defun racket-company-backend (command &optional arg &rest ignore)
       (interactive (list 'interactive))
       (case command
         ('interactive (company-begin-backend 'racket-company-backend))
         ('prefix (racket--company-prefix))
         ('candidates (racket--company-candidates
                       (substring-no-properties arg)))
         ('meta (format "This value is named %s" arg))))
     (defun racket--do-company-setup (enable)
       (set (make-local-variable 'company-default-lighter) " co")
       (set (make-local-variable 'company-echo-delay) 0.01)
       (set (make-local-variable 'company-backends)
            (and enable '(racket-company-backend)))
       (company-mode (if enable 1 -1)))))

(defun racket--company-setup (enable)
  (when (fboundp 'racket--do-company-setup)
    (racket--do-company-setup enable)))

(make-variable-buffer-local
 (defvar racket--company-completions nil))

(defun racket--company-prefix ()
  (if (nth 8 (syntax-ppss))
      'stop
    (let* ((prefix (and (looking-at-p "\\_>")
                        (racket--get-repl-buffer-process)
                        (buffer-substring-no-properties
                         (racket--complete-prefix-begin)
                         (point))))
           (cmps (and prefix (racket--complete-prefix prefix))))
      (setq racket--company-completions (cons prefix cmps))
      prefix)))

(defun racket--company-candidates (prefix)
  (and (equal prefix (car racket--company-completions))
       (cdr racket--company-completions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar racket-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m lisp-mode-shared-map)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("<f5>"      racket-run)
            ("M-C-<f5>"  racket-racket)
            ("C-<f5>"    racket-test)
            ("M-C-x"     racket-send-definition)
            ("C-x C-e"   racket-send-last-sexp)
            ("C-c C-r"   racket-send-region)
            ("C-c C-e x" racket-expand-definition)
            ("C-c C-e e" racket-expand-last-sexp)
            ("C-c C-e r" racket-expand-region)
            ("C-c C-e a" racket-expand-again)
            ("RET"       racket-cr)
            (")"         racket-insert-closing-paren)
            ("]"         racket-insert-closing-bracket)
            ("}"         racket-insert-closing-brace)
            ("C-c C-p"   racket-cycle-paren-shapes)
            ("M-C-y"     racket-insert-lambda)
            ("C-c C-d"   racket-doc)
            ("M-."       racket-visit-definition)
            ("M-C-."     racket-visit-module)
            ("M-,"       racket-unvisit)
            ("C-c C-f"   racket-fold-all-tests)
            ("C-c C-U"   racket-unfold-all-tests)))
    m)
  "Keymap for Racket mode. Inherits from `lisp-mode-shared-map'.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for Racket mode."
  '("Racket"
    ("Run"
     ["in REPL" racket-run]
     ["via `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["via `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests]
     ["Unfold All" racket-unfold-all-tests])
    ("Eval"
     ["Region" racket-send-region :active (region-active-p)]
     ["Definition" racket-send-definition]
     ["Last S-Expression" racket-send-last-sexp])
    ("Macro Expand"
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp]
     "---"
     ["Again" racket-expand-again])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    "---"
    ["Visit Definition" racket-visit-definition]
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["Racket documentation" racket-doc]
    ["Customize..." customize-mode]))

(defvar racket-imenu-generic-expression
  '((nil
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Struct"
     "^(struct\\s-+\\(\\sw+\\)" 1)
    ("Syntax"
     "^(define-syntax\\s-+(?\\(\\sw+\\)" 1))
  "Imenu generic expression for racket mode.  See `imenu-generic-expression'.")

(defun racket--variables-imenu ()
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       racket-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?!$%_&~^:" . "w"))))

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket--variables-for-both-modes)
  (racket--variables-imenu)
  (racket--company-setup t)
  (hs-minor-mode t))

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.rkt\\'" . racket-mode)
                ("\\.rktd\\'" . racket-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Racket REPL mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun racket-input-filter (str)
  "Don't save anything matching `racket-repl-filter-regexp'."
  (not (string-match racket-repl-filter-regexp str)))

(defun racket-get-old-input ()
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

(defvar racket-sandbox-rkt
  (let ((elisp-dir
         (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name "sandbox.rkt" elisp-dir))
  "Path to sandbox.rkt")

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
                               racket-sandbox-rkt))
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
  "Run the DrRacket GUI macro stepper on the current buffer.

EXPERIMENTAL. May be changed or removed."
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

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.
\\{racket-repl-mode-map}"
  (racket--variables-for-both-modes)
  (setq-local comint-prompt-regexp "^[^>\n]*> +")
  ;; (setq-local comint-use-prompt-regexp t)
  ;; (setq-local comint-prompt-read-only t)
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter (function racket-input-filter))
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   '(("^;?[ ]*\\([^ :]+\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" 1 2 3) ;errs, defns
     ("^;?[ ]*at:[ ]+\\([^ :]+\\):\\([0-9]+\\)[.]\\([0-9]+\\)$" 1 2 3) ;contract
     ("#<path:\\([^>]+\\)> \\([0-9]+\\) \\([0-9]+\\)" 1 2 3)   ;rackunit
     ("#<path:\\([^>]+\\)>" 1 nil nil 0)                       ;path struct
     ))
  (setq-local comint-get-old-input (function racket-get-old-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs version compatibility
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In case not Emacs 24.1+, define equivalent of its
;; `pop-to-buffer-same-window'.
(defun racket-pop-to-buffer-same-window
  (&optional buffer-or-name norecord label)
  "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
  (if (fboundp 'pop-to-buffer-same-window)
      (funcall
       'pop-to-buffer-same-window buffer-or-name norecord)
    (funcall 'switch-to-buffer buffer-or-name norecord)))

;; In case not Emacs 24.3+, define equivalent of its `setq-local'.
(eval-and-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      `(set (make-local-variable ',var) ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'racket-mode)

;;; racket-mode.el ends here
