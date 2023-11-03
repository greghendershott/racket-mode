;;; racket-hash-lang.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'seq)
(require 'racket-cmd)
(require 'racket-mode)
(require 'racket-repl)

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `((("C-c C-c"
       "C-c C-k")   ,#'racket-run-module-at-point)
     ("C-c C-z"     ,#'racket-repl)
     ("<f5>"        ,#'racket-run-and-switch-to-repl)
     ("M-C-<f5>"    ,#'racket-racket)
     ("C-<f5>"      ,#'racket-test)
     ("C-c C-t"     ,#'racket-test)
     ("C-c C-l"     ,#'racket-logger)
     ("C-c C-o"     ,#'racket-profile)
     ("C-c C-e f"   ,#'racket-expand-file)
     ("C-c C-x C-f" ,#'racket-open-require-path)
     ("TAB"         ,#'indent-for-tab-command)
     ;; ("C-c C-p"     racket-cycle-paren-shapes) equivalent using paren-matches?
     ("M-C-y"       ,#'racket-insert-lambda)
     ("RET"         ,#'newline-and-indent)
     ("C-M-b"       ,#'racket-hash-lang-backward)
     ("C-M-f"       ,#'racket-hash-lang-forward)
     ("C-M-u"       ,#'racket-hash-lang-up)
     ("C-M-d"       ,#'racket-hash-lang-down)
     ("C-M-q"       ,#'racket-hash-lang-C-M-q-dwim))))

(easy-menu-define racket-hash-lang-mode-menu racket-hash-lang-mode-map
  "Menu for `racket-hash-lang-mode'."
  '("Racket-Hash-Lang"
    ("Run"
     ["in REPL" racket-run]
     ["in REPL and switch to REPL" racket-run-and-switch-to-repl]
     ["in *shell* using `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["in *shell* using `raco test`" racket-raco-test])
    ("Macro Expand"
     ["File" racket-expand-file])
    ["Switch to REPL" racket-repl]
    ("Tools"
     ["Profile" racket-profile]
     ["Error Trace" racket-run-with-errortrace]
     ["Step Debug" racket-run-with-debugging]
     ["Toggle XP Mode" racket-xp-mode])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    "---"
    ["Open Require Path" racket-open-require-path]
    ["Find Collection" racket-find-collection]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    ["Customize..." customize-mode]))

(defvar-local racket--hash-lang-submit-predicate-p nil)

(defvar racket-hash-lang-module-language-hook nil
  "Hook run when the module language changes.

The hook is called when a file is first visited, and thereafter
whenever the \"#lang\" line is edited -- provided that results in
new language info; for example changing from \"#lang racket\" to
\"#lang racket/base\" will /not/ run the hook.

The function is called with a string returned by the lang's
\"module-language\" info key. This info key is supplied
automatically when a language is defined using
syntax/module-reader:

  <https://docs.racket-lang.org/syntax/reader-helpers.html#%28mod-path._syntax%2Fmodule-reader%29>.

Otherwise a lang might not supply this and the value will be nil.

The hook is useful when you want to vary Emacs behavior in ways
that go beyond what a lang can describe. This may include
enabling \"fancy\" or \"classic\" Emacs behaviors only for
s-expression langs.

For example, maybe you want to use `paredit-mode' when it is
suitable for the module language, otherwise stick with the
plainer `electric-pair-mode'.

#+BEGIN_SRC elisp
  (defun my-hook (module-language)
    (cond
     ((member module-language (list \"racket\" \"racket/base\"
                                    \"typed/racket\" \"typed/racket/base\"))
      (electric-pair-local-mode -1)
      (paredit-mode 1))
    (t
     (paredit-mode -1)
     (electric-pair-local-mode 1))))
  (add-hook \\='racket-hash-lang-module-language-hook #\\='my-hook)
#+END_SRC
")

;;;###autoload
(define-derived-mode racket-hash-lang-mode prog-mode
  "#lang"
  "Use color-lexer, indent, and navigation supplied by a #lang.

An experimental major mode alternative to `racket-mode' for
source file edit buffers.

In your Emacs configuration, you may want to update the
variable `auto-mode-alist' to use `racket-hash-lang-mode' for
file extensions like \".rkt\", \".scrbl\", and/or \".rhm\".

See also the customization variable
`racket-hash-lang-token-face-alist' and the hook variable
`racket-hash-lang-module-language-hook'.

A discussion of the information provided by a Racket language:

  <https://docs.racket-lang.org/tools/lang-languages-customization.html>

Note that langs supply colors only for lexer tokens like strings
and comments. If you enable the minor mode `racket-xp-mode', it
can contribute more colors; see the customization variable
`racket-xp-binding-font-lock-face-modes'.

\\{racket-hash-lang-mode-map}
"
  (racket-call-racket-repl-buffer-name-function)
  (add-hook 'kill-buffer-hook
            #'racket-mode-maybe-offer-to-kill-repl-buffer
            nil t)
  (set-syntax-table racket--plain-syntax-table)
  (setq-local font-lock-defaults nil)
  (setq-local font-lock-fontify-region-function
              #'racket--hash-lang-font-lock-fontify-region)
  (font-lock-set-defaults) ;issue #642
  (setq-local syntax-propertize-function nil)
  (setq-local text-property-default-nonsticky
              (append
               (racket--hash-lang-text-prop-list #'cons t)
               text-property-default-nonsticky))
  (add-hook 'after-change-functions #'racket--hash-lang-after-change-hook t t)
  (add-hook 'kill-buffer-hook #'racket--hash-lang-delete t t)
  (add-hook 'change-major-mode-hook #'racket--hash-lang-delete t t)
  (electric-indent-local-mode -1)
  (setq-local electric-indent-inhibit t)
  (setq-local blink-paren-function nil)
  (setq-local imenu-create-index-function nil)
  (setq-local completion-at-point-functions nil) ;rely on racket-xp-mode
  (setq-local eldoc-documentation-function nil)
  (setq racket-submodules-at-point-function nil) ;might change in on-new-lang
  (racket--hash-lang-create))

(defvar-local racket--hash-lang-id nil
  "Unique integer used to identify the back end hash-lang object.
Although it's tempting to use `buffer-file-name' for the ID, not
all buffers have files. Although it's tempting to use
`buffer-name', buffers can be renamed. Although it's tempting to
use the buffer object, we can't serialize that.")
(defvar racket--hash-lang-next-id 0
  "Increment when we need a new id.")

(defvar-local racket--hash-lang-generation 1
  "Monotonic increasing value for hash-lang updates.

This is set to 1 when we hash-lang create, incremented every time
we do a hash-lang update, and then supplied for all other, query
hash-lang operations. That way the queries can block if necessary
until the back end has handled the update commands and also
re-tokenization has progressed sufficiently.")

;; For use by both racket-hash-lang-mode and racket-repl-mode
(defun racket--hash-lang-create (&optional other-buffer)
  (setq-local racket--hash-lang-id (cl-incf racket--hash-lang-next-id))
  (setq-local racket--hash-lang-generation 1)
  (cl-case major-mode
   ((racket-hash-lang-mode)
    (let ((text (save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      ;; On the one hand, racket--cmd/await would be simpler to use
      ;; here. On the other hand, when someone visits a file without the
      ;; back end running yet, there's a delay for that to start, during
      ;; which the buffer isn't displayed and Emacs seems frozen. On the
      ;; third hand, if we use async the buffer could try to interact
      ;; with a back end object that doesn't yet exist, and error.
      ;;
      ;; Warm bowl of porridge: Make buffer read-only and not font-lock.
      ;; Set a timer to show a message in the header-line after awhile.
      ;; Send command async. Only when the response arrives, i.e. the
      ;; back end object is ready, enable read/write and font-lock.
      ;;
      ;; Finally, handle the back end returning nil for the create,
      ;; meaning there's no sufficiently new syntax-color-lib.
      (font-lock-mode -1)
      (read-only-mode 1)
      (unless (racket--cmd-open-p)
        (setq-local header-line-format "Waiting for back end to start..."))
      (racket--cmd/async
       nil
       `(hash-lang create ,racket--hash-lang-id ,nil ,text)
       (lambda (maybe-id)
         (font-lock-mode 1)
         (read-only-mode -1)
         (setq-local header-line-format nil)
         (unless maybe-id
           (prog-mode)
           (message "hash-lang support not available; needs newer syntax-color-lib"))))))
   ((racket-repl-mode)
    (let ((other-lang-source
           (when other-buffer
             (with-current-buffer other-buffer
               (save-restriction
                 (widen)
                 (buffer-substring-no-properties (point-min) (min 4096 (point-max)))))))
          (text
           (racket--hash-lang-repl-buffer-string (point-min) (point-max))))
      (racket--cmd/async
       nil
       `(hash-lang create ,racket--hash-lang-id ,other-lang-source ,text))))
   (otherwise
    (error "racket--hash-lang-create doesn't work for %s" major-mode))))

(defun racket--hash-lang-delete ()
  (when racket--hash-lang-id
    (ignore-errors
      (racket--cmd/await
       (when (eq major-mode 'racket-repl-mode) racket--repl-session-id)
       `(hash-lang delete ,racket--hash-lang-id)))
    (setq racket--hash-lang-id nil)
    (setq-local racket--hash-lang-generation 1)))

;;; Handle back end stopping

(defun racket--hash-lang-on-stop-back-end ()
  "Because `racket-hash-lang-mode' buffers can't work without a
live back end, downgrade them all to `prog-mode'."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'racket-hash-lang-mode)
          (prog-mode))))))
(add-hook 'racket-stop-back-end-hook #'racket--hash-lang-on-stop-back-end)

;;; Other

(defun racket--hash-lang-find-buffer (id)
  "Find the buffer whose local value for `racket--hash-lang-id' is ID."
  (cl-some (lambda (buf)
             (when (equal id (buffer-local-value 'racket--hash-lang-id buf))
               buf))
           (buffer-list)))

(defun racket--make-non-sexp-syntax-table (parens quotes)
  "Make a syntax-table with the given parens and quotes.

Intended for use by things like `electric-pair-mode'."
  (let ((table (make-syntax-table racket--plain-syntax-table)))
    (dolist (str-pair parens)
      (pcase-let ((`(,open . ,close) str-pair))
        ;; Unsure how to handle in syntax-table when > 1 char.
        (when (and (= 1 (length open)) (= 1 (length close)))
          (modify-syntax-entry (aref open 0)
                               (concat "(" (substring close 0 1) "  ")
                               table)
          (modify-syntax-entry (aref close 0)
                               (concat ")" (substring open 0 1) "  ")
                               table))))
    (dolist (str quotes)
      (when (= 1 (length str))
        (modify-syntax-entry (aref str 0) "\"   " table)))
    table))

;;; Updates: Front end --> back end

(defun racket--hash-lang-repl-buffer-string (beg end)
  "Like `buffer-substring-no-properties' treat as whitespace,
preserving only line breaks for indentation, everything that is
not a value output since the last run, or input after the last
live prompt."
  (let ((result-str ""))
    (racket--repl-call-with-value-and-input-ranges
     beg end
     (lambda (beg end is-value-or-input-p)
       (let ((raw (buffer-substring-no-properties beg end)))
         (setq
          result-str
          (concat result-str
                  (if is-value-or-input-p
                      raw
                    (replace-regexp-in-string "[^\r\n]+"
                                              (lambda (s)
                                                (make-string (length s) 32))
                                              raw)))))))
    result-str))

(defun racket--hash-lang-after-change-hook (beg end len)
  ;;;(message "racket--hash-lang-after-change-hook %s %s %s" beg end len)
  ;; This might be called as frequently as once per single changed
  ;; character.
  (racket--cmd/async
   nil
   `(hash-lang update
               ,racket--hash-lang-id
               ,(cl-incf racket--hash-lang-generation)
               ,beg
               ,len
               ,(if (eq major-mode 'racket-repl-mode)
                    (racket--hash-lang-repl-buffer-string beg end)
                  (buffer-substring-no-properties beg end)))))

;;; Notifications: Front end <-- back end

(defun racket--hash-lang-on-notify (id params)
  (when-let (buf (racket--hash-lang-find-buffer id))
    (with-current-buffer buf
      (pcase params
        (`(lang . ,plist)         (racket--hash-lang-on-new-lang plist))
        (`(update ,gen ,beg ,end) (racket--hash-lang-on-changed-tokens gen beg end))))))

(defun racket--hash-lang-on-new-lang (plist)
  "We get this whenever any #lang supplied attributes have changed.

We do /not/ get notified when a new lang uses exactly the same
attributes as the old one. For example changing from #lang racket
to #lang racket/base will /not/ notify us, because none of the
lang's attributes that we care about have changed."
  ;;;(message "racket--hash-lang-on-new-lang %s" plist)
  (with-silent-modifications
    (save-restriction
      (widen)
      (unless (eq major-mode 'racket-repl-mode)
        (racket--hash-lang-remove-text-properties (point-min) (point-max))
        (font-lock-flush (point-min) (point-max)))
      ;; If the lang uses racket-grouping-position, i.e. it uses
      ;; s-expressions, then use racket-mode-syntax-table. That way
      ;; other Emacs features and packages are more likely to work.
      ;; Otherwise, assume nothing about the lang and set a "plain"
      ;; syntax table where no characters are assumed to delimit
      ;; parens, comments, or strings.
      (set-syntax-table (if (plist-get plist 'racket-grouping)
                            racket-mode-syntax-table
                          (racket--make-non-sexp-syntax-table
                           (plist-get plist 'paren-matches)
                           (plist-get plist 'quote-matches))))
      ;; Similarly for `forward-sexp-function'. The
      ;; drracket:grouping-position protocol doesn't support a nuance
      ;; where a `forward-sexp-function' should signal an exception
      ;; containing failure positions. Although this is N/A for simple
      ;; forward/backward scenarios (such as when `prog-indent-sexp'
      ;; uses `forward-sexp' to set a region), it matters when things
      ;; like `up-list' use `forward-sexp'.
      (setq-local forward-sexp-function (unless (plist-get plist 'racket-grouping)
                                          #'racket-hash-lang-forward-sexp))
      (syntax-ppss-flush-cache (point-min))
      (setq-local indent-line-function
                  #'racket-hash-lang-indent-line-function)
      (setq-local indent-region-function
                  (when (plist-get plist 'range-indenter)
                    #'racket-hash-lang-indent-region-function))
      (setq-local racket--hash-lang-submit-predicate-p
                  (plist-get plist 'submit-predicate))
      ;; If racket-grouping i.e.sexp lang then we can probably
      ;; determine submodules textually from sexprs. Something like
      ;; racket-pdb-mode could determine this non-textually (albeit
      ;; after an analysis delay) someday.
      (setq racket-submodules-at-point-function
            (and (plist-get plist 'racket-grouping)
                 #'racket-submodules-at-point-text-sexp))
      ;; (setq-local racket-hash-lang-mode-lighter
      ;;             (concat " #lang"
      ;;                     (when (plist-get plist 'racket-grouping) "()")
      ;;                     (when (plist-get plist 'range-indenter) "⇉")))
      (pcase-let ((`(,start ,continue ,end ,padding)
                   (plist-get plist 'comment-delimiters)))
        (setq-local comment-start      start)
        (setq-local comment-continue   continue)
        (setq-local comment-end        end)
        (setq-local comment-padding    padding)
        (setq-local comment-use-syntax nil)
        ;; Use `comment-normalize-vars' to recalc the skip regexps.
        (setq-local comment-start-skip nil)
        (setq-local comment-end-skip   nil)
        (comment-normalize-vars))
      ;; Finally run user's module-language-hook.
      (run-hook-with-args 'racket-hash-lang-module-language-hook
                          (plist-get plist 'module-language)))))

(defun racket--hash-lang-on-changed-tokens (_gen beg end)
  "The back end has processed a change that resulted in new tokens.

All we do here is mark the span as not fontified, then let
jit-lock do its thing if/when this span ever becomes visible."
  ;;;(message "racket--hash-lang-on-changed-tokens %s %s %s" _gen beg end)
  (font-lock-flush beg end))

;;; Fontification

(defun racket--hash-lang-font-lock-fontify-region (beg end &optional _loudly)
  "Our value for the variable `font-lock-fontify-region-function'.

We ask the back end for tokens, and handle its response
asynchronously in `racket--hash-lang-on-tokens' which does the
actual application of faces and syntax. It wouldn't be
appropriate to wait for a response while being called from Emacs
C redisplay engine, as is the case with `jit-lock-mode'."
  ;;;(message "racket--hash-lang-font-lock-fontify-region %s %s" beg end)
  (racket--cmd/async
   nil
   `(hash-lang get-tokens
               ,racket--hash-lang-id
               ,racket--hash-lang-generation
               ,beg
               ,end)
   #'racket--hash-lang-on-tokens)
  `(jit-lock-bounds ,beg . ,end))

(defun racket--hash-lang-on-tokens (tokens)
  (save-restriction
    (widen)
    (with-silent-modifications
      (cl-flet* ((put-face (beg end face) (put-text-property beg end 'face face))
                 (put-stx  (beg end stx)  (put-text-property beg end 'syntax-table stx))
                 (put-fence (beg end stx)
                            (put-stx beg (1+ beg) stx)
                            (put-stx (1- end) end stx)))
        (dolist (token tokens)
          (pcase-let ((`(,beg ,end ,kinds) token))
            (setq beg (max (point-min) beg))
            (setq end (min end (point-max)))
            (racket--hash-lang-remove-text-properties beg end)
            ;; Add 'racket-token just for me to examine results using
            ;; `describe-char'; use vector b/c `describe-property-list'
            ;; assumes lists of symbols are "widgets".
            (put-text-property beg end 'racket-token (apply #'vector kinds))
            (dolist (kind kinds)
              (pcase kind
                ('comment
                 (put-face beg end 'font-lock-comment-face)
                 (put-fence beg end '(14)))
                ('sexp-comment ;just the "#;" prefix not following sexp body
                 (put-face beg end 'font-lock-comment-face)
                 (put-fence beg end '(14)))
                ('string
                 (put-face beg end 'font-lock-string-face)
                 (put-fence beg end '(15)))
                ;; Note: This relies on the back end supplying `kinds`
                ;; with sexp-comment-body last, so that we can modify
                ;; the face property already set by the previous
                ;; kind(s).
                ('sexp-comment-body
                 (put-face beg end (racket--sexp-comment-face
                                    (get-text-property beg 'face))))
                ('parenthesis (when (facep 'parenthesis)
                                (put-face beg end 'parenthesis)))
                ('text (put-stx beg end racket--plain-syntax-table))
                (sym
                 (when-let (face (cdr (assq sym racket-hash-lang-token-face-alist)))
                   (put-face beg end face)))))))))))

(defconst racket--hash-lang-text-properties
  '(face syntax-table racket-token)
  "The text properties we use.")

(defun racket--hash-lang-text-prop-list (f val)
  (mapcar (lambda (prop-sym) (funcall f prop-sym val))
          racket--hash-lang-text-properties))

(defun racket--hash-lang-remove-text-properties (beg end)
  "Remove `racket--hash-lang-text-properties' from region BEG..END."
  (remove-text-properties beg end
                          (apply #'append
                                 (racket--hash-lang-text-prop-list #'list nil))))

;;; Indent

(defun racket-hash-lang-indent-line-function ()
  "Use drracket:indentation supplied by the lang.

If a lang doesn't supply this, or if the supplied function ever
returns false, then we always use the standard s-expression
indenter from syntax-color/racket-indentation.

We never use `racket-indent-line' from traditional
`racket-mode'."
  (let* ((bol (save-excursion (beginning-of-line) (point)))
         (pos (- (point-max) (point)))
         (col (racket--cmd/await        ; await = :(
               nil
               `(hash-lang indent-amount
                           ,racket--hash-lang-id
                           ,racket--hash-lang-generation
                           ,(point)))))
    (goto-char bol)
    (skip-chars-forward " \t") ;;TODO: Is this reliable for all langs?
    (unless (= col (current-column))
      (delete-region bol (point))
      (indent-to col))
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (when (< (point) (- (point-max) pos))
      (goto-char (- (point-max) pos)))))

(defun racket-hash-lang-indent-region-function (from upto)
  "Maybe use #lang drracket:range-indentation, else plain `indent-region'."
  (pcase (racket--cmd/await   ; await = :(
                    nil
                    `(hash-lang indent-region-amounts
                                ,racket--hash-lang-id
                                ,racket--hash-lang-generation
                                ,from
                                ,upto))
    ('false (let ((indent-region-function nil))
              (indent-region from upto)))
    (`() nil)
    (results
     (save-excursion
       (goto-char from)
        ;; drracket:range-indent docs say `results` could have more
        ;; elements than lines in from..upto, and we should ignore
        ;; extras. Handle that. (Although it could also have fewer, we
        ;; need no special handling for that here.)
        (let ((results (seq-take results (count-lines from upto))))
          (dolist (result results)
            (pcase-let ((`(,delete-amount ,insert-string) result))
              (beginning-of-line)
              (when (< 0 delete-amount) (delete-char delete-amount))
              (unless (equal "" insert-string) (insert insert-string))
              (end-of-line 2))))))))

;; Motion

(defun racket-hash-lang-move (direction &optional count)
  (let ((count (or count 1)))
    (pcase (racket--cmd/await       ; await = :(
            nil
            `(hash-lang grouping
                        ,racket--hash-lang-id
                        ,racket--hash-lang-generation
                        ,(point)
                        ,direction
                        0
                        ,count))
      ((and (pred numberp) pos)
       (goto-char pos))
      (_ (user-error "Cannot move %s%s" direction (if (memq count '(-1 0 1))
                                                      ""
                                                    (format " %s times" count)))))))

(defun racket-hash-lang-backward (&optional count)
  "Like `backward-sexp' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'backward count))

(defun racket-hash-lang-forward (&optional count)
  "Like `forward-sexp' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'forward count))

(defun racket-hash-lang-up (&optional count)
  "Like `backward-up-list' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'up count))

(defun racket-hash-lang-down (&optional count)
  "Like `down-list' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'down count))

(defun racket-hash-lang-forward-sexp (&optional arg)
  "A value for the variable `forward-sexp-function'.

Caveat: This uses drracket:grouping-position, which doesn't have
a concept of signaling the position of a \"barrier\" that
prevented navigation forward/backward. Some users of
`forward-sexp' depend on that signal, for example `up-list'.
However other users don't need that, so we supply this
`forward-sexp-function' as \"better than nothing\"."
  (let* ((arg (or arg 1))
         (dir (if (< arg 0) 'backward 'forward))
         (cnt (abs arg)))
    (racket-hash-lang-move dir cnt)))

;;; Fill

(defun racket-hash-lang-C-M-q-dwim (&optional prefix)
  "Fill or indent depending on lang lexer's token at point.

When the lang lexer token is...

  - \"text\", for example in Scribble document text, do
    `fill-paragraph'.

  - \"comment\", do `fill-comment'.

  - \"whitespace\", give an error message.

  - anything else, do `prog-indent-sexp'.
"
  (interactive "P")
  (racket--cmd/async nil
                     `(hash-lang
                       classify
                       ,racket--hash-lang-id
                       ,racket--hash-lang-generation
                       ,(point))
                     (pcase-lambda (`(,_beg ,_end ,type))
                       (cl-case type
                         ((whitespace) (user-error "ambiguous; did nothing"))
                         ((text) (fill-paragraph prefix))
                         ((comment) (fill-comment-paragraph prefix))
                         (otherwise (prog-indent-sexp prefix))))))

;;; REPL

(defvar racket-hash-lang-repl-mode-map
  (racket--easy-keymap-define
   `(("C-M-b" ,#'racket-hash-lang-backward)
     ("C-M-f" ,#'racket-hash-lang-forward)
     ("C-M-u" ,#'racket-hash-lang-up)
     ("C-M-d" ,#'racket-hash-lang-down)
     ("C-M-q" ,#'racket-hash-lang-C-M-q-dwim))))

(define-minor-mode racket-hash-lang-repl-mode
  "A minor mode just to override some keybindings in `racket-repl-mode'.

\\{racket-hash-lang-repl-mode-map}
"
  :lighter " #lang"
  :keymap racket-hash-lang-repl-mode-map)

(defun racket--hash-lang-configure-repl-buffer-from-edit-buffer ()
  "Update the `racket-repl-mode' buffer associated with the current edit buffer.

A value for the hook `racket--repl-configure-buffer-hook'.

To be called when a `racket-mode' or `racket-hash-lang-mode' edit
buffer is `current-buffer'.

It is possible for multiple edit buffers to \"take turns\" using
the same `racket-repl-mode' buffer, for successive `racket-run'
commands. Even if various edit buffers all use
`racket-hash-lang-mode', the hash-lang for each may differ, e.g.
one buffer is \"#lang racket\" while another is \"#lang
rhombus\"."
  ;;;(message "racket--hash-lang-configure-repl called from buffer %s" (buffer-name))
  (let ((hl (and (eq major-mode 'racket-hash-lang-mode)
                 racket--hash-lang-id))
        (edit-buffer (current-buffer)))
    (with-racket-repl-buffer
      ;; Clean up from previous hash-lang use of REPL, if any
      (racket--hash-lang-delete)
      ;; char-syntax
      (set-syntax-table (with-current-buffer edit-buffer (syntax-table)))
      (setq-local syntax-propertize-function
                  (with-current-buffer edit-buffer syntax-propertize-function))
      ;; font-lock
      (setq-local font-lock-defaults
                  (with-current-buffer edit-buffer font-lock-defaults))
      (setq-local font-lock-fontify-region-function
                  (racket--repl-limited-fontify-region
                   (with-current-buffer edit-buffer font-lock-fontify-region-function)))
      (font-lock-set-defaults)
      ;; indent
      (setq-local indent-line-function
                  (with-current-buffer edit-buffer indent-line-function))
      (setq-local indent-region-function
                  (with-current-buffer edit-buffer indent-region-function))
      ;; nav
      (setq-local forward-sexp-function
                  (with-current-buffer edit-buffer forward-sexp-function))
      (racket-hash-lang-repl-mode (if hl 1 -1)) ;keybindings
      (when hl
        (racket--hash-lang-create edit-buffer))
      (if hl
          (add-hook 'after-change-functions #'racket--hash-lang-after-change-hook t t)
        (remove-hook 'after-change-functions  #'racket--hash-lang-after-change-hook t))
      (setq-local racket-repl-submit-function
                  (if hl #'racket-hash-lang-submit nil)))))
(add-hook 'racket--repl-before-run-hook
          #'racket--hash-lang-configure-repl-buffer-from-edit-buffer)

(defun racket-hash-lang-submit (input)
  ""
  (or (not racket--hash-lang-submit-predicate-p)
      (racket--cmd/await nil
                         `(hash-lang
                           submit-predicate
                           ,racket--hash-lang-id
                           ,input
                           t))))

(provide 'racket-hash-lang)

;; racket-hash-lang.el ends here
