;;; racket-hash-lang.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'elec-pair)
(require 'seq)
(require 'racket-cmd)
(require 'racket-mode)
(require 'racket-repl)

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `((("C-c C-c"
       "C-c C-k")   ,#'racket-run-module-at-point)
     ("C-c C-z"     ,#'racket-edit-switch-to-repl)
     ("C-c C-t"     ,#'racket-test)
     ("C-c C-l"     ,#'racket-logger)
     ("C-c C-o"     ,#'racket-profile)
     ("M-C-x"       racket-send-definition)
     ("C-x C-e"     racket-send-last-sexp)
     ("C-c C-r"     racket-send-region)
     ("C-c C-e f"   ,#'racket-expand-file)
     ("C-c C-e x"   racket-expand-definition)
     ("C-c C-e e"   racket-expand-last-sexp)
     ("C-c C-e r"   racket-expand-region)
     ("C-c C-x C-f" ,#'racket-open-require-path)
     ("TAB"         ,#'racket-hash-lang-indent)
     ("<backtab>"   ,#'racket-hash-lang-indent-reverse)
     ;; ("C-c C-p"     racket-cycle-paren-shapes) equivalent using paren-matches?
     ("M-C-y"       ,#'racket-insert-lambda)
     ("C-c C-f"     racket-fold-all-tests)
     ("C-c C-u"     racket-unfold-all-tests)
     ("RET"         ,#'newline-and-indent)
     ("C-M-b"       ,#'racket-hash-lang-backward)
     ("C-M-f"       ,#'racket-hash-lang-forward)
     ("C-M-u"       ,#'racket-hash-lang-up)
     ("C-M-d"       ,#'racket-hash-lang-down)
     ("C-M-q"       ,#'racket-hash-lang-C-M-q-dwim)
     ,@racket--f5-bindings)))

(easy-menu-define racket-hash-lang-mode-menu racket-hash-lang-mode-map
  "Menu for `racket-hash-lang-mode'."
  '("Racket-Hash-Lang"
    ("Run"
     ["in REPL" racket-run]
     ["in REPL and switch to REPL" racket-run-and-switch-to-repl]
     ["in *shell* using `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["in *shell* using `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests :active (racket--sexp-edit-mode-p)]
     ["Unfold All" racket-unfold-all-tests :active (racket--sexp-edit-mode-p)])
    ("Eval"
     ["Region" racket-send-region :active (and (region-active-p) (racket--sexp-edit-mode-p))]
     ["Definition" racket-send-definition :active (racket--sexp-edit-mode-p)]
     ["Last S-Expression" racket-send-last-sexp :active (racket--sexp-edit-mode-p)])
    ("Macro Expand"
     ["File" racket-expand-file]
     ["Region" racket-expand-region :active (and (region-active-p) (racket--sexp-edit-mode-p))]
     ["Definition" racket-expand-definition :active (racket--sexp-edit-mode-p)]
     ["Last S-Expression" racket-expand-last-sexp  :active (racket--sexp-edit-mode-p)])
    ["Switch to REPL" racket-repl]
    ("Tools"
     ["Profile" racket-profile]
     ["Error Trace" racket-run-with-errortrace]
     ["Step Debug" racket-run-with-debugging]
     ["Toggle XP Mode" racket-xp-mode])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Insert Symbol" racket-insert-symbol]
    ["Indent" racket-hash-lang-indent]
    ["Indent Reverse" racket-hash-lang-indent-reverse]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes :active (racket--sexp-edit-mode-p)]
    ["Align" racket-align :active (racket--sexp-edit-mode-p)]
    ["Unalign" racket-unalign :active (racket--sexp-edit-mode-p)]
    "---"
    ["Open Require Path" racket-open-require-path]
    ["Find Collection" racket-find-collection]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["List Racket Packages" list-racket-packages]
    ["Customize..." customize-mode]))

(defvar-local racket--hash-lang-submit-predicate-p nil)

(defvar racket-hash-lang-module-language-hook nil
  "Hook run when the module language changes.

Typically in Emacs each language gets its own major mode. As a
result, the major mode hook is your opportunity to express
preferences. However `racket-hash-lang-mode' handles radically
different kinds of hash langs in one major mode. And a given
buffer can change langs when you edit the \"#lang\" line. As a
result, `racket-hash-lang-mode-hook' is not useful for per-lang
configuration. Instead you need a kind of \"sub major mode
hook\". This is that hook.

The hook is run when a file is first visited, and thereafter
whenever the \"#lang\" line is edited -- provided that results in
different language info; for example changing from \"#lang
racket\" to \"#lang racket/base\" will /not/ run the hook.

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
suitable for the module language, else `electric-pair-local-mode':

#+BEGIN_SRC elisp
  (defun my-hook (module-language)
    (let ((rackety
           (member module-language
                   (list \"racket\" \"racket/base\"
                         \"typed/racket\" \"typed/racket/base\"))))
      (electric-pair-local-mode (if rackety -1 1))
      (paredit-mode (if rackety 1 -1))))
  (add-hook \\='racket-hash-lang-module-language-hook #\\='my-hook)
#+END_SRC

A similar tactic can be used for `smartparens'. In general,
neither of these modes is likely to work well unless the
hash-lang uses racket for drracket:grouping-position, in which
case `racket-hash-lang-mode' uses the classic `racket-mode'
syntax-table for the buffer. Otherwise you should not enable one
of these modes, and instead just use the simple delimiter
matching of `electric-pair-local-mode', as configured by
`racket-hash-lang-mode'.

As another example, if you prefer more colors than just tokens,
choices include:

- Enable `racket-xp-mode' in `racket-hash-lang-mode-hook' and in
  the module language hook locally set
  `racket-xp-add-binding-faces':

#+BEGIN_SRC elisp
  (setq-local racket-xp-add-binding-faces t)
#+END_SRC

  OR

- Use some of the regexp search-based fontification from classic
  `racket-mode' for rackety module languages:

#+BEGIN_SRC elisp
  (require \\='racket-font-lock)
  (if rackety
      (font-lock-add-keywords nil
                              (append racket-font-lock-keywords-2
                                      racket-font-lock-keywords-3))
    (font-lock-remove-keywords nil
                               (append racket-font-lock-keywords-2
                                       racket-font-lock-keywords-3)))
#+END_SRC
")

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

(defvar-local racket-hash-lang-mode-lighter "#lang")

(defconst racket--agnostic-syntax-table
  (let ((table (make-syntax-table)))
    ;; From Emacs Lisp Info node "Syntax Table Internals":
    ;;
    ;;     Code Class
    ;;     0    whitespace
    ;;     1    punctuation
    ;;     2    word
    ;;     3    symbol
    ;;     4    open parenthesis
    ;;     5    close parenthesis
    (map-char-table (lambda (key code+char)
                      (unless (<= 0 (car code+char) 5)
                        (aset table key '(3))))
                    table)
    table)
  "Like `standard-syntax-table' but even simpler.

The only syntax categories in this table are whitespace,
punctuation, word, symbol, and open/close parens. Chars with any
other syntax are changed to symbol syntax.

For example we change all string-quote syntax to symbol, because
the chars used to delimit strings vary among programming
languages. Although that example happens to be the only practical
difference from `standard-syntax-table', today, we still make a
generalized pass over it to be sure.

Note: Open/close paren syntax is preserved on the theory that,
although the /meaning/ of those characters may vary among langs,
their use as paired delimiters is likely universal, and it is
useful to support various Emacs features such as
rainbow-delimiters.

Note: `standard-syntax-table' is a better choice for spans lexed
as \"text\" tokens, because ?\" is definitely a string delimiter
in English.")

;;;###autoload
(define-derived-mode racket-hash-lang-mode prog-mode
  'racket-hash-lang-mode-lighter
  "An \"experimental\" major mode to edit any Racket #lang.

This major mode uses color-lexer, indent, and navigation supplied
by each #lang -- which means Racket Mode's back end process needs
to be running.

In your Emacs configuration, you may want to update the
variable `auto-mode-alist' to use `racket-hash-lang-mode' for
file extensions like \".rkt\", \".scrbl\", and/or \".rhm\".

Languages supply colors for lexer tokens like strings and
comments; see the customization variable
`racket-hash-lang-token-face-alist'. For more colors see the hook
variable `racket-hash-lang-module-language-hook', which can also
be used to vary configurations per language.

A discussion of the information provided by a Racket language:

  <https://docs.racket-lang.org/tools/lang-languages-customization.html>

\\{racket-hash-lang-mode-map}
"
  (racket--polite-user-f-keys racket-hash-lang-mode-map racket--f5-bindings)
  (racket-call-racket-repl-buffer-name-function)
  (add-hook 'kill-buffer-hook
            #'racket-mode-maybe-offer-to-kill-repl-buffer
            nil t)
  (set-syntax-table racket--agnostic-syntax-table)
  ;; Tell `parse-partial-sexp' to consider syntax-table text
  ;; properties.
  (setq-local parse-sexp-lookup-properties t)
  ;; Here we do the usual, approved thing: Set `font-lock-defaults'
  ;; (and let `font-lock-set-defaults' to calculate and set other
  ;; font-lock-xxx variables correctly).
  (setq font-lock-defaults
        (list
         ;; "keywords": Although we contribute none here (we only use
         ;; lang lexer tokens), we support other parties using
         ;; `font-lock-add-keywords', such as a minor mode -- or even
         ;; an end user adding static `racket-mode' font-lock keyword
         ;; lists when the hash-lang is racket.
         nil
         ;; "keywords-only?": We absolutely don't want any syntactic
         ;; fontification; see e.g. #679. Any char syntax table we set
         ;; is intended to hep fit into the Emacs ecosystem for things
         ;; like `paredit'. Using that for font-lock isn't reliable;
         ;; we trust the lang lexer tokens, only.
         t))
  (setq-local text-property-default-nonsticky
              (append (list (cons 'racket-token t))
                      text-property-default-nonsticky))
  ;; Default values for electric-pair-local-mode; see also
  ;; `racket--hash-lang-configure-pairs' for values set whenever the
  ;; hash lang changes.
  (electric-pair-local-mode -1)
  (setq-local electric-pair-pairs nil)
  (setq-local electric-pair-text-pairs nil)
  (setq-local electric-pair-open-newline-between-pairs nil) ;#685
  (electric-indent-local-mode -1)
  (setq-local electric-indent-inhibit t)
  (setq-local blink-paren-function nil)
  (setq-local imenu-create-index-function nil)
  (setq-local completion-at-point-functions nil) ;rely on racket-xp-mode
  (setq racket-submodules-at-point-function nil) ;might change in on-new-lang
  (when (boundp 'paredit-space-for-delimiter-predicates)
    (setq-local paredit-space-for-delimiter-predicates
                (list #'racket--paredit-space-for-delimiter-predicate)))
  ;; Create back end hash-lang object.
  ;;
  ;; On the one hand, `racket--cmd/await' would be simpler to use
  ;; here. On the other hand, when the back end isn't running, there's
  ;; a delay for that to start, during which the buffer isn't
  ;; displayed and Emacs seems frozen. On the third hand, if we use
  ;; `racket--cmd/async' naively the buffer could try to interact with
  ;; a back end object that doesn't yet exist, and error.
  ;;
  ;; Warm bowl of porridge: Make buffer read-only and use async
  ;; command to create hash-lang object. Only when the response
  ;; arrives, i.e. the back end object is ready, enable read/write and
  ;; set various hook functions that depend on `racket--hash-lang-id'.
  ;;
  ;; Also, handle the back end returning nil for the create -- meaning
  ;; there's no sufficiently new syntax-color-lib -- by downgrading to
  ;; plain `prog-mode'.
  (setq-local racket--hash-lang-id nil) ;until async command response
  (setq-local racket--hash-lang-generation 1)
  (unless (racket--cmd-ready-p)
    (setq-local header-line-format "Waiting for back end to start..."))
  (setq-local buffer-read-only t)
  (racket--cmd/async
   nil
   `(hash-lang create
               ,(cl-incf racket--hash-lang-next-id)
               ,nil
               ,(buffer-substring-no-properties (point-min) (point-max)))
   (lambda (maybe-id)
     (setq-local header-line-format nil)
     (cond
      (maybe-id
       (setq-local racket--hash-lang-id maybe-id)
       ;; These need non-nil `racket--hash-lang-id':
       (setq-local font-lock-fontify-region-function #'racket--hash-lang-fontify-region)
       (add-hook 'after-change-functions #'racket--hash-lang-after-change t t)
       (add-hook 'kill-buffer-hook #'racket--hash-lang-delete t t)
       (add-hook 'change-major-mode-hook #'racket--hash-lang-delete t t)
       (setq-local buffer-read-only nil))
      (t
       (prog-mode) ;wipes all local variables including buffer-read-only
       (message "hash-lang support not available; needs newer syntax-color-lib")))))  )

(defun racket--hash-lang-delete ()
  (when racket--hash-lang-id
    ;; When back end running, delete the hash-lang object. (Otherwise,
    ;; don't start the back end just to delete something that doesn't
    ;; exist.)
    (when (racket--cmd-open-p)
      (ignore-errors
        (racket--cmd/await
         (when (eq major-mode 'racket-repl-mode) racket--repl-session-id)
         `(hash-lang delete ,racket--hash-lang-id))))
    (setq-local racket--hash-lang-id nil)
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

;;; Updates: Front end --> back end

(defun racket--hash-lang-repl-buffer-string (beg end)
  "Like `buffer-substring-no-properties' but treat as whitespace --
preserving only line breaks for indentation -- everything that is
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

(defun racket--hash-lang-after-change (beg end len)
  ;;;(message "racket--hash-lang-after-change %s %s %s" beg end len)
  ;; This might be called as frequently as once per single changed
  ;; character.
  (when racket--hash-lang-id
    (let ((str (if (eq major-mode 'racket-repl-mode)
                   (racket--hash-lang-repl-buffer-string beg end)
                 (buffer-substring-no-properties beg end))))
      (racket--cmd/async
       nil
       `(hash-lang update
                   ,racket--hash-lang-id
                   ,(cl-incf racket--hash-lang-generation)
                   ,beg
                   ,len
                   ,str)))))

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
  ;;;(message "racket--hash-lang-on-new-lang %S" plist)
  (with-silent-modifications
    (save-restriction
      (widen)
      (unless (eq major-mode 'racket-repl-mode)
        (racket--hash-lang-remove-text-properties (point-min) (point-max))
        (font-lock-flush (point-min) (point-max)))
      (racket--hash-lang-configure-pairs (plist-get plist 'paren-matches)
                                         (plist-get plist 'quote-matches))
      ;; If the lang uses racket-grouping-position, i.e. it uses
      ;; s-expressions, then use racket-mode-syntax-table. That way
      ;; some other "classic" Emacs features and packages are more
      ;; likely to work. Otherwise, make a syntax table assuming
      ;; nothing and relying solely on the syntax-table text
      ;; properties we add from tokens.
      (set-syntax-table (if (plist-get plist 'racket-grouping)
                            racket-mode-syntax-table
                          racket--agnostic-syntax-table))
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
      (setq-local racket-hash-lang-mode-lighter
                  (concat "#lang"
                          (when (plist-get plist 'racket-grouping) "()")
                          (when (plist-get plist 'range-indenter) "⇉")))
      ;; Finally run user's module-language-hook.
      (run-hook-with-args 'racket-hash-lang-module-language-hook
                          (plist-get plist 'module-language)))))

(defun racket--hash-lang-on-changed-tokens (_gen beg end)
  "The back end has processed a change that resulted in new tokens.

All we do here is mark the span as not fontified, then let
jit-lock do its thing as/when this span ever becomes visible."
  ;;;(message "racket--hash-lang-on-changed-tokens %s %s %s" _gen beg end)
  (save-restriction
    (widen)
    (jit-lock-refontify (max beg (point-min))
                        (min end (point-max)))))

;;; Fontification

(defun racket--hash-lang-fontify-region (beg end _loudly)
  "Our value for the variable `font-lock-fontify-region-function'.

Just claim we fontified the region now, and ask the back end for
tokens asynchronously. Inappropriate to wait for a response while
being called from Emacs C redisplay engine."
  ;;;(message "racket--hash-lang-fontify-region %s %s" beg end)
  (when racket--hash-lang-id
    (let ((beg (if (markerp beg) (marker-position beg) beg))
          (end (if (markerp end) (marker-position end) end)))
      (racket--cmd/async nil
                         `(hash-lang get-tokens
                                     ,racket--hash-lang-id
                                     ,racket--hash-lang-generation
                                     ,beg
                                     ,end)
                         (lambda (tokens)
                           (racket--hash-lang-tokens+fontify beg end tokens))))
    `(jit-lock-bounds ,beg . ,end)))

(defun racket--hash-lang-tokens+fontify (beg end tokens)
  "Put token properties and do \"normal\" keyword fontification, both.

Although we could have done the normal fontification earlier
synchronously, and done token propertization here later, the
result wouldn't always be consistent. It's best to handle both
together -- and best to token propertize first, since that sets
syntax-table props for comments and strings, thereby correctly
preventing keyword fontification inside those.

We only call `font-lock-fontify-keywords-region', not the full
`font-lock-default-fontify-region'. Why: 1. We only support
keyword fontification, not syntactic. Even though we set
`font-lock-keywords-only' true in our mode initialization,
belt+suspenders here. 2. It makes moot the value of
`font-lock-extend-region-functions', so that's one less value
that need be set."
  ;;;(message "racket--hash-lang-tokens+fontify %S %S <tokens>" beg end)
  (with-silent-modifications
    (let ((beg (min beg (point-max)))
          (end (min end (point-max))))
      ;; As this removes face property do it before adding face props
      ;; from tokens.
      (save-excursion
        (font-lock-unfontify-region beg end))
      (racket--hash-lang-put-tokens tokens)
      (save-excursion
        (font-lock-fontify-keywords-region beg end)))))

(defun racket--hash-lang-put-tokens (tokens)
  ;;;(message "racket--hash-lang-put-tokens %S" tokens)
  ;; Assumes called within dynamic extent of `with-silent-modifications'.
  (save-restriction
    (widen)
    (cl-flet* ((put-face (beg end face)
                         (put-text-property beg end 'face face))
               (get-face-at (pos)
                            (get-text-property pos 'face))
               (remove-face (beg end)
                            (remove-list-of-text-properties beg end '(face)))
               (put-stx (beg end stx)
                        (put-text-property beg end 'syntax-table stx))
               (put-fence (beg end stx)
                          (put-stx beg (1+ beg) stx)
                          (put-stx (1- end) end stx)))
      (dolist (token tokens)
        (pcase-let ((`(,beg ,end ,kinds) token))
          (setq beg (max (point-min) beg))
          (setq end (min end (point-max)))
          (racket--hash-lang-remove-text-properties beg end)
          ;; Add a 'racket-token prop used just for me to inspect via
          ;; `describe-char'. Use a vector of symbols as the value
          ;; because `describe-property-list' presents lists of
          ;; symbols as "widgets" in the UI.
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
               (put-face beg end (racket--sexp-comment-face (get-face-at beg))))
              ('parenthesis (when (facep 'parenthesis)
                              (put-face beg end 'parenthesis)))
              ('text (put-stx beg end (standard-syntax-table))
                     (put-face beg end racket-hash-lang-text))
              (kind
               (if-let (face (cdr (assq kind racket-hash-lang-token-face-alist)))
                   (put-face beg end face)
                 (remove-face beg end))))))))))

(defun racket--hash-lang-remove-text-properties (beg end)
  "Remove `racket--hash-lang-text-properties' from region BEG..END."
  (remove-list-of-text-properties beg end '(syntax-table racket-token)))

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
  "Supplied for use only by `indent-region'.

Not used by `racket-hash-lang-indent'."
  (pcase (racket--hash-lang-get-range-indent-changes from upto nil)
    ('false (let ((indent-region-function nil))
              (indent-region from upto)))
    (`() nil)
    (changes (racket--hash-lang-apply-changes from upto changes))))

(defun racket-hash-lang-indent ()
  "Indent the line or region using behavior supplied by the #lang.

A variation of `indent-for-tab-command' intended to be bound to the TAB
key. Unlike that command, any active region -- in the sense of
`transient-mark-mode' -- is preserved.

Some langs support indentation alternatives, in which case repeatedly
invoking this command will cycle among those choices indefinitely.

When this command does not change indent, and `tab-always-indent' is
\\='complete, it behaves like `indent-for-tab-command' and calls
`completion-at-point'."
  (interactive)
  (racket--hash-lang-range-indent nil))

(defun racket-hash-lang-indent-reverse ()
  "Like `racket-hash-lang-indent', but reversing indent choices if any.

Uses drracket:range-indentation/reverse-choices when supplied by a lang,
otherwise equivalent to `racket-hash-lang-indent'.

Note that this is not necessarily an \"outdent\" command."
  (interactive)
  (racket--hash-lang-range-indent t))

(defun racket--hash-lang-range-indent (reverse)
  (let* ((deactivate-mark nil) ;don't deactivate mark
         (region-p (use-region-p))
         (from (if region-p (region-beginning) (line-beginning-position)))
         (upto (if region-p (region-end)       (line-end-position)))
         (upto (if (= from upto) (1+ upto) upto)))
    (pcase (racket--hash-lang-get-range-indent-changes from upto reverse)
      ('false
       ;; When drracket:range-indentation/reverse-choices not
       ;; supplied, fall back to trying drracket:range-indentation,
       ;; then fall back to drracket:indentation via our
       ;; `indent-line-function' via either `indent-region' or
       ;; `indent-for-tab-command'.
       (if reverse
           (racket--hash-lang-range-indent nil)
         (if region-p
             (let ((indent-region-function nil))
               (indent-region from upto))
           (indent-for-tab-command))))
      (changes
       (let* ((old-tick (buffer-chars-modified-tick))
              (old-point (point))
              (within-indent-p (and (not region-p)
                                    (save-excursion
                                      (back-to-indentation)
                                      (<= old-point (point))))))
         (racket--hash-lang-apply-changes from upto changes)
         ;; As with 'indent-for-tab-command', do `back-to-indentation'
         ;; IFF point was already within the leading indentation.
         (when within-indent-p
           (back-to-indentation))
         ;; As with `indent-for-tab-command', when indent did not
         ;; change maybe try completion.
         (when (and (not reverse)
                    (not region-p)
                    (eq tab-always-indent 'complete)
                    (eql old-point (point))
                    (eql old-tick (buffer-chars-modified-tick)))
           (completion-at-point)))))))

(defun racket--hash-lang-get-range-indent-changes (from upto reverse)
  (racket--cmd/await             ;await = :(
   nil
   `(hash-lang indent-range-amounts
               ,racket--hash-lang-id
               ,racket--hash-lang-generation
               ,from
               ,upto
               ,reverse)))

(defun racket--hash-lang-apply-changes (from upto changes)
  (save-excursion
    (goto-char from)
    ;; drracket:range-indentation docs say `changes` could have more
    ;; elements than lines in from..upto, and we should ignore extras.
    ;; Handle that. (Although it could also have fewer, we need no
    ;; special handling for that here.)
    (let ((changes (seq-take changes (count-lines from upto))))
      (dolist (change changes)
        (pcase-let ((`(,delete-amount ,insert-string) change))
          (beginning-of-line)
          (when (< 0 delete-amount) (delete-char delete-amount))
          (unless (equal "" insert-string) (insert insert-string))
          (end-of-line 2))))))

;; Motion

(defun racket-hash-lang-move (direction count &optional scan-error-p)
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
      (_
       (if scan-error-p
           (signal 'scan-error
                   (list (format "Cannot move %s" direction)
                         (point) (point)))
         (user-error "Cannot move %s%s"
                     direction
                     (if (memq count '(-1 0 1))
                         ""
                       (format " %s times" count))))))))

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
    (racket-hash-lang-move dir cnt t)))

;;; Pairs

(defun racket--hash-lang-configure-pairs (paren-matches quote-matches)
  "Configure and enable `electric-pair-local-mode'.

Caveat: Because elec-pair handles only single character
delimiters we must ignore multi-character paren or quote strings.

Caveat: When quote-matches includes \\=' we ignore that, because
it's undesirable inside strings or comments. Although it might be
more correct to omit that only from `electric-pair-text-pairs',
elec-pair doesn't know how to use that based on our buffer
tokenization, so for now this is a practical compromise."
  (let ((pairs nil))
    (cl-flet ((add (open close)
                (when (and (= 1 (length open))
                           (= 1 (length close)))
                  (push (cons (aref open  0)
                              (aref close 0))
                        pairs))))
      (dolist (p paren-matches)
        (pcase-let ((`(,open . ,close) p))
          (add open close)))
      (dolist (q quote-matches)
        (unless (equal q "'")
          (add q q))))
    (setq-local electric-pair-pairs (reverse pairs)))
  (setq-local electric-pair-text-syntax-table
              racket--agnostic-syntax-table)
  (setq-local electric-pair-skip-self t) ;#747
  (electric-pair-local-mode 1))

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
                     (pcase-lambda (`(,_beg ,_end (,kind . ,_)))
                       (cl-case kind
                         ((white-space) (message "whitespace; did nothing"))
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

(defun racket--configure-repl-buffer-from-edit-buffer (edit-buffer repl-buffer)
  "Configure REPL-BUFFER from EDIT-BUFFER.

To be called upon each run command. EDIT-BUFFER is the buffer
where the run command was issued, REPL-BUFFER is the
`racket-repl-mode' buffer to be used.

It is possible for multiple edit buffers to \"take turns\" using
the same `racket-repl-mode' buffer, for successive `racket-run'
commands. Even if various edit buffers all use
`racket-hash-lang-mode', the hash-lang for each may differ, e.g.
one buffer is \"#lang racket\" while another is \"#lang
rhombus\"."
  ;;;(message "%S" (list 'racket--configure-repl-buffer-from-edit-buffer edit-buffer repl-buffer))
  (let ((hash-lang-p (with-current-buffer edit-buffer (eq major-mode 'racket-hash-lang-mode))))
    (with-current-buffer repl-buffer
      ;; Clean up from previous hash-lang use of REPL, if any
      (racket--hash-lang-delete)

      ;; Maybe create hash-lang object, synchronously.
      (when hash-lang-p
        (setq-local
         racket--hash-lang-id
         (racket--cmd/await
          nil
          `(hash-lang
            create
            ,(cl-incf racket--hash-lang-next-id)
            ,(with-current-buffer edit-buffer
               (save-restriction
                 (widen)
                 (buffer-substring-no-properties (point-min) (min 4096 (point-max)))))
            ,(racket--hash-lang-repl-buffer-string (point-min) (point-max))))))

      ;; char-syntax
      (set-syntax-table (with-current-buffer edit-buffer (syntax-table)))
      (setq-local syntax-propertize-function
                  (with-current-buffer edit-buffer syntax-propertize-function))
      (setq-local syntax-propertize-extend-region-functions
                  (with-current-buffer edit-buffer syntax-propertize-extend-region-functions))
      ;; font-lock
      (setq-local font-lock-keywords
                  (with-current-buffer edit-buffer font-lock-keywords))
      (setq-local font-lock-keywords-only t) ;#751
      (setq-local racket--repl-fontify-region-function
                  (with-current-buffer edit-buffer font-lock-fontify-region-function))
      (font-lock-flush)
      ;; indent
      (setq-local indent-line-function
                  (with-current-buffer edit-buffer indent-line-function))
      (setq-local indent-region-function
                  (with-current-buffer edit-buffer indent-region-function))
      ;; nav
      (setq-local forward-sexp-function
                  (with-current-buffer edit-buffer forward-sexp-function))
      (racket-hash-lang-repl-mode (if hash-lang-p 1 -1)) ;keybindings
      (if hash-lang-p
          (add-hook 'after-change-functions #'racket--hash-lang-after-change t t)
        (remove-hook 'after-change-functions  #'racket--hash-lang-after-change t))
      (setq-local racket-repl-submit-function
                  (if hash-lang-p #'racket-hash-lang-submit nil)))))

(defun racket--hash-lang-repl-on-stop-back-end ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'racket-repl-mode)
                 (buffer-live-p buf))
        ;; Clean up from previous hash-lang use of REPL, if any
        (racket-hash-lang-repl-mode -1)
        (setq-local racket--repl-fontify-region-function
                    #'font-lock-default-fontify-region)
        (setq-local racket--hash-lang-id nil)
        (setq-local racket--hash-lang-generation 1)))))
(add-hook 'racket-stop-back-end-hook
          #'racket--hash-lang-repl-on-stop-back-end)

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
