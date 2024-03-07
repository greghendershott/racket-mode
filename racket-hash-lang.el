;;; racket-hash-lang.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'elec-pair)
(require 'ob-core)
(require 'org-src)
(require 'seq)
(require 'racket-cmd)
(require 'racket-mode)
(require 'racket-repl)

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `((("C-c C-c"
       "C-c C-k")   ,#'racket-run-module-at-point)
     ("C-c C-z"     ,#'racket-edit-switch-to-repl)
     ("<f5>"        ,#'racket-run-and-switch-to-repl)
     ("M-C-<f5>"    ,#'racket-racket)
     ("C-<f5>"      ,#'racket-test)
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
     ("TAB"         ,#'indent-for-tab-command)
     ;; ("C-c C-p"     racket-cycle-paren-shapes) equivalent using paren-matches?
     ("M-C-y"       ,#'racket-insert-lambda)
     ("C-c C-f"     racket-fold-all-tests)
     ("C-c C-u"     racket-unfold-all-tests)
     ("RET"         ,#'newline-and-indent)
     ("DEL"         ,#'racket-hash-lang-delete-backward-char)
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
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes :active (racket--sexp-edit-mode-p)]
    ["Align" racket-align :active (racket--sexp-edit-mode-p)]
    ["Unalign" racket-unalign :active (racket--sexp-edit-mode-p)]
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
suitable for the module language:

#+BEGIN_SRC elisp
  (defun my-hook (module-language)
    (let ((rackety
           (member module-language
                   (list \"racket\" \"racket/base\"
                         \"typed/racket\" \"typed/racket/base\"))))
      (if rackety
          (paredit-mode 1)
        (paredit-mode -1))))
  (add-hook \\='racket-hash-lang-module-language-hook #\\='my-hook)
#+END_SRC

A similar tactic can be used for `smartparens' or
`electric-pair-mode'. In general, none of these
delimiter-matching modes is likely to work well unless the
hash-lang uses racket for drracket:grouping-position, in which
case `racket-hash-lang-mode' uses the classic `racket-mode'
syntax-table for the buffer. Otherwise you should not enable one
of these modes, and instead just use the simple delimiter
matching built into `racket-hash-lang-mode'; see
`racket-hash-lang-pairs'.

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

;;;###autoload
(define-derived-mode racket-hash-lang-mode prog-mode
  'racket-hash-lang-mode-lighter
  "Use color-lexer, indent, and navigation supplied by a #lang.

An experimental major mode alternative to `racket-mode' for
source file edit buffers.

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
  (racket-call-racket-repl-buffer-name-function)
  (add-hook 'kill-buffer-hook
            #'racket-mode-maybe-offer-to-kill-repl-buffer
            nil t)
  (set-syntax-table racket--plain-syntax-table)
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
  (add-hook 'post-self-insert-hook #'racket-hash-lang-post-self-insert nil t)
  (add-hook 'self-insert-uses-region-functions #'racket-hash-lang-will-use-region nil t)
  (electric-pair-local-mode -1)
  (setq-local electric-pair-pairs nil)
  (setq-local electric-pair-text-pairs nil)
  (setq-local electric-pair-open-newline-between-pairs nil) ;#685
  (electric-indent-local-mode -1)
  (setq-local electric-indent-inhibit t)
  (setq-local blink-paren-function nil)
  (setq-local imenu-create-index-function nil)
  (setq-local completion-at-point-functions nil) ;rely on racket-xp-mode
  (setq-local eldoc-documentation-function nil)
  (setq racket-submodules-at-point-function nil) ;might change in on-new-lang
  (setq-local racket--hash-lang-id
              (racket--cmd/await
               nil
               `(hash-lang
                 create
                 ,(cl-incf racket--hash-lang-next-id)
                 ,nil
                 ,(buffer-substring-no-properties (point-min) (point-max)))))
  (cond
   (racket--hash-lang-id
    (setq-local racket--hash-lang-generation 1)
    ;; These need non-nil `racket--hash-lang-id':
    (setq-local font-lock-fontify-region-function #'racket--hash-lang-fontify-region)
    (setq-local font-lock-ensure-function #'racket--hash-lang-font-lock-ensure)
    (add-hook 'after-change-functions #'racket--hash-lang-after-change-hook t t)
    (add-hook 'kill-buffer-hook #'racket--hash-lang-delete t t)
    (add-hook 'change-major-mode-hook #'racket--hash-lang-delete t t)
    (message ""))
   (t
    (prog-mode)  ;note: resets all buffer-local variables
    (message "hash-lang support not available; needs newer syntax-color-lib"))))

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

;;; Org babel support

(defmacro racket-declare-hash-lang-for-org-babel (lang ext)
  "Arrange for a Racket hash-lang to work with org-babel.

LANG should be an unquoted symbol, same as you would use in a
Racket =#lang= line.

EXT should be a string with the file extension for LANG, /not/
including any dot.

Examples:

  (racket-define-hash-lang rhombus \"rhm\")
  (racket-define-hash-lang scribble/manual \"scrbl\")

This macro will:

0. Define a major mode derived from `racket-hash-lang-mode' named
   `racket-hash-lang:LANG-mode'.

1. Add the language to `org-src-lang-modes' and
   `org-babel-tangle-lang-exts'.

2. Define a org-babel-edit-prep:LANG function.

3. Define a org-babel-execute:LANG function, which delegates to
   `racket--hash-lang-org-babel-execute'. See its doc string for
   more information -- including why this macro /cannot/ also
   define a org-babel-expand-body:LANG function.

4. Allow a buffer to omit the explicit #lang line, when it is
   created by `org-mode' for user editing or formatting of a
   source code block whose language property is LANG.

Discussion:

A valid Racket program consists of one outermost module per
source file, using one lang. Typically this is expressed using a
=#lang= line -- which must occur exactly once at the start of the
file. In such a buffer, `racket-hash-lang-mode' \"just works\".

When using multiple `org-mode' source blocks of the same lang,
the situation is trickier:

- Although you could start /every/ block with a lang line, that's
  tedious, and org-tangle will concatenate them into an invalid
  program.

- On the other hand, if you start only the /first/ block with a
  lang line, then various org-babel features won't work properly
  with the subsequent blocks. Basically this is because org
  creates a hidden buffer using `racket-hash-lang-mode', but the
  source block's lang property value is not available to that
  buffer, so it can't know what lang line to add automatically.

- Similarly, if you use the :shebang property to tangle
  correctly, that property value is not available in the hidden
  buffers created by org mode.

TL;DR: Org assumes that each lang will have a major mode that
knows enough to do what is required. To accommodate this it is
simplest to define a distinct major mode for each org source
block language."
  (let* ((lang-str (symbol-name lang))
         (lighter (concat "#lang:" lang-str))
         (doc (format "Major mode for #lang %s derived from `racket-hash-lang-mode'."
                      lang))
         (no-suffix-mode-name (intern (concat "racket-hash-lang:" lang-str)))
         (full-mode-name (intern (concat (symbol-name no-suffix-mode-name) "-mode")))
         (org-babel-execute-name (intern (concat "org-babel-execute:" lang-str)))
         (org-babel-edit-prep-name (intern (concat "org-babel-edit-prep:" lang-str))))
    `(progn
       ;; Tell `org-mode' that this org source block language is
       ;; handled by our major mode -- note that the -mode suffix is
       ;; intentionally omitted here.
       (require 'org-src)
       (add-to-list 'org-src-lang-modes (cons ,lang-str ',no-suffix-mode-name))

       ;; Tell `org-babel-tangle' to write source blocks to files with
       ;; this extension (when no property specifies a filename).
       (require 'ob-tangle)
       (add-to-list 'org-babel-tangle-lang-exts (cons ,lang-str ,ext))

       ;; Note: In this macro we follow the (usually) best practice of
       ;; delegating most of the work to plain old helper functions
       ;; (limiting the macro things that can only be done via macro).

       ;; Define a suitable org-babel-execute:<lang> function.
       (defun ,org-babel-execute-name (body params)
         ,(format "A %s lang wrapper for `racket--hash-lang-org-babel-execute'."
                  lang-str)
         (racket--hash-lang-org-babel-execute ,lang-str body params))

       ;; Define a suitable org-babel-edit-prep:<lang> function.
       (defun ,org-babel-edit-prep-name (_babel-info)
         (racket--hash-lang-org-babel-edit-prep ,lang-str))

       (define-derived-mode ,full-mode-name racket-hash-lang-mode
         ,lighter
         ,doc
         (racket--hash-lang-init-derived-mode ,lang-str)))))

(defun racket--hash-lang-init-derived-mode (lang-str)
  ;; Allow buffers to omit the #lang line, which can be useful
  ;; when the buffer is being used from an `org-mode' source
  ;; block to do formatting (font-lock).
  ;;
  ;; Use the option (also used by the REPL) where we give the back end
  ;; hash-lang object the lang line directly, instead of it looking in
  ;; the normal program text.
  ;;
  ;; Note that we have no opportunity to run before the parent mode
  ;; function, so all we can do here is RE-create the hash-lang
  ;; object.
  (let ((lang-line-text (concat "#lang " lang-str "\n")))
    (racket--hash-lang-delete)
    (setq-local racket--hash-lang-id
                (racket--cmd/await
                 nil
                 `(hash-lang
                   create
                   ,(cl-incf racket--hash-lang-next-id)
                   ,lang-line-text
                   ,(buffer-substring-no-properties (point-min) (point-max)))))
    (unless racket--hash-lang-id
      (prog-mode) ;note: resets all buffer-local variables
      (message "hash-lang support not available; needs newer syntax-color-lib"))))

(defun racket--hash-lang-org-babel-edit-prep (lang-str)
  (racket--hash-lang-maybe-add-lang-line lang-str t))

(defun racket--hash-lang-org-babel-execute (lang-str body params)
  "A basic way to run Racket programs using any #lang.

If a lang-specific org-babel-expand-body:<lang> function exists
it is called with BODY, to support optional functionality that we
can't possibly know how to do for any given lang's syntax and
semantics, for example :vars input.

Only supports :result-type output -- not values."
  (let* ((processed-params (org-babel-process-params params))
         (result-params (assq :result-params processed-params))
         (result-type (cdr (assq :result-type processed-params)))
         (_ (unless (eq result-type 'output)
              (error "Can only handle :result-type output.")))
         (expand-body (intern (concat "org-babel-expand-body:" lang-str)))
         (body (if (fboundp expand-body)
                   (funcall expand-body body params processed-params)
                 body))
         (tmp-src-file (org-babel-temp-file "racket-hash-lang-src-" ".rkt"))
         (_ (with-temp-file tmp-src-file
              (insert body)
              (racket--hash-lang-maybe-add-lang-line lang-str nil)))
         (cmdline (concat racket-program " " tmp-src-file))
         (result (org-babel-eval cmdline "")))
    (delete-file tmp-src-file)
    (org-babel-result-cond result-params result)))

(defun racket--hash-lang-maybe-add-lang-line (lang-str &optional set-write-back-p)
  "When the buffer lacks a lang line, add one.

Otherwise things like `racket-xp-mode' will report errors.

IFF we add one, arrange for a write-back function to remove it.
This is possible starting in Org 9.0.9 which IIUC is in Emacs
25.2+, due to the `org-src--allow-write-back' var, which may be a
function value. Note: Because `org-src--contents-for-write-back'
strips text properties, we can't insert a propertized string to
look for later, so we must rely on searching for the literal text
we actually added, if any."
  (let* ((lang-line-str (concat "#lang " lang-str "\n"))
         (end-pos (1+ (length lang-line-str))))
    (unless (string= (buffer-substring-no-properties (point-min) end-pos)
                     lang-line-str)
      (save-excursion
        (goto-char (point-min))
        (insert lang-line-str))
      (when (and set-write-back-p
                 (boundp 'org-src--allow-write-back)) ;>25.1
        (setq org-src--allow-write-back
              (lambda ()
                (when (string= (buffer-substring-no-properties (point-min) end-pos)
                               lang-line-str)
                  (delete-region (point-min) end-pos))))))))

;; The above suffices for font-lock, edit and tangle. Suffices for
;; execute in simple cases, and leaves it up to a user-defined
;; org-babel-expand-body:<lang> to do fancier but totally
;; lang-dependent things like handling input vars by wrapping user
;; program in bindings or definitions.
;;
;; See https://orgmode.org/worg/org-contrib/babel/languages/index.html
;; and https://git.sr.ht/~bzg/worg/tree/master/item/org-contrib/babel/ob-template.el
;;
;; See ob-c, ob-clojure, and others for examples.

;; Go ahead and define derived modes for some common hash-langs.

(racket-declare-hash-lang-for-org-babel rhombus "rhm")
(racket-declare-hash-lang-for-org-babel scribble "scrbl")

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
  (when racket--hash-lang-id
    (racket--cmd/async
     nil
     `(hash-lang update
                 ,racket--hash-lang-id
                 ,(cl-incf racket--hash-lang-generation)
                 ,beg
                 ,len
                 ,(if (eq major-mode 'racket-repl-mode)
                      (racket--hash-lang-repl-buffer-string beg end)
                    (buffer-substring-no-properties beg end))))))

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
                          racket--plain-syntax-table))
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

(defun racket--hash-lang-font-lock-ensure (beg end)
  "Our value for the variable `font-lock-ensure-function'.

Provided for things like `org-src-font-lock-fontify-block' that
call `font-lock-ensure' expecting it means ensured /now/."
  (when racket--hash-lang-id
    ;; Because `org-src-font-lock-fontify-block' inserts text with
    ;; `inhibit-modification-hooks', we need to update the back end
    ;; hash-lang object before getting tokens to fontify. It's fine to
    ;; reuse the after-change hook here: although it issues the
    ;; hash-lang update command async, it also increments
    ;; `racket--hash-lang-generation', so the subsequent hash-lang
    ;; get-tokens command will block until the update has completed
    ;; through that generation.
    (racket--hash-lang-after-change-hook beg end (- end beg))
    (let ((tokens (racket--cmd/await nil
                                     `(hash-lang get-tokens
                                                 ,racket--hash-lang-id
                                                 ,racket--hash-lang-generation
                                                 ,beg
                                                 ,end))))
      (racket--hash-lang-tokens+fontify beg end tokens ))))

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
    ;; As this removes face property do it before adding face props
    ;; from tokens.
    (save-excursion
      (font-lock-unfontify-region beg end))
    (racket--hash-lang-put-tokens tokens)
    (save-excursion
      (font-lock-fontify-keywords-region beg end))))

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
              ('text (put-stx beg end racket--plain-syntax-table))
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
  "Maybe use #lang drracket:range-indentation, else plain `indent-region'."
  (pcase (racket--cmd/await             ;await = :(
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

;;; Pairs

;; Although this may seem like (and in fact be) an Alan Perlis
;; implementation of half of fancier auto-pair modes, we have two
;; justifications:
;;
;; 1. A Racket lang may supply multi-chararacter open and close
;; delimiters. AFAICT electric-pair-mode can't handle this.
;;
;; 2. Even with single characters, I couldn't see how to make
;; electric-pair-mode work consistently -- including having it _not_
;; pair things like ' inside tokens like comments, strings, text.

(defvar-local racket-hash-lang-pairs nil
  "Pairs of delimiters to insert or delete automatically.

The format of each item is (cons string string).

This is initialized whenever a module language changes, using
values from the language's reported values for
drracket:paren-matches and drracket:quote-matches.

You may customize this default initialization in
`racket-hash-lang-module-language-hook'.")

(defvar-local racket-hash-lang-pairs-predicate
  #'racket-hash-lang-pairs-predicate-default)
(defun racket-hash-lang-pairs-predicate-default (pair pos)
  (not
   (and (equal (car pair) "'")
        (pcase-let ((`(,_beg ,_end (,kind . ,_))
                     (racket-hash-lang-classify (1- pos))))
          (memq kind '(string comment text))))))

(defun racket-hash-lang-classify (pos)
  (racket--cmd/await nil
                     `(hash-lang
                       classify
                       ,racket--hash-lang-id
                       ,racket--hash-lang-generation
                       ,pos)))

(defun racket--hash-lang-configure-pairs (paren-matches quote-matches)
  (let ((pairs nil))
    (dolist (p paren-matches) (push p pairs))
    (dolist (q quote-matches) (push (cons q q) pairs))
    (setq-local racket-hash-lang-pairs (reverse pairs))))

(defun racket--hash-lang-lookup-pair (char pos &optional prefer-larger-match-p)
  ;; The idea behind PREFER-LARGER-MATCHES-P is that a lang might have
  ;; paren-matches like both () and '()' as indeed does rhombus. When
  ;; inserting let's treat that as '' then (). But when deleting back
  ;; over '( we'd prefer to just delete that as one thing. So here we can
  ;; lookup either way.
  ;;
  ;; This is written _not_ to assume that CHAR is already in the
  ;; buffer, so that we can be used by a self-insert-uses-region
  ;; function. Of course when OPEN consists of multiple characters, we
  ;; must look for the others already in the buffer before POS.
  (seq-reduce
   (lambda (answer-so-far pair)
     (let* ((open (car pair))
            (len (length open)))
       (or (and (< 0 (- pos 1 (1- len)))
                (equal open
                       (concat
                        (buffer-substring-no-properties (- pos 1 (1- len)) (- pos 1))
                        (string char)))
                (funcall racket-hash-lang-pairs-predicate pair (point))
                (or (not answer-so-far)
                    (funcall (if prefer-larger-match-p #'> #'<)
                             (length open) (length (car answer-so-far))))
                pair)
           answer-so-far)))
   racket-hash-lang-pairs
   nil))

(defun racket-hash-lang-will-use-region ()
  "A value for `self-insert-uses-region-functions'."
  (and (use-region-p)
       (racket--hash-lang-lookup-pair last-command-event (1+ (point)))
       t))

(defun racket-hash-lang-post-self-insert ()
  "A value for `post-self-insert-hook'."
  (pcase (racket--hash-lang-lookup-pair last-command-event (point))
    (`(,open . ,close)
     (cond ((not (use-region-p))
            (save-excursion
              (insert close)))
           ((< (point) (mark))
            (save-excursion
              (goto-char (mark))
              (insert close))
            (goto-char (1- (point))))
           ((< (mark) (point))
            ;; Delete open already inserted after region.
            (delete-char (- (length open)))
            (insert close)
            (save-excursion
              (goto-char (mark))
              (insert open)))))))

(defun racket-hash-lang-delete-backward-char ()
  "Delete previous character, and possibly paired delimiters.

When point immediately follows text matching the longest open
delimiter string in `racket-hash-lang-pairs`, delete that. When
point also immediately precedes the matching close, also delete
that."
  (interactive)
  (pcase (racket--hash-lang-lookup-pair (char-before) (point) t)
    (`(,open . ,close)
     (when (equal close
                  (buffer-substring-no-properties (point) (+ (point) (length close))))
       (save-excursion (delete-char (length close))))
     (delete-char (- (length open))))
    (_ (delete-char -1))))

(put 'racket-hash-lang-delete-backward-char 'delete-selection 'supersede)

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
      ;; font-lock
      (setq-local font-lock-keywords
                  (with-current-buffer edit-buffer font-lock-keywords))
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
          (add-hook 'after-change-functions #'racket--hash-lang-after-change-hook t t)
        (remove-hook 'after-change-functions  #'racket--hash-lang-after-change-hook t))
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
