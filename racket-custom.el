;;; racket-custom.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; All `defcustom's and `defface's go here.
;;; This makes it easier to provide a consistent UI.

;; NOTE: `:prefix` is disabled as of Emacs 24.3, so I'm using explicit
;; `:tag`s. But also note that options are sorted (by default; user
;; can turn that off) based on the identifier name not the `:tag`. As
;; a result, I'm defining `:tag`s AS IF `:prefix "racket-"` did work.
;; In other words defcustom of racket-foo-bar has a :tag "Foo Bar".

(require 'rx)
(require 'sh-script) ;for sh-heredoc face
(require 'comint) ;for comint-simple-send in racket-shell-or-terminal

(defgroup racket nil
  "Modes for the Racket language."
  :group 'languages
  :link '(url-link :tag "README on GitHub" "https://github.com/greghendershott/racket-mode/blob/master/README.md"))

;; These aliases need be _before_ the `defcustom' of `racket-program'
;; (see note in doc for `define-obsolete-variable-alias').
(define-obsolete-variable-alias 'racket-racket-program 'racket-program "2017-06-02")
(define-obsolete-variable-alias 'racket-raco-program   'racket-program "2017-06-02")

(defvar racket--winp (eq 'windows-nt system-type))

(defcustom racket-program (if racket--winp "Racket.exe" "racket")
  "Pathname of the Racket executable.

Note that a back end configuration can override this with a
non-nil `racket-program` property list value. See
`racket-add-back-end'."
  :tag "Racket Program"
  :type '(file :must-match t)
  :risky t
  :group 'racket)

(make-obsolete-variable 'racket-command-port nil "2020-04-25")

(make-obsolete-variable 'racket-command-startup nil "2020-01-23")

(defcustom racket-command-timeout 10
  "How many seconds to wait for command server responses.

Note: This is mostly obsolete, fortunately, because it applies
only to commands that must block the Emacs UI until they get a
response. Instead most Racket Mode commands these days receive
their response asychronously."
  :tag "Command Timeout"
  :type 'integer
  :risky t
  :group 'racket)

(make-obsolete-variable 'racket-path-from-emacs-to-racket-function nil "2020-08-26")

(make-obsolete-variable 'racket-path-from-racket-to-emacs-function nil "2020-08-26")

(defcustom racket-browse-url-function
  'racket-browse-url-using-temporary-file
  "Function to call to browse a URL."
  :tag "Browse URL Function"
  :type 'function
  :risky t
  :group 'racket)

(defcustom racket-documentation-search-location
  "https://docs.racket-lang.org/search/index.html?q=%s"
  "The location of the Racket \"Search Manuals\" web page.
Where `racket-documentation-search', `racket-xp-documentation'
and `racket-repl-documentation' should look for the search page.

- If the value of this variable is the symbol \"local\", open the
  search page from the local documentation, as with \"raco doc\".

- Otherwise, the value is a string recognizable by `format', with
  \"%s\" at the point at which to insert the user's search text
  after applying `url-hexify-string'. Apart from \"%s\", the
  string should be a properly encoded URL."
  :tag "Documentation Search Location"
  :type '(choice (string :tag "URL")
                 (const :tag "Local" local))
  :safe (lambda (val) (or (stringp val) (eq val 'local)))
  :group 'racket)

(defcustom racket-shell-or-terminal-function 'racket-shell
  "How `racket-racket' and `racket-raco-test' run commands.

The function should accept a command string, not including a
newline, get or create a suitable buffer, send the command, and
send a newline or enter.

Predefined choices include `racket-shell', `racket-term',
`racket-ansi-term', and `racket-vterm'."
  :tag "Shell or Terminal"
  :type 'function
  :options '(racket-shell
             racket-term
             racket-ansi-term
             racket-vterm)
  :risky t
  :group 'racket)

;;; Xp Mode

(defgroup racket-xp nil
  "`racket-xp-mode' options"
  :tag "Xp Mode"
  :group 'racket)

(defcustom racket-xp-after-change-refresh-delay 1
  "Seconds to wait before refreshing `racket-xp-mode' annotations.

Set to nil to disable automatic refresh and manually use `racket-xp-annotate'."
  :tag "Racket XP Mode After Change Refresh Delay"
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Off" nil))
  :safe #'integerp
  :group 'racket-xp)

(defcustom racket-xp-mode-lighter
  '(:eval (racket--xp-mode-lighter))
  "Mode line lighter for `racket-xp-mode'.

Set to nil to disable the mode line completely."
  :tag "Racket Xp Mode Lighter"
  :type 'sexp
  :risky t
  :group 'racket-xp)

(defcustom racket-xp-highlight-unused-regexp "^[^_]"
  "Only give `racket-xp-unused-face' to unused bindings that match this regexp.

The default is to highlight identifiers that do not start with
an underline, which is a common convention."
  :tag "Racket Xp Mode Do Not Highlight Unused Regexp"
  :type 'regexp
  :safe #'stringp
  :group 'racket-xp)

(defcustom racket-xp-binding-font-lock-face-modes '(racket-hash-lang-mode)
  "Major modes where `racket-xp-mode' will fontify binding identifier sites.

A \\='font-lock-face property is added for bindings from:

  - the module language, using `racket-xp-binding-lang-face' and
    `racket-xp-binding-lang-use-face'.

  - other imports, using `racket-xp-binding-import-face' and
    `racket-xp-binding-import-use-face'.

  - local definitions, using `racket-xp-binding-local-face' and
    `racket-xp-binding-local-use-face'.

This has a visible effect only when there is /not/ also a
\\='face property applied by the major mode's fontification."
  :tag "Racket Xp Mode Binding Font Lock Face Modes"
  :type '(repeat symbol)
  :safe #'listp
  :group 'racket-xp)

;;; Hash Lang

(defgroup racket-hash-lang nil
  "`racket-hash-lang-mode' options"
  :tag "Hash Lang"
  :group 'racket)

(defcustom racket-hash-lang-token-face-alist
  `((constant           . font-lock-constant-face)
    (error              . error)
    (other              . font-lock-doc-face)
    (keyword            . font-lock-keyword-face)
    (hash-colon-keyword . racket-keyword-argument-face)
    (at                 . font-lock-doc-face))
  "An association list from color-lexer token symbols to face symbols.

Note: In many Racket languages, the lexer classifies tokens for
identifiers as \\='symbol. In many programs, a majority of the
source consists of identifiers at binding definition and use
sites. Therefore the appearance of \"symbol\" tokens is
significant, and a matter of personal preference.

  - If you prefer a \"plainer\" appearance, similar to Dr Racket:
    Add \\='symbol with the value \\='default. This gives an
    explicit \\='face property that prevails over any
    \\='font-lock-face property that a minor mode might apply to
    enhance the basic fontification.

  - If you prefer a more \"colorful\" appearance, similar to
    \"classic\" `racket-mode': Do /not/ map \\='symbol tokens in
    this list. Instead enable `racket-xp-mode' and let it do
    \"semantic\" highlighting of bindings; see the customization
    variable `racket-xp-binding-font-lock-face-modes'.

Note: Some tokens are hardwired and not customizable by this
list: Comment tokens use the face `font-lock-comment-face',
sometimes blended with other faces. Parenthesis tokens use the
face `parenthesis' if defined, as by the paren-face package.
String tokens use `font-lock-string-face'. Text tokens, e.g.
Scribble text, use the face `default'"
  :tag "Hash Lang Token Face Association List"
  :type '(alist :key-type symbol :value-type face)
  :safe #'listp
  :group 'racket-hash-lang)

;;; REPL

(defgroup racket-repl nil
  "`racket-repl-mode' options"
  :tag "REPL"
  :group 'racket)

(defcustom racket-repl-buffer-name-function nil
  "How to associate `racket-mode' edit buffers with `racket-repl-mode' buffers.

The default is nil, which is equivalent to supplying
`racket-repl-buffer-name-shared': One REPL buffer is shared.

Other predefined choices include `racket-repl-buffer-name-unique'
and `racket-repl-buffer-name-project'.

This is used when a `racket-mode' buffer is created. Changing
this to a new value only affects `racket-mode' buffers created
later.

Any such function takes no arguments, should look at the variable
`buffer-file-name' if necessary, and either `setq-default' or
`setq-local' the variable `racket-repl-buffer-name' to a desired
`racket-repl-mode' buffer name. As a result, `racket-run'
commands will use a buffer of that name, creating it if
necessary."
  :tag "REPL Buffer Name Function"
  :type '(choice (const :tag "One REPL buffer for all edit buffers" nil)
                 (const :tag "One REPL buffer for all project edit buffers" racket-repl-buffer-name-project)
                 (const :tag "One REPL buffer for each edit buffer" racket-repl-buffer-name-unique)
                 (function :tag "Other function"))
  :risky t
  :group 'racket-repl)

(defcustom racket-submodules-to-run '((test) (main))
  "Extra submodules to run.

This is a list of submodules. Each submodule is described as a
list, to support submodules nested to any depth.

This is used by commands that emulate the DrRacket Run command:

\\<racket-mode-map>

 - `racket-run'
 - `racket-run-and-switch-to-repl' \\[racket-run-and-switch-to-repl]

It is NOT used by commands that run one specific module, such as:

 - `racket-run-module-at-point' \\[racket-run-module-at-point]
 - `racket-test' \\[racket-test]
 - `racket-profile'"
  :tag "Submodules to Run"
  :type '(repeat (repeat :tag "Module path" symbol))
  :safe #'listp
  :group 'racket-repl)

(defcustom racket-memory-limit 2048
  "Terminate the Racket process if memory use exceeds this value in MB.

Changes to this value take effect upon the next `racket-run'. A value
of 0 means no limit.

Caveat: This uses Racket's `custodian-limit-memory`, which does
not enforce the limit exactly. Instead, the program will be
terminated upon the first garbage collection where memory exceeds
the limit (maybe by a significant amount)."
  :tag "Memory Limit"
  :type 'integer
  :safe #'integerp
  :group 'racket-repl)

(defcustom racket-error-context 'medium
  "The amount of context for error messages.

Each increasing level supplies better context (\"stack trace\")
for error messages, but causing your program to run more slowly.

  - low corresponds to compile-enforce-module-constants #t and
    compile-context-preservation-enabled #f.

  - medium corresponds to compile-enforce-module-constants #f and
    compile-context-preservation-enabled #t, which disables some
    optimizations like inlining.

  - high corresponds to medium plus the use of errortrace, which
    extensively instruments your code and therefore might cause
    it to run significantly slower.

Tip: Regardless of this setting, you can enable high errortrace
for a specific `racket-run' or `racket-run-module-at-point' by
using \\[universal-argument]. This lets you normally run with a
lower, faster setting, and re-run when desired to get a
more-helpful error message."
  :tag "Error Context"
  :type '(radio (const :tag "Low" low)
                (const :tag "Medium (better context but slower)" medium)
                (const :tag "High (best context but slowest)" high))
  :risky t
  :group 'racket-repl)

(make-obsolete-variable 'racket-retry-as-skeleton nil "2020-02-26")

(defcustom racket-repl-history-directory
  (locate-user-emacs-file (file-name-as-directory "racket-mode"))
  "Directory for `racket-repl-mode' history files."
  :tag "REPL History Directory"
  :type 'file
  :group 'racket-repl)

(defcustom racket-history-filter-regexp "\\`\\s *\\'"
  "Input matching this regexp are NOT saved on the history list.
Default value is a regexp to ignore input that is all whitespace."
  :tag "History Filter Regexp"
  :type 'regexp
  :safe #'stringp
  :group 'racket-repl)

(defcustom racket-images-inline t
  "Whether to display inline images in the REPL."
  :tag "Images Inline"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-repl)

(defcustom racket-imagemagick-props nil
  "Use ImageMagick with these properties for REPL images.

When this property list is not empty -- and the variable
`racket-images-inline' is true, and Emacs is built with with
ImageMagick support -- then `create-image' is called with
\"imagemagick\" as the type and with this property list.

For example, to scale images whose width is larger than 500
pixels, supply (:max-width 500)."
  :tag "ImageMagick Props"
  :type '(plist :key-type symbol
                :value-type (choice number string))
  :options '((:max-width integer)
             (:max-height integer)
             (:background string)
             (:width integer)
             (:height integer)
             (:rotation float))
  :risky t
  :group 'racket-repl)

(defcustom racket-images-keep-last 100
  "How many images to keep in the image cache."
  :tag "Images Keep Last"
  :type 'integer
  :safe #'integerp
  :group 'racket-repl)

(defcustom racket-images-system-viewer (if (eq system-type 'darwin)
                                           "open"
                                         "display")
  "The image viewer program to use for `racket-view-image'."
  :tag "Images System Viewer"
  :type 'string
  :risky t
  :group 'racket-repl)

(defcustom racket-pretty-print t
  "Use pretty-print instead of print in REPL?"
  :tag "Pretty Print"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-repl)

(defcustom racket-use-repl-submit-predicate nil
  "Should `racket-repl-submit' use a drracket:submit-predicate?

A language can provide such a predicate, for example when the
language syntax is not s-expressions. When t `racket-repl-submit'
will use this to decide whether to submit your input, yet."
  :tag "Use REPL Submit Predicate"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-repl)


(defcustom racket-before-run-hook nil
  "Normal hook done before various Racket Mode run commands.

Here \"before\" means that the `racket-repl-mode' buffer might not
exist yet.

When hook functions are called, `current-buffer' is that of the
edit buffer when the run command was issued. If a hook function
instead needs the `racket-repl-mode' buffer, it should get that
from the variable `racket-repl-buffer-name'."
  :tag "Before Run Hook"
  :type 'hook
  :risky t
  :group 'racket-repl)

(defcustom racket-after-run-hook nil
  "Normal hook done after various Racket Mode run commands.

Here \"after\" means that the run has completed and the REPL is
waiting at another prompt.

When hook functions are called, `current-buffer' is that of the
buffer when the run command was issued. If a hook function
instead needs the `racket-repl-mode' buffer, it should get that
from the variable `racket-repl-buffer-name'."
  :tag "After Run Hook"
  :type 'hook
  :risky t
  :group 'racket-repl)

;;; Other

(defgroup racket-other nil
  "Other Options"
  :tag "Other"
  :group 'racket)

(defcustom racket-indent-curly-as-sequence t
  "Indent `{}` with items aligned with the head item?

This is indirectly disabled if `racket-indent-sequence-depth' is 0.
This is safe to set as a file-local variable."
  :tag "Indent Curly As Sequence"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-other)

(defcustom racket-indent-sequence-depth 0
  "To what depth should `racket-indent-line' search.

This affects the indentation of forms like \\='() \\=`() #() --
and {} if `racket-indent-curly-as-sequence' is t --- but not
#\\='() #\\=`() ,() ,@(). A zero value disables, giving the
normal indent behavior of DrRacket or Emacs `lisp-mode' derived
modes like `scheme-mode'. Setting this to a high value can make
indentation noticeably slower. This is safe to set as a
file-local variable."
  :tag "Indent Sequence Depth"
  :type 'integer
  :safe #'integerp
  :group 'racket-other)

(defcustom racket-pretty-lambda nil
  "Display lambda keywords using λ. This is DEPRECATED.

Instead use `prettify-symbols-mode' in newer verisons of Emacs,
or, use `racket-insert-lambda' to insert actual λ characters."
  :tag "Pretty Lambda"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-other)

(defcustom racket-smart-open-bracket-enable nil
  "This variable is obsolete and has no effect.

Instead of using this variable, you may bind the `[` key to the
`racket-smart-open-bracket' command in the `racket-mode-map'
and/or `racket-repl-mode-map' keymaps."
  :tag "Smart Open Bracket Enable"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-other)

(defcustom racket-module-forms
  (rx (syntax ?\()
      (or (seq "module" (zero-or-one (any ?* ?+)))
          "library"))
  "Regexp for the start of a `module`-like form.

Affects what `beginning-of-defun' will move to. This is safe to
set as a file-local variable."
  :tag "Top Level Forms"
  :type 'string
  :safe #'stringp
  :group 'racket-other)

(defcustom racket-logger-config
  '((cm-accomplice           . warning)
    (GC                      . info)
    (module-prefetch         . warning)
    (optimizer               . info)
    (racket/contract         . error)
    (racket-mode-debugger    . info)
    (sequence-specialization . info)
    (*                       . fatal))
  "Configuration of `racket-logger-mode' topics and levels.

The topic \"*\" respresents the default level used for topics not
assigned a level. Otherwise, the topic symbols are the same as
used by Racket's `define-logger`.

The levels are those used by Racket's logging system: \"debug\",
\"info\", \"warning\", \"error\", \"fatal\".

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

The default value sets some known \"noisy\" topics to be one
level quieter. That way you can set the \"*\" topic to a level
like \"debug\" and not get overhwelmed by these noisy topics."
  :tag "Logger Configuration"
  :type '(alist :key-type symbol :value-type symbol)
  :safe (lambda (xs)
          (cl-every (lambda (x)
                      (and (symbolp (car x))
                           (symbolp (cdr x))))
                    xs))
  :load "racket-cmd"
  :set (lambda (var val)
         (set-default var val)
         (when (fboundp 'racket--logger-activate-config)
           (racket--logger-activate-config)))
  :group 'racket-other)

(defcustom racket-show-functions
  (list 'racket-show-pseudo-tooltip)
  "An \"abnormal hook\" variable to customize `racket-show'.

This is a list of one or more functions.

Each such function must accept two arguments: STR and POS.

STR is one of:

  - Non-blank string: Display the string somehow.

  - Blank string: Hide any previously displayed string.

  - nil: Hide any persistent UI that might have been created. For
    instance `racket-show-header-line' hides the header line.

POS may be nil when STR is nil or a blank string.

Otherwise POS is the buffer position -- typically the end of a
span -- that the non-blank STR describes.

A function that shows STR near POS should position it not to hide
the span, i.e. below and/or right of POS. Examples:
`racket-show-pseudo-tooltip' and `racket-show-pos-tip'.

A function that shows STR in a fixed location may of course
ignore POS. Examples: `racket-show-echo-area' and
`racket-show-header-line'"
  :tag "Racket Show Functions"
  :type '(repeat function)
  :options '(racket-show-pseudo-tooltip
             racket-show-echo-area
             racket-show-header-line
             racket-show-pos-tip)
  :risky t
  :group 'racket-other)

;;; Faces

(defgroup racket-faces nil
  "Racket Faces"
  :tag "Racket Faces"
  :group 'faces
  :group 'racket)

(defmacro defface-racket (id facespec docstr tag)
  `(progn
     (defconst ,id ',id)
     (defface ,id
       ,facespec
       ,docstr
       :tag ,tag
       :group 'racket-faces)))

(defface-racket racket-xp-def-face
  '((t (:inherit match :underline (:style line))))
  "Face `racket-xp-mode' uses when point is on a definition."
  "Definition Face")

(defface-racket racket-xp-use-face
  '((t (:inherit match)))
  "Face `racket-xp-mode' uses when point is on a use."
  "Use Face")

(defface-racket racket-xp-binding-lang-face
  '((t (:inherit font-lock-doc-face)))
  "Face `racket-xp-mode' gives to the module language name.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Lang Face")

(defface-racket racket-xp-binding-lang-use-face
  '((t (:inherit font-lock-keyword-face)))
  "Face `racket-xp-mode' gives uses of bindings imported from the module language.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Lang Use Face")

(defface-racket racket-xp-binding-import-face
  '((t (:inherit default)))
  "Face `racket-xp-mode' gives to imported module names.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Import Face")

(defface-racket racket-xp-binding-import-use-face
  '((t (:inherit font-lock-keyword-face)))
  "Face `racket-xp-mode' gives uses of imported bindings.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Import Use Face")

(defface-racket racket-xp-binding-local-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face `racket-xp-mode' gives to local definitions.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Local Face")

(defface-racket racket-xp-binding-local-use-face
  '((t (:inherit default)))
  "Face `racket-xp-mode' gives to uses of local definitions.

See the variable `racket-xp-binding-font-lock-face-modes'."
  "Binding Local Use Face")

(defface-racket racket-xp-error-face
  '((t (:underline (:color "red" :style wave))))
  "Face `racket-xp-mode' uses to highlight errors."
  "Error Face")

(defface-racket racket-xp-unused-face
  '((t (:strike-through t)))
  "Face `racket-xp-mode' uses to highlight unused requires or definitions."
  "Unused Face")

(defface-racket racket-xp-tail-target-face
  '((t (:inherit match :underline (:style wave))))
  "Face `racket-xp-mode' uses to highlight targets of a tail position."
  "Tail Target Face")

(defface-racket racket-xp-tail-position-face
  '((t (:inherit match)))
  "Face `racket-xp-mode' uses to highlight expressions in a tail position."
  "Tail Position Face")

(defface-racket racket-keyword-argument-face
  '((((background dark))
     (:foreground "IndianRed"))
    (((background light))
     (:foreground "Red3")))
  "Face for `#:keyword` arguments."
  "Keyword Argument Face")

;; Note: Don't use `define-obsolete-face-alias'; see issue #583.
(defface racket-paren-face nil
  "This face is unused since 2017-06-13.

Instead customize the face `paren-face', which is provided by the
optional package `paren-face'.")

;; Note: Don't use `define-obsolete-face-alias'; see issue #583.
(defface racket-selfeval-face nil
  "This face is unused since 2021-10-20.

Instead customize the face `font-lock-constant-face'.")

(defface-racket racket-reader-quoted-symbol-face
  '((t (:inherit font-lock-constant-face)))
  "Face for symbols quoted using \\=' or \\=`.

This face is given only to symbols directly quoted using the
reader shorthands \\=' or \\=`. All other directly quoted values,
including symbols quoted using \"quote\" or \"quasiquote\", get
the face `font-lock-constant-face'."
  "Reader Quoted Symbol Face")

(defface-racket racket-reader-syntax-quoted-symbol-face
  '((t (:inherit default)))
  "Face for symbols quoted using #\\=' or #\\=`.

This face is given only to symbols directly quoted using the
reader shorthands #\\=' or #\\=`. All other directly quoted
values, including symbols quoted using \"syntax\" or
\"quasisyntax\", get the face `font-lock-constant-face'."
  "Reader Syntax Quoted Symbol Face")

(defface-racket racket-here-string-face
  '((t (:inherit sh-heredoc)))
  "Face for here strings."
  "Here String Face")

(defface-racket racket-logger-config-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for `racket-logger-mode' configuration."
  "Racket Logger Config Face")

(defface-racket racket-logger-topic-face
  '((t (:inherit font-lock-function-name-face :slant italic)))
  "Face for `racket-logger-mode' topics."
  "Racket Logger Config Face")

(defface-racket racket-logger-fatal-face
  '((t (:inherit error :weight bold)))
  "Face for `racket-logger-mode' fatal level."
  "Racket Logger Fatal Face")

(defface-racket racket-logger-error-face
  '((t (:inherit error)))
  "Face for `racket-logger-mode' error level."
  "Racket Logger Error Face")

(defface-racket racket-logger-warning-face
  '((t (:inherit warning)))
  "Face for `racket-logger-mode' warning level."
  "Racket Logger Warning Face")

(defface-racket racket-logger-info-face
  '((t (:inherit font-lock-string-face)))
  "Face for `racket-logger-mode' info level."
  "Racket Logger Info Face")

(defface-racket racket-logger-debug-face
  '((t (:inherit font-lock-constant-face)))
  "Face for `racket-logger-mode' debug level."
  "Racket Logger Debug Face")

(defface-racket racket-debug-break-face
  '((t (:background "red")))
  "Face for `racket-debug-mode' break position."
  "Racket Debug Break Face")

(defface-racket racket-debug-breakpoint-face
  '((t (:foreground "red" :weight bold)))
  "Face for `racket-debug-mode' breakpoint overlays."
  "Racket Debug Breakpoint Face")

(defface-racket racket-debug-locals-face
  '((t (:inherit font-lock-constant-face :box (:line-width -1) :slant italic)))
  "Face for `racket-debug-mode' local variables."
  "Racket Debug Locals Face")

(defface-racket racket-debug-result-face
  '((t (:inherit font-lock-constant-face :box (:line-width -1) :slant italic :weight bold)))
  "Face for `racket-debug-mode' result values."
  "Racket Debug Result Face")

(defface-racket racket-doc-link-face
  '((t (:underline (:color "gray" :style line))))
  "Face `racket-describe-mode' uses for links within documentation.
Note: When some special face is already specified by the
documentation, then to avoid visual clutter this face is NOT also
added."
  "Racket Doc Link Face")

(defface-racket racket-ext-link-face
  '((t (:underline (:style wave) :slant italic :weight bold)))
  "Face `racket-describe-mode' uses for external links.
See the variable `racket-browse-url-function'."
  "Racket Ext Link Face")

(defface-racket racket-doc-output-face
  '((t (:inherit fixed-pitch-serif)))
  "Face `racket-describe-mode' uses for Scribble @example or @interactions output."
  "Racket Doc Output Face")

(defface-racket racket-doc-litchar-face
  '((t (:inherit holiday)))
  "Face `racket-describe-mode' uses for Scribble @litchar."
  "Racket Doc Litchar Face")

(defface-racket racket-repl-message
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face `racket-repl-mode' uses for messages from the back end."
  "Racket REPL Message")

(defface-racket racket-repl-prompt
  '((t (:inherit bold)))
  "Face `racket-repl-mode' uses for prompts."
  "Racket REPL Prompt")

(defface-racket racket-repl-value
  '((t (:inherit font-lock-constant-face)))
  "Face `racket-repl-mode' uses for values output by current-print."
  "Racket REPL Value")

(defface-racket racket-repl-error-message
  '((t (:inherit error)))
  "Face `racket-repl-mode' uses for error messages."
  "Racket REPL Error Message")

(defface-racket racket-repl-error-location
  '((t (:inherit underline)))
  "Face `racket-repl-mode' uses for error locations."
  "Racket REPL Error Location")

(defface-racket racket-repl-error-label
  '((t (:inherit font-lock-variable-name-face)))
  "Face `racket-repl-mode' uses for error labels."
  "Racket REPL Error Label")

(defface-racket racket-repl-stdout
  '((t (:inherit default)))
  "Face `racket-repl-mode' uses for output to current-output-port."
  "Racket REPL Stdout")

(defface-racket racket-repl-stderr
  '((t (:inherit error)))
  "Face `racket-repl-mode' uses for output to current-error-port."
  "Racket REPL Stderr")

(provide 'racket-custom)

;;; racket-custom.el ends here
