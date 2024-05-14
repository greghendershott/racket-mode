;;; racket-custom.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; All `defcustom's and `defface's go here.
;;; This makes it easier to provide a consistent UI.

(require 'rx)
(require 'sh-script) ;for sh-heredoc face
(require 'comint) ;for comint-simple-send in racket-shell-or-terminal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket group

(defgroup racket nil
  "Modes for the Racket language."
  :group 'languages
  :link '(url-link :tag "README on GitHub" "https://github.com/greghendershott/racket-mode/blob/master/README.md"))

;; These aliases need be _before_ the `defcustom' of `racket-program'
;; (see note in doc for `define-obsolete-variable-alias').
(define-obsolete-variable-alias 'racket-racket-program 'racket-program "2017-06-02")
(define-obsolete-variable-alias 'racket-raco-program   'racket-program "2017-06-02")

(defvar racket--macp (eq 'darwin system-type))
(defvar racket--winp (eq 'windows-nt system-type))

(defcustom racket-program (if racket--winp "Racket.exe" "racket")
  "Pathname of the Racket executable.

Note that a back end configuration can override this with a
non-nil `racket-program` property list value. See
`racket-add-back-end'."
  :type '(file :must-match t)
  :risky t)

(make-obsolete-variable 'racket-command-port nil "2020-04-25")

(make-obsolete-variable 'racket-command-startup nil "2020-01-23")

(defcustom racket-command-timeout 10
  "How many seconds to wait for command server responses.

Note: This is mostly obsolete, fortunately, because it applies
only to commands that must block the Emacs UI until they get a
response. Instead most Racket Mode commands these days receive
their response asychronously."
  :type 'integer
  :risky t)

(make-obsolete-variable 'racket-path-from-emacs-to-racket-function nil "2020-08-26")

(make-obsolete-variable 'racket-path-from-racket-to-emacs-function nil "2020-08-26")

(defcustom racket-browse-url-function
  (if racket--macp
      'racket-browse-url-using-temporary-file
    'browse-url)
  "Function to call to browse a URL.

Defaults to `racket-browse-url-using-temporary-file' on macOS and
`browse-url' on other platforms."
  :type 'function
  :risky t)

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
  :type '(choice (string :tag "URL")
                 (const :tag "Local" local))
  :safe (lambda (val) (or (stringp val) (eq val 'local))))

(defcustom racket-shell-or-terminal-function 'racket-shell
  "How `racket-racket' and `racket-raco-test' run commands.

The function should accept a command string, not including a
newline, get or create a suitable buffer, send the command, and
send a newline or enter.

Predefined choices include `racket-shell', `racket-term',
`racket-ansi-term', and `racket-vterm'."
  :type 'function
  :options '(racket-shell
             racket-term
             racket-ansi-term
             racket-vterm)
  :risky t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket-xp group

(defgroup racket-xp nil
  "`racket-xp-mode' options"
  :group 'racket)

(defcustom racket-xp-after-change-refresh-delay 1
  "Seconds to wait before refreshing `racket-xp-mode' annotations.

Set to nil to disable automatic refresh and manually use `racket-xp-annotate'."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Off" nil))
  :safe #'integerp)

(defcustom racket-xp-mode-lighter
  '(:eval (racket--xp-mode-lighter))
  "Mode line lighter for `racket-xp-mode'.

Set to nil to disable the mode line completely."
  :type 'sexp
  :risky t)

(defcustom racket-xp-highlight-unused-regexp "^[^_]"
  "Only give `racket-xp-unused-face' to unused bindings that match this regexp.

The default is to highlight identifiers that do not start with
an underline, which is a common convention."
  :type 'regexp
  :safe #'stringp)

(defcustom racket-xp-add-binding-faces nil
  "Have `racket-xp-mode' fontify binding identifier sites.

A \\='font-lock-face property is added for bindings from:

  - the module language, using `racket-xp-binding-lang-face' and
    `racket-xp-binding-lang-use-face'.

  - other imports, using `racket-xp-binding-import-face' and
    `racket-xp-binding-import-use-face'.

  - local definitions, using `racket-xp-binding-local-face' and
    `racket-xp-binding-local-use-face'.

This has a visible effect only when there is /not/ also a
\\='face property applied by the major mode's fontification."
  :type '(repeat symbol)
  :safe #'listp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket-hash-lang group

(defgroup racket-hash-lang nil
  "`racket-hash-lang-mode' options"
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
    this list. See `racket-hash-lang-module-language-hook' for
    ideas.

Note: Some tokens are hardwired and not customizable by this
list: Comment tokens use the face `font-lock-comment-face',
sometimes blended with other faces. Parenthesis tokens use the
face `parenthesis' if defined, as by the paren-face package.
String tokens use `font-lock-string-face'. Text tokens, e.g.
Scribble text, use the face `default'"
  :type '(alist :key-type symbol :value-type face)
  :safe #'listp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket-repl group

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
  :type '(choice (const :tag "One REPL buffer for all edit buffers" nil)
                 (const :tag "One REPL buffer for all project edit buffers" racket-repl-buffer-name-project)
                 (const :tag "One REPL buffer for each edit buffer" racket-repl-buffer-name-unique)
                 (function :tag "Other function"))
  :risky t)

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
  :type '(repeat (repeat :tag "Module path" symbol))
  :safe #'listp)

(defcustom racket-memory-limit 2048
  "Terminate the Racket process if memory use exceeds this value in MB.

Changes to this value take effect upon the next `racket-run'. A value
of 0 means no limit.

Caveat: This uses Racket's `custodian-limit-memory`, which does
not enforce the limit exactly. Instead, the program will be
terminated upon the first garbage collection where memory exceeds
the limit (maybe by a significant amount)."
  :type 'integer
  :safe #'integerp)

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
  :type '(radio (const :tag "Low" low)
                (const :tag "Medium (better context but slower)" medium)
                (const :tag "High (best context but slowest)" high))
  :risky t)

(make-obsolete-variable 'racket-retry-as-skeleton nil "2020-02-26")

(defcustom racket-repl-history-directory
  (locate-user-emacs-file (file-name-as-directory "racket-mode"))
  "Directory for `racket-repl-mode' history files."
  :type 'file)

(defcustom racket-history-filter-regexp "\\`\\s *\\'"
  "Input matching this regexp are NOT saved on the history list.
Default value is a regexp to ignore input that is all whitespace."
  :type 'regexp
  :safe #'stringp)

(defcustom racket-images-inline t
  "Whether to display inline images in the REPL."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-imagemagick-props nil
  "Use ImageMagick with these properties for REPL images.

When this property list is not empty -- and the variable
`racket-images-inline' is true, and Emacs is built with with
ImageMagick support -- then `create-image' is called with
\"imagemagick\" as the type and with this property list.

For example, to scale images whose width is larger than 500
pixels, supply (:max-width 500)."
  :type '(plist :key-type symbol
                :value-type (choice number string))
  :options '((:max-width integer)
             (:max-height integer)
             (:background string)
             (:width integer)
             (:height integer)
             (:rotation float))
  :risky t)

(defcustom racket-images-keep-last 100
  "How many images to keep in the image cache."
  :type 'integer
  :safe #'integerp)

(defcustom racket-images-system-viewer (if (eq system-type 'darwin)
                                           "open"
                                         "display")
  "The image viewer program to use for `racket-view-image'."
  :type 'string
  :risky t)

(defcustom racket-images-do-not-use-svg nil
  "Do not use SVG to render images?

Note: This value is used only when starting a back end -- /not/
for each run. If you change this, for it to take effect you must
restart by using `racket-start-back-end'."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-pretty-print t
  "Use pretty-print instead of print in REPL?"
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-use-repl-submit-predicate nil
  "Should `racket-repl-submit' use a drracket:submit-predicate?

A language can provide such a predicate, for example when the
language syntax is not s-expressions. When t `racket-repl-submit'
will use this to decide whether to submit your input, yet."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-before-run-hook nil
  "Normal hook done before various Racket Mode run commands.

Here \"before\" means that the `racket-repl-mode' buffer might not
exist yet.

When hook functions are called, `current-buffer' is that of the
edit buffer when the run command was issued. If a hook function
instead needs the `racket-repl-mode' buffer, it should get that
from the variable `racket-repl-buffer-name'."
  :type 'hook
  :risky t)

(defcustom racket-after-run-hook nil
  "Normal hook done after various Racket Mode run commands.

Here \"after\" means that the run has completed and the REPL is
waiting at another prompt.

When hook functions are called, `current-buffer' is that of the
buffer when the run command was issued. If a hook function
instead needs the `racket-repl-mode' buffer, it should get that
from the variable `racket-repl-buffer-name'."
  :type 'hook
  :risky t)

(defcustom racket-repl-command-file
  (expand-file-name "repl.rkt"
                    (locate-user-emacs-file (file-name-as-directory "racket-mode")))
  "Name of the file used by `racket-repl'."
  :type 'file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket-other group

(defgroup racket-other nil
  "Other Options"
  :group 'racket)

(defcustom racket-indent-curly-as-sequence t
  "Indent `{}` with items aligned with the head item?

This is indirectly disabled if `racket-indent-sequence-depth' is 0.
This is safe to set as a file-local variable."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-indent-sequence-depth 0
  "To what depth should `racket-indent-line' search.

This affects the indentation of forms like \\='() \\=`() #() --
and {} if `racket-indent-curly-as-sequence' is t --- but not
#\\='() #\\=`() ,() ,@(). A zero value disables, giving the
normal indent behavior of DrRacket or Emacs `lisp-mode' derived
modes like `scheme-mode'. Setting this to a high value can make
indentation noticeably slower. This is safe to set as a
file-local variable."
  :type 'integer
  :safe #'integerp)

(defcustom racket-pretty-lambda nil
  "Display lambda keywords using λ. This is DEPRECATED.

Instead use `prettify-symbols-mode' in newer verisons of Emacs,
or, use `racket-insert-lambda' to insert actual λ characters."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-smart-open-bracket-enable nil
  "This variable is obsolete and has no effect.

Instead of using this variable, you may bind the `[` key to the
`racket-smart-open-bracket' command in the `racket-mode-map'
and/or `racket-repl-mode-map' keymaps."
  :type 'boolean
  :safe #'booleanp)

(defcustom racket-module-forms
  (rx (syntax ?\()
      (or (seq "module" (zero-or-one (any ?* ?+)))
          "library"))
  "Regexp for the start of a `module`-like form.

Affects what `beginning-of-defun' will move to. This is safe to
set as a file-local variable."
  :type 'string
  :safe #'stringp)

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
           (racket--logger-activate-config))))

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
  :type '(repeat function)
  :options '(racket-show-pseudo-tooltip
             racket-show-echo-area
             racket-show-header-line
             racket-show-pos-tip)
  :risky t)

(defcustom racket-expand-hiding 'standard
  "The macro hiding policy for commands like `racket-expand-file'."
  :type '(choice
          (const :tag "Disable" disable)
          (const :tag "Standard" standard)
          (list :tag "Custom" :value (t t t t nil)
                (boolean :tag "Hide racket syntax")
                (boolean :tag "Hide library syntax")
                (boolean :tag "Hide contracts")
                (boolean :tag "Hide phase>0")
                (repeat
                 :tag "More rules (see macro-debugger/model/hiding-policies \"Entry\" and \"Condition\")"
                 (list (choice (const :tag "show-if" show-if)
                               (const :tag "hide-if" hide-if))
                       (choice (const :tag "lexical" (lexical))
                               (const :tag "unbound" (unbound))
                               (const :tag "from-kernel-module" (from-kernel-module))
                               (list :tag "from-def-module"
                                     (const from-def-module)
                                     (choice :tag "module path" string symbol))
                               (list :tag "from-nom-module"
                                     (const from-nom-module)
                                     (choice :tag "module path" string symbol))
                               (list :tag "from-collection"
                                     (const from-collection)
                                     (repeat :tag "collection-string" string))
                               (list :tag "symbol=?"
                                     (const symbol=?)
                                     (symbol))
                               (list :tag "symbol-like"
                                     (const symbol-like)
                                     (string :tag "racket regexp"))
                               (list :tag "phase>=?"
                                     (const phase>=?)
                                     (natnum :tag "natural number"))
                               (sexp :tag "sexp")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; racket-faces group

(defgroup racket-faces nil
  "Racket Faces"
  :group 'faces
  :group 'racket)

(defmacro defface-racket (id facespec docstr)
  `(progn
     (defconst ,id ',id)
     (defface ,id
       ,facespec
       ,docstr)))

(defface-racket racket-xp-def-face
  '((t (:inherit match :underline (:style line))))
  "Face `racket-xp-mode' uses when point is on a definition.")

(defface-racket racket-xp-use-face
  '((t (:inherit match)))
  "Face `racket-xp-mode' uses when point is on a use.")

(defface-racket racket-xp-binding-lang-face
  '((t (:inherit font-lock-doc-face)))
  "Face `racket-xp-mode' gives to the module language name.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-binding-lang-use-face
  '((t (:inherit font-lock-keyword-face)))
  "Face `racket-xp-mode' gives uses of bindings imported from the module language.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-binding-import-face
  '((t (:inherit default)))
  "Face `racket-xp-mode' gives to imported module names.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-binding-import-use-face
  '((t (:inherit font-lock-keyword-face)))
  "Face `racket-xp-mode' gives uses of imported bindings.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-binding-local-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face `racket-xp-mode' gives to local definitions.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-binding-local-use-face
  '((t (:inherit default)))
  "Face `racket-xp-mode' gives to uses of local definitions.

See the variable `racket-xp-binding-font-lock-face-modes'.")

(defface-racket racket-xp-error-face
  '((t (:underline (:color "red" :style wave))))
  "Face `racket-xp-mode' uses to highlight errors.")

(defface-racket racket-xp-unused-face
  '((t (:strike-through t)))
  "Face `racket-xp-mode' uses to highlight unused requires or definitions.")

(defface-racket racket-xp-tail-target-face
  '((t (:inherit match :underline (:style wave))))
  "Face `racket-xp-mode' uses to highlight targets of a tail position.")

(defface-racket racket-xp-tail-position-face
  '((t (:inherit match)))
  "Face `racket-xp-mode' uses to highlight expressions in a tail position.")

(defface-racket racket-keyword-argument-face
  '((((background dark))
     (:foreground "IndianRed"))
    (((background light))
     (:foreground "Red3")))
  "Face for `#:keyword` arguments.")

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
the face `font-lock-constant-face'.")

(defface-racket racket-reader-syntax-quoted-symbol-face
  '((t (:inherit default)))
  "Face for symbols quoted using #\\=' or #\\=`.

This face is given only to symbols directly quoted using the
reader shorthands #\\=' or #\\=`. All other directly quoted
values, including symbols quoted using \"syntax\" or
\"quasisyntax\", get the face `font-lock-constant-face'.")

(defface-racket racket-here-string-face
  '((t (:inherit sh-heredoc)))
  "Face for here strings.")

(defface-racket racket-logger-config-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for `racket-logger-mode' configuration.")

(defface-racket racket-logger-topic-face
  '((t (:inherit font-lock-function-name-face :slant italic)))
  "Face for `racket-logger-mode' topics.")

(defface-racket racket-logger-fatal-face
  '((t (:inherit error :weight bold)))
  "Face for `racket-logger-mode' fatal level.")

(defface-racket racket-logger-error-face
  '((t (:inherit error)))
  "Face for `racket-logger-mode' error level.")

(defface-racket racket-logger-warning-face
  '((t (:inherit warning)))
  "Face for `racket-logger-mode' warning level.")

(defface-racket racket-logger-info-face
  '((t (:inherit font-lock-string-face)))
  "Face for `racket-logger-mode' info level.")

(defface-racket racket-logger-debug-face
  '((t (:inherit font-lock-constant-face)))
  "Face for `racket-logger-mode' debug level.")

(defface-racket racket-debug-break-face
  '((t (:background "red")))
  "Face for `racket-debug-mode' break position.")

(defface-racket racket-debug-breakpoint-face
  '((t (:foreground "red" :weight bold)))
  "Face for `racket-debug-mode' breakpoint overlays.")

(defface-racket racket-debug-locals-face
  '((t (:inherit font-lock-constant-face :box (:line-width -1) :slant italic)))
  "Face for `racket-debug-mode' local variables.")

(defface-racket racket-debug-result-face
  '((t (:inherit font-lock-constant-face :box (:line-width -1) :slant italic :weight bold)))
  "Face for `racket-debug-mode' result values.")

(defface-racket racket-doc-link-face
  '((t (:underline (:color "gray" :style line))))
  "Face `racket-describe-mode' uses for links within documentation.
Note: When some special face is already specified by the
documentation, then to avoid visual clutter this face is NOT also
added.")

(defface-racket racket-ext-link-face
  '((t (:underline (:style wave) :slant italic :weight bold)))
  "Face `racket-describe-mode' uses for external links.
See the variable `racket-browse-url-function'.")

(defface-racket racket-doc-output-face
  '((t (:inherit fixed-pitch-serif)))
  "Face `racket-describe-mode' uses for Scribble @example or @interactions output.")

(defface-racket racket-doc-litchar-face
  '((t (:foreground "dark red" :background "gray")))
  "Face `racket-describe-mode' uses for Scribble @litchar.")

(defface-racket racket-repl-message
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face `racket-repl-mode' uses for messages from the back end.")

(defface-racket racket-repl-prompt
  '((t (:inherit bold)))
  "Face `racket-repl-mode' uses for prompts.")

(defface-racket racket-repl-value
  '((t (:inherit font-lock-constant-face)))
  "Face `racket-repl-mode' uses for values output by current-print.")

(defface-racket racket-repl-error-message
  '((t (:inherit error)))
  "Face `racket-repl-mode' uses for error messages.")

(defface-racket racket-repl-error-location
  '((t (:inherit underline)))
  "Face `racket-repl-mode' uses for error locations.")

(defface-racket racket-repl-error-label
  '((t (:inherit font-lock-variable-name-face)))
  "Face `racket-repl-mode' uses for error labels.")

(defface-racket racket-repl-stdout
  '((t (:inherit default)))
  "Face `racket-repl-mode' uses for output to current-output-port.")

(defface-racket racket-repl-stderr
  '((t (:inherit error)))
  "Face `racket-repl-mode' uses for output to current-error-port.")

(provide 'racket-custom)

;;; racket-custom.el ends here
