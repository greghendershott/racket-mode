;;; racket-custom.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
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

;;; All `defcustom's and `defface's go here.
;;; This makes it easier to provide a consistent UI.

;; NOTE: `:prefix` is disabled as of Emacs 24.3, so I'm using explicit
;; `:tag`s. But also note that options are sorted (by default; user
;; can turn that off) based on the identifier name not the `:tag`. As
;; a result, I'm defining `:tag`s AS IF `:prefix "racket-"` did work.
;; In other words defcustom of racket-foo-bar has a :tag "Foo Bar".

(require 'rx)
(require 'cl-lib)
(require 'sh-script) ;for sh-heredoc face

(defgroup racket nil
  "Modes for the Racket language."
  :group 'languages
  :link '(url-link :tag "README on GitHub" "https://github.com/greghendershott/racket-mode/blob/master/README.md"))

;; This should be _before_ the `defcustom' of `racket-program' (see
;; note in doc for `define-obsolete-variable-alias').
(define-obsolete-variable-alias
  'racket-racket-program
  'racket-program
  "2017-06-02")

(make-obsolete-variable
  'racket-raco-program
  "You need only set `racket-program' to the Racket executable pathname."
  "2017-06-02")

(defvar racket--winp (string-match "windows" (symbol-name system-type)))

(defcustom racket-program (cond (racket--winp "Racket.exe")
                                (t            "racket"))
  "Pathname of the racket executable."
  :tag "Racket Program"
  :type '(file :must-match t)
  :risky t
  :group 'racket)

(make-obsolete-variable
 'racket-command-port
 "This no longer has any effect. The Racket Mode back end chooses an ephemeral TCP port for REPL sessions and I/O."
 "2020-04-25")

(make-obsolete-variable
  'racket-command-startup
  "This no longer has any effect."
  "2020-01-23")

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

(defcustom racket-path-from-emacs-to-racket-function
  #'identity
  "A function to transform Emacs Lisp pathnames given to the Racket back end.

If you run Emacs on Windows Subsystem for Linux, and want to run
Racket programs using Windows Racket.exe rather than Linux
racket, you can set this to `racket-wsl-to-windows'. In that case
you probably also want to customize the \"reverse\":
`racket-path-from-racket-to-emacs-function'."
  :tag "Path from Emacs to Racket Function"
  :type 'function
  :safe 'functionp
  :group 'racket)

(defcustom racket-path-from-racket-to-emacs-function
  (if racket--winp
      (lambda (path) (subst-char-in-string ?\\ ?/ path))
      #'identity)
  "A function to transform Racket back end pathnames given to Emacs Lisp.

The default on Windows replaces back with forward slashes. The
default elsewhere is `identity'.

If you run Emacs on Windows Subsystem for Linux, and want to run
Racket programs using Windows Racket.exe rather than Linux
racket, you can set this to `racket-windows-to-wsl'. In that case
you probably also want to customize the \"reverse\":
`racket-path-from-emacs-to-racket-function'."
  :tag "Path from Racket to Emacs Function"
  :type 'function
  :safe #'functionp
  :group 'racket)

(defcustom racket-browse-url-function
  'racket-browse-url-using-temporary-file
  "Function to call to browse a URL."
  :tag "Browse URL Function"
  :type 'function
  :safe #'functionp
  :group 'racket)

(defcustom racket-documentation-search-location
  "https://docs.racket-lang.org/search/index.html?q=%s"
  "The location of the Racket \"Search Manuals\" web page.
Where `racket-documentation-search', `racket-xp-documentation'
and `racket-repl-documentation' should look for the search page.

- If the value of this variable is 'local, open the search page
  from the local documentation, as with \"raco doc\".

- Otherwise, the value is a string recognizable by `format', with
  \"%s\" at the point at which to insert the user's search text.
  the help desk. Apart from \"%s\", the string should be a
  properly encoded URL."
  :tag "Where to search documentation"
  :type '(choice (string :tag "URL")
                 (const :tag "Local" 'local))
  :safe (lambda (val) (or (stringp val) (eq val 'local)))
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

Any such function takes no arguments, should look at
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

(make-obsolete-variable
 'racket-retry-as-skeleton
 "The motivation for this is now N/A with `racket-xp-mode'."
 "2020-02-26")

(defcustom racket-repl-history-directory
  (locate-user-emacs-file (convert-standard-filename "racket-mode/"))
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
  :safe nil
  :group 'racket-repl)

(defcustom racket-before-run-hook nil
  "Normal hook done before various Racket Mode run commands.

When hook functions are called, `current-buffer' is that of the
`racket-mode' buffer when the run command was issued. If a hook
function instead needs the `racket-repl-mode' buffer, it should
get that from the variable `racket-repl-buffer-name'."
  :tag "Before Run Hook"
  :type 'hook
  :safe nil
  :group 'racket-repl)

(defcustom racket-after-run-hook nil
  "Normal hook done after various Racket Mode run commands.

Here \"after\" means that the run has completed and the REPL is
waiting at another prompt.

When hook functions are called, `current-buffer' is that of the
`racket-mode' buffer when the run command was issued. If a hook
function instead needs the `racket-repl-mode' buffer, it should
get that from the variable `racket-repl-buffer-name'."
  :tag "After Run Hook"
  :type 'hook
  :safe 'nil
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

This affects the indentation of forms like '() `() #() --
and {} if `racket-indent-curly-as-sequence' is t --- but not
#'() #`() ,() ,@(). A zero value disables, giving the normal
indent behavior of DrRacket or Emacs `lisp-mode' derived modes
like `scheme-mode'. Setting this to a high value can make
indentation noticeably slower. This is safe to set as a
file-local variable."
  :tag "Indent Sequence Depth"
  :type 'integerp
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
    (sequence-specialization . info)
    (*                       . fatal))
  "Configuration of `racket-logger-mode' topics and levels.

The topic '* respresents the default level used for topics not
assigned a level. Otherwise, the topic symbols are the same as
used by Racket's `define-logger`.

The levels are those used by Racket's logging system: 'debug,
'info, 'warning, 'error, 'fatal.

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

The default value sets some known \"noisy\" topics to be one
level quieter. That way you can set the '* topic to a level like
'debug and not get overhwelmed by these noisy topics."
  :tag "Logger Configuration"
  :type '(alist :key-type symbol :value-type symbol)
  :safe (lambda (xs)
          (cl-every (lambda (x)
                      (and (symbolp (car x))
                           (symbolp (cdr x))))
                    xs))
  :group 'racket-other)

(defcustom racket-show-functions
  (list 'racket-show-pseudo-tooltip)
  "A special hook variable to customize `racket-show'.

Example functions include:

  - `racket-show-pseudo-tooltip'
  - `racket-show-echo-area'
  - `racket-show-pos-tip'
  - `racket-show-header-line'

Each function should accept two arguments: VAL and POS.

VAL is:

  - Non-blank string: Display the string somehow.

  - Blank string: Hide any previously displayed string.

  - nil: Hide any persistent UI that might have been created to
    show strings, such as by `racket-show-header-line'.

POS is the buffer position for which to show the message. It may
be nil only when VAL is nil or a blank string. When the buffer
content is a span, POS should be the end of the span. That way,
for example, a function that shows a tooltip can position it not
to hide the interesting span in the buffer."
  :tag "Racket Show Functions"
  :type 'hook
  :options '(racket-show-pseudo-tooltip
             racket-show-echo-area
             racket-show-header-line
             racket-show-pos-tip)
  :safe #'functionp
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
  "Face `racket-xp-mode' uses to highlight definitions."
  "Definition Face")

(defface-racket racket-xp-use-face
  '((t (:inherit match)))
  "Face `racket-xp-mode' uses to highlight uses."
  "Use Face")

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

(define-obsolete-face-alias
 'racket-paren-face
 "Instead use the `paren-face' package: <https://melpa.org/#/paren-face>."
 "2017-06-13")

(defface-racket racket-selfeval-face
  '((t (:foreground "SeaGreen")))
  "Face for self-evaluating expressions like numbers, symbols, strings."
  "Selfeval Face")

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

(defface-racket racket-debug-locals-face
  '((t (:inherit racket-selfeval-face :box (:line-width -1) :slant italic)))
  "Face for `racket-debug-mode' local variables."
  "Racket Debug Locals Face")

(defface-racket racket-debug-result-face
  '((t (:inherit racket-selfeval-face :box (:line-width -1) :slant italic :weight bold)))
  "Face for `racket-debug-mode' result values."
  "Racket Debug Result Face")

(provide 'racket-custom)

;; racket-custom.el ends here
