;;; racket-custom.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
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
(require 'sh-script) ;for sh-heredoc-face

(defgroup racket nil
  "Editing and REPL for the Racket language."
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

(defcustom racket-command-port 55555
  "Port number for Racket REPL command server."
  :tag "Command Port"
  :type 'integer
  :risky t
  :group 'racket)

(defcustom racket-command-timeout 10
  "Timeout for Racket REPL command server."
  :tag "Command Timeout"
  :type 'integer
  :risky t
  :group 'racket)

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
  :group 'racket)

(defcustom racket-error-context 'medium
  "The level of context used for `racket-run' error stack traces.

Each level improves stack trace information, but causes your
program to run more slowly.

  - 'low corresponds to `compile-context-preservation-enabled`
    `#f`.

  - 'medium corresponds to `compile-context-preservation-enabled`
    `#t`, which disables some optimizations like inlining.

  - 'high corresponds to `compile-context-preservation-enabled`
    `#t` and to use of `errortrace`, which heavily instruments
    your code and therefore may be significantly slower.

Tip: Regardless of this setting, you can enable 'high errortrace
for a specific `racket-run' using a C-u prefix. This lets you
normally run with a faster setting, and temporarily re-run to get
a more-helpful error message."
  :tag "Error Context"
  :type '(radio (const :tag "Low" low)
                (const :tag "Medium (slower)" medium)
                (const :tag "High (much slower)" high))
  :risky t
  :group 'racket)

;;; REPL

(defgroup racket-repl nil
  "REPL Options"
  :tag "REPL"
  :group 'racket)

(defcustom racket-history-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
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

(defcustom racket-images-keep-last 100
  "How many images to keep in the image cache."
  :tag "Images Keep Last"
  :type 'integer
  :safe #'integerp
  :group 'racket-repl)

(defcustom racket-images-system-viewer "display"
  "Which system image viewer program to invoke upon M-x
 `racket-view-last-image'."
  :tag "Images System Viewer"
  :type 'string
  :risky t
  :group 'racket-repl)

(defcustom racket-pretty-print t
  "Use pretty-print instead of print in REPL."
  :tag "Pretty Print"
  :type 'boolean
  :safe #'booleanp
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
This affects the indentation of forms like `` '()` `() #() `` --
and `{}` if `racket-indent-curly-as-sequence' is t -- but not
`` #'() #`() ,() ,@() ``. A zero value disables, giving the
normal indent behavior of DrRacket or Emacs `lisp-mode' derived
modes like `scheme-mode'. Setting this to a high value can make
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
  "Use `racket-smart-open-bracket' when `[` is pressed?"
  :tag "Smart Open Bracket Enable"
  :type 'boolean
  :safe #'booleanp
  :group 'racket-other)

(defcustom racket-module-forms
  (rx (syntax ?\()
      (or (seq "module" (zero-or-one (any ?* ?+)))
          "library"))
  "Regexp for the start of a `module`-like form.
Affects what `beginning-of-defun' will move to.
This is safe to set as a file-local variable."
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
  "Configuration of `racket-logger-mode' topics and levels

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

(defface-racket racket-check-syntax-def-face
  '((t (:foreground "Black" :background "SeaGreen1" :weight bold)))
  "Face `racket-check-syntax' uses to highlight definitions."
  "Check Syntax Def Face")

(defface-racket racket-check-syntax-use-face
  '((t (:foreground "Black" :background "PaleGreen1" :slant italic)))
  "Face `racket-check-syntax' uses to highlight uses."
  "Check Syntax Use Face")

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
  '((t (:inherit sh-heredoc-face)))
  "Face for self-evaluating expressions like numbers, symbols, strings."
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

(provide 'racket-custom)

;; racket-custom.el ends here
