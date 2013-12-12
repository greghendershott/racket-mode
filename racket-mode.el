;;; racket-mode.el --- Racket mode for Emacs

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;;; Commentary:

;; Goals:
;; - Focus on Racket (not various Schemes).
;; - Fontify all Racket keywords, builtins, and so on.
;; - Fontify variations of define for functions and variables.
;; - Follow DrRacket concepts where applicable.
;; - Compatible with Emacs 23.4 and 24+.
;;
;; Acknowledgements:
;;
;; - Obviously the existing Emacs Scheme mode and Inferior Scheme mode.
;;
;; - The source code for Neil Van Dyke's Quack provided a model for
;;   many of the scheme-indent-function settings, smart paren closing,
;;   and pretty lambda.

;;; Code:

(defconst racket-mode-copyright
  "Copyright (c) 2013 by Greg Hendershott. Portions Copyright (c) Free Software Foundation and Copyright (c) 2002-2012 Neil Van Dyke.")

(defconst racket-mode-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  See
http://www.gnu.org/licenses/ for details.")

(defconst racket-mode-version "0.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Racket mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar racket-program "racket"
  "Pathname of Racket program.")

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Like DrRacket's F5 (Run)

(defun racket-eval (str)
  (let ((w (selected-window)))
    (save-buffer) ;sufficient our `run!` (wouldn't be for Racket's `enter!`)

    ;; If racket process already visible in a window use that, else
    ;; use previous window.
    (let ((rw (get-buffer-window inferior-racket-buffer-name)))
      (if rw
          (select-window rw)
        (other-window -1)))

    (run-racket)
    (select-window w)

    (comint-send-string (get-inferior-racket-buffer-process) str)

    (pop-to-buffer inferior-racket-buffer-name t)
    (select-window w)))

(defun racket-run ()
  "Save and evaluate the buffer in a fresh REPL like DrRacket."
  (interactive)
  (racket-eval (format "(run! \"%s\")\n" (buffer-file-name))))

(defun racket-shell (cmd)
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

(defun racket-racket ()
  "Do `racket <file>` in *shell* buffer."
  (interactive)
  (racket-shell (concat racket-program
                        " "
                        (shell-quote-argument (buffer-file-name)))))

(defun racket-test ()
  "Do (require (submod \".\" test)) in *racket* buffer."
  (interactive)
  (racket-run) ;start fresh, so (require) will have an effect
  (racket-eval (concat "(begin (displayln \"Running tests...\")\n"
                       "       (require (submod \".\" test)))\n")))

(defun racket-raco-test ()
  "Do `raco test -x <file>` in *shell* buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket-shell (concat (expand-file-name "raco" (file-name-directory racket-program))
                        " test -x "
                        (shell-quote-argument (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enter = enter + indent

(defun racket-newline ()
  (interactive)
  (newline)
  (lisp-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert lambda char (like DrRacket)

(defun racket-insert-lambda ()
  (interactive)
  (insert-char racket-lambda-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically insert matching \?) \?] or \?}

(defvar racket-matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket-insert-closing (prefix char)
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
                (delete-backward-char 1)
                (insert close-char))))))))
  (when blink-paren-function (funcall blink-paren-function)))

(defun racket-insert-closing-paren (&optional prefix)
  (interactive "P")
  (racket-insert-closing prefix ?\)))

(defun racket-insert-closing-bracket (&optional prefix)
  (interactive "P")
  (racket-insert-closing prefix ?\]))

(defun racket-insert-closing-brace (&optional prefix)
  (interactive "P")
  (racket-insert-closing prefix ?\}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun racket-indent-for-fold (state indent-point normal-indent)
  ;; see http://community.schemewiki.org/?emacs-indentation
  (let ((containing-sexp-start (elt state 1))
        containing-sexp-point
        containing-sexp-column
        body-indent
        clause-indent)
    ;;Move to the start of containing sexp, calculate its
    ;;indentation, store its point and move past the function
    ;;symbol so that we can use 'parse-partial-sexp'.
    ;;
    ;;'lisp-indent-function' guarantees that there is at least
    ;;one word or symbol character following open paren of
    ;;containing sexp.
    (forward-char 1)
    (goto-char containing-sexp-start)
    (setq containing-sexp-point (point))
    (setq containing-sexp-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-sexp-column))
    (forward-char 1)    ;Move past the open paren.
    (forward-sexp 2)    ;Move to the next sexp, past its close paren
    (backward-sexp 1)   ;Move to its start paren
    (setq clause-indent (current-column))
    (forward-sexp 1)    ;Move back past close paren
    ;;Now go back to the beginning of the line holding
    ;;the indentation point. Count the sexps on the way.
    (parse-partial-sexp (point) indent-point 1 t)
    (let ((n 1))
      (while (and (< (point) indent-point)
                  (condition-case ()
                      (progn
                        (setq n (+ 1 n))
                        (forward-sexp 1)
                        (parse-partial-sexp (point) indent-point 1 t))
                    (error nil))))
      ;;(debug body-indent clause-indent n)
      (list (cond ((= 1 n) clause-indent)
                  (t body-indent))
            containing-sexp-point))))

(defun racket-set-indentation ()
  (mapc (lambda (x)
          (put (car x) 'scheme-indent-function (cdr x)))
        '((begin0 . 1)
          (c-declare . 0)
          (c-lambda . 2)
          (case-lambda . 0)
          (catch . 1)
          (chicken-setup . 1)
          (class . 'defun)
          (class* . 'defun)
          (compound-unit/sig . 0)
          (dynamic-wind . 0)
          (instantiate . 2)
          (interface . 1)
          (lambda/kw . 1)
          (let*-values . 1)
          (let+ . 1)
          (let-values . 1)
          (let/ec . 1)
          (mixin . 2)
          (module . 2)
          (module+ . 1)
          (module* . 1)
          (opt-lambda . 1)
          (parameterize . 1)
          (parameterize-break . 1)
          (parameterize* . 1)
          (quasisyntax/loc . 1)
          (receive . 2)
          (send* . 1)
          (sigaction . 1)
          (syntax-case . 2)
          (syntax/loc . 1)
          (unit . 'defun)
          (unit/sig . 2)
          (unless . 1)
          (when . 1)
          (while . 1)
          (with-handlers . 1)
          (with-method . 1)
          (with-syntax . 1)
          (with-float . 1)
          (with-fixed . 1)
          (for . 1)
          (for/list . 1)
          (for/vector . 1)
          (for/hash . 1)
          (for/hasheq . 1)
          (for/hasheqv . 1)
          (for/and . 1)
          (for/or . 1)
          (for/lists . 1)
          (for/first . 1)
          (for/last . 1)
          (for/fold . racket-indent-for-fold)
          (for/flvector . 1)
          (for/set . 1)
          (for/sum . 1)
          (for* . 1)
          (for*/list . 1)
          (for*/vector . 1)
          (for*/hash . 1)
          (for*/hasheq . 1)
          (for*/hasheqv . 1)
          (for*/and . 1)
          (for*/or . 1)
          (for*/lists . 1)
          (for*/first . 1)
          (for*/last . 1)
          (for*/fold . racket-indent-for-fold)
          (for*/flvector . 1)
          (for*/set . 1)
          (for*/sum . 1)
          (match . 1)
          (match* . 1)
          (match-let . 1)
          (match-let* . 1)
          (syntax-parse . 1)
          (syntax-parameterize . 1)
          (splicing-syntax-parameterize . 1)
          (syntax-parse . 1)
          (with-syntax* . 1)
          (dict-set . 1)
          (dict-set* . 1)
          (call-with-input-file* . 1)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

(defconst racket-keyword-argument-face 'racket-keyword-argument-face)
(defface racket-keyword-argument-face
  '((((background dark))
     (:foreground "IndianRed"))
    (((background light))
     (:foreground "Red3")))
  "Face used for #:keywords."
  :group 'racket)

(defconst racket-selfeval-face 'racket-selfeval-face)
(defface racket-selfeval-face
  '((t
     (:foreground "SeaGreen")))
  "Face for self-evaluating expressions"
  :group 'racket)

(defconst racket-paren-face 'racket-paren-face)
(defface racket-paren-face
  (let ((fg (face-foreground 'default)))
    `((t (:foreground ,fg))))
  "Face for parentheses () [] {}"
  :group 'racket)

(defvar racket-keywords
  '("#%app"
    "#%datum"
    "#%expression"
    "#%module-begin"
    "#%plain-app"
    "#%plain-lambda"
    "#%plain-module-begin"
    "#%provide"
    "#%require"
    "#%stratified-body"
    "#%top"
    "#%top-interaction"
    "#%variable-reference"
    "..."
    "and"
    "begin"
    "begin-for-syntax"
    "begin0"
    "case"
    "case-lambda"
    "cond"
    "define"
    "define/contract"
    "define-for-syntax"
    "define-logger"
    "define-struct"
    "define-syntax"
    "define-syntax-rule"
    "define-syntaxes"
    "define-values"
    "define-values-for-syntax"
    "delay"
    "do"
    "fluid-let"
    "if"
    "lambda" "λ"
    "let"
    "let*"
    "let*-values"
    "let-struct"
    "let-syntax"
    "let-syntaxes"
    "let-values"
    "let/cc"
    "let/ec"
    "letrec"
    "letrec-syntax"
    "letrec-syntaxes"
    "letrec-syntaxes+values"
    "letrec-values"
    "module"
    "module+"
    "module*"
    "or"
    "parameterize"
    "parameterize*"
    "parameterize-break"
    "provide"
    "provide-for-label"
    "provide-for-syntax"
    "quasiquote"
    "quasisyntax"
    "quasisyntax/loc"
    "quote"
    "quote-syntax"
    "quote-syntax/prune"
    "require"
    "require-for-label"
    "require-for-syntax"
    "require-for-template"
    "set!"
    "set!-values"
    "syntax"
    "syntax-case"
    "syntax-case*"
    "syntax-id-rules"
    "syntax-rules"
    "syntax/loc"
    "time"
    "unless"
    "unquote"
    "unquote-splicing"
    "unsyntax"
    "unsyntax-splicing"
    "when"
    "with-continuation-mark"
    "with-handlers"
    "with-handlers*"
    "with-syntax"
    "with-syntax*"
    "λ"
   )
  "Racket keywords")

(defvar racket-builtins
  '("*"
    "+"
    "-"
    "/"
    "<"
    "<="
    "="
    ">"
    ">="
    "abort-current-continuation"
    "abs"
    "absolute-path?"
    "acos"
    "add1"
    "alarm-evt"
    "always-evt"
    "andmap"
    "angle"
    "append"
    "apply"
    "arithmetic-shift"
    "arity-at-least"
    "arity-at-least-value"
    "arity-at-least?"
    "asin"
    "assoc"
    "assq"
    "assv"
    "atan"
    "banner"
    "bitwise-and"
    "bitwise-bit-field"
    "bitwise-bit-set?"
    "bitwise-ior"
    "bitwise-not"
    "bitwise-xor"
    "boolean?"
    "bound-identifier=?"
    "box"
    "box-cas!"
    "box-immutable"
    "box?"
    "break-enabled"
    "break-thread"
    "build-path"
    "build-path/convention-type"
    "byte-pregexp"
    "byte-pregexp?"
    "byte-ready?"
    "byte-regexp"
    "byte-regexp?"
    "byte?"
    "bytes"
    "bytes->immutable-bytes"
    "bytes->list"
    "bytes->path"
    "bytes->path-element"
    "bytes->string/latin-1"
    "bytes->string/locale"
    "bytes->string/utf-8"
    "bytes-append"
    "bytes-close-converter"
    "bytes-convert"
    "bytes-convert-end"
    "bytes-converter?"
    "bytes-copy"
    "bytes-copy!"
    "bytes-fill!"
    "bytes-length"
    "bytes-open-converter"
    "bytes-ref"
    "bytes-set!"
    "bytes-utf-8-index"
    "bytes-utf-8-length"
    "bytes-utf-8-ref"
    "bytes<?"
    "bytes=?"
    "bytes>?"
    "bytes?"
    "caaaar"
    "caaadr"
    "caaar"
    "caadar"
    "caaddr"
    "caadr"
    "caar"
    "cadaar"
    "cadadr"
    "cadar"
    "caddar"
    "cadddr"
    "caddr"
    "cadr"
    "call-in-nested-thread"
    "call-with-break-parameterization"
    "call-with-composable-continuation"
    "call-with-continuation-barrier"
    "call-with-continuation-prompt"
    "call-with-current-continuation"
    "call-with-escape-continuation"
    "call-with-exception-handler"
    "call-with-immediate-continuation-mark"
    "call-with-input-file"
    "call-with-input-file*"
    "call-with-output-file"
    "call-with-parameterization"
    "call-with-semaphore"
    "call-with-semaphore/enable-break"
    "call-with-values"
    "call/cc"
    "call/ec"
    "car"
    "cdaaar"
    "cdaadr"
    "cdaar"
    "cdadar"
    "cdaddr"
    "cdadr"
    "cdar"
    "cddaar"
    "cddadr"
    "cddar"
    "cdddar"
    "cddddr"
    "cdddr"
    "cddr"
    "cdr"
    "ceiling"
    "channel-get"
    "channel-put"
    "channel-put-evt"
    "channel-put-evt?"
    "channel-try-get"
    "channel?"
    "chaperone-box"
    "chaperone-continuation-mark-key"
    "chaperone-evt"
    "chaperone-hash"
    "chaperone-of?"
    "chaperone-procedure"
    "chaperone-prompt-tag"
    "chaperone-struct"
    "chaperone-struct-type"
    "chaperone-vector"
    "chaperone?"
    "char->integer"
    "char-alphabetic?"
    "char-blank?"
    "char-ci<=?"
    "char-ci<?"
    "char-ci=?"
    "char-ci>=?"
    "char-ci>?"
    "char-downcase"
    "char-foldcase"
    "char-general-category"
    "char-graphic?"
    "char-iso-control?"
    "char-lower-case?"
    "char-numeric?"
    "char-punctuation?"
    "char-ready?"
    "char-symbolic?"
    "char-title-case?"
    "char-titlecase"
    "char-upcase"
    "char-upper-case?"
    "char-utf-8-length"
    "char-whitespace?"
    "char<=?"
    "char<?"
    "char=?"
    "char>=?"
    "char>?"
    "char?"
    "check-duplicate-identifier"
    "checked-procedure-check-and-extract"
    "choice-evt"
    "cleanse-path"
    "close-input-port"
    "close-output-port"
    "collect-garbage"
    "collection-file-path"
    "collection-path"
    "compile"
    "compile-allow-set!-undefined"
    "compile-context-preservation-enabled"
    "compile-enforce-module-constants"
    "compile-syntax"
    "compiled-expression?"
    "compiled-module-expression?"
    "complete-path?"
    "complex?"
    "cons"
    "continuation-mark-key?"
    "continuation-mark-set->context"
    "continuation-mark-set->list"
    "continuation-mark-set->list*"
    "continuation-mark-set-first"
    "continuation-mark-set?"
    "continuation-marks"
    "continuation-prompt-available?"
    "continuation-prompt-tag?"
    "continuation?"
    "copy-file"
    "cos"
    "current-break-parameterization"
    "current-code-inspector"
    "current-command-line-arguments"
    "current-compile"
    "current-compiled-file-roots"
    "current-continuation-marks"
    "current-custodian"
    "current-directory"
    "current-drive"
    "current-error-port"
    "current-eval"
    "current-evt-pseudo-random-generator"
    "current-gc-milliseconds"
    "current-get-interaction-input-port"
    "current-inexact-milliseconds"
    "current-input-port"
    "current-inspector"
    "current-library-collection-paths"
    "current-load"
    "current-load-extension"
    "current-load-relative-directory"
    "current-load/use-compiled"
    "current-locale"
    "current-memory-use"
    "current-milliseconds"
    "current-module-declare-name"
    "current-module-declare-source"
    "current-module-name-resolver"
    "current-namespace"
    "current-output-port"
    "current-parameterization"
    "current-preserved-thread-cell-values"
    "current-print"
    "current-process-milliseconds"
    "current-prompt-read"
    "current-pseudo-random-generator"
    "current-read-interaction"
    "current-reader-guard"
    "current-readtable"
    "current-seconds"
    "current-security-guard"
    "current-subprocess-custodian-mode"
    "current-thread"
    "current-thread-group"
    "current-thread-initial-stack-size"
    "current-write-relative-directory"
    "custodian-box-value"
    "custodian-box?"
    "custodian-limit-memory"
    "custodian-managed-list"
    "custodian-memory-accounting-available?"
    "custodian-require-memory"
    "custodian-shutdown-all"
    "custodian?"
    "custom-print-quotable-accessor"
    "custom-print-quotable?"
    "custom-write-accessor"
    "custom-write?"
    "date"
    "date*"
    "date*-nanosecond"
    "date*-time-zone-name"
    "date*?"
    "date-day"
    "date-dst?"
    "date-hour"
    "date-minute"
    "date-month"
    "date-second"
    "date-time-zone-offset"
    "date-week-day"
    "date-year"
    "date-year-day"
    "date?"
    "datum->syntax-object"
    "datum-intern-literal"
    "default-continuation-prompt-tag"
    "define-generics"
    "delete-directory"
    "delete-file"
    "denominator"
    "directory-exists?"
    "directory-list"
    "display"
    "displayln"
    "dump-memory-stats"
    "dynamic-require"
    "dynamic-require-for-syntax"
    "dynamic-wind"
    "eof"
    "eof-object?"
    "ephemeron-value"
    "ephemeron?"
    "eprintf"
    "eq-hash-code"
    "eq?"
    "equal-hash-code"
    "equal-secondary-hash-code"
    "equal?"
    "equal?/recur"
    "eqv-hash-code"
    "eqv?"
    "error"
    "error-display-handler"
    "error-escape-handler"
    "error-print-context-length"
    "error-print-source-location"
    "error-print-width"
    "error-value->string-handler"
    "eval"
    "eval-jit-enabled"
    "eval-syntax"
    "even?"
    "evt?"
    "exact->inexact"
    "exact-integer?"
    "exact-nonnegative-integer?"
    "exact-positive-integer?"
    "exact?"
    "executable-yield-handler"
    "exit"
    "exit-handler"
    "exn"
    "exn-continuation-marks"
    "exn-message"
    "exn:break"
    "exn:break-continuation"
    "exn:break:hang-up"
    "exn:break:hang-up?"
    "exn:break:terminate"
    "exn:break:terminate?"
    "exn:break?"
    "exn:fail"
    "exn:fail:contract"
    "exn:fail:contract:arity"
    "exn:fail:contract:arity?"
    "exn:fail:contract:continuation"
    "exn:fail:contract:continuation?"
    "exn:fail:contract:divide-by-zero"
    "exn:fail:contract:divide-by-zero?"
    "exn:fail:contract:non-fixnum-result"
    "exn:fail:contract:non-fixnum-result?"
    "exn:fail:contract:variable"
    "exn:fail:contract:variable-id"
    "exn:fail:contract:variable?"
    "exn:fail:contract?"
    "exn:fail:filesystem"
    "exn:fail:filesystem:exists"
    "exn:fail:filesystem:exists?"
    "exn:fail:filesystem:version"
    "exn:fail:filesystem:version?"
    "exn:fail:filesystem?"
    "exn:fail:network"
    "exn:fail:network?"
    "exn:fail:out-of-memory"
    "exn:fail:out-of-memory?"
    "exn:fail:read"
    "exn:fail:read-srclocs"
    "exn:fail:read:eof"
    "exn:fail:read:eof?"
    "exn:fail:read:non-char"
    "exn:fail:read:non-char?"
    "exn:fail:read?"
    "exn:fail:syntax"
    "exn:fail:syntax-exprs"
    "exn:fail:syntax:unbound"
    "exn:fail:syntax:unbound?"
    "exn:fail:syntax?"
    "exn:fail:unsupported"
    "exn:fail:unsupported?"
    "exn:fail:user"
    "exn:fail:user?"
    "exn:fail?"
    "exn:srclocs-accessor"
    "exn:srclocs?"
    "exn?"
    "exp"
    "expand"
    "expand-once"
    "expand-path"
    "expand-syntax"
    "expand-syntax-once"
    "expand-syntax-to-top-form"
    "expand-to-top-form"
    "expand-user-path"
    "expt"
    "file-exists?"
    "file-or-directory-identity"
    "file-or-directory-modify-seconds"
    "file-or-directory-permissions"
    "file-position"
    "file-position*"
    "file-size"
    "file-stream-buffer-mode"
    "file-stream-port?"
    "filesystem-root-list"
    "find-executable-path"
    "find-library-collection-paths"
    "find-system-path"
    "fixnum?"
    "floating-point-bytes->real"
    "flonum?"
    "floor"
    "flush-output"
    "for-each"
    "for"
    "for/list"
    "for/vector"
    "for/hash"
    "for/hasheq"
    "for/hasheqv"
    "for/and"
    "for/or"
    "for/lists"
    "for/first"
    "for/last"
    "for/fold"
    "for/flvector"
    "for/set"
    "for/sum"
    "for*"
    "for*/list"
    "for*/vector"
    "for*/hash"
    "for*/hasheq"
    "for*/hasheqv"
    "for*/and"
    "for*/or"
    "for*/lists"
    "for*/first"
    "for*/last"
    "for*/fold"
    "for*/flvector"
    "for*/set"
    "for*/sum"
    "force"
    "format"
    "fprintf"
    "free-identifier=?"
    "gcd"
    "generate-temporaries"
    "gensym"
    "get-output-bytes"
    "get-output-string"
    "getenv"
    "global-port-print-handler"
    "guard-evt"
    "handle-evt"
    "handle-evt?"
    "hash"
    "hash-equal?"
    "hash-eqv?"
    "hash-has-key?"
    "hash-placeholder?"
    "hash-ref!"
    "hash-table-copy"
    "hash-table-count"
    "hash-table-for-each"
    "hash-table-get"
    "hash-table-iterate-first"
    "hash-table-iterate-key"
    "hash-table-iterate-next"
    "hash-table-iterate-value"
    "hash-table-map"
    "hash-table-put!"
    "hash-table-remove!"
    "hash-table?"
    "hasheq"
    "hasheqv"
    "identifier-binding"
    "identifier-label-binding"
    "identifier-prune-lexical-context"
    "identifier-prune-to-source-module"
    "identifier-remove-from-definition-context"
    "identifier-template-binding"
    "identifier-transformer-binding"
    "identifier?"
    "imag-part"
    "immutable?"
    "impersonate-box"
    "impersonate-continuation-mark-key"
    "impersonate-hash"
    "impersonate-procedure"
    "impersonate-prompt-tag"
    "impersonate-struct"
    "impersonate-vector"
    "impersonator-of?"
    "impersonator-prop:application-mark"
    "impersonator-property-accessor-procedure?"
    "impersonator-property?"
    "impersonator?"
    "inexact->exact"
    "inexact-real?"
    "inexact?"
    "input-port?"
    "inspector?"
    "integer->char"
    "integer->integer-bytes"
    "integer-bytes->integer"
    "integer-length"
    "integer-sqrt"
    "integer-sqrt/remainder"
    "integer?"
    "internal-definition-context-seal"
    "internal-definition-context?"
    "keyword->string"
    "keyword<?"
    "keyword?"
    "kill-thread"
    "lcm"
    "length"
    "liberal-define-context?"
    "link-exists?"
    "list"
    "list*"
    "list->bytes"
    "list->string"
    "list->vector"
    "list-immutable"
    "list-ref"
    "list-tail"
    "list?"
    "load"
    "load-extension"
    "load-on-demand-enabled"
    "load-relative"
    "load-relative-extension"
    "load/cd"
    "load/use-compiled"
    "local-expand"
    "local-expand/capture-lifts"
    "local-transformer-expand"
    "local-transformer-expand/capture-lifts"
    "locale-string-encoding"
    "log"
    "log-max-level"
    "magnitude"
    "make-arity-at-least"
    "make-bytes"
    "make-channel"
    "make-continuation-mark-key"
    "make-continuation-prompt-tag"
    "make-custodian"
    "make-custodian-box"
    "make-date"
    "make-date*"
    "make-derived-parameter"
    "make-directory"
    "make-ephemeron"
    "make-exn"
    "make-exn:break"
    "make-exn:break:hang-up"
    "make-exn:break:terminate"
    "make-exn:fail"
    "make-exn:fail:contract"
    "make-exn:fail:contract:arity"
    "make-exn:fail:contract:continuation"
    "make-exn:fail:contract:divide-by-zero"
    "make-exn:fail:contract:non-fixnum-result"
    "make-exn:fail:contract:variable"
    "make-exn:fail:filesystem"
    "make-exn:fail:filesystem:exists"
    "make-exn:fail:filesystem:version"
    "make-exn:fail:network"
    "make-exn:fail:out-of-memory"
    "make-exn:fail:read"
    "make-exn:fail:read:eof"
    "make-exn:fail:read:non-char"
    "make-exn:fail:syntax"
    "make-exn:fail:syntax:unbound"
    "make-exn:fail:unsupported"
    "make-exn:fail:user"
    "make-file-or-directory-link"
    "make-hash-placeholder"
    "make-hash-table"
    "make-hasheq-placeholder"
    "make-hasheqv"
    "make-hasheqv-placeholder"
    "make-immutable-hash-table"
    "make-immutable-hasheqv"
    "make-impersonator-property"
    "make-input-port"
    "make-inspector"
    "make-known-char-range-list"
    "make-namespace"
    "make-output-port"
    "make-parameter"
    "make-phantom-bytes"
    "make-pipe"
    "make-placeholder"
    "make-polar"
    "make-prefab-struct"
    "make-pseudo-random-generator"
    "make-reader-graph"
    "make-readtable"
    "make-rectangular"
    "make-rename-transformer"
    "make-resolved-module-path"
    "make-security-guard"
    "make-semaphore"
    "make-set!-transformer"
    "make-shared-bytes"
    "make-sibling-inspector"
    "make-special-comment"
    "make-srcloc"
    "make-string"
    "make-struct-field-accessor"
    "make-struct-field-mutator"
    "make-struct-type"
    "make-struct-type-property"
    "make-syntax-delta-introducer"
    "make-syntax-introducer"
    "make-thread-cell"
    "make-thread-group"
    "make-vector"
    "make-weak-box"
    "make-weak-hasheqv"
    "make-will-executor"
    "map"
    "match"
    "match*"
    "match-let"
    "match-let"
    "match-define"
    "max"
    "mcar"
    "mcdr"
    "mcons"
    "member"
    "memq"
    "memv"
    "min"
    "module->exports"
    "module->imports"
    "module->language-info"
    "module->namespace"
    "module-compiled-exports"
    "module-compiled-imports"
    "module-compiled-language-info"
    "module-compiled-name"
    "module-compiled-submodules"
    "module-declared?"
    "module-identifier=?"
    "module-label-identifier=?"
    "module-path-index-join"
    "module-path-index-resolve"
    "module-path-index-split"
    "module-path-index-submodule"
    "module-path-index?"
    "module-path?"
    "module-predefined?"
    "module-provide-protected?"
    "module-template-identifier=?"
    "module-transformer-identifier=?"
    "modulo"
    "mpair?"
    "nack-guard-evt"
    "namespace-attach-module"
    "namespace-attach-module-declaration"
    "namespace-base-phase"
    "namespace-mapped-symbols"
    "namespace-module-identifier"
    "namespace-module-registry"
    "namespace-require"
    "namespace-require/constant"
    "namespace-require/copy"
    "namespace-require/expansion-time"
    "namespace-set-variable-value!"
    "namespace-symbol->identifier"
    "namespace-syntax-introduce"
    "namespace-transformer-require"
    "namespace-undefine-variable!"
    "namespace-unprotect-module"
    "namespace-variable-value"
    "namespace?"
    "negative?"
    "never-evt"
    "newline"
    "normal-case-path"
    "not"
    "null"
    "null?"
    "number->string"
    "number?"
    "numerator"
    "object-name"
    "odd?"
    "open-input-bytes"
    "open-input-file"
    "open-input-output-file"
    "open-input-string"
    "open-output-bytes"
    "open-output-file"
    "open-output-string"
    "ormap"
    "output-port?"
    "pair?"
    "parameter-procedure=?"
    "parameter?"
    "parameterization?"
    "path->bytes"
    "path->complete-path"
    "path->directory-path"
    "path->string"
    "path-add-suffix"
    "path-convention-type"
    "path-element->bytes"
    "path-element->string"
    "path-for-some-system?"
    "path-list-string->path-list"
    "path-replace-suffix"
    "path-string?"
    "path?"
    "peek-byte"
    "peek-byte-or-special"
    "peek-bytes"
    "peek-bytes!"
    "peek-bytes-avail!"
    "peek-bytes-avail!*"
    "peek-bytes-avail!/enable-break"
    "peek-char"
    "peek-char-or-special"
    "peek-string"
    "peek-string!"
    "phantom-bytes?"
    "pipe-content-length"
    "placeholder-get"
    "placeholder-set!"
    "placeholder?"
    "poll-guard-evt"
    "port-closed-evt"
    "port-closed?"
    "port-commit-peeked"
    "port-count-lines!"
    "port-count-lines-enabled"
    "port-display-handler"
    "port-file-identity"
    "port-file-unlock"
    "port-next-location"
    "port-print-handler"
    "port-progress-evt"
    "port-provides-progress-evts?"
    "port-read-handler"
    "port-try-file-lock?"
    "port-write-handler"
    "port-writes-atomic?"
    "port-writes-special?"
    "port?"
    "positive?"
    "prefab-key->struct-type"
    "prefab-key?"
    "prefab-struct-key"
    "pregexp"
    "pregexp?"
    "primitive-closure?"
    "primitive-result-arity"
    "primitive?"
    "print"
    "print-as-expression"
    "print-boolean-long-form"
    "print-box"
    "print-graph"
    "print-hash-table"
    "print-mpair-curly-braces"
    "print-pair-curly-braces"
    "print-reader-abbreviations"
    "print-struct"
    "print-syntax-width"
    "print-unreadable"
    "print-vector-length"
    "printf"
    "procedure->method"
    "procedure-arity"
    "procedure-arity-includes?"
    "procedure-arity?"
    "procedure-closure-contents-eq?"
    "procedure-extract-target"
    "procedure-reduce-arity"
    "procedure-rename"
    "procedure-struct-type?"
    "procedure?"
    "progress-evt?"
    "promise?"
    "prop:arity-string"
    "prop:checked-procedure"
    "prop:custom-print-quotable"
    "prop:custom-write"
    "prop:equal+hash"
    "prop:evt"
    "prop:exn:srclocs"
    "prop:impersonator-of"
    "prop:input-port"
    "prop:liberal-define-context"
    "prop:method-arity-error"
    "prop:output-port"
    "prop:procedure"
    "prop:rename-transformer"
    "prop:set!-transformer"
    "pseudo-random-generator->vector"
    "pseudo-random-generator-vector?"
    "pseudo-random-generator?"
    "putenv"
    "quotient"
    "quotient/remainder"
    "raise"
    "raise-argument-error"
    "raise-arguments-error"
    "raise-arity-error"
    "raise-mismatch-error"
    "raise-range-error"
    "raise-result-error"
    "raise-syntax-error"
    "raise-type-error"
    "raise-user-error"
    "random"
    "random-seed"
    "rational?"
    "rationalize"
    "read"
    "read-accept-bar-quote"
    "read-accept-box"
    "read-accept-compiled"
    "read-accept-dot"
    "read-accept-graph"
    "read-accept-infix-dot"
    "read-accept-lang"
    "read-accept-quasiquote"
    "read-accept-reader"
    "read-byte"
    "read-byte-or-special"
    "read-bytes"
    "read-bytes!"
    "read-bytes-avail!"
    "read-bytes-avail!*"
    "read-bytes-avail!/enable-break"
    "read-bytes-line"
    "read-case-sensitive"
    "read-char"
    "read-char-or-special"
    "read-curly-brace-as-paren"
    "read-decimal-as-inexact"
    "read-eval-print-loop"
    "read-language"
    "read-line"
    "read-on-demand-source"
    "read-square-bracket-as-paren"
    "read-string"
    "read-string!"
    "read-syntax"
    "read-syntax/recursive"
    "read/recursive"
    "readtable-mapping"
    "readtable?"
    "real->double-flonum"
    "real->floating-point-bytes"
    "real->single-flonum"
    "real-part"
    "real?"
    "regexp"
    "regexp-match"
    "regexp-match-peek"
    "regexp-match-peek-immediate"
    "regexp-match-peek-positions"
    "regexp-match-peek-positions-immediate"
    "regexp-match-peek-positions-immediate/end"
    "regexp-match-peek-positions/end"
    "regexp-match-positions"
    "regexp-match-positions/end"
    "regexp-match/end"
    "regexp-match?"
    "regexp-max-lookbehind"
    "regexp-replace"
    "regexp-replace*"
    "regexp?"
    "relative-path?"
    "remainder"
    "rename-file-or-directory"
    "rename-transformer-target"
    "rename-transformer?"
    "reroot-path"
    "resolve-path"
    "resolved-module-path-name"
    "resolved-module-path?"
    "reverse"
    "round"
    "seconds->date"
    "security-guard?"
    "semaphore-peek-evt"
    "semaphore-peek-evt?"
    "semaphore-post"
    "semaphore-try-wait?"
    "semaphore-wait"
    "semaphore-wait/enable-break"
    "semaphore?"
    "set!-transformer-procedure"
    "set!-transformer?"
    "set-box!"
    "set-mcar!"
    "set-mcdr!"
    "set-phantom-bytes!"
    "set-port-next-location!"
    "shared-bytes"
    "shell-execute"
    "simplify-path"
    "sin"
    "single-flonum?"
    "sleep"
    "special-comment-value"
    "special-comment?"
    "splicing-syntax-parameterize"
    "split-path"
    "sqrt"
    "srcloc"
    "srcloc-column"
    "srcloc-line"
    "srcloc-position"
    "srcloc-source"
    "srcloc-span"
    "srcloc?"
    "string"
    "string->bytes/latin-1"
    "string->bytes/locale"
    "string->bytes/utf-8"
    "string->immutable-string"
    "string->keyword"
    "string->list"
    "string->number"
    "string->path"
    "string->path-element"
    "string->symbol"
    "string->uninterned-symbol"
    "string->unreadable-symbol"
    "string-append"
    "string-ci<=?"
    "string-ci<?"
    "string-ci=?"
    "string-ci>=?"
    "string-ci>?"
    "string-copy"
    "string-copy!"
    "string-downcase"
    "string-fill!"
    "string-foldcase"
    "string-length"
    "string-locale-ci<?"
    "string-locale-ci=?"
    "string-locale-ci>?"
    "string-locale-downcase"
    "string-locale-upcase"
    "string-locale<?"
    "string-locale=?"
    "string-locale>?"
    "string-normalize-nfc"
    "string-normalize-nfd"
    "string-normalize-nfkc"
    "string-normalize-nfkd"
    "string-ref"
    "string-set!"
    "string-titlecase"
    "string-upcase"
    "string-utf-8-length"
    "string<=?"
    "string<?"
    "string=?"
    "string>=?"
    "string>?"
    "string?"
    "struct->vector"
    "struct-accessor-procedure?"
    "struct-constructor-procedure?"
    "struct-info"
    "struct-mutator-procedure?"
    "struct-predicate-procedure?"
    "struct-type-info"
    "struct-type-make-constructor"
    "struct-type-make-predicate"
    "struct-type-property-accessor-procedure?"
    "struct-type-property?"
    "struct-type?"
    "struct:arity-at-least"
    "struct:date"
    "struct:date*"
    "struct:exn"
    "struct:exn:break"
    "struct:exn:break:hang-up"
    "struct:exn:break:terminate"
    "struct:exn:fail"
    "struct:exn:fail:contract"
    "struct:exn:fail:contract:arity"
    "struct:exn:fail:contract:continuation"
    "struct:exn:fail:contract:divide-by-zero"
    "struct:exn:fail:contract:non-fixnum-result"
    "struct:exn:fail:contract:variable"
    "struct:exn:fail:filesystem"
    "struct:exn:fail:filesystem:exists"
    "struct:exn:fail:filesystem:version"
    "struct:exn:fail:network"
    "struct:exn:fail:out-of-memory"
    "struct:exn:fail:read"
    "struct:exn:fail:read:eof"
    "struct:exn:fail:read:non-char"
    "struct:exn:fail:syntax"
    "struct:exn:fail:syntax:unbound"
    "struct:exn:fail:unsupported"
    "struct:exn:fail:user"
    "struct:srcloc"
    "struct?"
    "sub1"
    "subbytes"
    "subprocess"
    "subprocess-group-enabled"
    "subprocess-kill"
    "subprocess-pid"
    "subprocess-status"
    "subprocess-wait"
    "subprocess?"
    "substring"
    "symbol->string"
    "symbol-interned?"
    "symbol-unreadable?"
    "symbol?"
    "sync"
    "sync/enable-break"
    "sync/timeout"
    "sync/timeout/enable-break"
    "syntax->list"
    "syntax-arm"
    "syntax-column"
    "syntax-disarm"
    "syntax-e"
    "syntax-line"
    "syntax-local-bind-syntaxes"
    "syntax-local-certifier"
    "syntax-local-context"
    "syntax-local-expand-expression"
    "syntax-local-get-shadower"
    "syntax-local-introduce"
    "syntax-local-lift-context"
    "syntax-local-lift-expression"
    "syntax-local-lift-module-end-declaration"
    "syntax-local-lift-provide"
    "syntax-local-lift-require"
    "syntax-local-lift-values-expression"
    "syntax-local-make-definition-context"
    "syntax-local-make-delta-introducer"
    "syntax-local-module-defined-identifiers"
    "syntax-local-module-exports"
    "syntax-local-module-required-identifiers"
    "syntax-local-name"
    "syntax-local-phase-level"
    "syntax-local-submodules"
    "syntax-local-transforming-module-provides?"
    "syntax-local-value"
    "syntax-local-value/immediate"
    "syntax-object->datum"
    "syntax-original?"
    "syntax-parse"
    "syntax-position"
    "syntax-property"
    "syntax-property-symbol-keys"
    "syntax-protect"
    "syntax-rearm"
    "syntax-recertify"
    "syntax-shift-phase-level"
    "syntax-source"
    "syntax-source-module"
    "syntax-span"
    "syntax-taint"
    "syntax-tainted?"
    "syntax-track-origin"
    "syntax-transforming-module-expression?"
    "syntax-transforming?"
    "syntax?"
    "system-big-endian?"
    "system-idle-evt"
    "system-language+country"
    "system-library-subpath"
    "system-path-convention-type"
    "system-type"
    "tan"
    "tcp-abandon-port"
    "tcp-accept"
    "tcp-accept-evt"
    "tcp-accept-ready?"
    "tcp-accept/enable-break"
    "tcp-addresses"
    "tcp-close"
    "tcp-connect"
    "tcp-connect/enable-break"
    "tcp-listen"
    "tcp-listener?"
    "tcp-port?"
    "terminal-port?"
    "thread"
    "thread-cell-ref"
    "thread-cell-set!"
    "thread-cell-values?"
    "thread-cell?"
    "thread-dead-evt"
    "thread-dead?"
    "thread-group?"
    "thread-resume"
    "thread-resume-evt"
    "thread-rewind-receive"
    "thread-running?"
    "thread-suspend"
    "thread-suspend-evt"
    "thread-wait"
    "thread/suspend-to-kill"
    "thread?"
    "time-apply"
    "transcript-off"
    "transcript-on"
    "truncate"
    "udp-addresses"
    "udp-bind!"
    "udp-bound?"
    "udp-close"
    "udp-connect!"
    "udp-connected?"
    "udp-open-socket"
    "udp-receive!"
    "udp-receive!*"
    "udp-receive!-evt"
    "udp-receive!/enable-break"
    "udp-receive-ready-evt"
    "udp-send"
    "udp-send*"
    "udp-send-evt"
    "udp-send-ready-evt"
    "udp-send-to"
    "udp-send-to*"
    "udp-send-to-evt"
    "udp-send-to/enable-break"
    "udp-send/enable-break"
    "udp?"
    "unbox"
    "uncaught-exception-handler"
    "use-collection-link-paths"
    "use-compiled-file-paths"
    "use-user-specific-search-paths"
    "values"
    "variable-reference->empty-namespace"
    "variable-reference->module-base-phase"
    "variable-reference->module-declaration-inspector"
    "variable-reference->module-path-index"
    "variable-reference->module-source"
    "variable-reference->namespace"
    "variable-reference->phase"
    "variable-reference->resolved-module-path"
    "variable-reference-constant?"
    "variable-reference?"
    "vector"
    "vector->immutable-vector"
    "vector->list"
    "vector->pseudo-random-generator"
    "vector->pseudo-random-generator!"
    "vector->values"
    "vector-fill!"
    "vector-immutable"
    "vector-length"
    "vector-ref"
    "vector-set!"
    "vector-set-performance-stats!"
    "vector?"
    "version"
    "void"
    "void?"
    "weak-box-value"
    "weak-box?"
    "will-execute"
    "will-executor?"
    "will-register"
    "will-try-execute"
    "with-input-from-file"
    "with-output-to-file"
    "wrap-evt"
    "write"
    "write-byte"
    "write-bytes"
    "write-bytes-avail"
    "write-bytes-avail*"
    "write-bytes-avail-evt"
    "write-bytes-avail/enable-break"
    "write-char"
    "write-special"
    "write-special-avail*"
    "write-special-evt"
    "write-string"
    "zero?"
   )
  "Racket builtins")

(defvar racket-font-lock-keywords
  `(
    ;; #lang
    ("\\(\\(#lang\\)[ ]+\\([^ \n]+\\)\\)"
     (2 font-lock-keyword-face nil t)
     (3 font-lock-function-name-face nil t))

    ;; keyword argument
    ("#:[^ ]+"                  . racket-keyword-argument-face)

    ;; symbol
    ("'\\sw+"                   . racket-selfeval-face)
    ("'|\\(\\sw\\| \\)+|"       . racket-selfeval-face)

    ;; literal char
    ("\\_<#\\\\\\([][-`~!@#$%&*()_+=^{}\;:'\"<>,.?/|\\\\]\\|\\sw+\\>\\)"
     . racket-selfeval-face)

    ;; paren
    ("[][(){}]"                 . racket-paren-face)

    (,(regexp-opt racket-builtins 'symbols) . font-lock-builtin-face)
    (,(regexp-opt racket-keywords 'symbols) . font-lock-keyword-face)

    ;; define -- vars
    ("(\\(define[ ]+\\([^ (]+\\)\\)" 2 font-lock-variable-name-face)
    ("(\\(define-values[ ]*(\\([^(]+\\))\\)" 2 font-lock-variable-name-face)

    ;; defineXxx -- functions
    ("(\\(define[^ ]*[ ]*([ ]*\\([^ ]+\\)\\)" 2 font-lock-function-name-face)

    ;; pretty lambda
    ("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
     2
     (progn (compose-region (match-beginning 2)
                            (match-end       2)
                            racket-lambda-char)
            nil))

    ;; #t #f
    (,(regexp-opt '("#t" "#f") 'symbols) . racket-selfeval-face)

    ;; From my Pygments lexer (maybe can simplify b/c unlike Pygments
    ;; we're not lexing for types like int vs. float).
    ;;
    ;; Numeric literals including Racket reader hash prefixes.
    ;; Caveat: None of these regexps attempt to exclude identifiers
    ;; that start with a number, such as a variable named
    ;; "100-Continue".

    ;; #d (or no hash prefix)
    ("\\_<\\(#d\\)?[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<\\(#d\\)?[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<\\(#d\\)?[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<\\(#d\\)?[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#d[^ ]*\\_>". font-lock-warning-face)

    ;; #x
    ("\\_<#x[-+]?[0-9a-fA-F]+\\.[0-9a-fA-F]+\\_>" . racket-selfeval-face)
    ;; the exponent variation (e.g. #x1e1) is N/A
    ("\\_<#x[-+]?[0-9a-fA-F]+/[0-9a-fA-F]+\\_>" . racket-selfeval-face)
    ("\\_<#x[-+]?[0-9a-fA-F]+\\_>" . racket-selfeval-face)
    ("\\_<#x[^ ]*\\_>" . font-lock-warning-face)

    ;; #b
    ("\\_<#b[-+]?[01]+\\.[01]+\\_>" . racket-selfeval-face)
    ("\\_<#b[01]+e[-+]?[01]+\\_>" . racket-selfeval-face)
    ("\\_<#b[-+]?[01]/[01]+\\_>" . racket-selfeval-face)
    ("\\_<#b[-+]?[01]+\\_>" . racket-selfeval-face)
    ("\\_<#b[^ ]*\\_>" . font-lock-warnng-face)

    ;; #e
    ("\\_<#e[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#e[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#e[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#e[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#e[^ ]*\\_>" . font-lock-warning-face)

    ;; #i
    ("\\_<#i[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#i[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#i[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#i[-+]?[0-9]+\\_>" . racket-selfeval-face)
    ("\\_<#i[^ ]*\\_>" . font-lock-warning-face)

    ;; #o
    ("\\_<#o[-+]?[0-7]+\\.[0-7]+\\_>" . racket-selfeval-face)
    ("\\_<#o[0-7]+e[-+]?[0-7]+\\_>" . racket-selfeval-face)
    ("\\_<#o[-+]?[0-7]+/[0-7]+\\_>" . racket-selfeval-face)
    ("\\_<#o[-+]?[0-7]+\\_>" . racket-selfeval-face)
    ("\\_<#o[^ ]*\\_>" . font-lock-warning-face)

    ;; numeric constants
    (,(regexp-opt '("+inf.0" "-inf.0" "+nan.0") 'symbols)
     . racket-selfeval-face)

    )
    "Font lock keywords for Racket mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar racket-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Racket")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar scheme] (cons "Racket" map))
    (define-key map [insert-lambda]
      '("Insert λ" . racket-insert-lambda))
    (define-key map [indent-region]
      '("Indent Region" . indent-region))
    (define-key map [comment-dwim]
      '("Comment" . comment-dwim))
    (define-key map [separator-1] '(menu-item "--"))
    (define-key map [send-def]
      '("Evaluate Last Definition" . racket-send-definition))
    (define-key map [send-region]
      '("Evaluate Region" . racket-send-region))
    (put 'racket-send-region 'menu-enable 'mark-active)
    (define-key map [send-sexp]
      '("Evaluate Last S-Expression" . racket-send-last-sexp))
    (define-key map [separator-2] '(menu-item "--"))
    (define-key map [racket-raco-test]
      '("Run Using `raco test' in *shell* buffer" . racket-raco-test))
    (define-key map [racket-test]
      '("Run Tests in *racket* buffer" . racket-test))
    (define-key map [racket-racket]
      '("Run Using `racket' in *shell* buffer" . racket-racket))
    (define-key map [racket-run]
      '("Run DrRacket Style, Fresh REPL in *racket* buffer" . racket-run))
    smap)
  "Keymap for Racket mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys

(define-key racket-mode-map (kbd "<f5>")     'racket-run)
(define-key racket-mode-map (kbd "M-C-<f5>") 'racket-racket)
(define-key racket-mode-map (kbd "C-<f5>")   'racket-test)
(define-key racket-mode-map "\r"             'racket-newline)
(define-key racket-mode-map ")"              'racket-insert-closing-paren)
(define-key racket-mode-map "]"              'racket-insert-closing-bracket)
(define-key racket-mode-map "}"              'racket-insert-closing-brace)
(define-key racket-mode-map "\M-\C-y"        'racket-insert-lambda)
(define-key racket-mode-map "\M-\C-x"        'racket-send-definition)
(define-key racket-mode-map "\C-x\C-e"       'racket-send-last-sexp)
(define-key racket-mode-map "\C-c\C-r"       'racket-send-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.rkt\\'" . racket-mode)
                ("\\.rktd\\'" . racket-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun racket-mode-variables (add?)
  ;; Font Lock
  (if add?
      (font-lock-add-keywords nil racket-font-lock-keywords)
    (setq font-lock-defaults `(,racket-font-lock-keywords)))

  ;; Indentation
  (racket-set-indentation)
  (setq indent-tabs-mode nil)

  ;; Syntax table
  ;; Make # and | symbol constituents.
  (modify-syntax-entry ?# "_ p14bn" racket-mode-syntax-table)
  (modify-syntax-entry ?| "_ 23bn"  racket-mode-syntax-table)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode racket-mode scheme-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket-mode-variables t)
  ;; ?? Run scheme-mode-hook so that things like Geiser minor mode work ??
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inferior Racket mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move this to its own file?

;; (require 'racket-mode)  ;if we move to its own file
(require 'comint)

(setq inferior-racket-buffer-name "*racket*")
(defun get-inferior-racket-buffer-process ()
  (get-buffer-process inferior-racket-buffer-name))

(define-derived-mode inferior-racket-mode comint-mode "Inferior Racket"
  "Major mode for interacting with Racket process."
  (setq comint-prompt-regexp "^[^>\n]*>+ *")
  (racket-mode-variables nil)
  (setq mode-line-process '(":%s"))
  (setq comint-input-filter (function racket-input-filter))
  (setq comint-get-old-input (function racket-get-old-input))
  (racket-mode-variables t))

(defcustom inferior-racket-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'racket)

(defun racket-input-filter (str)
  "Don't save anything matching `inferior-racket-filter-regexp'."
  (not (string-match inferior-racket-filter-regexp str)))

(defun racket-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defvar racket-sandbox-rkt
  (let ((elisp-dir (file-name-directory load-file-name)))
    (expand-file-name "sandbox.rkt" elisp-dir))
  "Path to sandbox.rkt")

;;;###autoload
(defun run-racket ()
  "Run an inferior Racket process, input and output via buffer `*racket*'.
If there is a process already running in `*racket*', switch to that buffer.
Runs the hook `inferior-racket-mode-hook' \(after the `comint-mode-hook'
is run)."
  (interactive)
  (unless (comint-check-proc inferior-racket-buffer-name)
    (set-buffer (make-comint "racket" racket-program nil racket-sandbox-rkt))
    (inferior-racket-mode))
  (setq racket-buffer inferior-racket-buffer-name)
  (racket-pop-to-buffer-same-window inferior-racket-buffer-name))

(defun racket-send-region (start end)
  "Send the current region to the inferior Racket process."
  (interactive "r")
  (comint-send-region (get-inferior-racket-buffer-process) start end)
  (comint-send-string (get-inferior-racket-buffer-process) "\n"))

(defun racket-send-definition ()
  "Send the current definition to the inferior Racket process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (racket-send-region (point) end))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the inferior Racket process."
  (interactive)
  (racket-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In case not Emacs 24.1, define equivalent of its
;; `pop-to-buffer-same-window'.
(defun racket-pop-to-buffer-same-window
  (&optional buffer-or-name norecord label)
  "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
  (if (fboundp 'pop-to-buffer-same-window)
      (funcall
       'pop-to-buffer-same-window buffer-or-name norecord)
    (funcall 'switch-to-buffer buffer-or-name norecord)))

(provide 'racket-mode)

;;; racket-mode.el ends here
