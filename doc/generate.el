;;; generate.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Generate a reference.org file from doc strings

;; The reference.org file is included by racket-mode.org as its last,
;; "Reference" section. Then racket-mode.org is used to create both
;; Info and HTML format documentation.
;;
;; Note: This file isn't loaded when running Racket Mode -- only to
;; build docs. In doc/Makefile see the reference.org target.

(require 'racket-mode)
(require 'racket-debug)
(require 'racket-profile)
(require 'racket-edit)
(require 'racket-xp)
(require 'racket-util)
(require 'racket-show)
(require 'racket-input)
(require 'racket-smart-open)
(require 'racket-repl-buffer-name)
(require 'racket-hash-lang)
(require 'racket-package)
(require 'seq)

(defun racket-generate-reference.org ()
  (with-temp-buffer
    (racket--insert-org-contents)
    (write-region nil nil "reference.org")))

(defun racket--insert-org-contents ()
  (racket--insert-commands)
  (racket--insert-variables)
  (racket--insert-functions)
  (racket--insert-faces))

;;; Interactive command functions

(defconst racket--commands
  `("Edit"
    racket-mode
    racket-insert-lambda
    racket-insert-symbol
    racket-fold-all-tests
    racket-unfold-all-tests
    racket-tidy-requires
    racket-trim-requires
    racket-base-requires
    racket-add-require-for-identifier
    racket-indent-line
    racket-smart-open-bracket-mode
    racket-insert-closing
    racket-cycle-paren-shapes
    racket-backward-up-list
    racket-input-mode
    racket-align
    racket-unalign
    racket-complete-at-point
    "Hash Langs"
    racket-hash-lang-mode
    (racket-hash-lang-backward   ,racket-hash-lang-mode-map)
    (racket-hash-lang-forward    ,racket-hash-lang-mode-map)
    (racket-hash-lang-up         ,racket-hash-lang-mode-map)
    (racket-hash-lang-down       ,racket-hash-lang-mode-map)
    (racket-hash-lang-C-M-q-dwim ,racket-hash-lang-mode-map)
    "Explore"
    racket-xp-mode
    (racket-xp-describe              ,racket-xp-mode-map)
    (racket-xp-documentation         ,racket-xp-mode-map)
    (racket-xp-next-definition       ,racket-xp-mode-map)
    (racket-xp-previous-definition   ,racket-xp-mode-map)
    (racket-xp-next-use              ,racket-xp-mode-map)
    (racket-xp-previous-use          ,racket-xp-mode-map)
    (racket-xp-next-error            ,racket-xp-mode-map)
    (racket-xp-previous-error        ,racket-xp-mode-map)
    (racket-xp-tail-up               ,racket-xp-mode-map)
    (racket-xp-tail-down             ,racket-xp-mode-map)
    (racket-xp-tail-next-sibling     ,racket-xp-mode-map)
    (racket-xp-tail-previous-sibling ,racket-xp-mode-map)
    racket-documentation-search
    racket-describe-mode
    racket-describe-search
    "Run"
    racket-repl-mode
    racket-run
    racket-run-and-switch-to-repl
    racket-run-module-at-point
    racket-repl
    (racket-repl-describe ,racket-repl-mode-map)
    (racket-repl-documentation ,racket-repl-mode-map)
    racket-racket
    racket-profile
    racket-profile-mode
    racket-logger
    racket-logger-mode
    racket-repl-clear
    racket-repl-clear-leaving-last-prompt
    "Test"
    racket-test
    racket-raco-test
    "Eval"
    racket-send-region
    racket-send-definition
    racket-send-last-sexp
    "Collections"
    racket-open-require-path
    "Macro expand"
    racket-stepper-mode
    racket-expand-file
    racket-expand-region
    racket-expand-definition
    racket-expand-last-sexp
    "Packages"
    racket-package-refresh
    list-racket-packages
    racket-package-mode
    describe-racket-package
    "Debug"
    racket-debug-mode
    (racket-debug-step                      ,racket-debug-mode-map)
    (racket-debug-step-over                 ,racket-debug-mode-map)
    (racket-debug-step-out                  ,racket-debug-mode-map)
    (racket-debug-forward-breakable         ,racket-debug-mode-map)
    (racket-debug-backward-breakable        ,racket-debug-mode-map)
    (racket-debug-run-to-here               ,racket-debug-mode-map)
    (racket-debug-set-break-expression      ,racket-debug-mode-map)
    (racket-debug-clear-break-expression    ,racket-debug-mode-map)
    (racket-debug-toggle-break-expression   ,racket-debug-mode-map)
    (racket-debug-forward-break-expression  ,racket-debug-mode-map)
    (racket-debug-backward-break-expression ,racket-debug-mode-map)
    (racket-debug-set-local                 ,racket-debug-mode-map)
    (racket-debug-continue                  ,racket-debug-mode-map)
    (racket-debug-go                        ,racket-debug-mode-map)
    "Other"
    racket-mode-start-faster
    racket-mode-start-slower)
  "Commands to include in the Reference.")

(defun racket--insert-commands ()
  (insert "* Commands\n\n")
  (dolist (s racket--commands)
    (pcase s
      ((and str (pred stringp))
       (insert (format "** %s\n\n" str)))
      ((and sym (pred symbolp))
       (insert-command-or-function sym racket-mode-map))
      (`(,sym ,keymap)
       (insert-command-or-function sym keymap)))))

(defun insert-command-or-function (sym keymap)
  (unless (fboundp sym)
    (error "not defined %s" sym))
  (insert (format "*** %s\n" sym)
          (if keymap
              (if (interactive-form sym)
                  (racket--bindings-as-kbd sym keymap)
                "")
            (format "~%s~\n" (cons sym (help-function-arglist sym))))
          "\n\n"
          (racket--format-doc-string
           (or (documentation sym t)
               "No documentation.\n\n"))
          "\n\n"))

;;; Configuration functions

(defconst racket--functions
  `("Showing information"
    racket-show-pseudo-tooltip
    racket-show-echo-area
    racket-show-header-line
    racket-show-pos-tip
    "Associating edit buffers with REPL buffers"
    racket-repl-buffer-name-shared
    racket-repl-buffer-name-unique
    racket-repl-buffer-name-project
    racket-project-root
    "Browsing file URLs with anchors"
    racket-browse-url-using-temporary-file
    "Configuring back ends"
    racket-add-back-end
    "Running racket and raco commands in a shell or terminal"
    racket-shell
    racket-term
    racket-ansi-term
    racket-vterm)
  "Configuration functions to include in the Reference.")

(defun racket--insert-functions ()
  (insert "* Configuration functions\n\n")
  (dolist (s racket--functions)
    (pcase s
      ((and str (pred stringp))
       (insert (format "** %s\n\n" str)))
      ((and sym (pred symbolp))
       (insert-command-or-function sym nil)))))

;;; Variables

(defconst racket--variables
  '("General variables"
    racket-program
    racket-command-timeout
    racket-memory-limit
    racket-error-context
    racket-user-command-line-arguments
    racket-browse-url-function
    racket-xp-after-change-refresh-delay
    racket-xp-highlight-unused-regexp
    racket-xp-add-binding-faces
    racket-xp-eldoc-level
    racket-documentation-search-location
    racket-expand-hiding
    "Hash lang variables"
    racket-hash-lang-token-face-alist
    racket-hash-lang-module-language-hook
    "REPL variables"
    racket-repl-buffer-name-function
    racket-submodules-to-run
    racket-repl-history-directory
    racket-history-filter-regexp
    racket-images-inline
    racket-imagemagick-props
    racket-images-keep-last
    racket-images-system-viewer
    racket-images-do-not-use-svg
    racket-pretty-print
    racket-repl-command-file
    racket-repl-echo-sent-expressions
    "Other variables"
    racket-doc-index-directory
    racket-doc-index-predicate-function
    racket-indent-curly-as-sequence
    racket-indent-sequence-depth
    racket-pretty-lambda
    racket-smart-open-bracket-enable
    racket-logger-config
    racket-before-run-hook
    racket-after-run-hook
    racket-sexp-comment-fade
    "Experimental debugger variables"
    racket-debuggable-files
    "Showing information"
    racket-show-functions
    "Running racket and raco commands in a shell or terminal"
    racket-shell-or-terminal-function
    "Racket input method"
    racket-input-prefix
    racket-input-translations)
  "Variables to include in the Reference.")

(defun racket--insert-variables ()
  (insert "* Variables\n\n")
  (dolist (s racket--variables)
    (if (stringp s)
        (insert (format "** %s\n\n" s))
      (unless (boundp s)
        (error "variable does not exist: %s" s))
      (insert
       (format "*** %s\n" s)
       (racket--format-doc-string
        (or (documentation-property s 'variable-documentation t)
            ;; Do check for function documentation here,
            ;; to support documenting values for
            ;; `-functions' variables.
            (documentation s t)
            "No documentation.\n\n"))
       "\n\n"))))

;;; Faces

(defconst racket--faces
  '(racket-keyword-argument-face
    racket-reader-quoted-symbol-face
    racket-reader-syntax-quoted-symbol-face
    racket-here-string-face
    racket-xp-def-face
    racket-xp-use-face
    racket-xp-unused-face
    racket-xp-tail-target-face
    racket-xp-tail-position-face
    racket-xp-binding-lang-face
    racket-xp-binding-lang-use-face
    racket-xp-binding-import-face
    racket-xp-binding-import-use-face
    racket-xp-binding-local-face
    racket-xp-binding-local-use-face
    racket-logger-config-face
    racket-logger-topic-face
    racket-logger-fatal-face
    racket-logger-error-face
    racket-logger-warning-face
    racket-logger-info-face
    racket-logger-debug-face
    racket-doc-link-face
    racket-ext-link-face
    racket-doc-output-face
    racket-doc-litchar-face
    racket-repl-message
    racket-repl-prompt
    racket-repl-value
    racket-repl-error-message
    racket-repl-error-location
    racket-repl-stdout
    racket-repl-stderr
    racket-hash-lang-text)
  "Faces to include in the Reference.")

(defun racket--insert-faces ()
  (insert "* Faces\n\n"
          "** All\n\n")
  (dolist (s racket--faces)
    (insert
     (format "*** %s\n" s)
     (racket--format-doc-string
      (or (documentation-property s 'face-documentation t)
          "No documentation.\n\n"))
     "\n\n")))

;;; Utility

(defun racket--format-doc-string (docstring)
  "Convert command key references and keymap references
in DOCSTRING to buttons.

Emacs uses \\= to escape \\[ references, so replace that
unescaping too."
  ;; Based on helpful--format-command-keys, which in turn is loosely
  ;; based on `substitute-command-keys'.
  (let ((keymap nil))
    (with-temp-buffer
      (insert docstring)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at
           ;; Text of the form \=X
           (rx "\\="))
          ;; Remove the escaping, then step over the escaped char.
          (delete-region (point) (+ (point) 2))
          (forward-char 1))
         ((looking-at
           ;; Text of the form `racket-XXX'
           (rx "`" (group "racket-" (+ (or (syntax word) (syntax symbol)))) "'"))
          (let* ((len (length (match-string 0)))
                 (name (match-string-no-properties 1))
                 (sym (intern-soft name)))
            (delete-region (point) (+ (point) len))
            (insert (racket--ref-or-code sym))))
         ((looking-at
           ;; Text of the form `contents' or `contents`
           (rx "`" (group (+ (not (in "`'")))) (in "`'")))
          (let* ((len (length (match-string 0)))
                 (contents (match-string 1)))
            (delete-region (point) (+ (point) len))
            (insert (format "~%s~" contents))))
         ((looking-at
           ;; Text of the form \\<foo-keymap>
           (rx "\\<" (group (+ (not (in ">")))) ">"
               (? "\n")))
          (let* ((len (length (match-string 0)))
                 (symbol-name (match-string 1)))
            ;; Remove the original string.
            (delete-region (point) (+ (point) len))
            ;; Set the new keymap.
            (setq keymap (symbol-value (intern symbol-name)))))
         ((looking-at
           ;; Text of the form \\{foo-mode-map}
           (rx "\\{" (group (+ (not (in "}")))) "}"))
          (let ((len (length (match-string 0)))
                (km (symbol-value (intern (match-string 1)))))
            ;; Remove the original string.
            (delete-region (point) (+ (point) len))
            ;; Insert an org-mode table
            (when km
              (insert "|Key|Binding|\n")
              (racket--insert-keymap km)
              (newline))))
         ((looking-at
           ;; Text of the form \\[foo-command]
           (rx "\\[" (group (+ (not (in "]")))) "]"))
          (let* ((len (length (match-string 0)))
                 (name (match-string 1)))
            ;; Remove the original string.
            (delete-region (point) (+ (point) len))
            (insert
             (if (string-equal name "universal-argument")
                 "{{{kbd(C-u)}}}"
               (racket--bindings-as-kbd (intern-soft name) keymap)))))
         ;; Don't modify other characters.
         (t
          (forward-char 1))))
      (buffer-string))))

(defun racket--insert-keymap (km)
  "Insert org table describing keymap KM.

Filter \"noise\" like bindings for `negative-argument' and
`digit-argument'.

Insert sorted by command name.

When multiple key-bindings for a command, group into one row,
sorted shortest key sequences first."
  ;; Accumulate nested keymaps into a flat association list. Each key
  ;; is a command symbol. Each value is a list of lists of keys
  ;; (potentially more than one key sequence bound for each command).
  (let ((alist nil))
    (cl-labels
        ((add (sym keys)
           (let ((cell (assq sym alist)))
             (if cell
                 (setcdr cell (push keys (cdr cell)))
               (push (cons sym (list keys)) alist))))
         (keymap (v prefix-keys)
           (dolist (v (cdr v))
             (pcase v
               (`(,(and key (pred numberp)) . ,(and km (pred keymapp)))
                (keymap km (cons key prefix-keys)))
               (`(,(and key (pred numberp)) keymap ,(and km (pred keymapp)))
                (keymap km (cons key prefix-keys)))
               (`(,(and key (pred numberp)) . ,(and sym (pred symbolp)))
                (unless (member sym '(negative-argument
                                      digit-argument
                                      describe-mode))
                  (add sym (reverse (cons key prefix-keys)))))))))
      ;; Recursively mutate `alist'.
      (keymap km nil)
      (dolist (v alist)
        (let* ((command-str (racket--ref-or-code (car v)))
               (keys-strs (seq-map (lambda (binding)
                                     (format "{{{kbd(%s)}}}"
                                             (racket--key-description
                                              binding)))
                                   (cdr v)))
               (keys-str (string-join (seq-sort-by #'length #'< keys-strs)
                                      " or ")))
          (insert "|" keys-str "|" command-str "|\n"))))))

(defun racket--key-description (xs)
  "Like `key-description' but escapes some chars for our \"KBD\" texi macro."
  (with-temp-buffer
    (insert (key-description xs))
    (goto-char (point-min))
    (while (re-search-forward (rx (or ?\, ?\` ?\})) nil t)
      (let ((str (match-string-no-properties 0)))
        (replace-match "")
        (insert (if (equal str "}") "@" "\\"))
        (insert str)))
    (buffer-string)))

(defun racket--bindings-as-kbd (symbol keymap)
  (let* ((bindings (or (racket--where-is symbol keymap)
                       (racket--where-is symbol racket-mode-map)))
         (strs (and
                bindings
                (seq-filter
                 #'identity
                 (mapcar (lambda (binding)
                           (let ((desc (racket--key-description binding)))
                             ;; I don't know how to escape { or }
                             ;; for texi
                             (unless (string-match-p "[{}]" desc)
                               (format "{{{kbd(%s)}}}" desc))))
                         bindings)))))
    (if strs
        (string-join strs "or ")
      (format "{{{kbd(M-x)}}} ~%s~" symbol))))

(defun racket--where-is (symbol keymap)
  "Like `where-is-internal' but omits menu items."
  (seq-filter (lambda (binding) (not (eq (aref binding 0) 'menu-bar)))
              (where-is-internal symbol keymap)))

(defun racket--ref-or-code (sym)
  "Return either a reference or code formatting for SYM."
  (if (or (seq-some (lambda (v)
                      (or (eq v sym)
                          (and (listp v)
                               (eq (car v) sym))))
                    racket--commands)
          (member sym racket--functions)
          (member sym racket--variables)
          (member sym racket--faces))
      (format "@@texinfo:@ref{%s}@@" sym)
    (format "~%s~" sym)))

;;; generate.el ends here
