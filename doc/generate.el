;;; generate.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Generate a reference.org file from doc strings

;; The reference.org file is included by racket-mode.org as its last,
;; "Reference" section. Then racket-mode.org is used to create both
;; Info and HTML format documentation.

(require 'racket-mode)
(require 'racket-debug)
(require 'racket-profile)
(require 'racket-edit)
(require 'racket-xp)
(require 'racket-util)
(require 'racket-show)
(require 'racket-unicode-input-method)
(require 'racket-smart-open)
(require 'racket-repl-buffer-name)
(require 'seq)

(defun racket-generate-reference.org ()
  (find-file "reference.org")
  (delete-region (point-min) (point-max))
  (insert (racket-generate--commands))
  (insert (racket-generate--variables))
  (insert (racket-generate--configuration-functions))
  (insert (racket-generate--faces))
  (save-buffer 0)) ;don't create reference.org~ backup

;;; Interactive command functions

(defconst racket-generate--commands
  `("Edit"
    racket-mode
    racket-insert-lambda
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
    racket-unicode-input-method-enable
    racket-align
    racket-unalign
    racket-complete-at-point
    "Explore"
    racket-xp-mode
    (racket-xp-describe ,racket-xp-mode-map)
    (racket-xp-documentation ,racket-xp-mode-map)
    (racket-xp-next-definition ,racket-xp-mode-map)
    (racket-xp-previous-definition ,racket-xp-mode-map)
    (racket-xp-next-use ,racket-xp-mode-map)
    (racket-xp-previous-use ,racket-xp-mode-map)
    (racket-xp-next-error ,racket-xp-mode-map)
    (racket-xp-previous-error ,racket-xp-mode-map)
    (racket-xp-tail-up ,racket-xp-mode-map)
    (racket-xp-tail-down ,racket-xp-mode-map)
    (racket-xp-tail-next-sibling ,racket-xp-mode-map)
    (racket-xp-tail-previous-sibling ,racket-xp-mode-map)
    racket-documentation-search
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
    (racket-debug-mode ,racket-xp-mode-map)
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
    "Other"
    racket-debug-toggle-breakpoint
    racket-mode-start-faster
    racket-mode-start-slower)
  "Commands to include in the Reference.")

(defun racket-generate--commands ()
  (apply
   #'concat
   "* Commands\n\n"
   (mapcar (lambda (s)
             (pcase s
               ((and str (pred stringp))
                (format "** %s\n\n" s))
               ((and sym (pred symbolp))
                (racket-generate--command-or-function sym racket-mode-map))
               (`(,sym ,keymap)
                (racket-generate--command-or-function sym keymap))))
           racket-generate--commands)))

(defun racket-generate--command-or-function (sym keymap)
  (unless (fboundp sym)
    (error "not defined %s" sym))
  (concat (format "*** %s\n" sym)
          (if keymap
              (when (interactive-form sym)
                (racket-generate--bindings-as-kbd sym keymap))
            (format "~%s~\n" (cons sym (help-function-arglist sym))))
          "\n\n"
          (racket-generate--format-doc-string
           (or (documentation sym t)
               "No documentation.\n\n"))
          "\n\n"))

;;; Configuration functions

(defconst racket-generate--configuration-functions
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
    racket-add-back-end)
  "Configuration functions to include in the Reference.")

(defun racket-generate--configuration-functions ()
  (apply
   #'concat
   "* Configuration functions\n\n"
   (mapcar (lambda (s)
             (pcase s
               ((and str (pred stringp))
                (format "** %s\n\n" s))
               ((and sym (pred symbolp))
                (racket-generate--command-or-function sym nil))))
           racket-generate--configuration-functions)))

;;; Variables

(defconst racket-generate--variables
  '("General variables"
    racket-program
    racket-command-timeout
    racket-memory-limit
    racket-error-context
    racket-user-command-line-arguments
    racket-browse-url-function
    racket-xp-after-change-refresh-delay
    racket-xp-highlight-unused-regexp
    racket-documentation-search-location
    "REPL variables"
    racket-repl-buffer-name-function
    racket-submodules-to-run
    racket-repl-history-directory
    racket-history-filter-regexp
    racket-images-inline
    racket-imagemagick-props
    racket-images-keep-last
    racket-images-system-viewer
    racket-pretty-print
    "Other variables"
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
    racket-show-functions)
  "Variables to include in the Reference.")

(defun racket-generate--variables ()
  (apply
   #'concat
   "* Variables\n\n"
   (mapcar (lambda (s)
             (if (stringp s)
                 (format "** %s\n\n" s)
               (unless (boundp s)
                 (error "variable does not exist: %s" s))
               (concat
                (format "*** %s\n" s)
                (racket-generate--format-doc-string
                 (or (documentation-property s 'variable-documentation t)
                     ;; Do check for function documentation here,
                     ;; to support documenting values for
                     ;; `-functions' variables.
                     (documentation s t)
                     "No documentation.\n\n"))
                "\n\n")))
           racket-generate--variables)))

;;; Faces

(defconst racket-generate--faces
  '(racket-keyword-argument-face
    racket-reader-quoted-symbol-face
    racket-reader-syntax-quoted-symbol-face
    racket-here-string-face
    racket-xp-def-face
    racket-xp-use-face
    racket-xp-unused-face
    racket-xp-tail-target-face
    racket-xp-tail-position-face
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
    racket-doc-litchar-face)
  "Faces to include in the Reference.")

(defun racket-generate--faces ()
  (apply
   #'concat
   "* Faces\n\n"
   "** All\n\n"
   (mapcar (lambda (symbol)
             (concat
              (format "*** %s\n" symbol)
              (racket-generate--format-doc-string
               (or (documentation-property symbol 'face-documentation t)
                   "No documentation.\n\n"))
              "\n\n"))
           racket-generate--faces)))

;;; Utility

(defun racket-generate--format-doc-string (docstring)
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
            (insert (racket-generate--ref-or-code sym))))
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
              (mapc #'racket-generate--insert-keymap-table-row
                    (cdr km))
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
               (racket-generate--bindings-as-kbd (intern-soft name) keymap)))))
         ;; Don't modify other characters.
         (t
          (forward-char 1))))
      (buffer-string))))

(defun racket-generate--insert-keymap-table-row (v &optional keys)
  (pcase v
    (`(,(and key (pred numberp)) keymap ,(and key+sym (pred consp)))
     (racket-generate--insert-keymap-table-row key+sym (cons key keys)))
    (`(,(and key (pred numberp)) keymap . ,(and more (pred consp)))
     (mapc (lambda (v)
             (racket-generate--insert-keymap-table-row v (cons key keys)))
           more))
    (`(,(and key (pred numberp)) . ,(and sym (pred symbolp)))
     (insert (format "|{{{kbd(%s)}}}|%s|\n"
                     (racket-generate--key-description (reverse (cons key keys)))
                     (racket-generate--ref-or-code sym))))))

(defun racket-generate--key-description (xs)
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

(defun racket-generate--bindings-as-kbd (symbol keymap)
  (let* ((bindings (or (racket-generate--where-is/no-menu symbol keymap)
                       (racket-generate--where-is/no-menu symbol racket-mode-map)))
         (strs (and
                bindings
                (seq-filter
                 #'identity
                 (mapcar (lambda (binding)
                           (let ((desc (racket-generate--key-description binding)))
                             ;; I don't know how to escape { or }
                             ;; for texi
                             (unless (string-match-p "[{}]" desc)
                               (format "{{{kbd(%s)}}}" desc))))
                         bindings)))))
    (if strs
        (string-join strs " or ")
      (format "{{{kbd(M-x)}}} ~%s~" symbol))))

(defun racket-generate--where-is/no-menu (symbol keymap)
  (seq-filter (lambda (binding) (not (eq (aref binding 0) 'menu-bar)))
              (where-is-internal symbol keymap)))

(defun racket-generate--ref-or-code (sym)
  "Return either a reference or code formatting for SYM."
  (if (or (seq-some (lambda (v)
                      (or (eq v sym)
                          (and (listp v)
                               (eq (car v) sym))))
                    racket-generate--commands)
          (member sym racket-generate--configuration-functions)
          (member sym racket-generate--variables)
          (member sym racket-generate--faces))
      (format "@@texinfo:@ref{%s}@@" sym)
    (format "~%s~" sym)))

;;; generate.el ends here
