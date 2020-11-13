;;; generate.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

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
(require 'cl-lib)

(defun racket-generate-reference.org ()
  (find-file "reference.org")
  (delete-region (point-min) (point-max))
  (insert (racket-generate--commands))
  (insert (racket-generate--variables))
  (insert (racket-generate--faces))
  (save-buffer 0)) ;don't create reference.org~ backup

;;; Commands

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
    racket-documentation-search
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
    "Test"
    racket-test
    racket-raco-test
    "Eval"
    racket-send-region
    racket-send-definition
    racket-send-last-sexp
    "Collections"
    racket-open-require-path
    racket-find-collection
    "Macro expand"
    racket-stepper-mode
    racket-expand-file
    racket-expand-region
    racket-expand-definition
    racket-expand-last-sexp
    "Other"
    racket-mode-start-faster
    "Showing information"
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
    racket-browse-url-using-temporary-file)
  "Commands to include in the Reference.")

(defun racket-generate--commands ()
  (apply #'concat
         (cons "* Commands\n\n"
               (mapcar #'racket-generate--command racket-generate--commands))))

(defun racket-generate--command (s)
  (pcase s
    ((and str (pred stringp))
     (format "** %s\n\n" s))
    ((and sym (pred symbolp))
     (racket-generate--command-2 sym racket-mode-map))
    (`(,sym ,keymap)
     (racket-generate--command-2 sym keymap))))

(defun racket-generate--command-2 (sym keymap)
  (unless (fboundp sym)
    (error "not defined %s" sym))
  (concat (format "*** %s\n" sym)
          (and (interactive-form sym)
               (racket-generate--bindings-as-kbd sym keymap))
          "\n\n"
          (racket-generate--quotes-to-tildes
           (racket-generate--linkify
            (racket-generate--bracket-command
             keymap
             (racket-generate--angle-mapvar
              (racket-generate--braces-mapvar
               (or (documentation sym t)
                   "No documentation.\n\n"))))))
          "\n\n"))

;;; Variables

(defconst racket-generate--variables
  '("General variables"
    racket-program
    racket-command-timeout
    racket-memory-limit
    racket-error-context
    racket-user-command-line-arguments
    racket-path-from-emacs-to-racket-function
    racket-path-from-racket-to-emacs-function
    racket-browse-url-function
    racket-xp-after-change-refresh-delay
    racket-xp-highlight-unused-regexp
    racket-documentation-search-location
    "REPL variables"
    racket-repl-buffer-name-function
    racket-submodules-to-run
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
    "Experimental debugger variables"
    racket-debuggable-files
    "Showing information"
    racket-show-functions)
  "Variables to include in the Reference.")

(defun racket-generate--variables ()
  (apply #'concat
         (cons "* Variables\n\n"
               (mapcar #'racket-generate--variable racket-generate--variables))))

(defun racket-generate--variable (s)
  (if (stringp s)
      (format "** %s\n\n" s)
    (unless (boundp s)
      (error "variable does not exist: %s" s))
    (concat (format "*** %s\n" s)
            (racket-generate--quotes-to-tildes
             (racket-generate--linkify
              (racket-generate--bracket-command
               racket-mode-map
               (racket-generate--angle-mapvar
                (or (documentation-property s 'variable-documentation t)
                    ;; Do check for function documentation here, to
                    ;; support documenting values for `-functions'
                    ;; variables.
                    (documentation s t)
                    "No documentation.\n\n")))))
            "\n\n")))

;;; Faces

(defconst racket-generate--faces
  '(racket-keyword-argument-face
    racket-selfeval-face
    racket-here-string-face
    racket-xp-def-face
    racket-xp-use-face
    racket-xp-unused-face
    racket-logger-config-face
    racket-logger-topic-face
    racket-logger-fatal-face
    racket-logger-error-face
    racket-logger-warning-face
    racket-logger-info-face
    racket-logger-debug-face)
  "Faces to include in the Reference.")

(defun racket-generate--faces ()
  (apply #'concat
         (cl-list* "* Faces\n\n"
                   "** All\n\n"
                   (mapcar #'racket-generate--face racket-generate--faces))))

(defun racket-generate--face (symbol)
  (concat (format "*** %s\n" symbol)
          (racket-generate--quotes-to-tildes
           (racket-generate--linkify
            (or (documentation-property symbol 'face-documentation t)
                "No documentation.\n\n")))
          "\n\n"))

;;; Utility

(defun racket-generate--linkify (s)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx ?\`
                                  (group "racket-" (+ (or (syntax word)
                                                          (syntax symbol))))
                                  ?\')
                              nil t)
      (let* ((name (match-string-no-properties 1))
             (sym (intern-soft name)))
        (when (or (cl-some (lambda (v)
                             (or (eq v sym)
                                 (and (listp v)
                                      (eq (car v) sym))))
                           racket-generate--commands)
                  (member sym racket-generate--variables)
                  (member sym racket-generate--faces))
          (replace-match (format "@@texinfo:@ref{%s}@@" name)
                         t t))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-generate--quotes-to-tildes (s)
  "Change \` \' and \` \` style quotes to ~~ style."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx (or (seq ?\`
                                           (group (+? any
                                                      ;; (or (syntax word)
                                                      ;;     (syntax symbol)
                                                      ;;     ?\# ?\'
                                                      ;;     (syntax whitespace)
                                                      ))
                                           (or ?\` ?\'))))
                              nil t)
      (let ((name (match-string-no-properties 1)))
        (replace-match (format "~%s~" name)
                       t t)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;(racket-generate--quotes-to-tildes "foo `bar` bazz")
;;(racket-generate--quotes-to-tildes "foo `bar bazz`")
;;(racket-generate--quotes-to-tildes "foo `'symbol`")
;;(racket-generate--quotes-to-tildes "Change from `#lang racket` to `#lang racket/base`.")

(defun racket-generate--braces-mapvar (s)
  ;; ‘\{MAPVAR}’ stands for a summary of the keymap which is the value
  ;; of the variable MAPVAR.
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx (or (seq ?\\
                                           ?\{
                                           (group (+ (or (syntax word)
                                                         (syntax symbol))))
                                           ?\})))
                              nil t)
      (let* ((name (match-string-no-properties 1))
             (sym (intern-soft name))
             (km (symbol-value sym)))
        (replace-match "")
        (insert "|Key|Binding|\n")
        (mapc #'racket-generate--insert-keymap-table-row
              (cdr km))
        (newline)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-generate--insert-keymap-table-row (v &optional keys)
  (pcase v
    (`(,(and key (pred numberp)) keymap ,(and key+sym (pred consp)))
     (racket-generate--insert-keymap-table-row key+sym (cons key keys)))
    (`(,(and key (pred numberp)) keymap . ,(and more (pred consp)))
     (mapc (lambda (v)
             (racket-generate--insert-keymap-table-row v (cons key keys)))
           more))
    (`(,(and key (pred numberp)) . ,(and sym (pred symbolp)))
     (insert (format "|{{{kbd(%s)}}}|`%s'|\n"
                     (racket-generate--key-description
                      (reverse (cons key keys)))
                     sym)))))

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
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-generate--angle-mapvar (s)
  ;; \\<MAPVAR> stands for no text itself. It is used only for a side
  ;; effect: it specifies MAPVAR’s value as the keymap for any
  ;; following ‘\[COMMAND]’ sequences in this documentation string.
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward (rx (or (seq ?\\
                                           ?\<
                                           (group (+ (or (syntax word)
                                                         (syntax symbol))))
                                           ?\>)))
                              nil t)
      (replace-match ""))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-generate--bracket-command (keymap str)
  ;; \\[COMMAND] stands for a key sequence that will invoke COMMAND,
  ;; or ‘M-x COMMAND’ if COMMAND has no key bindings.
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward (rx (or (seq ?\\
                                           ?\[
                                           (group (+ (or (syntax word)
                                                         (syntax symbol))))
                                           ?\])))
                              nil t)
      (let ((name (match-string-no-properties 1)))
        (replace-match "")
        (insert
         (if (string-equal name "universal-argument")
             "{{{kbd(C-u)}}}"
           (racket-generate--bindings-as-kbd (intern-soft name) keymap)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun racket-generate--bindings-as-kbd (symbol keymap)
  (let* ((bindings (or (racket-generate--where-is-no-menu symbol keymap)
                       (racket-generate--where-is-no-menu symbol racket-mode-map)))
         (strs (and
                bindings
                (cl-remove-if-not
                 #'identity
                 (mapcar (lambda (binding)
                           (let ((desc (racket-generate--key-description binding)))
                             ;; I don't know how to escape { or }
                             ;; for texi
                             (unless (string-match-p "[{}]" desc)
                               (format "{{{kbd(%s)}}}" desc))))
                         bindings)))))
    (if strs
        (mapconcat #'identity strs " or ")
      (format "{{{kbd(M-x)}}} ~%s~" symbol))))

(defun racket-generate--where-is-no-menu (symbol keymap)
  (cl-remove-if (lambda (binding) (eq (aref binding 0) 'menu-bar))
                (where-is-internal symbol keymap)))

;;; generate.el ends here
