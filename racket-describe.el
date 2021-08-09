;;; racket-describe.el -*- lexical-binding: t -*-

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

(require 'shr)
(require 'subr-x)
(require 'racket-cmd)
(require 'racket-util)
(require 'racket-visit)
;; Don't (require 'racket-repl). Mutual dependency. Instead:
(declare-function 'racket--repl-session-id "racket-repl")
(autoload         'racket--repl-session-id "racket-repl")

(defun racket--do-describe (how
                            repl-session-id
                            str
                            &optional
                            display-and-pop-to-p
                            visit-thunk
                            doc-thunk)
  "Create a `racket-describe-mode' buffer.

HOW is supplied as the first argument to the back-end
\"describe\" command. See it for details that we don't need to
know or care about in this function.

STR is the string form of an identifier that is to be described.

DISPLAY-AND-POP-TO-P should be t for use by direct user commands
like `racket-xp-describe' and `racket-repl-describe' -- in which
the buffer is displayed -- and nil for use as
a :company-doc-buffer function.

VISIT-THUNK and DOC-THUNK are, when not nil, used to insert
\"Visit Definition\" and \"Documentation in Browser\" buttons.

Returns the buffer in which the description was written."
  ;; Work around what seems to be a bug with `shr-insert-document' --
  ;; elements are out of order when an existing Racket Describe buffer
  ;; hasn't had a `quit-window' -- by re-creating the buffer.
  (with-current-buffer (racket--get-buffer-recreate "*Racket Describe*")
    (let* ((html (racket--cmd/await repl-session-id
                                    `(describe ,how ,str)))
           ;; Because shr removes leading &nbsp; from <td> elements --
           ;; which messes up the indentation of s-expressions
           ;; including contracts -- replace &nbsp with `spc' in the
           ;; source HTML. Below we'll replace `spc' with " " in the
           ;; result of `shr-insert-document'.
           (spc (string #x2020))       ;unlikely character (hopefully)
           (dom (with-temp-buffer
                  (insert html)
                  (goto-char (point-min))
                  (while (re-search-forward "&nbsp;" nil t)
                    (replace-match spc t t))
                  (libxml-parse-html-region (point-min) (point-max)))))
      (racket-describe-mode)
      (read-only-mode -1)
      (let ((shr-use-fonts nil))
        (shr-insert-document dom))
      (goto-char (point-min))
      (while (re-search-forward spc nil t)
        (replace-match " " t t))
      (when display-and-pop-to-p
        (goto-char (point-max))
        (insert "\n")
        (when visit-thunk
          (insert-text-button "Visit Definition"
                              'follow-link t
                              'action
                              (lambda (_btn) (funcall visit-thunk)))
          (insert "   "))
        (when doc-thunk
          (insert-text-button "Documentation in Browser"
                              'follow-link t
                              'action
                              (lambda (_btn) (funcall doc-thunk))))
        (insert "          [q]uit"))
      (read-only-mode 1)
      (goto-char (point-min))
      (when display-and-pop-to-p
        (display-buffer (current-buffer) t)
        (pop-to-buffer (current-buffer))
        (message "Type TAB to move to links, 'q' to restore previous window"))
      (current-buffer))))

(defvar racket-describe-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("<tab>"   racket-describe--next-button)
            ("S-<tab>" racket-describe--prev-button)))
    m)
  "Keymap for Racket Describe mode.")

(define-derived-mode racket-describe-mode special-mode
  "RacketDescribe"
  "Major mode for describing Racket functions.
\\{racket-describe-mode-map}"
  (setq show-trailing-whitespace nil))

(defun racket-describe--next-button ()
  (interactive)
  (forward-button 1 t t))

(defun racket-describe--prev-button ()
  (interactive)
  (forward-button -1 t t))

;;; Search and disambiguation using local docs

;; For people who don't want to use a web browser at all: Search local
;; documentation, disambiguate in a buffer, and view in a
;; racket-describe-mode buffer.

;; Note: If we merge the multi back ends branch, this will need to
;; become a hash-table.
(defvar racket--local-doc-index-names nil)

(defun racket--local-doc-index-names ()
  (unless racket--local-doc-index-names
    (with-temp-message "Reading local documentation index..."
      (setq racket--local-doc-index-names
            (racket--cmd/await nil `(doc-index-names)))))
  (unless racket--local-doc-index-names
    (error "Can't read documentation index"))
  racket--local-doc-index-names)

(defun racket-search-describe ()
  "Search installed documentation; view using `racket-describe-mode'.

Always prompts you to enter a symbol, defaulting to the symbol at
point if any.

- If just one module exports the name, you go directly to a
  Racket Describe buffer with its documentation.

- If multiple modules export the name, you go first to a
  \"disambiguation\" buffer similar to the Racket \"Search
  Manuals\" web page. You may press RET on any item to get a
  Racket Describe buffer for that module's version of the thing.
"
  (interactive)
  (let ((key (racket--symbol-at-point-or-prompt t "Describe: "
                                                (racket--local-doc-index-names))))
    (pcase (racket--cmd/await nil `(doc-index-lookup ,key))
      (`() (message "not found"))
      (`((,_mods ,path ,anchor)) (racket-search-describe-visit key path anchor))
      (vs
       (with-current-buffer (get-buffer-create
                             (format "*Racket Search Describe `%s`*" key))
         (let ((col-name (format "`%s` as provided by" key)))
           (racket-search-describe-mode)
           (setq tabulated-list-format (vector (list col-name 80 t)))
           (setq tabulated-list-entries
                 (mapcar (pcase-lambda (`(,mods ,path ,anchor))
                           (list nil
                                 (vector
                                  (list (string-join mods ", ")
                                        'face   'default
                                        'key    key
                                        'path   path
                                        'anchor anchor
                                        'action #'racket-search-describe-button))))
                         vs))
           (tabulated-list-init-header)
           (tabulated-list-print)
           (pop-to-buffer (current-buffer))))))))

(defun racket-search-describe-button (button)
  (racket-search-describe-visit (button-get button 'key)
                                (button-get button 'path)
                                (button-get button 'anchor)))

(defun racket-search-describe-visit (term path anchor)
  (racket--do-describe (cons path anchor)
                       (racket--repl-session-id)
                       term
                       t))

(defvar racket-search-describe-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode racket-search-describe-mode tabulated-list-mode
  "RacketSearchDescribe"
  "Major mode for disambiguating documentation search results.")

(provide 'racket-describe)

;; racket-describe.el ends here
