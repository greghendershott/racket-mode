;;; racket-doc.el -*- lexical-binding: t -*-

;; Copyright (c) 2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'url-util)
(require 'racket-browse-url)
(require 'racket-cmd)
(require 'racket-custom)
(require 'racket-util)
(require 'racket-back-end)
(declare-function racket--repl-session-id "racket-repl.el" ())

(defun racket--doc-assert-local-back-end ()
  (unless (racket--back-end-local-p)
    (user-error "Cannot use web browser to browse remote documentation; instead use `racket-describe'")))

(defun racket--doc (prefix how completions)
  "A helper for `racket-xp-documentation' and `racket-repl-documentation'."
  (let ((search-p (equal prefix '(16))))
    (pcase (racket--symbol-at-point-or-prompt prefix
                                              "Documentation for: "
                                              (unless search-p completions)
                                              search-p)
      ((and (pred stringp) str)
       (if search-p
           (racket--search-doc str)
         (racket--doc-assert-local-back-end)
         (racket--doc-command (when (eq how 'namespace)
                                (racket--repl-session-id))
                              how
                              str))))))

(defun racket--doc-command (repl-session-id how str)
  "A helper for `racket--doc', `racket-xp-describe', and `racket-repl-describe'.

Centralizes how to issue doc command and handle response correctly."
  (let ((how (racket-how-front-to-back how)))
    (racket--cmd/async repl-session-id
                       `(doc ,how ,str)
                       (lambda (maybe-url)
                         (if maybe-url
                             (racket-browse-url maybe-url)
                           (racket--search-doc str))))))

(defun racket--search-doc (str)
  "Search docs where the variable `racket-documentation-search-location' says."
  (pcase racket-documentation-search-location
    ((and (pred stringp) url) (racket-browse-url (format url (url-hexify-string str))))
    ('local                   (racket--search-doc-locally str))
    (_ (user-error "Unknown value for `racket-documentation-search-location': %s"
                   racket-documentation-search-location))))

(defun racket--search-doc-locally (str)
  (racket--doc-assert-local-back-end)
  (call-process (expand-file-name racket-program)
                nil ;INFILE: none
                0   ;DESTINATION: discard/don't wait
                nil ;DISPLAY: none
                "-l" "raco" "docs" str))

(provide 'racket-doc)

;; racket-doc.el ends here
