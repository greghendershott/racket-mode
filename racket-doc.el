;;; racket-doc.el -*- lexical-binding: t -*-

;; Copyright (c) 2020 by Greg Hendershott.
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

(require 'racket-browse-url)
(require 'racket-cmd)
(require 'racket-custom)
(require 'racket-util)
(declare-function racket--repl-session-id "racket-repl.el" ())

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
         (racket--doc-command (when (eq how 'namespace)
                                (racket--repl-session-id))
                              how
                              str))))))

(defun racket--doc-command (repl-session-id how str)
  "A helper for `racket--doc', `racket-xp-describe', and `racket-repl-describe'.

Centralizes how to issue doc command and handle response correctly."
  (racket--cmd/async repl-session-id
                     `(doc ,how ,str)
                     (lambda (maybe-url)
                       (if maybe-url
                           (racket-browse-url maybe-url)
                         (racket--search-doc str)))))

(defun racket--search-doc (str)
  "Search docs where the variable `racket-documentation-search-location' says."
  (pcase racket-documentation-search-location
    ((and (pred stringp) url) (racket-browse-url (format url str)))
    ('local                   (racket--search-doc-locally str))))

(defun racket--search-doc-locally (str)
  (call-process racket-program nil 0 nil "-l" "raco" "doc" str))

(provide 'racket-doc)

;; racket-doc.el ends here
