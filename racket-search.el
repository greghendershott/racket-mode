;;; racket-complete.el -*- lexical-binding: t -*-

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

(require 'racket-cmd)
(require 'racket-describe)
(require 'racket-browse-url)
(require 'racket-repl)

(defun racket--do-search (repl-session-id expr num-results)
  "Create a `racket-search-mode buffer'.

EXPR is used as the search term.

NUM-RESULTS is the number of search results to return from the command server.

Returns the buffer in which the description was written."
  (with-current-buffer (racket--get-buffer-recreate "*Racket Search*")
    (let* ((search-results (racket--cmd/await repl-session-id
					      `(search ,expr ,num-results))))
      (racket-search-mode)
      (read-only-mode -1)
      (cl-loop
       for result in search-results
       do (pcase result
            (`(,terms ,mods ,how)
             (insert (string-join terms ", "))
             (insert "\t")
             (letrec
                 ((mods/string
                   (lambda (mods)
                     (cond
                      ((listp mods)
                       (string-join
                        (mapcar mods/string mods)
                        ", "))
                      ((symbolp mods)
                       (symbol-name mods))))))
               (insert (funcall mods/string mods)))
             (insert "\t")
             (insert-text-button "Describe"
                                 'follow-link t
                                 'action
                                 (lambda (_btn)
                                   (racket--do-describe
                                    how nil "" t
                                    nil
                                    (lambda ()
                                      (racket-browse-url
                                       (concat "file://" (car how) "#" (cdr how))))))
                                 'racket-xp-doc
                                 how)
             (newline))))
      (read-only-mode))
    (display-buffer (current-buffer) t)
    (pop-to-buffer (current-buffer))
    (current-buffer)))

(define-derived-mode racket-search-mode special-mode
  "RacketDescribe"
  "Major mode for searching Racket functions.
\\{racket-describe-mode-map}"
  (setq show-trailing-whitespace nil))

(defun racket-search (expr)
  "Do a search using the provided EXPR in a `*Racket Search*` buffer.

The buffer includes links which opens a buffer using `racket-xp-describe'. "
  (interactive "M")
  (racket--do-search
   (racket--repl-session-id)
   expr
   10))

(provide 'racket-search)
