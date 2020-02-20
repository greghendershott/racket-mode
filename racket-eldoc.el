;;; racket-eldoc.el -*- lexical-binding: t -*-

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

(defun racket--do-eldoc (how repl-session-id)
  (and (racket--cmd-open-p)
       (> (point) (point-min))
       (save-excursion
         (condition-case nil
             ;; The char-before and looking-at checks below are to
             ;; skip when the sexp is quoted or when its first elem
             ;; couldn't be a Racket function name.
             (let* ((beg (progn
                           (backward-up-list)
                           (and (not (memq (char-before) '(?` ?' ?,)))
                                (progn (forward-char 1) (point)))))
                    (beg (and beg (looking-at "[^0-9#'`,\"]") beg))
                    (end (and beg (progn (forward-sexp) (point))))
                    (end (and end
                              (char-after (point))
                              (eq ?\s (char-syntax (char-after (point))))
                              end))
                    (sym (and beg end (buffer-substring-no-properties beg end)))
                    (str (and sym (racket--cmd/await repl-session-id
                                                     `(type ,how ,sym)))))
               str)
           (scan-error nil)))))

(provide 'racket-eldoc)

;; racket-eldoc.el ends here
