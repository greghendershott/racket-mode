;;; racket-eldoc.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-cmd)
(require 'racket-back-end)

(defun racket--do-eldoc (how repl-session-id)
  (and (racket--cmd-open-p)
       (> (point) (point-min))
       (save-excursion
         (condition-case _
             ;; The char-before and looking-at-p checks below are to
             ;; skip when the sexp is quoted or when its first elem
             ;; couldn't be a Racket function name.
             (let* ((beg (progn
                           (backward-up-list)
                           (and (not (memq (char-before) '(?` ?' ?,)))
                                (progn (forward-char 1) (point)))))
                    (beg (and beg (looking-at-p "[^0-9#'`,\"]") beg))
                    (end (and beg (progn (forward-sexp) (point))))
                    (end (and end
                              (char-after (point))
                              (eq ?\s (char-syntax (char-after (point))))
                              end))
                    (sym (and beg end (buffer-substring-no-properties beg end)))
                    (how (racket-how-front-to-back how))
                    (str (and sym (racket--cmd/await repl-session-id
                                                     `(type ,how ,sym)))))
               str)
           (scan-error nil)))))

(provide 'racket-eldoc)

;; racket-eldoc.el ends here
