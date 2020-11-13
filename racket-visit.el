;;; racket-visit.el -*- lexical-binding: t -*-

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

(require 'simple)
(require 'xref)
(require 'racket-complete)

(defun racket--module-path-name-at-point ()
  "Treat point as a Racket module path name, possibly in a multi-in form.

The returned string has text properties:

- A 'racket-module-path property whose value is either 'absolute
  or 'relative.

- The original properties from the buffer. However if a multi-in
  form, these are only the properties from the suffix, e.g. the
  \"base\" in \"(multi-in racket (base))\", and they are only
  applied only to that portion of the returned string, e.g. the
  \"base\" portion of \"racket/base\".

- Regardless of the preceding point, the original 'racket-xp-def
  property if any from the buffer is applied to the ENTIRE
  returned string. That way the caller can simply use an index of
  0 for `get-text-property'."
  (when (racket--in-require-form-p)
    (save-excursion
      (condition-case ()
          (progn
            (forward-sexp 1)
            (backward-sexp 1)
            (when (eq ?\" (char-syntax (char-before)))
              (backward-char))
            (let ((str (thing-at-point 'sexp)))
              (pcase (read str)
                ((and (pred identity) sexp)
                 (let* ((relative-p (stringp sexp))
                        (multi-in-prefix
                         (condition-case ()
                             (progn
                               (backward-up-list 1)
                               (backward-sexp 2)
                               (when (looking-at-p "multi-in")
                                 (forward-sexp 2)
                                 (backward-sexp 1)
                                 (when (eq ?\" (char-syntax (char-before)))
                                   (backward-char))
                                 (let* ((v (read (thing-at-point 'sexp t))))
                                   (unless (equal relative-p (stringp v))
                                     (user-error "multi-in mixes absolute and relative paths"))
                                   (format "%s/" v))))
                           (scan-error nil))))
                   (propertize (concat multi-in-prefix str)
                               'racket-module-path
                               (if relative-p 'relative 'absolute)
                               'racket-xp-def
                               (get-text-property 0 'racket-xp-def str)))))))
        (scan-error nil)))))

(defun racket--pop-to-xref-location (item)
  "Similar to the private function `xref--pop-to-location'.

But not using that, and not using other private functions in its
implementation."
  (xref-push-marker-stack)
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    (switch-to-buffer buf)
    ;; Like (`xref--goto-char' marker)
    (unless (and (<= (point-min) marker) (<= marker (point-max)))
      (if widen-automatically
          (widen)
        (user-error "Position is outside accessible part of buffer")))
    (goto-char marker)))

(define-obsolete-function-alias 'racket-visit-module
  'xref-find-definitions  "2020-11-10")
(define-obsolete-function-alias 'racket-visit-definition
  'xref-find-definitions "2020-11-10")
(define-obsolete-function-alias 'racket-xp-visit-definition
  'xref-find-definitions  "2020-11-10")
(define-obsolete-function-alias 'racket-repl-visit-definition
  'xref-find-definitions  "2020-11-10")
(define-obsolete-function-alias 'racket-unvisit
  'xref-pop-marker-stack "2020-11-10")

(provide 'racket-visit)

;; racket-visit.el ends here
