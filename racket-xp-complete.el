;;; racket-xp-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'seq)
(require 'racket-complete)
(require 'racket-describe)
(require 'racket-scribble-anchor)

(defvar-local racket--xp-completion-table-all nil
  "A completion table of all bindings; for use by a CAPF.

Includes both imports and lexical bindings. Better for use by
`completion-at-point' in an edit buffer, because in general more
completion candidates offer more opportunities to minimize
typing.

The table includes category and affixation-function metadata; the
latter shows the module from which an identifier was imported,
when not a lexical binding.")

(defvar-local racket--xp-completion-table-imports nil
  "A completion table of import bindings; for use in minibuffer.

Includes only imports, not lexical bindings. Definitely better
for use by commands that look up documentation. Sometimes better
for use by `completing-read' in the minibuffer, because that
returns strings stripped of all text properties -- unless a
command is able to find a suitable matching string in the buffer
and use its text properties.

The table includes category and affixation-function metadata.")

(defun racket--set-xp-binding-completions (mods+syms)
  ;; The back end gives us data optimized for space when serializing:
  ;;
  ;;  ((modA symA0 symA1 ...)
  ;;   (modB symB0 symB1 ...) ...)
  ;;
  ;; Reshape that to a list of strings, each propertized with its mod,
  ;; for use as completion table.
  (let* ((all nil)
         (imports nil)
         (affixator (racket--make-affix [16 0]))
         (metadata `((category . ,racket--identifier-category)
                     (affixation-function . ,affixator))))
    (dolist (mod+syms mods+syms)
      (pcase-let ((`(,mod . ,syms) mod+syms))
        (dolist (sym syms)
          (push (propertize sym 'racket-affix (list mod)) all)
          (when mod
            (push (propertize sym 'racket-affix (list mod)) imports)))))
    (setq racket--xp-completion-table-all
          (racket--completion-table all metadata))
    (setq racket--xp-completion-table-imports
          (racket--completion-table imports metadata))))

(defvar-local racket--xp-module-completions nil
  "A completion table for available collection module paths.
Do not `setq' directly; instead call `racket--xp-set-module-completions'.")

(defun racket--set-xp-module-completions (completions)
  (setq-local racket--xp-module-completions
              (racket--completion-table completions
                                        `((category . ,racket--module-category)))))

(defun racket-xp-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

- Within a textually apparent \"require\" form, when completing:

  - A symbol immediately after an opening paren: Candidates are
    names of require transformers.

  - Another symbol: Candidates are absolute module paths like
    \"racket/path\".

  - Anything `thing-at-point' thinks is a filename: Candidates
    are from `completion-file-name-table'.

- Otherwise, when completing a symbol: Candidates are bindings as
  found by drracket/check-syntax plus our own back end analysis
  of imported bindings."
  (if (racket--in-require-form-p)
      (or (racket--call-with-completion-prefix-positions
           (lambda (beg end)
             (if (eq ?\( (char-syntax (char-before beg)))
                 (racket--xp-capf-require-transformers beg end)
               (racket--xp-capf-absolute-module-paths beg end))))
          (racket--xp-capf-relative-module-paths))
    (racket--call-with-completion-prefix-positions
     #'racket--xp-capf-bindings)))

(defun racket--xp-capf-bindings (beg end)
  (list beg
        end
        racket--xp-completion-table-all
        ;; ^table metadata already has :affixation-function
        :exclusive           'no
        :company-location    (racket--xp-make-company-location-proc)
        :company-doc-buffer  (racket--xp-make-company-doc-buffer-proc)))

(defun racket--xp-capf-require-transformers (beg end)
  "Note: Currently this returns too many candidates -- all
available bindings, not just those that are require transformers.
Although not ideal, I think it's less-worse than having some
hardwired list of require transformers. In general with
completion candidates, if you have to err, better to err on the
side of too many not too few. Having said that, someday maybe our
back end could give us the exact subset of available bindings
that are require transformers."
  (racket--xp-capf-bindings beg end))

(defun racket--xp-capf-absolute-module-paths (beg end)
  (list beg
        end
        racket--xp-module-completions
        :exclusive 'no))

(defun racket--xp-capf-relative-module-paths ()
  (when-let (bounds (bounds-of-thing-at-point 'filename))
    (list (car bounds)
          (cdr bounds)
          #'completion-file-name-table
          :exclusive 'no)))

(defun racket--xp-make-company-location-proc ()
  (when (racket--cmd-ready-p)
    (let ((how (racket-how-front-to-back (buffer-file-name))))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (pcase (racket--cmd/await nil `(def ,how ,str))
            (`(,path ,line ,_)
             (cons (racket-file-name-back-to-front path) line))))))))

(defun racket--xp-make-company-doc-buffer-proc ()
  (when (racket--cmd-ready-p)
    (let ((how (racket-how-front-to-back (buffer-file-name))))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (racket--company-doc-buffer how str))))))

(provide 'racket-xp-complete)

;; racket-xp-complete.el ends here
