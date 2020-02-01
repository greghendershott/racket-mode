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

(require 'cl-lib)
(require 'ido)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-keywords-and-builtins)
(require 'racket-describe)
(require 'racket-visit)
(require 'racket-util)

;; TODO:
;;
;; More work making racket-mode into a "plain edit mode" that works
;; without a REPL. People enable the racket-check-syntax-mode (better
;; name like "racket-ide-mode"?) for that stuff.
;;
;; 1. Make racket-mode completion only use the font-lock symbols list
;; not the command server. If people want more accurate completion,
;; they need to use racket-check-syntax-mode, which can do this
;; without needing to run the file.
;;
;; 2. racket-repl-mode should have its own completion function that
;; does use `(syms)` command for live namespace symbols.
;;
;; 3. Of course move the code that determines the "prefix" in the
;; buffer, to some common shared file. (racket-check-sytnax-mode
;; already has a copypasta of this. We don't need/want three
;; instances.)
;;
;; 4. Similar story for visit-definition. racket-mode shouldn't have
;; this command at all. Instead racket-check-syntax-mode supplies it.
;; And, racket-repl-mode may provide this in the old status quo way
;; (i.e. effectively move the code from here to racket-repl.el).
;;
;; 5. The CAPF :company-location thing? I don't have a great story for
;; that in a "plain" racket-mode. :( Even with
;; racket-check-syntax-mode, we don't supply that information -- we
;; don't proactively fetch source location for all imported symbols!
;; Way too slow. Is there some clever hack where we, idk, insert that
;; symbol in a copy of the current buffer, run check-syntax again, and
;; use that annotation? Again the idea is that we don't need to _run_
;; the file, just expand it. Maybe we can expose _that_ expansion-only
;; flavor in the server, not even needing to use
;; drracket/check-syntax.
;;
;; 6. Ditto eldoc-mode. Of course they can still work in the status
;; quo way for racket-repl-mode.
;;
;; 7. racket-describe: This is a mix. The "visit" and "doc" links can
;; be supplied instead by racket-check-syntax-mode annotations. I'm
;; not sure what to do about the stuff where it uses the ,type command
;; because no Scribble doc available. And again, no problem in
;; racket-repl-mode.
;;
;; 8. Consider renaming racket-check-syntax-mode to something like
;; racket-ide-mode. Then make racket-mode work 100% without any REPL
;; -- features people can use if they only want to edit, and will run
;; their code some other way. Any other functionality is supplied by
;; either racket-ide-mode (only needs the server, not the file to
;; racket-run) or racket-repl-mode. Both of these minor modes can
;; override keys, and add things like completion-at-point-functions,
;; to enhance the simpler racket-mode.

;;; namespace symbols i.e. completion candidates

(defvar-local racket--namespace-symbols (list racket-type-list
                                              racket-keywords
                                              racket-builtins-1-of-2
                                              racket-builtins-2-of-2)
  "A list of list of Racket namespace symbols suitable for completion candidates.

This var is local to each buffer, including the REPL buffer.

Defaults to the `defconst' lists of strings we use for font-lock.
To support this case -- while avoiding `concat' and allocation of
such large lists of strings -- is why it is a list of list of
strings.")

(defun racket--refresh-namespace-symbols ()
  "Both current `racket-mode' buffer and `racket-repl-mode' buffer (if any)."
  (racket--cmd/async
   '(syms)
   (lambda (syms)
     (setq racket--namespace-symbols (list syms))
     (with-racket-repl-buffer
       (setq racket--namespace-symbols (list syms))))))

(add-hook 'racket--repl-after-run-hook #'racket--refresh-namespace-symbols)

(defun racket--completion-candidates-for-prefix (prefix)
  (cl-reduce (lambda (results strs)
               (append results (all-completions prefix strs)))
             racket--namespace-symbols
             :initial-value ()))

(defun racket--call-with-completion-prefix-positions (proc)
  (let ((beg (save-excursion (skip-syntax-backward "^-()>") (point))))
    (unless (or (eq beg (point-max))
                (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
      (condition-case nil
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (let ((end (point)))
              (and
               (<= (+ beg 2) end) ;prefix at least 2 chars
               (funcall proc beg end))))
        (scan-error nil)))))

(defun racket-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

Completion candidates are drawn from the namespace symbols
resulting from the most recent `racket-run' of each .rkt file. If
a file has never been run, candidates default to values also used
for font-lock -- an assortment of symbols from common Racket
modules such as `racket`, `typed/racket`, and `syntax/parse`.

Returns extra :company-doc-buffer and :company-location
properties for use by the `company-mode' backend `company-capf'
-- but not :company-docsig, because it is frequently impossible
to supply this quickly enough or at all."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (completion-table-dynamic
            #'racket--completion-candidates-for-prefix)
           :predicate #'identity
           :exclusive 'no
           ;; racket--get-type is too slow for :company-docsig
           :company-doc-buffer #'racket--company-doc-buffer
           :company-location #'racket--company-location))))

(defun racket--company-doc-buffer (str)
  (racket--do-describe 'namespace str))

(defun racket--company-location (str)
  (pcase (racket--cmd/await `(def-in-namespace ,str))
    (`(,path ,line ,_) (cons path line))))

;;; "types" (i.e. TR types, contracts, and/or function signatures)

(defvar-local racket--type-cache (make-hash-table :test #'eq)
  "Memoize \",type\" commands in Racket REPL.

This var is local to each buffer, including the REPL buffer.

`racket-run' should call `racket-invalidate-type-cache'.")

(defun racket--invalidate-type-cache ()
  "Both current `racket-mode' buffer and `racket-repl-mode' buffer (if any)."
  (setq racket--type-cache (make-hash-table :test #'eq))
  (with-racket-repl-buffer
    (setq racket--type-cache (make-hash-table :test #'eq))))

(add-hook 'racket--repl-before-run-hook #'racket--invalidate-type-cache)

(defun racket--get-type (str)
  (let* ((sym (intern str))
         (v (gethash sym racket--type-cache)))
    (or v
        (and (racket--in-repl-or-its-file-p)
             (pcase (racket--cmd/await `(type ,sym))
               (`() `())
               (v   (puthash sym v racket--type-cache)
                    v))))))

;;; eldoc

(defun racket-eldoc-function ()
  "A value suitable for the variable `eldoc-documentation-function'.

By default Racket Mode sets `eldoc-documentation-function' to nil
-- no `eldoc-mode' support. You may set it to this function in a
`racket-mode-hook' if you really want to use `eldoc-mode' with
Racket. But it is not a very satisfying experience because Racket
is not a very \"eldoc friendly\" language. Although Racket Mode
attempts to discover argument lists, contracts, or types this
doesn't work in many common cases:

- Many Racket functions are defined in #%kernel. There's no easy
  way to determine their argument lists. Most are not provided
  with a contract.

- Many of the interesting Racket forms are syntax (macros) not
  functions. There's no easy way to determine their \"argument
  lists\".

A more satisfying experience is to use `racket-describe' or
`racket-doc'."
  (and (> (point) (point-min))
       (save-excursion
         (condition-case nil
             ;; The char-before and looking-at checks below are to
             ;; avoid calling `racket--get-type' when the sexp is
             ;; quoted or when its first elem couldn't be a Racket
             ;; function name.
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
                    (str (and sym (racket--get-type sym))))
               str)
           (scan-error nil)))))

(provide 'racket-complete)

;; racket-complete.el ends here
