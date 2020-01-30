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
(require 'racket-util)
(require 'shr)
(declare-function racket--do-visit-def-or-mod "racket-edit.el")

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
           :company-location #'racket--get-def-file+line))))

(defun racket--company-doc-buffer (str &optional display-and-pop-to-p)
  (racket--do-describe 'namespace str display-and-pop-to-p))

(defun racket--get-def-file+line (sym)
  "Return a value suitable for use as :company-location."
  (pcase (racket--cmd/await `(def-in-namespace ,sym))
    (`(,path ,line ,_) (cons path line))
    (_ nil)))

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

;;; at-point

(defun racket--symbol-at-point-or-prompt (force-prompt-p prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses `read-from-minibuffer'. Returns `stringp' not
`symbolp' to simplify using the result in a sexpr that can be
passed to Racket backend. Likewise text properties are stripped."
  (let ((sap (racket--thing-at-point 'symbol t)))
    (if (or force-prompt-p (not sap))
        (let ((s (read-from-minibuffer prompt sap)))
          (if (equal "" (racket--trim s))
              nil
            s))
      sap)))

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

;;; describe

(defun racket-describe (&optional prefix)
"Describe the identifier at point in a `*Racket Describe*` buffer.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser.

This buffer is also displayed when you use `company-mode' and
press F1 or C-h in its pop up completion list.

- If the identifier has installed Racket documentation, then a
  simplified version of the HTML is presented in the buffer,
  including the \"blue box\", documentation prose, and examples.

- Otherwise, if the identifier is a function, then its signature
  is displayed, for example `(name arg-1-name arg-2-name)`. If it
  has a contract or a Typed Racket type, that is also displayed.

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: ")
    (`nil nil)
    (str (racket--do-describe 'namespace str t))))

(defun racket--do-describe (how str &optional display-and-pop-to-p)
  "A helper used by both `racket-describe' and `company-mode'.

DISPLAY-AND-POP-TO-P should be t for use by `racket-describe' --
in which case some buttons are added and the buffer is displayed
-- and nil for use by `company-mode'.

Returns the buffer in which the description was written."
  ;; Work around what seems to be a bug with `shr-insert-document' --
  ;; elements are out of order when an existing Racket Describe buffer
  ;; hasn't had a `quit-window' -- by re-creating the buffer.
  (with-current-buffer (racket--get-buffer-recreate "*Racket Describe*")
    (let* ((html (racket--cmd/await `(describe ,how ,str)))
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
        (insert-text-button "Definition"
                            'follow-link t
                            'action
                            (lambda (_btn)
                              (racket--do-visit-def-or-mod `(def ,how ,str))))
        (insert "   ")
        (insert-text-button "Documentation in Browser"
                            'follow-link t
                            'action
                            (lambda (_btn)
                              (racket--cmd/await `(doc ,str))))
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


(provide 'racket-complete)

;; racket-complete.el ends here
