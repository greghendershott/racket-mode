;;; racket-check-syntax.el -*- lexical-binding: t -*-

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

;;; The general approach here:
;;;
;;; - Add 'help-echo and 'point-{entered left} text properties with
;;;   information supplied by the drracket/check-syntax module.
;;;
;;; - Refresh in an after-change hook, plus an idle-timer delay.
;;;
;;; - Point motion hooks add/remove temporarily overlays to highlight
;;;   defs and uses. (We can't draw GUI arrows as in Dr Racket.)
;;;
;;; - Use a header line so our information isn't "fighting" with other
;;;   things using the echo area.

(require 'racket-custom)
(require 'racket-repl)
(require 'racket-edit)
(require 'racket-util)
(require 'racket-show)
(require 'rx)
(require 'pos-tip)

(declare-function racket--do-visit-def-or-mod "racket-edit.el")

;; TODO: Expose as a defcustom? Or even as commands to turn on/off?
;; Also note there are really 3 categories here: 'local 'import
;; 'module-lang, so could be more granularity.
(defvar racket-check-syntax-highlight-imports-p nil
  "Highlight imported definitions and uses thereof? When nil,
only local defs/uses are highlighted. When t, all are highlighted
-- similar to how DrRacket draws arrows for everything. If you
find that too \"nosiy\", set this to nil.")

(defvar racket-check-syntax-control-c-hash-keymap
  (racket--easy-keymap-define
   '(("j" racket-check-syntax-next-definition)
     ("k" racket-check-syntax-previous-definition)
     ("n" racket-check-syntax-next-use)
     ("p" racket-check-syntax-previous-use)
     ("." racket-check-syntax-visit-definition)
     ("r" racket-check-syntax-rename))) )

;;;###autoload
(define-minor-mode racket-check-syntax-mode
  "Use drracket/check-syntax to annotate and to enhance completion and visit definition.

This minor mode is an optional enhancement to `racket-mode' edit
buffers. Like any minor mode, you can turn it on or off for a
specific buffer. If you always want to use it, put the following
code in your Emacs init file:

#+BEGIN_SRC elisp
    (require 'racket-check-syntax)
    (add-hook 'racket-mode-hook #'racket-check-syntax-mode)
#+END_SRC

Note: This mode won't do anything unless/until the Racket Mode
back end is running. It will try to start the back end
automatically. You do /not/ need to `racket-run' the buffer you
are editing.

When point is on a definition or use, related items are
highlighted using `racket-check-syntax-def-face' and
`racket-check-syntax-use-face' -- instead of drawing arrows as in
Dr Racket -- and \"mouse over\". Information is displayed using
the function(s) in the hook variable `racket-show-functions'; it
is also available when hovering the mouse cursor.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses.

\"M-.\" is rebound from `racket-visit-definition' -- which only
works when a buffer has been `racket-run' -- to
`racket-check-syntax-visit-definition'. This uses information
provided by check-syntax for local and imported definitions.

Similarly, `racket-check-syntax-complete-at-point' is added to
the variable `completion-at-point-functions'. This works even if
a buffer has not been `racket-run'. In addition to supply
completion candidates, it supports the :company-location property
to inspect the definition of a candidate. Support
for :company-doc-buffer is not yet implemented.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-check-syntax-after-change-refresh-delay' seconds. The
request is made asynchronously so that Emacs will not block --
the drracket/check-syntax analysis can take even tens of seconds
for even moderately complex source files. When the results are
ready, all annotations for the buffer are completely refreshed.

Tip: This follows the convention that a minor mode may only use a
prefix key consisting of C-c followed by a punctuation key. As a
result, `racket-check-syntax-control-c-hash-keymap' is bound to
\"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-check-syntax-mode-map}
"
  :lighter racket-check-syntax-mode-lighter
  :keymap (racket--easy-keymap-define
           `(("C-c #"   ,racket-check-syntax-control-c-hash-keymap)
             ("M-."     ,#'racket-check-syntax-visit-definition)
             ("C-c C-." ,#'racket-check-syntax-describe)
             ("C-c C-d" ,#'racket-check-syntax-documentation)))
  (unless (eq major-mode 'racket-mode)
    (setq racket-check-syntax-mode nil)
    (user-error "racket-check-syntax-mode only works with racket-mode buffers"))
  (cond (racket-check-syntax-mode
         (racket--check-syntax-annotate)
         (add-hook 'after-change-functions
                   #'racket--check-syntax-after-change-hook
                   t t)
         (add-hook 'completion-at-point-functions
                   #'racket-check-syntax-complete-at-point
                   nil t)
         (when (fboundp 'cursor-sensor-mode)
           (cursor-sensor-mode 1)))
        (t
         (racket-show nil)
         (racket--check-syntax-clear)
         (remove-hook 'after-change-functions
                      #'racket--check-syntax-after-change-hook
                      t)
         (remove-hook 'completion-at-point-functions
                      #'racket-check-syntax-complete-at-point
                      t)
         (when (fboundp 'cursor-sensor-mode)
           (cursor-sensor-mode 0)))))

(defvar-local racket--check-syntax-completions nil)

(defun racket-check-syntax-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

`racket-check-syntax-mode' adds this in addition to the one set
by `racket-mode'."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (completion-table-dynamic
            (lambda (prefix)
              (all-completions prefix racket--check-syntax-completions)))
           :predicate          #'identity
           :exclusive          'no
           :company-location   (racket--check-syntax-make-company-location-proc)
           :company-doc-buffer (racket--check-syntax-make-company-doc-buffer-proc)))))

(defun racket--check-syntax-make-company-location-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (pcase (racket--cmd/await `(def ,how ,str))
            (`(,path ,line ,_) (cons path line))))))))

(defun racket--check-syntax-make-company-doc-buffer-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (racket--do-describe how str))))))

(defun racket-check-syntax-describe (&optional prefix)
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
  is displayed, for example `(name arg-1-name arg-2-name)`..

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: ")
    (`nil nil)
    ;; TODO: Although this uses the path-str not 'namespace variant of
    ;; describe, which is works for file-module-level identifiers, if
    ;; there is a 'racket-check-syntax-doc property here, it would be
    ;; better to pass that to the describe command. That will be
    ;; correct even for identifiers from submodules that have
    ;; different module-lang or require imports than does the file
    ;; module. In other words, in that case we don't need the back-end
    ;; to find the correct help for us, we only need it to
    ;; extract/massage the Scribble HTML for us. This is similar in
    ;; spirit to 'racket-check-syntax-visit props: They are kind of
    ;; like a lazy thunk with the correct information about the file
    ;; containing a definition and some possible names, and we use the
    ;; back-end only to dive into the file and find the location --
    ;; not to identify the file. In both cases, a simple example of
    ;; the issue is "does 'define' come from racket/base or
    ;; typed/racket/base -- which 'define'?" and the check-syntax
    ;; analys is, when available, always the best answer than
    ;; something assuming the file-module-level imports.
    (str (racket--do-describe (buffer-file-name) str t))))

(defconst racket--check-syntax-overlay-name 'racket-check-syntax-overlay)

(defun racket--check-syntax-overlay-p (o)
  (eq (overlay-get o 'name)
      racket--check-syntax-overlay-name))

(defun racket--highlight (beg end face)
  (unless (cl-some #'racket--check-syntax-overlay-p
                   (overlays-in beg end))
    (let ((o (make-overlay beg end)))
      (overlay-put o 'name racket--check-syntax-overlay-name)
      (overlay-put o 'priority 0) ;below other overlays e.g. isearch
      (overlay-put o 'face face))))

(defun racket--unhighlight (beg end)
  (remove-overlays beg end 'name racket--check-syntax-overlay-name))

(defun racket--unhighlight-all ()
  (racket--unhighlight (point-min) (point-max)))

(defun racket--check-syntax-cursor-sensor (window old dir)
  (let ((new (window-point window)))
    (cl-case dir
      ('entered
       (pcase (get-text-property new 'help-echo)
         ((and s (pred racket--non-empty-string-p))
          (let ((end (next-single-property-change new 'help-echo)))
            (racket-show s end))))
       (pcase (get-text-property new 'racket-check-syntax-def)
         (`(,kind ,_id ,(and uses `((,beg ,_end) . ,_)))
          (when (or (eq kind 'local)
                    racket-check-syntax-highlight-imports-p)
            (pcase (get-text-property beg 'racket-check-syntax-use)
              (`(,beg ,end)
               (racket--highlight beg end racket-check-syntax-def-face)))
            (dolist (use uses)
              (pcase use
                (`(,beg ,end)
                 (racket--highlight beg end racket-check-syntax-use-face)))))))
       (pcase (get-text-property new 'racket-check-syntax-use)
         (`(,def-beg ,def-end)
          (pcase (get-text-property def-beg 'racket-check-syntax-def)
            (`(,kind ,_id ,uses)
             (when (or (eq kind 'local)
                       racket-check-syntax-highlight-imports-p)
               (racket--highlight def-beg def-end racket-check-syntax-def-face)
               (dolist (use uses)
                 (pcase use
                   (`(,beg ,end)
                    (racket--highlight beg end racket-check-syntax-use-face))))))))))
      ('left
       (when (get-text-property old 'help-echo)
         (racket-show ""))
       (racket--unhighlight-all)))))

(defun racket-check-syntax-visit-definition (&optional prefix)
  "When point is on a use, go to its definition.

With a prefix, prompts you, but beware this only knows about
definitions used in the main module, not submodules."
  (interactive)
  (unless (pcase (get-text-property (point) 'racket-check-syntax-use)
            (`(,beg ,_end)
             (pcase (get-text-property beg 'racket-check-syntax-def)
               (`(local ,_id ,_uses)
                (racket--push-loc)
                (goto-char beg)
                t))))
    (if prefix
        (pcase (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")
          (`nil nil)
          (str (racket--do-visit-def-or-mod `(def ,(buffer-file-name) ,str))))
      (pcase (get-text-property (point) 'racket-check-syntax-visit)
        (`(,path ,subs ,ids)
         (racket--cmd/async
          `(def/dr-jump ,path ,subs, ids)
          (lambda (results)
            (pcase results
              (`(,path ,line ,col)
               (racket--push-loc)
               (find-file (funcall racket-path-from-racket-to-emacs-function path))
               (goto-char (point-min))
               (forward-line (1- line))
               (forward-char col))))))
        (_
         (pcase (get-text-property (point) 'racket-check-syntax-def)
           (`(import ,id . ,_)
            ;; TODO: TEST ME
            (racket--do-visit-def-or-mod `(mod ,id)))))))))

(defun racket-check-syntax-documentation ()
  "Show help found by check-syntax, if any, else `racket-doc'."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-doc)
    (`(,path ,anchor)
     (browse-url (concat "file://" path "#" anchor)))
    (_
     (racket-doc))))

(defun racket-check-syntax--forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,_end)
     (pcase (get-text-property beg 'racket-check-syntax-def)
       (`(,_kind ,_id ,uses)
        (let* ((pt (point))
               (ix-this (cl-loop for ix from 0 to (1- (length uses))
                                 for use = (nth ix uses)
                                 when (and (<= (car use) pt) (< pt (cadr use)))
                                 return ix))
               (ix-next (+ ix-this amt))
               (ix-next (if (> amt 0)
                            (if (>= ix-next (length uses)) 0 ix-next)
                          (if (< ix-next 0) (1- (length uses)) ix-next)))
               (next (nth ix-next uses)))
          (goto-char (car next))))))
    (_ (pcase (get-text-property (point) 'racket-check-syntax-def)
         (`(,_kind ,_id ((,beg ,_end) . ,_)) (goto-char beg))))))

(defun racket-check-syntax-next-use ()
  "When point is on a use, go to the next, sibling use."
  (interactive)
  (racket-check-syntax--forward-use 1))

(defun racket-check-syntax-previous-use ()
  "When point is on a use, go to the previous, sibling use."
  (interactive)
  (racket-check-syntax--forward-use -1))

(defun racket-check-syntax-rename ()
  "Rename a local definition and its uses in the current file."
  (interactive)
  (pcase-let*
      (;; Try to get a def prop and a use prop at point
       (def-prop     (get-text-property (point) 'racket-check-syntax-def))
       (uses-prop    (get-text-property (point) 'racket-check-syntax-use))
       (_            (unless (or uses-prop def-prop)
                       (user-error "Not a definition or use")))
       ;; OK, we have one of the props. Use it to get the the other one.
       (uses-prop    (or uses-prop
                         (pcase-let ((`(,_kind ,_id ((,beg ,_end) . ,_)) def-prop))
                           (get-text-property beg 'racket-check-syntax-use))))
       (def-prop     (or def-prop
                         (pcase-let ((`(,beg ,_end) uses-prop))
                           (get-text-property beg 'racket-check-syntax-def))))
       (`(,kind ,old-id ,uses-locs)  def-prop)
       (_            (unless (eq kind 'local)
                       (user-error "Can only rename local definitions, not imports")))
       (def-loc      uses-prop)
       (locs         (cons def-loc uses-locs))
       (new-id       (read-from-minibuffer (format "Rename %s to: " old-id)))
       (marker-pairs (mapcar (lambda (loc)
                               (let ((beg (make-marker))
                                     (end (make-marker)))
                                 (set-marker beg (nth 0 loc) (current-buffer))
                                 (set-marker end (nth 1 loc) (current-buffer))
                                 (list beg end)))
                             locs))
       (point-marker (let ((m (make-marker)))
                       (set-marker m (point) (current-buffer)))))
    ;; Don't let our after-change hook run until all changes are
    ;; made, otherwise check-syntax will find a syntax error.
    (let ((inhibit-modification-hooks t))
      (dolist (marker-pair marker-pairs)
        (let ((beg (marker-position (nth 0 marker-pair)))
              (end (marker-position (nth 1 marker-pair))))
          (delete-region beg end)
          (goto-char beg)
          (insert new-id))))
    (goto-char (marker-position point-marker))
    (racket--check-syntax-annotate
     (lambda () ;nudge the cursor-sense stuff to refresh
       (racket--check-syntax-cursor-sensor (selected-window)
                                           (point-min)
                                           'entered)))))

(defun racket-check-syntax-next-definition ()
  "Move point to the next definition."
  (interactive)
  (let ((pos (next-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (next-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(defun racket-check-syntax-previous-definition ()
  "Move point to the previous definition."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (previous-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(defvar-local racket--check-syntax-timer nil)

(defun racket--check-syntax-after-change-hook (_beg _end _len)
  (when (timerp racket--check-syntax-timer)
    (cancel-timer racket--check-syntax-timer))
  (racket--check-syntax-set-status 'outdated)
  (setq racket--check-syntax-timer
        (run-with-idle-timer racket-check-syntax-after-change-refresh-delay
                             nil ;no repeat
                             (racket--restoring-current-buffer
                              #'racket--check-syntax-annotate))))

(defun racket--check-syntax-annotate (&optional after-thunk)
  (racket--check-syntax-set-status 'running)
  (racket--cmd/async
   `(check-syntax ,(or (racket--buffer-file-name) (buffer-name))
                  ,(buffer-substring-no-properties (point-min) (point-max)))
   (racket--restoring-current-buffer
    (lambda (response)
      (racket-show "")
      (pcase response
        (`(check-syntax-ok
           (completions . ,completions)
           (annotations . ,annotations))
         (racket--check-syntax-clear)
         (setq racket--check-syntax-completions completions)
         (racket--check-syntax-insert annotations)
         (racket--check-syntax-set-status 'ok)
         (when (and annotations after-thunk)
           (funcall after-thunk)))
        (`(check-syntax-errors . ,xs)
         (racket--check-syntax-insert xs)
         (racket--check-syntax-set-status 'err)))))))

(defun racket--check-syntax-insert (xs)
  "Insert text properties. Convert integer positions to markers."
  (with-silent-modifications
    (overlay-recenter (point-max)) ;faster
    (dolist (x xs)
      (pcase x
        (`(error ,path ,beg ,end ,str)
         (when (equal path (racket--buffer-file-name))
           (let ((beg (copy-marker beg t))
                 (end (copy-marker end t)))
             (remove-text-properties
              beg end
              (list 'help-echo               nil
                    'racket-check-syntax-def nil
                    'racket-check-syntax-use nil
                    'cursor-sensor-functions nil))
             (add-text-properties
              beg end
              (list 'face                    racket-check-syntax-error-face
                    'help-echo               str
                    'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor)))
             ;; Show now using echo area, only. (Not tooltip because
             ;; error loc might not be at point. Not header-line
             ;; because unlikely to show whole error message in one
             ;; line.)
             (message "%s" str))))
        (`(info ,beg ,end ,str)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'help-echo               str
                  'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor)))))
        (`(def/uses ,def-beg ,def-end ,req ,id ,uses)
         (let ((def-beg (copy-marker def-beg t))
               (def-end (copy-marker def-end t))
               (uses    (mapcar (lambda (xs)
                                  (mapcar (lambda (x)
                                            (copy-marker x t))
                                          xs))
                                uses)))
           (add-text-properties
            def-beg def-end
            (list 'racket-check-syntax-def (list req id uses)
                  'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor)))
          (dolist (use uses)
            (pcase-let* ((`(,use-beg ,use-end) use))
              (add-text-properties
               use-beg use-end
               (append
                (list 'racket-check-syntax-use (list def-beg def-end)
                      'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor))
                (when (eq req 'local)
                  (list 'help-echo "Defined locally"))))))))
        (`(external-def ,beg ,end ,path ,subs ,ids)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'racket-check-syntax-visit (list path subs ids)))))
        (`(doc ,beg ,end ,path ,anchor)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'racket-check-syntax-doc (list path anchor)))))))))

(defun racket--check-syntax-clear ()
  (with-silent-modifications
    (setq racket--check-syntax-completions nil)
    (remove-text-properties
     (point-min) (point-max)
     (list 'help-echo                 nil
           'racket-check-syntax-def   nil
           'racket-check-syntax-use   nil
           'racket-check-syntax-visit nil
           'racket-check-syntax-doc   nil
           'cursor-sensor-functions   nil))
    ;; TODO: Remove 'face properties that have our special values, only
    (racket--unhighlight-all)))

;;; Mode line status

(defvar-local racket--check-syntax-mode-status nil)

(defun racket--check-syntax-set-status (&optional which)
  (setq racket--check-syntax-mode-status which)
  (force-mode-line-update))

(defcustom racket-check-syntax-mode-lighter
  '(:eval (racket--check-syntax-mode-lighter))
  "Mode line lighter for `racket-check-syntax-mode'.
Set to nil to disable the mode line completely."
  :group 'racket-mode
  :risky t
  :type 'sexp)

(defun racket--check-syntax-mode-lighter ()
  (let ((prefix "RktChk"))
    (pcase-let ((`(,suffix . ,face)
                 (cl-case racket--check-syntax-mode-status
                   ((ok)       '("✓" . '(:inherit compilation-info)))
                   ((err)      `("!" . '(:inherit compilation-error)))
                   ((outdated) `("?" . '(:slant italic)))
                   ((running)  '("*" . '(:slant italic)) )
                   (otherwise  '("-" . '(:strike-through t))))))
      `(" " (:propertize ,(concat prefix suffix) face ,face)))))

(provide 'racket-check-syntax)

;; racket-check-syntax.el ends here
