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
(require 'racket-complete)
(require 'racket-describe)
(require 'racket-visit)
(require 'racket-util)
(require 'racket-show)
(require 'rx)
(require 'pos-tip)
(require 'easymenu)

;; TODO: Expose as a defcustom? Or even as commands to turn on/off?
;; Also note there are really 3 categories here: 'local 'import
;; 'module-lang, so could be more granularity.
(defvar racket-check-syntax-highlight-imports-p nil
  "Highlight imported definitions and uses thereof? When nil,
only local defs/uses are highlighted. When t, all are highlighted
-- similar to how DrRacket draws arrows for everything. If you
find that too \"noisy\", set this to nil.")

(defvar racket-check-syntax-control-c-hash-keymap
  (racket--easy-keymap-define
   `(("j" ,#'racket-check-syntax-next-definition)
     ("k" ,#'racket-check-syntax-previous-definition)
     ("n" ,#'racket-check-syntax-next-use)
     ("p" ,#'racket-check-syntax-previous-use)
     ("." ,#'racket-check-syntax-visit-definition)
     ("r" ,#'racket-check-syntax-rename))) )

(defvar racket-check-syntax-mode-map
  (racket--easy-keymap-define
   `(("C-c #"   ,racket-check-syntax-control-c-hash-keymap)
     ("M-."     ,#'racket-check-syntax-visit-definition)
     ("C-c C-." ,#'racket-check-syntax-describe)
     ("C-c C-d" ,#'racket-check-syntax-documentation))))

(easy-menu-define racket-check-syntax-mode-menu racket-check-syntax-mode-map
  "Menu for Racket Check Syntax mode."
  '("Racket Check Syntax"
    ["Next Definition" racket-check-syntax-next-definition]
    ["Previous Definition" racket-check-syntax-previous-definition]
    ["Next Use" racket-check-syntax-next-use]
    ["Previous Use" racket-check-syntax-previous-use]
    ["Rename" racket-check-syntax-rename]
    "---"
    ["Visit Definition" racket-check-syntax-visit-definition]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Racket Documentation" racket-check-syntax-documentation]
    ["Describe" racket-check-syntax-describe]
    "---"
    ["Customize..." customize-mode]))

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
is also available when hovering the mouse cursor. Note: If you
find these features too distracting and/or slow, you may disable
`cursor-sensor-mode'. The remaining features discussed below will
still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses.

Various features augment or replace those in plain `racket-mode'.
One advantage they share is that they do /not/ require the file
to be `racket-run' in order to work properly. Also they usually
are smarter about identifiers in submodules. For example:

#+BEGIN_SRC racket
  #lang racket/base
  (define x 1)
  x
  (module m typed/racket/base
    (define x 2)
    x)
#+END_SRC

Not only can they distinguish the `x`s, they understand that the
two `define`s are different.

- \"M-.\" is rebound from `racket-visit-definition' to
  `racket-check-syntax-visit-definition'.

- \"C-c C-.\" is rebound from `racket-describe' to
  `racket-check-syntax-describe'.

- \"C-c C-d\" is rebound from `racket-doc` to
  `racket-check-syntax-documentation'.

- `racket-check-syntax-complete-at-point' is added to the
  variable `completion-at-point-functions'. Note that in this
  case, it is not smart about submodules; identifiers are assumed
  to be those from the file's module. In addition to supplying
  completion candidates, it supports the :company-location
  property to inspect the definition of a candidate, and
  supports :company-doc-buffer to view documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-check-syntax-after-change-refresh-delay' seconds. The
request is made asynchronously so that Emacs will not block --
the drracket/check-syntax analysis can take even tens of seconds
for even moderately complex source files. When the results are
ready, all annotations for the buffer are completely refreshed.

Tip: This mode follows the convention that a minor mode may only
use a prefix key consisting of C-c followed by a punctuation key.
As a result, `racket-check-syntax-control-c-hash-keymap' is bound
to \"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-check-syntax-mode-map}
"
  :lighter racket-check-syntax-mode-lighter
  :keymap racket-check-syntax-mode-map
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
         (setq next-error-function #'racket-check-syntax-next-error)
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
         (kill-local-variable next-error-function) ;correct?
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
  is displayed, for example \"(name arg-1-name arg-2-name)\".

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: ")
    ((and (pred stringp) str)
     ;; When there is a racket-check-syntax-doc property, use its path
     ;; and anchor, because that will be correct even for an
     ;; identifier in a submodule with different imports than the file
     ;; module. Else supply the file path-str, and the "describe"
     ;; command will treat it as a file module identifier.
     (let ((how (pcase (get-text-property (point) 'racket-check-syntax-doc)
                  (`(,path ,anchor) `(,path . ,anchor))
                  (_                (buffer-file-name))))
           ;; These two thunks are effectively lazy
           ;; `racket-check-syntax-visit-definition' and
           ;; `racket-check-syntax-documentation' using values
           ;; captured from the racket-mode buffer now. They are used
           ;; if/when the user "clicks" a "button" in the Describe
           ;; buffer. By the time that happens, this racket-mode
           ;; buffer might no longer exist. Even if it exists, point
           ;; may have changed.
           (visit-thunk
            (pcase (get-text-property (point) 'racket-check-syntax-visit)
              (`(,path ,subs ,ids)
               (lambda ()
                 (racket--do-visit-def-or-mod
                  `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids))))
              (_
               (pcase (get-text-property (point) 'racket-check-syntax-def)
                 (`(import ,id . ,_)
                  (lambda ()
                    (racket--do-visit-def-or-mod `(mod ,id))))))))
           (doc-thunk
            (pcase (get-text-property (point) 'racket-check-syntax-doc)
              (`(,path ,anchor)
               (lambda ()
                 (browse-url (concat "file://" path "#" anchor))))
              (_
               (lambda ()
                 (racket--cmd/async `(doc ,(buffer-file-name) ,str)
                                    #'browse-url))))))
       (racket--do-describe how str t visit-thunk doc-thunk)))))

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
definitions used in the file module, not submodules."
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
         (racket--do-visit-def-or-mod
          `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids)))
        (_
         (pcase (get-text-property (point) 'racket-check-syntax-def)
           (`(import ,id . ,_)
            (racket--do-visit-def-or-mod `(mod ,id)))))))))

(defun racket-check-syntax-documentation ()
  "Show help found by check-syntax, if any, else `racket-doc'."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-doc)
    (`(,path ,anchor)
     (browse-url (concat "file://" path "#" anchor)))
    (_
     (pcase (racket--symbol-at-point-or-prompt nil "Documentation for: ")
       ((and (pred stringp) str)
        (racket--cmd/async `(doc ,(buffer-file-name) ,str)
                           #'browse-url))))))

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

(defun racket--check-syntax-forward-prop (prop amt)
  "Move point to the next or previous occurrence of PROP, if any.
If moved, return the new position, else nil."
  ;; FIXME: Handle more than just -1 or 1
  (let ((f (cl-case amt
             (-1 #'previous-single-property-change)
             ( 1 #'next-single-property-change))))
    (pcase (and f (funcall f (point) prop))
      ((and (pred integerp) pos)
       ;; Unless this is where the prop starts, find that.
       (unless (get-text-property pos prop)
         (setq pos (funcall f pos prop)))
       (when pos (goto-char pos))
       pos))))

(defun racket-check-syntax-next-definition ()
  "Move point to the next definition."
  (interactive)
  (racket--check-syntax-forward-prop 'racket-check-syntax-def 1))

(defun racket-check-syntax-previous-definition ()
  "Move point to the previous definition."
  (interactive)
  (racket--check-syntax-forward-prop 'racket-check-syntax-def -1))

;;; Errors

(defvar-local racket--check-syntax-errors       (vector))
(defvar-local racket--check-syntax-errors-index 0)

(defun racket--check-syntax-clear-errors ()
  (setq racket--check-syntax-errors       (vector))
  (setq racket--check-syntax-errors-index 0))

(defun racket--check-syntax-add-error (path beg str)
  (setq racket--check-syntax-errors
        (vconcat racket--check-syntax-errors
                 (vector (list path beg str)))))

(defun racket-check-syntax-next-error (&optional amt reset)
  "Our value for the variable `next-error-function'.

If there are any check-syntax errors, move point to the next or
previous one, if any.

Otherwise delegate to `compilation-next-error-function' in
`racket-repl-mode'. That way, things still work as you would want
when using `racket-run', e.g. for runtime evaluation errors that
won't be found merely from expansion."
  (interactive)
  (let ((len (length racket--check-syntax-errors)))
    (cond ((< 0 len)
           (when reset
             (setq racket--check-syntax-errors-index 0))
           (setq racket--check-syntax-errors-index
                 (+ racket--check-syntax-errors-index amt))
           (cond ((and (<= 1 racket--check-syntax-errors-index)
                       (<= racket--check-syntax-errors-index len))
                  (pcase-let ((`(,path ,pos ,str)
                               (aref racket--check-syntax-errors
                                     (1- racket--check-syntax-errors-index))))
                    (cond ((equal path (racket--buffer-file-name))
                           (goto-char pos))
                          (t
                           (find-file path)
                           (goto-char pos)))
                    (message "%s" str)))
                 (t (message "No more errors"))))
          (t
           (with-racket-repl-buffer (compilation-next-error-function amt reset))))))

;;; Update

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

;; TODO: Also provide a command wrapping this, for people who want to
;; disable the timer and run manually.
(defun racket--check-syntax-annotate (&optional after-thunk)
  (racket--check-syntax-set-status 'running)
  (racket--cmd/async
   `(check-syntax ,(or (racket--buffer-file-name) (buffer-name))
                  ,(buffer-substring-no-properties (point-min) (point-max)))
   (racket--restoring-current-buffer
    (lambda (response)
      (racket-show "")
      (racket--check-syntax-clear-errors)
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
         ;; Show now using echo area, only. (Not tooltip because error
         ;; loc might not be at point, or even in the selected
         ;; window.. Not header-line also because that, plus unlikely
         ;; to show whole error message in one line.)
         (message "%s" str)
         (racket--check-syntax-add-error path beg str)
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
                    'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor))))))
        (`(info ,beg ,end ,str)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'help-echo               str
                  'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor)))
           (when (string-equal str "no bound occurrences")
             (add-face-text-property beg end racket-check-syntax-unused-face))))
        (`(unused-require ,beg ,end)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'help-echo               "unused require"
                  'cursor-sensor-functions (list #'racket--check-syntax-cursor-sensor)))
           (add-face-text-property beg end racket-check-syntax-unused-face)))
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
    (racket--check-syntax-clear-errors)
    (remove-text-properties
     (point-min) (point-max)
     (list 'help-echo                 nil
           'racket-check-syntax-def   nil
           'racket-check-syntax-use   nil
           'racket-check-syntax-visit nil
           'racket-check-syntax-doc   nil
           'cursor-sensor-functions   nil))
    (racket--remove-face-text-properties '(racket-check-syntax-error-face
                                           racket-check-syntax-unused-face))
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
    (pcase-let*
        ((status (and (racket--cmd-open-p)
                      racket--check-syntax-mode-status))
         (`(,suffix ,face ,help-echo)
          (cl-case status
            ((ok)       '("✓" nil
                          "Syntax OK"))
            ((err)      `("✗" '(:inherit error)
                          "Syntax error"))
            ((outdated) `("…" nil
                          "Waiting `racket-check-syntax-after-change-refresh-delay'"))
            ((running)  '("λ" nil
                          "Getting analysis from Racket Mode back-end and annotating"))
            (otherwise  '("λ" '(:strike-through t)
                          "Racket Mode back-end not available")))))
      `(" " (:propertize ,(concat prefix suffix)
                         ,@(if face
                               `(face ,face)
                             `())
                         help-echo ,help-echo)))))

(provide 'racket-check-syntax)

;; racket-check-syntax.el ends here
