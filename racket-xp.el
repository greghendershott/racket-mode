;;; racket-xp.el -*- lexical-binding: t -*-

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
(defvar racket-xp-highlight-imports-p nil
  "Highlight imported definitions and uses thereof?

When nil, only local defs/uses are highlighted. When t, all are
highlighted -- similar to how DrRacket draws arrows for
everything. If you find that too \"noisy\", set this to nil.")

(defvar racket-xp-control-c-hash-keymap
  (racket--easy-keymap-define
   `(("j" ,#'racket-xp-next-definition)
     ("k" ,#'racket-xp-previous-definition)
     ("n" ,#'racket-xp-next-use)
     ("p" ,#'racket-xp-previous-use)
     ("." ,#'racket-xp-visit-definition)
     ("r" ,#'racket-xp-rename)
     ("g" ,#'racket-xp-annotate))))

(defvar racket-xp-mode-map
  (racket--easy-keymap-define
   `(("C-c #"   ,racket-xp-control-c-hash-keymap)
     ("M-."     ,#'racket-xp-visit-definition)
     ("C-c C-." ,#'racket-xp-describe)
     ("C-c C-d" ,#'racket-xp-documentation))))

(easy-menu-define racket-xp-mode-menu racket-xp-mode-map
  "Menu for `racket-xp-mode'."
  '("RacketXP"
    ["Next Definition" racket-xp-next-definition]
    ["Previous Definition" racket-xp-previous-definition]
    ["Next Use" racket-xp-next-use]
    ["Previous Use" racket-xp-previous-use]
    ["Rename" racket-xp-rename]
    "---"
    ["Visit Definition" racket-xp-visit-definition]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Racket Documentation" racket-xp-documentation]
    ["Describe" racket-xp-describe]
    "---"
    ["Annotate Now" racket-xp-annotate-]
    ["Customize..." customize-mode]))

;;;###autoload
(define-minor-mode racket-xp-mode
  "A minor mode that analyzes expanded code to explain and explore.

This minor mode is an optional enhancement to `racket-mode' edit
buffers. Like any minor mode, you can turn it on or off for a
specific buffer. If you always want to use it, put the following
code in your Emacs init file:

#+BEGIN_SRC elisp
    (require 'racket-xp)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
#+END_SRC

Note: This mode won't do anything unless/until the Racket Mode
back end is running. It will try to start the back end
automatically. You do /not/ need to `racket-run' the buffer you
are editing.

This mode uses the `drracket/check-syntax` package to analyze
fully-expanded programs, without needing to evaluate a.k.a.
\"run\" them. The resulting analysis provides information for:

- Visually annotating bindings -- local or imported definitions
  and references to them.

- Completion candidates.

- Defintions' source and documentation.

When point is on a definition or use, related items are
highlighted using `racket-xp-def-face' and `racket-xp-use-face'
-- instead of drawing arrows as in Dr Racket -- and \"mouse
over\". Information is displayed using the function(s) in the
hook variable `racket-show-functions'; it is also available when
hovering the mouse cursor. Note: If you find these features too
distracting and/or slow, you may disable `cursor-sensor-mode'.
The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses.

In the following little example, not only does
`drracket/check-syntax` distinguish the various `x` bindings, it
understands the two different imports of `define`:

#+BEGIN_SRC racket
  #lang racket/base
  (define x 1)
  x
  (let ([x x])
    (+ x 1))
  (module m typed/racket/base
    (define x 2)
    x)
#+END_SRC

The function `racket-xp-complete-at-point' is added to the
variable `completion-at-point-functions'. Note that in this case,
it is not smart about submodules; identifiers are assumed to be
definitions from the file's module or its imports. In addition to
supplying completion candidates, it supports the
`:company-location` property to inspect the definition of a
candidate and the `:company-doc-buffer` property to view its
documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-xp-after-change-refresh-delay' seconds. The request is
made asynchronously so that Emacs will not block -- the
drracket/check-syntax analysis can take even tens of seconds for
even moderately complex source files. When the results are ready,
all annotations for the buffer are completely refreshed.

Tip: This mode follows the convention that a minor mode may only
use a prefix key consisting of \"C-c\" followed by a punctuation
key. As a result, `racket-xp-control-c-hash-keymap' is bound to
\"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-xp-mode-map}
"
  :lighter racket-xp-mode-lighter
  :keymap racket-xp-mode-map
  (unless (eq major-mode 'racket-mode)
    (setq racket-xp-mode nil)
    (user-error "racket-xp-mode only works with racket-mode buffers"))
  (cond (racket-xp-mode
         (racket--xp-annotate)
         (add-hook 'after-change-functions
                   #'racket--xp-after-change-hook
                   t t)
         (add-hook 'completion-at-point-functions
                   #'racket-xp-complete-at-point
                   nil t)
         (setq next-error-function #'racket-xp-next-error)
         (when (fboundp 'cursor-sensor-mode)
           (cursor-sensor-mode 1)))
        (t
         (racket-show nil)
         (racket--xp-clear)
         (remove-hook 'after-change-functions
                      #'racket--xp-after-change-hook
                      t)
         (remove-hook 'completion-at-point-functions
                      #'racket-xp-complete-at-point
                      t)
         (kill-local-variable next-error-function) ;correct?
         (when (fboundp 'cursor-sensor-mode)
           (cursor-sensor-mode 0)))))

(defvar-local racket--xp-completions nil)

(defun racket-xp-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

`racket-xp-mode' adds this in addition to the one set
by `racket-mode'."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (completion-table-dynamic
            (lambda (prefix)
              (all-completions prefix racket--xp-completions)))
           :predicate          #'identity
           :exclusive          'no
           :company-location   (racket--xp-make-company-location-proc)
           :company-doc-buffer (racket--xp-make-company-doc-buffer-proc)))))

(defun racket--xp-make-company-location-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (pcase (racket--cmd/await `(def ,how ,str))
            (`(,path ,line ,_) (cons path line))))))))

(defun racket--xp-make-company-doc-buffer-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (racket--do-describe how str))))))

(defun racket-xp-describe (&optional prefix)
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
     ;; When there is a racket-xp-doc property, use its path
     ;; and anchor, because that will be correct even for an
     ;; identifier in a submodule with different imports than the file
     ;; module. Else supply the file path-str, and the "describe"
     ;; command will treat it as a file module identifier.
     (let ((how (pcase (get-text-property (point) 'racket-xp-doc)
                  (`(,path ,anchor) `(,path . ,anchor))
                  (_                (buffer-file-name))))
           ;; These two thunks are effectively lazy
           ;; `racket-xp-visit-definition' and
           ;; `racket-xp-documentation' using values captured from the
           ;; racket-mode buffer now. They are used if/when the user
           ;; "clicks" a "button" in the Describe buffer. By the time
           ;; that happens, this racket-mode buffer might no longer
           ;; exist. Even if it exists, point may have changed.
           (visit-thunk
            (pcase (get-text-property (point) 'racket-xp-visit)
              (`(,path ,subs ,ids)
               (lambda ()
                 (racket--do-visit-def-or-mod
                  `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids))))
              (_
               (pcase (get-text-property (point) 'racket-xp-def)
                 (`(import ,id . ,_)
                  (lambda ()
                    (racket--do-visit-def-or-mod `(mod ,id))))))))
           (doc-thunk
            (pcase (get-text-property (point) 'racket-xp-doc)
              (`(,path ,anchor)
               (lambda ()
                 (browse-url (concat "file://" path "#" anchor))))
              (_
               (lambda ()
                 (racket--cmd/async `(doc ,(buffer-file-name) ,str)
                                    #'browse-url))))))
       (racket--do-describe how str t visit-thunk doc-thunk)))))

(defconst racket--xp-overlay-name 'racket-xp-overlay)

(defun racket--xp-overlay-p (o)
  (eq (overlay-get o 'name)
      racket--xp-overlay-name))

(defun racket--highlight (beg end face)
  (unless (cl-some #'racket--xp-overlay-p
                   (overlays-in beg end))
    (let ((o (make-overlay beg end)))
      (overlay-put o 'name racket--xp-overlay-name)
      (overlay-put o 'priority 0) ;below other overlays e.g. isearch
      (overlay-put o 'face face))))

(defun racket--unhighlight (beg end)
  (remove-overlays beg end 'name racket--xp-overlay-name))

(defun racket--unhighlight-all ()
  (racket--unhighlight (point-min) (point-max)))

(defun racket--xp-cursor-sensor (window old dir)
  (let ((new (window-point window)))
    (cl-case dir
      ('entered
       (pcase (get-text-property new 'help-echo)
         ((and s (pred racket--non-empty-string-p))
          (let ((end (next-single-property-change new 'help-echo)))
            (racket-show s end))))
       (pcase (get-text-property new 'racket-xp-def)
         (`(,kind ,_id ,(and uses `((,beg ,_end) . ,_)))
          (when (or (eq kind 'local)
                    racket-xp-highlight-imports-p)
            (pcase (get-text-property beg 'racket-xp-use)
              (`(,beg ,end)
               (racket--highlight beg end racket-xp-def-face)))
            (dolist (use uses)
              (pcase use
                (`(,beg ,end)
                 (racket--highlight beg end racket-xp-use-face)))))))
       (pcase (get-text-property new 'racket-xp-use)
         (`(,def-beg ,def-end)
          (pcase (get-text-property def-beg 'racket-xp-def)
            (`(,kind ,_id ,uses)
             (when (or (eq kind 'local)
                       racket-xp-highlight-imports-p)
               (racket--highlight def-beg def-end racket-xp-def-face)
               (dolist (use uses)
                 (pcase use
                   (`(,beg ,end)
                    (racket--highlight beg end racket-xp-use-face))))))))))
      ('left
       (when (get-text-property old 'help-echo)
         (racket-show ""))
       (racket--unhighlight-all)))))

(defun racket-xp-visit-definition (&optional prefix)
  "When point is on a use, go to its definition.

With a prefix, prompts you, but in this case beware it assumes
definitions in or imported by the file module -- not locals or
definitions in submodules."
  (interactive "P")
  (unless (unless prefix
            (pcase (get-text-property (point) 'racket-xp-use)
              (`(,beg ,_end)
               (pcase (get-text-property beg 'racket-xp-def)
                 (`(local ,_id ,_uses)
                  (racket--push-loc)
                  (goto-char beg)
                  t)))))
    (if prefix
        (pcase (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")
          ((and (pred stringp) str)
           (racket--do-visit-def-or-mod `(def ,(buffer-file-name) ,str))))
      (pcase (get-text-property (point) 'racket-xp-visit)
        (`(,path ,subs ,ids)
         (racket--do-visit-def-or-mod
          `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids)))
        (_
         (pcase (get-text-property (point) 'racket-xp-def)
           (`(import ,id . ,_)
            (racket--do-visit-def-or-mod `(mod ,id)))))))))

(defun racket-xp-documentation (&optional prefix)
  "Show documentation for the identifier at point.

This gives a \"file:\" URL to `browse-url', which typically opens
an external web browser, but see that and the variable
`browse-url-browser-function'.

With a prefix, prompts you, but in this case beware it assumes
definitions in or imported by the file module -- not locals or
definitions in submodules."
  (interactive "P")
  (pcase (get-text-property (point) 'racket-xp-doc)
    ((and `(,path ,anchor) (guard (not prefix)))
     (browse-url (concat "file://" path "#" anchor)))
    (_
     (pcase (racket--symbol-at-point-or-prompt prefix "Documentation for: ")
       ((and (pred stringp) str)
        (racket--cmd/async `(doc ,(buffer-file-name) ,str)
                           #'browse-url))))))

(defun racket-xp--forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-xp-use)
    (`(,beg ,_end)
     (pcase (get-text-property beg 'racket-xp-def)
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
    (_ (pcase (get-text-property (point) 'racket-xp-def)
         (`(,_kind ,_id ((,beg ,_end) . ,_)) (goto-char beg))))))

(defun racket-xp-next-use ()
  "When point is on a use, go to the next, sibling use."
  (interactive)
  (racket-xp--forward-use 1))

(defun racket-xp-previous-use ()
  "When point is on a use, go to the previous, sibling use."
  (interactive)
  (racket-xp--forward-use -1))

(defun racket-xp-rename ()
  "Rename a local definition and its uses in the current file."
  (interactive)
  (pcase-let*
      (;; Try to get a def prop and a use prop at point
       (def-prop     (get-text-property (point) 'racket-xp-def))
       (uses-prop    (get-text-property (point) 'racket-xp-use))
       (_            (unless (or uses-prop def-prop)
                       (user-error "Not a definition or use")))
       ;; OK, we have one of the props. Use it to get the the other one.
       (uses-prop    (or uses-prop
                         (pcase-let ((`(,_kind ,_id ((,beg ,_end) . ,_)) def-prop))
                           (get-text-property beg 'racket-xp-use))))
       (def-prop     (or def-prop
                         (pcase-let ((`(,beg ,_end) uses-prop))
                           (get-text-property beg 'racket-xp-def))))
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
    (racket-xp-annotate)))

(defun racket--xp-forward-prop (prop amt)
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

(defun racket-xp-next-definition ()
  "Move point to the next definition."
  (interactive)
  (racket--xp-forward-prop 'racket-xp-def 1))

(defun racket-xp-previous-definition ()
  "Move point to the previous definition."
  (interactive)
  (racket--xp-forward-prop 'racket-xp-def -1))

;;; Errors

(defvar-local racket--xp-errors       (vector))
(defvar-local racket--xp-errors-index 0)

(defun racket--xp-clear-errors ()
  (setq racket--xp-errors       (vector))
  (setq racket--xp-errors-index 0))

(defun racket--xp-add-error (path beg str)
  (setq racket--xp-errors
        (vconcat racket--xp-errors
                 (vector (list path beg str)))))

(defun racket-xp-next-error (&optional amt reset)
  "Our value for the variable `next-error-function'.

If there are any check-syntax errors, move point to the next or
previous one, if any.

Otherwise delegate to `compilation-next-error-function' in
`racket-repl-mode'. That way, things still work as you would want
when using `racket-run', e.g. for runtime evaluation errors that
won't be found merely from expansion."
  (interactive)
  (let ((len (length racket--xp-errors)))
    (cond ((< 0 len)
           (when reset
             (setq racket--xp-errors-index 0))
           (setq racket--xp-errors-index
                 (+ racket--xp-errors-index amt))
           (cond ((and (<= 1 racket--xp-errors-index)
                       (<= racket--xp-errors-index len))
                  (pcase-let ((`(,path ,pos ,str)
                               (aref racket--xp-errors
                                     (1- racket--xp-errors-index))))
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

(defvar-local racket--xp-timer nil)

(defun racket--xp-after-change-hook (_beg _end _len)
  (when (timerp racket--xp-timer)
    (cancel-timer racket--xp-timer))
  (racket--xp-set-status 'outdated)
  (when racket-xp-after-change-refresh-delay
    (setq racket--xp-timer
          (run-with-idle-timer racket-xp-after-change-refresh-delay
                               nil      ;no repeat
                               (racket--restoring-current-buffer
                                #'racket--xp-annotate)))))

(defun racket-xp-annotate ()
  "Request the buffer to be analyzed and annotated.

If you have set `racket-xp-after-change-refresh-delay' to nil --
or to a very large amount -- you can use this command to annotate
manually."
  (interactive)
  (racket--xp-annotate
   (lambda () ;nudge the cursor-sensor stuff to refresh
       (when (and (boundp 'cursor-sensor-mode) cursor-sensor-mode)
         (racket--xp-cursor-sensor (selected-window)
                                   (point-min)
                                   'entered)))))

(defun racket--xp-annotate (&optional after-thunk)
  (racket--xp-set-status 'running)
  (racket--cmd/async
   `(check-syntax ,(or (racket--buffer-file-name) (buffer-name))
                  ,(buffer-substring-no-properties (point-min) (point-max)))
   (racket--restoring-current-buffer
    (lambda (response)
      (racket-show "")
      (racket--xp-clear-errors)
      (pcase response
        (`(check-syntax-ok
           (completions . ,completions)
           (annotations . ,annotations))
         (racket--xp-clear)
         (setq racket--xp-completions completions)
         (racket--xp-insert annotations)
         (racket--xp-set-status 'ok)
         (when (and annotations after-thunk)
           (funcall after-thunk)))
        (`(check-syntax-errors . ,xs)
         (racket--xp-insert xs)
         (racket--xp-set-status 'err)))))))

(defun racket--xp-insert (xs)
  "Insert text properties. Convert integer positions to markers."
  (with-silent-modifications
    (dolist (x xs)
      (pcase x
        (`(error ,path ,beg ,end ,str)
         ;; Show now using echo area, only. (Not tooltip because error
         ;; loc might not be at point, or even in the selected
         ;; window.. Not header-line also because that, plus unlikely
         ;; to show whole error message in one line.)
         (message "%s" str)
         (racket--xp-add-error path beg str)
         (when (equal path (racket--buffer-file-name))
           (let ((beg (copy-marker beg t))
                 (end (copy-marker end t)))
             (remove-text-properties
              beg end
              (list 'help-echo               nil
                    'racket-xp-def           nil
                    'racket-xp-use           nil
                    'cursor-sensor-functions nil))
             (add-text-properties
              beg end
              (list 'face                    racket-xp-error-face
                    'help-echo               str
                    'cursor-sensor-functions (list #'racket--xp-cursor-sensor))))))
        (`(info ,beg ,end ,str)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'help-echo               str
                  'cursor-sensor-functions (list #'racket--xp-cursor-sensor)))
           (when (string-equal str "no bound occurrences")
             (add-face-text-property beg end racket-xp-unused-face))))
        (`(unused-require ,beg ,end)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'help-echo               "unused require"
                  'cursor-sensor-functions (list #'racket--xp-cursor-sensor)))
           (add-face-text-property beg end racket-xp-unused-face)))
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
            (list 'racket-xp-def (list req id uses)
                  'cursor-sensor-functions (list #'racket--xp-cursor-sensor)))
           (dolist (use uses)
             (pcase-let* ((`(,use-beg ,use-end) use))
               (add-text-properties
                use-beg use-end
                (append
                 (list 'racket-xp-use           (list def-beg def-end)
                       'cursor-sensor-functions (list #'racket--xp-cursor-sensor))
                 (when (eq req 'local)
                   (list 'help-echo "Defined locally"))))))))
        (`(external-def ,beg ,end ,path ,subs ,ids)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'racket-xp-visit (list path subs ids)))))
        (`(doc ,beg ,end ,path ,anchor)
         (let ((beg (copy-marker beg t))
               (end (copy-marker end t)))
           (add-text-properties
            beg end
            (list 'racket-xp-doc (list path anchor)))))))))

(defun racket--xp-clear ()
  (with-silent-modifications
    (setq racket--xp-completions nil)
    (racket--xp-clear-errors)
    (remove-text-properties
     (point-min) (point-max)
     (list 'help-echo               nil
           'racket-xp-def           nil
           'racket-xp-use           nil
           'racket-xp-visit         nil
           'racket-xp-doc           nil
           'cursor-sensor-functions nil))
    (racket--remove-face-text-properties '(racket-xp-error-face
                                           racket-xp-unused-face))
    (racket--unhighlight-all)))

;;; Mode line status

(defvar-local racket--xp-mode-status nil)

(defun racket--xp-set-status (&optional which)
  (setq racket--xp-mode-status which)
  (force-mode-line-update))

(defcustom racket-xp-mode-lighter
  '(:eval (racket--xp-mode-lighter))
  "Mode line lighter for `racket-xp-mode'.
Set to nil to disable the mode line completely."
  :group 'racket-mode
  :risky t
  :type 'sexp)

(defun racket--xp-mode-lighter ()
  (let ((prefix "Rkt"))
    (pcase-let*
        ((status (and (racket--cmd-open-p)
                      racket--xp-mode-status))
         (`(,suffix ,face ,help-echo)
          (cl-case status
            ((ok)       '("✓" nil
                          "Syntax OK"))
            ((err)      `("✗" '(:inherit error)
                          "Syntax error"))
            ((outdated) `("…" nil
                          "Outdated: Waiting for `racket-xp-after-change-refresh-delay' or manual `racket-xp-annotate'"))
            ((running)  '("λ" nil
                          "Getting analysis from Racket Mode back-end and annotating"))
            (otherwise  '("λ" '(:strike-through t)
                          "Racket Mode back-end not available")))))
      `(" " (:propertize ,(concat prefix suffix)
                         ,@(if face
                               `(face ,face)
                             `())
                         help-echo ,help-echo)))))

(provide 'racket-xp)

;; racket-xp.el ends here
