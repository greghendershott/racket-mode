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
(require 'racket-browse-url)
(require 'racket-repl)
(require 'racket-describe)
(require 'racket-eldoc)
(require 'racket-visit)
(require 'racket-util)
(require 'racket-show)
(require 'racket-xp-complete)
(require 'rx)
(require 'pos-tip)
(require 'easymenu)

(declare-function racket-complete-at-point "racket-mode.el")

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
     ("g" ,#'racket-xp-annotate)
     ("N" ,#'racket-xp-next-error)
     ("P" ,#'racket-xp-previous-error))))

(defvar racket-xp-mode-map
  (racket--easy-keymap-define
   `(("C-c #"     ,racket-xp-control-c-hash-keymap)
     ("M-."       ,#'racket-xp-visit-definition)
     ("C-c C-."   ,#'racket-xp-describe)
     ("C-c C-d"   ,#'racket-xp-documentation))))

(easy-menu-define racket-xp-mode-menu racket-xp-mode-map
  "Menu for `racket-xp-mode'."
  '("RacketXP"
    ["Next Error" racket-xp-next-error]
    ["Previous Error" racket-xp-previous-error]
    "---"
    ["Next Definition" racket-xp-next-definition]
    ["Previous Definition" racket-xp-previous-definition]
    "---"
    ["Next Use" racket-xp-next-use]
    ["Previous Use" racket-xp-previous-use]
    "---"
    ["Rename" racket-xp-rename]
    "---"
    ["Visit Definition" racket-xp-visit-definition]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Racket Documentation" racket-xp-documentation]
    ["Describe" racket-xp-describe]
    "---"
    ["Annotate Now" racket-xp-annotate]))

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

This mode uses the drracket/check-syntax package to analyze
fully-expanded programs, without needing to evaluate a.k.a.
\"run\" them. The resulting analysis provides information for:

- Visually annotating bindings -- local or imported definitions
  and references to them.

- Completion candidates.

- Defintions' source and documentation.

When point is on a definition or use, related items are
highlighted using `racket-xp-def-face' and `racket-xp-use-face'
-- instead of drawing arrows as in Dr Racket. Information is
displayed using the function(s) in the hook variable
`racket-show-functions'; it is also available when hovering the
mouse cursor.

Note: If you find these point-motion features too distracting
and/or slow, in your `racket-xp-mode-hook' you may disable them:

#+BEGIN_SRC elisp
  (require 'racket-xp)
  (add-hook 'racket-xp-mode-hook
            (lambda ()
              (remove-hook 'pre-redisplay-functions
                           #'racket-xp-pre-redisplay
                           t))
#+END_SRC

The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses.

In the following little example, not only does
drracket/check-syntax distinguish the various \"x\" bindings, it
understands the two different imports of \"define\":

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
\":company-location\" property to inspect the definition of a
candidate and the \":company-doc-buffer\" property to view its
documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-xp-after-change-refresh-delay' seconds. The request is
made asynchronously so that Emacs will not block -- for
moderately complex source files, it can take some seconds simply
to fully expand them, as well as a little more time for the
drracket/check-syntax analysis. When the results are ready, all
annotations for the buffer are completely refreshed.

You may also set `racket-xp-after-change-refresh-delay' to nil
and use the `racket-xp-annotate' command manually.

The mode line changes to reflect the current status of
annotations, and whether or not you had a syntax error.

If you have one or more syntax errors, `racket-xp-next-error' and
`racket-xp-previous-error' to navigate among them. Although most
languages will stop after the first syntax error, some like Typed
Racket will try to collect and report multiple errors.

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
  (setq-local text-property-default-nonsticky
              (append text-property-default-nonsticky
                      '((racket-xp-def . t)
                        (racket-xp-use . t)
                        (racket-xp-visit . t)
                        (racket-xp-doc . t))))
  (cond (racket-xp-mode
         (racket--xp-annotate)
         (add-hook 'after-change-functions
                   #'racket--xp-after-change-hook
                   t t)
         (remove-hook 'completion-at-point-functions
                      #'racket-complete-at-point
                      t)
         (add-hook 'completion-at-point-functions
                   #'racket-xp-complete-at-point
                   t t)
         (racket--cmd/async nil
                            `(module-names)
                            (lambda (result)
                              (setq racket--xp-module-completions result)))
         (add-hook 'pre-redisplay-functions
                   #'racket-xp-pre-redisplay
                   nil t))
        (t
         (racket-show nil)
         (racket--xp-clear)
         (remove-hook 'after-change-functions
                      #'racket--xp-after-change-hook
                      t)
         (remove-hook 'completion-at-point-functions
                      #'racket-xp-complete-at-point
                      t)
         (add-hook 'completion-at-point-functions
                   #'racket-complete-at-point
                   t t)
         (remove-hook 'pre-redisplay-functions
                      #'racket-xp-pre-redisplay
                      t))))

(defun racket-xp-describe (&optional prefix)
"Describe the identifier at point in a `*Racket Describe*` buffer.

With a prefix, prompts you, but in this case beware it assumes
definitions in or imported by the file module -- not locals or
definitions in submodules.

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
TAB, and activate using RET -- for `racket-xp-visit-definition'
and `racket-xp-documentation'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: "
                                            racket--xp-binding-completions)
    ((and (pred stringp) str)
     ;; When there is a racket-xp-doc property, use its path
     ;; and anchor, because that will be correct even for an
     ;; identifier in a submodule with different imports than the file
     ;; module. Else supply the file path-str, and the "describe"
     ;; command will treat it as a file module identifier.
     (let ((how (pcase (get-text-property (point) 'racket-xp-doc)
                  (`(,path ,anchor) `(,path . ,anchor))
                  (_                (racket--buffer-file-name))))
           ;; These two thunks are effectively lazy
           ;; `racket-xp-visit-definition' and
           ;; `racket-xp-documentation'. The thunks might be called
           ;; later, if/when the user "clicks" a "button" in the
           ;; `racket-describe-mode' buffer. By the time that happens,
           ;; this `racket-mode' buffer might no longer exist. Even if
           ;; it exists, point may have changed. That's why it is
           ;; important to capture values from the `racket-mode'
           ;; buffer, now.
           (visit-thunk
            (pcase (get-text-property (point) 'racket-xp-visit)
              (`(,path ,subs ,ids)
               (let ((bfn (racket--buffer-file-name)))
                 (lambda ()
                   (racket--do-visit-def-or-mod
                    nil
                    `(def/drr ,bfn ,path ,subs ,ids)))))
              (_
               (pcase (get-text-property (point) 'racket-xp-def)
                 (`(import ,id . ,_)
                  (lambda ()
                    (racket--do-visit-def-or-mod nil `(mod ,id))))))))
           (doc-thunk
            (pcase (get-text-property (point) 'racket-xp-doc)
              (`(,path ,anchor)
               (lambda ()
                 (racket-browse-url (concat "file://" path "#" anchor))))
              (_
               (let ((bfn (racket--buffer-file-name)))
                 (lambda ()
                   (racket--cmd/async nil
                                      `(doc ,bfn ,str)
                                      #'racket-browse-url)))))))
       (racket--do-describe how nil str t visit-thunk doc-thunk)))))

(defun racket-xp-eldoc-function ()
  "A value for the variable `eldoc-documentation-function'.

By default `racket-xp-mode' sets `eldoc-documentation-function'
to nil -- no `eldoc-mode' support. You may set it to this
function in a `racket-xp-mode-hook' if you really want to use
`eldoc-mode'. But it is not a very satisfying experience because
Racket is not a very \"eldoc friendly\" language.

Sometimes we can discover function signatures from source -- but
this can be slow.

Many interesting Racket forms are syntax (macros) without any
easy way to discover their \"argument lists\". Similarly many
Racket functions or syntax are defined in #%kernel and the source
is not available. If they have documentation with a \"bluebox\",
we can show it -- but often it is not a single-line format
typical for eldoc.

Finally, when `racket-xp-after-change-refresh-delay' is a small
value, you may start to type some expression, and pause for
guidance from `eldoc-mode'. However in its incomplete form your
expression might be a syntax error. The resulting error message
might \"fight\" with `eldoc-mode' in the echo area. You could
avoid this by setting the variable `racket-show-functions' not to
include `racket-show-echo-area'. Even so, and worse, the syntax
error might result in a namespace that is empty -- in which case
we won't find blueboxes, types, or contracts.

So if you are expecting an eldoc experience similar to Emacs
Lisp, you will be disappointed.

A more satisfying experience is to use `racket-xp-describe'
or `racket-repl-describe'."
  (racket--do-eldoc (racket--buffer-file-name) nil))

(defun racket--add-overlay (beg end face)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'priority 0) ;below other overlays e.g. isearch
    (overlay-put o 'face face)
    (overlay-put o 'modification-hooks (list #'racket--modifying-overlay-deletes-it))
    o))

(defun racket--modifying-overlay-deletes-it (o &rest _)
  (let ((inhibit-modification-hooks t))
    (delete-overlay o)))

(defun racket--remove-overlays (beg end face)
  (remove-overlays beg end 'face face))

(defun racket--remove-overlays-in-buffer (&rest faces)
  (dolist (face faces)
    (racket--remove-overlays (point-min) (point-max) face)))

(defun racket-xp-pre-redisplay (window)
  (let ((point (window-point window)))
    (unless (equal point (window-parameter window 'racket-xp-point))
      (set-window-parameter window 'racket-xp-point point)
      (pcase (get-text-property point 'help-echo)
        ((and s (pred racket--non-empty-string-p))
         (racket-show s
                      (or (next-single-property-change point 'help-echo)
                          (point-max))))
        (_ (racket-show "")))
      (let ((def (get-text-property point 'racket-xp-def))
            (use (get-text-property point 'racket-xp-use)))
        (unless (and (equal def (window-parameter window 'racket-xp-def))
                     (equal use (window-parameter window 'racket-xp-use)))
          (set-window-parameter window 'racket-xp-def def)
          (set-window-parameter window 'racket-xp-use use)
          (racket--remove-overlays-in-buffer racket-xp-def-face
                                             racket-xp-use-face)
          (pcase def
            (`(,kind ,_id ,(and uses `((,beg ,_end) . ,_)))
             (when (or (eq kind 'local)
                       racket-xp-highlight-imports-p)
               (pcase (get-text-property beg 'racket-xp-use)
                 (`(,beg ,end)
                  (racket--add-overlay beg end racket-xp-def-face)))
               (dolist (use uses)
                 (pcase use
                   (`(,beg ,end)
                    (racket--add-overlay beg end racket-xp-use-face)))))))
          (pcase use
            (`(,def-beg ,def-end)
             (pcase (get-text-property def-beg 'racket-xp-def)
               (`(,kind ,_id ,uses)
                (when (or (eq kind 'local)
                          racket-xp-highlight-imports-p)
                  (racket--add-overlay def-beg def-end racket-xp-def-face)
                  (dolist (use uses)
                    (pcase use
                      (`(,beg ,end)
                       (racket--add-overlay beg end racket-xp-use-face))))))))))))))

(defun racket-xp--force-redisplay (window)
  (dolist (param '(racket-xp-point racket-xp-use racket-xp-def))
    (set-window-parameter window param nil))
  (racket-xp-pre-redisplay window))

(defun racket-xp-visit-definition (&optional prefix)
  "When point is on a use, go to its definition.

With a prefix, prompts you, but in this case beware it assumes
definitions in or imported by the file module -- not locals or
definitions in submodules."
  (interactive "P")
  (if prefix
      (pcase (racket--symbol-at-point-or-prompt t "Visit definition of: "
                                                racket--xp-binding-completions)
        ((and (pred stringp) str)
         (racket--do-visit-def-or-mod nil `(def ,(buffer-file-name) ,str))))
    (or
     ;; Use of binding defined in this file
     (pcase (get-text-property (point) 'racket-xp-use)
       (`(,beg ,_end)
        (pcase (get-text-property beg 'racket-xp-def)
          (`(local ,_id ,_uses)
           (racket--push-loc)
           (goto-char beg)
           t))))
     ;; Something annotated for jump-to-def by drracket/check-syntax
     (pcase (get-text-property (point) 'racket-xp-visit)
       (`(,path ,subs ,ids)
        (racket--do-visit-def-or-mod
         nil
         `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids))
        t))
     ;; Annotated by dr/cs as imported module; visit the module
     (pcase (get-text-property (point) 'racket-xp-def)
       (`(import ,_id . ,_)
        (racket-visit-module prefix)
        t))
     ;; Something that, for whatever reason, drracket/check-syntax did
     ;; not annotate.
     (if (racket--in-require-form-p)
         ;; If point within a textually apparent require form, assume
         ;; it is more likely that dr/cs didn't annotate a module name
         ;; (as opposed to some piece of require transformer syntax)
         ;; and therefore defer to `racket-visit-module'.
         (racket-visit-module)
       ;; Try using the 'def command. This might result in opening the
       ;; defining file at 1:0, or in a "defined in #%kernel" or "not
       ;; found" message. Ut that's better UX than nothing at all
       ;; happening.
       (pcase (racket--symbol-at-point-or-prompt nil "Visit definition of: "
                                                 racket--xp-binding-completions)
         ((and (pred stringp) str)
          (racket--do-visit-def-or-mod nil `(def ,(buffer-file-name) ,str))))))))

(defun racket-xp-documentation (&optional prefix)
  "Show documentation for the identifier at point.

This gives a \"file:\" URL to `racket-browse-url', which
typically opens an external web browser, but see that and the
variable `racket-browse-url-function'.

With a prefix, prompts you, but in this case beware it assumes
definitions in or imported by the file module -- not locals or
definitions in submodules."
  (interactive "P")
  (pcase (get-text-property (point) 'racket-xp-doc)
    ((and `(,path ,anchor) (guard (not prefix)))
     (racket-browse-url (concat "file://" path "#" anchor)))
    (_
     (pcase (racket--symbol-at-point-or-prompt prefix "Documentation for: "
                                               racket--xp-binding-completions)
       ((and (pred stringp) str)
        (racket--cmd/async nil
                           `(doc ,(buffer-file-name) ,str)
                           #'racket-browse-url))))))

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
  ;; Someday/maybe: Handle more than just -1 or 1.
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

(defun racket--xp-next-error (&optional amt reset)
  "Move AMT errors, if any.

If there are any check-syntax errors, moves among them, wrapping
around at the first and last errors.

Otherwise delegate to `next-error'. That way, things still work
as you would want when using `racket-run' -- e.g. for runtime
evaluation errors that won't be found merely from expansion -- or
`compilation-mode'."
  (interactive)
  (let ((len (length racket--xp-errors)))
    (if (zerop len)
        (next-error amt reset)
      (if reset
          (setq racket--xp-errors-index 0)
        (setq racket--xp-errors-index
              (mod (+ racket--xp-errors-index amt)
                   len)))
      (pcase-let ((`(,path ,pos ,str)
                   (aref racket--xp-errors
                         racket--xp-errors-index)))
        (cond ((equal path (racket--buffer-file-name))
               (goto-char pos))
              (t
               (find-file path)
               (goto-char pos)))
        (message "%s" str)))))

(defun racket-xp-next-error ()
  (interactive)
  (racket--xp-next-error 1 nil))

(defun racket-xp-previous-error ()
  (interactive)
  (racket--xp-next-error -1 nil))

;;; Change hook and idle timer

(defvar-local racket--xp-annotate-idle-timer nil)

(defun racket--xp-after-change-hook (_beg _end _len)
  (when (timerp racket--xp-annotate-idle-timer)
    (cancel-timer racket--xp-annotate-idle-timer))
  (racket--xp-set-status 'outdated)
  (when racket-xp-after-change-refresh-delay
    (racket--xp-start-idle-timer (current-buffer))))

(defun racket--xp-start-idle-timer (buffer)
  (setq racket--xp-annotate-idle-timer
        (run-with-idle-timer racket-xp-after-change-refresh-delay
                             nil        ;no repeat
                             #'racket--xp-on-idle-timer
                             buffer)))

(defun racket--xp-on-idle-timer (buffer)
  "Handle after-change-hook => idle-timer expiration.
If no longer current-buffer, don't annotate at all. Otherwise, if
we detect some completion process is underway, don't annotate
now, set timer to check again later. Why? Typically, if the user
then makes some completion choice, that will edit the buffer,
causing the after-change-hook to run again, and schedule another
idle timer. But just in case they don't, we schedule a retry."
  (when (equal buffer (current-buffer))
    (if (racket--xp-completing-p)
        (racket--xp-start-idle-timer buffer)
      (racket--xp-annotate))))

(defun racket--xp-completing-p ()
  "Is completion underway?
This is ad hoc and forensic."
  (or (get-buffer-window "*Completions*")
      (and (boundp 'company-pseudo-tooltip-overlay)
           company-pseudo-tooltip-overlay)))

;;; Annotation

(defun racket-xp-annotate ()
  "Request the buffer to be analyzed and annotated.

If you have set `racket-xp-after-change-refresh-delay' to nil --
or to a very large amount -- you can use this command to annotate
manually."
  (interactive)
  (racket--xp-annotate (lambda ()
                         (racket-xp--force-redisplay (selected-window)))))

(defun racket--xp-annotate (&optional after-thunk)
  (racket--xp-set-status 'running)
  (racket--cmd/async
   nil
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
         (setq-local racket--xp-binding-completions completions)
         (racket--xp-insert annotations)
         (racket--xp-set-status 'ok)
         (when (and annotations after-thunk)
           (funcall after-thunk)))
        (`(check-syntax-errors
           (errors      . ,errors)
           (annotations . ,annotations))
         ;; Don't do full `racket--xp-clear': The old completions and
         ;; some old annotations may be helpful to user while editing
         ;; to correct the error. However do clear things related to
         ;; previous _errors_.
         (racket--xp-clear t)
         (racket--xp-insert errors)
         (racket--xp-insert annotations)
         (racket--xp-set-status 'err)
         (when (and annotations after-thunk)
           (funcall after-thunk))))))))

(defun racket--xp-insert (xs)
  "Insert text properties."
  (with-silent-modifications
    (overlay-recenter (point-max))
    (dolist (x xs)
      (pcase x
        (`(error ,path ,beg ,end ,str)
         (racket--xp-add-error path beg str)
         (when (equal path (racket--buffer-file-name))
           (remove-text-properties
            beg end
            (list 'help-echo     nil
                  'racket-xp-def nil
                  'racket-xp-use nil))
           (racket--add-overlay beg end racket-xp-error-face)
           (add-text-properties
            beg end
            (list 'help-echo str))))
        (`(info ,beg ,end ,str)
         (add-text-properties
          beg end
          (list 'help-echo str))
         (when (and (string-equal str "no bound occurrences")
                    (string-match-p racket-xp-highlight-unused-regexp
                                    (buffer-substring beg end)))
           (racket--add-overlay beg end racket-xp-unused-face)))
        (`(unused-require ,beg ,end)
         (add-text-properties
          beg end
          (list 'help-echo "unused require"))
         (racket--add-overlay beg end racket-xp-unused-face))
        (`(def/uses ,def-beg ,def-end ,req ,id ,uses)
         (let ((def-beg (copy-marker def-beg t))
               (def-end (copy-marker def-end t))
               (uses    (mapcar (lambda (use)
                                  (mapcar (lambda (pos)
                                            (copy-marker pos t))
                                          use))
                                uses)))
           (add-text-properties
            (marker-position def-beg)
            (marker-position def-end)
            (list 'racket-xp-def (list req id uses)))
           (dolist (use uses)
             (pcase-let* ((`(,use-beg ,use-end) use))
               (add-text-properties
                (marker-position use-beg)
                (marker-position use-end)
                (append
                 (list 'racket-xp-use (list def-beg def-end))))))))
        (`(external-def ,beg ,end ,path ,subs ,ids)
         (add-text-properties
          beg end
          (list 'racket-xp-visit (list path subs ids))))
        (`(doc ,beg ,end ,path ,anchor)
         (add-text-properties
          beg end
          (list 'racket-xp-doc (list path anchor))))))))

(defun racket--xp-clear (&optional only-errors-p)
  (with-silent-modifications
    (racket-show "")
    (racket--xp-clear-errors)
    (racket--remove-overlays-in-buffer racket-xp-error-face)
    (remove-text-properties (point-min) (point-max)
                            (list 'help-echo nil))
    (unless only-errors-p
      (setq-local racket--xp-binding-completions nil)
      (racket--remove-overlays-in-buffer racket-xp-def-face
                                         racket-xp-use-face
                                         racket-xp-unused-face)
      (remove-text-properties (point-min) (point-max)
                              (list 'racket-xp-def   nil
                                    'racket-xp-use   nil
                                    'racket-xp-visit nil
                                    'racket-xp-doc   nil)))))

;;; Mode line status

(defvar-local racket--xp-mode-status nil)

(defun racket--xp-set-status (&optional which)
  (setq racket--xp-mode-status which)
  (force-mode-line-update))

(defun racket--xp-mode-lighter ()
  (let ((prefix "Rkt"))
    (pcase-let*
        ((status (and (racket--cmd-open-p)
                      racket--xp-mode-status))
         (`(,suffix ,face ,help-echo)
          (cl-case status
            ((ok)       '("✓" nil
                          "Syntax OK"))
            ((err)      `("✗" (face (:inherit error))
                          "Syntax error"))
            ((outdated) `("…" nil
                          "Outdated: Waiting for `racket-xp-after-change-refresh-delay' or manual `racket-xp-annotate'"))
            ((running)  '("λ" nil
                          "Getting analysis from Racket Mode back-end and annotating"))
            (otherwise  '("λ" (face (:strike-through t))
                          "Racket Mode back-end not available")))))
      `(" " (:propertize ,(concat prefix suffix)
                         ,@face
                         help-echo ,help-echo)))))

(provide 'racket-xp)

;; racket-xp.el ends here
