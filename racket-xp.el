;;; racket-xp.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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
(require 'racket-doc)
(require 'racket-repl)
(require 'racket-describe)
(require 'racket-eldoc)
(require 'racket-imenu)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-show)
(require 'racket-xp-complete)
(require 'easymenu)
(require 'imenu)
(require 'rx)
(require 'seq)
(require 'xref)

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
     ("." ,#'xref-find-definitions)
     ("?" ,#'xref-find-references)
     ("r" ,#'racket-xp-rename)
     ("^" ,#'racket-xp-tail-up)
     ("v" ,#'racket-xp-tail-down)
     (">" ,#'racket-xp-tail-next-sibling)
     ("<" ,#'racket-xp-tail-previous-sibling)
     ("g" ,#'racket-xp-annotate)
     ("N" ,#'racket-xp-next-error)
     ("P" ,#'racket-xp-previous-error))))

(defvar racket-xp-mode-map
  (racket--easy-keymap-define
   `(("C-c #"     ,racket-xp-control-c-hash-keymap)
     ("M-."       ,#'xref-find-definitions)
     ("C-c C-."   ,#'racket-xp-describe)
     ("C-c C-d"   ,#'racket-xp-documentation))))

(easy-menu-define racket-xp-mode-menu racket-xp-mode-map
  "Menu for `racket-xp-mode'."
  '("Racket-XP"
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
    ["Tail up" racket-xp-tail-up]
    ["Tail down" racket-xp-tail-down]
    ["Tail next" racket-xp-tail-next-sibling]
    ["Tail previous" racket-xp-tail-previous-sibling]
    "---"
    ["Visit Definition" xref-find-definitions]
    ["Return from Visit" xref-pop-marker-stack]
    ["Find References" xref-find-references]
    "---"
    ["Racket Documentation" racket-xp-documentation]
    ["Describe" racket-xp-describe]
    "---"
    ["Annotate Now" racket-xp-annotate]))

(defvar racket-xp-buffer-size-limit 128000
  "When `buffer-size' is at least this amount, disable auto refresh.

Also show yes/no warning for manual `racket-xp-annotate'.

See issue #522.

The default value is of course arbitrary. For comparison it is
about half the size of the largest handwritten file I know in the
Racket sources, drracket/private/unit.rkt.")

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

- Visually annotating expressions in a tail position, as well as
  the enclosing expression with respect to which they are in a
  tail position.

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
                           t)))
#+END_SRC

The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses:

  - `racket-xp-next-definition'
  - `racket-xp-previous-definition'
  - `racket-xp-next-use'
  - `racket-xp-previous-use'

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

When point is on the opening parenthesis of an expression in tail
position, it is highlighted using the face
`racket-xp-tail-position-face'.

When point is on the opening parenthesis of an enclosing
expression with respect to which one or more expressions are in
tail position, it is highlighted using the face
`racket-xp-tail-target-face'.

Furthermore, when point is on the opening parenthesis of either
kind of expression, all of the immediately related expressions
are also highlighted. Various commands move among them:

  - `racket-xp-tail-up'
  - `racket-xp-tail-down'
  - `racket-xp-tail-next-sibling'
  - `racket-xp-tail-previous-sibling'

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
`racket-xp-previous-error' navigate among them. Although most
languages will stop after the first syntax error, some like Typed
Racket will try to collect and report multiple errors.

You may use `xref-find-definitions' \\[xref-find-definitions],
`xref-pop-marker-stack' \\[xref-pop-marker-stack], and
`xref-find-references': `racket-xp-mode' adds a backend to the
variable `xref-backend-functions'. This backend uses information
from the drracket/check-syntax static analysis. Its ability to
find references is limited to the current file; when it finds
none it will try the default xref backend implementation which is
grep-based.

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
                        (racket-xp-tail-position . t)
                        (racket-xp-tail-target . t)
                        (racket-xp-visit . t)
                        (racket-xp-doc . t))))
  (cond (racket-xp-mode
         (if (< (buffer-size) racket-xp-buffer-size-limit)
             (racket--xp-annotate)
           (setq racket-xp-after-change-refresh-delay nil)
           (message "Extremely large buffer; setting racket-xp-after-change-refresh-delay set to nil"))
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
         (add-hook 'xref-backend-functions
                   #'racket-xp-xref-backend-function
                   nil t)
         (setq-local imenu-create-index-function #'racket-xp-imenu-create-index-function)
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
         (setq-local imenu-create-index-function #'racket-imenu-create-index-function)
         (remove-hook 'xref-backend-functions
                      #'racket-xp-xref-backend-function
                      t)
         (remove-hook 'pre-redisplay-functions
                      #'racket-xp-pre-redisplay
                      t))))

(defun racket-xp-describe (&optional prefix)
"Describe the identifier at point in a `*Racket Describe*` buffer.

With \\[universal-argument] you are prompted enter the
identifier, but in this case it only considers definitions or
imports at the file's module level -- not local bindings nor
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
TAB, and activate using RET -- for `xref-find-definitions'
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
           ;; `xref-find-definitions' and `racket-xp-documentation'.
           ;; The thunks might be called later, if/when the user
           ;; "clicks" a "button" in the `racket-describe-mode'
           ;; buffer. By the time that happens, this `racket-mode'
           ;; buffer might no longer exist. Even if it exists, point
           ;; may have changed. That's why it is important to capture
           ;; values from the `racket-mode' buffer, now.
           (visit-thunk
            (pcase (xref-backend-definitions 'racket-xp-xref str)
              (`(,xref) (lambda () (racket--pop-to-xref-location xref)))))
           (doc-thunk
            (pcase (get-text-property (point) 'racket-xp-doc)
              (`(,path ,anchor)
               (lambda ()
                 (racket-browse-url (concat "file://" path "#" anchor))))
              (_
               (let ((bfn (racket--buffer-file-name)))
                 (lambda ()
                   (racket--doc-command nil bfn str)))))))
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

(defun racket--add-overlay (beg end face &optional priority)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'priority (or priority 0)) ;below other overlays e.g. isearch
    (overlay-put o 'face face)
    (dolist (p '(modification-hooks
                 insert-in-front-hooks
                 insert-behind-hooks))
      (overlay-put o p (list #'racket--modifying-overlay-deletes-it)))
    (overlay-put o 'insert-in-front-hooks (list #'racket--modifying-overlay-deletes-it))
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
  (with-current-buffer (window-buffer window)
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
                         (racket--add-overlay beg end racket-xp-use-face)))))))))))
        (let ((target  (get-text-property point 'racket-xp-tail-target))
              (context (get-text-property point 'racket-xp-tail-position)))
          (unless (and (equal target (window-parameter window 'racket-xp-tail-target))
                       (equal context   (window-parameter window 'racket-xp-tail-position)))
            (set-window-parameter window 'racket-xp-tail-target  target)
            (set-window-parameter window 'racket-xp-tail-position context)
            (racket--remove-overlays-in-buffer racket-xp-tail-target-face
                                               racket-xp-tail-position-face)
            ;; This is slightly simpler than def/uses because there are
            ;; no beg..end ranges, just single positions.
            (pcase target
              ((and (pred listp) contexts `(,pos . ,_))
               (pcase (get-text-property pos 'racket-xp-tail-position)
                 ((and (pred markerp) pos)
                  (racket--add-overlay pos (1+ pos) 'racket-xp-tail-target-face 1)
                  (dolist (context contexts)
                    (racket--add-overlay context (1+ context) 'racket-xp-tail-position-face 2))))))
            (pcase context
              ((and (pred markerp) target-pos)
               (pcase (get-text-property target-pos 'racket-xp-tail-target)
                 ((and (pred listp) contexts)
                  (racket--add-overlay target-pos (1+ target-pos) 'racket-xp-tail-target-face 1)
                  (dolist (context contexts)
                    (racket--add-overlay context (1+ context) 'racket-xp-tail-position-face 2))))))))))))

(defun racket-xp--force-redisplay (window)
  (dolist (param '(racket-xp-point
                   racket-xp-use racket-xp-def
                   racket-xp-tail-target racket-xp-tail-position))
    (set-window-parameter window param nil))
  (racket-xp-pre-redisplay window))

(defun racket-xp-documentation (&optional prefix)
  "View documentation in an external web browser.

The command varies based on how many \\[universal-argument] command prefixes you supply.

1. None.

   Uses the symbol at point. Tries to find documentation for an
   identifer defined in the expansion of the current buffer.

   If no such identifer exists, opens the Search Manuals page. In
   this case, the variable `racket-documentation-search-location'
   determines whether the search is done locally as with `raco
   doc`, or visits a URL.

2. \\[universal-argument]

   Prompts you to enter a symbol, defaulting to the symbol at
   point if any.

   Otherwise behaves like 1.

3. \\[universal-argument] \\[universal-argument]

   Prompts you to enter anything, defaulting to the symbol at
   point if any.

   Proceeds directly to the Search Manuals page. Use this if you
   would like to see documentation for all identifiers named
   \"define\", for example."
  (interactive "P")
  (pcase (get-text-property (point) 'racket-xp-doc)
    ((and `(,path ,anchor) (guard (not prefix)))
     (racket-browse-url (concat "file://" path "#" anchor)))
    (_
     (racket--doc prefix (buffer-file-name) racket--xp-binding-completions))))

(defun racket-xp--forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-xp-use)
    (`(,beg ,_end)
     (pcase (get-text-property beg 'racket-xp-def)
       (`(,_kind ,_id ,uses)
        (let* ((ix-this (seq-position uses (point)
                                      (lambda (use pt)
                                        (pcase use
                                          (`(,beg ,end) (and (<= beg pt)
                                                             (< pt end)))))))
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

;;; tail and enclosing expressions

(defun racket-xp-tail-up ()
  "Go \"up\" to the expression enclosing an expression in tail position.

When point is on the opening parenthesis of an expression in tail
position, go its \"target\" -- that is, go to the enclosing
expression with the same continuation as the tail expression."
  (interactive)
  (pcase (get-text-property (point) 'racket-xp-tail-position)
    ((and (pred markerp) pos)
     (goto-char pos))
    (_ (user-error "Expression not in tail position"))))

(defun racket-xp-tail-down ()
  "Go \"down\" to the first tail position enclosed by the current expression."
  (interactive)
  (pcase (get-text-property (point) 'racket-xp-tail-target)
    (`(,pos . ,_) (goto-char pos))
    (_ (user-error "Expression does not enclose an expression in tail position"))))

(defun racket-xp--forward-tail (amt)
  "When point is on a tail, go AMT tails forward. AMT may be negative.

Moving before/after the first/last tail wraps around."
  (pcase (get-text-property (point) 'racket-xp-tail-position)
    ((and (pred markerp) pos)
     (pcase (get-text-property pos 'racket-xp-tail-target)
       ((and (pred listp) tails)
        (let* ((ix-this (seq-position tails (point-marker)))
               (ix-next (+ ix-this amt))
               (ix-next (if (> amt 0)
                            (if (>= ix-next (length tails)) 0 ix-next)
                          (if (< ix-next 0) (1- (length tails)) ix-next)))
               (next (nth ix-next tails)))
          (goto-char next)
          t))))))

(defun racket-xp-tail-next-sibling ()
  "Go to the next tail position sharing the same enclosing expression."
  (interactive)
  (unless (racket-xp--forward-tail 1)
    (user-error "Expression is not in tail position")))

(defun racket-xp-tail-previous-sibling ()
  "Go to the previous tail position sharing the same enclosing expression."
  (interactive)
  (unless (racket-xp--forward-tail -1)
    (user-error "Expression is not in tail position")))

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
  "Go to the next error."
  (interactive)
  (racket--xp-next-error 1 nil))

(defun racket-xp-previous-error ()
  "Go to the previous error."
  (interactive)
  (racket--xp-next-error -1 nil))

;;; xref

(defun racket-xp-xref-backend-function ()
  'racket-xp-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-xp-xref)))
  (or (racket--module-path-name-at-point)
      (thing-at-point 'symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-xp-xref)))
  (completion-table-dynamic
   (lambda (prefix)
     (all-completions prefix racket--xp-binding-completions))))

(cl-defmethod xref-backend-definitions ((_backend (eql racket-xp-xref)) str)
  (or
   (pcase (get-text-property 0 'racket-module-path str)
     (`absolute
      (pcase (racket--cmd/await nil `(mod ,(substring-no-properties str)))
        (`(,path ,line ,col)
         (list (xref-make str (xref-make-file-location path line col))))))
     (`relative
      (let ((path (expand-file-name (substring-no-properties str 1 -1))))
        (list (xref-make str (xref-make-file-location path 1 0))))))
   ;; Something annotated for jump-to-definition by drracket/check-syntax
   (pcase (get-text-property 0 'racket-xp-visit str)
     (`(,path ,subs ,ids)
      (pcase (racket--cmd/await nil `(def/drr ,(racket--buffer-file-name) ,path ,subs ,ids))
        (`(,path ,line ,col)
         (list (xref-make str
                          (xref-make-file-location path line col)))))))
   (pcase (get-text-property 0 'racket-xp-use str)
     (`(,beg ,end)
      (list
       (xref-make (buffer-substring beg end)
                  (xref-make-buffer-location (current-buffer)
                                             (marker-position beg))))))
   ;; Annotated by dr/cs as imported module; visit the module
   (pcase (get-text-property 0 'racket-xp-def str)
     (`(import ,id . ,_)
      (xref-backend-definitions 'racket-xref-module id)))
   ;; Something that, for whatever reason, drracket/check-syntax did
   ;; not annotate.
   (pcase (racket--cmd/await nil `(def ,(racket--buffer-file-name)
                                       ,(substring-no-properties str)))
     (`(,path ,line ,col)
      (list (xref-make str
                       (xref-make-file-location path line col))))
     (`kernel
      (list (xref-make str
                       (xref-make-bogus-location
                        "Defined in #%%kernel -- source not available")))))))

(cl-defmethod xref-backend-references ((backend (eql racket-xp-xref)) str)
  ;; Note: Our ability to find references is limited to those
  ;; annotated by drracket/check-syntax. Currently this includes only:
  ;;
  ;; 1. References within this file to bindings defined within this
  ;;    file. (The good news is, this does include lexical bindings.)
  ;;
  ;; 2. References within this file to bindings from an imported
  ;;    module (required, or, the #lang).
  ;;
  ;; Otherwise, we're out of luck because there exists no datbase of
  ;; references project-wide.
  (or (pcase (get-text-property 0 'racket-xp-def str)
        (`(,_any-kind ,_def ,uses)
         (mapcar (lambda (use)
                   (pcase-let ((`(,beg ,end) use))
                     (xref-make
                      (buffer-substring beg end)
                      (xref-make-buffer-location
                       (current-buffer) (marker-position beg)))))
                 uses)))
      ;; As a fallback use the xref-backend-references default
      ;; implementation, which greps major-mode files within the
      ;; project. Be careful to strip properties because it is given
      ;; to grep. Also be careful with major-mode-alist regexps as
      ;; they're given to grep.
      (cl-call-next-method backend (substring-no-properties str))))

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

One scenario to keep in mind: The user has typed a few characters
-- which are likely to be a syntax error -- and is in the process
of using manual or auto completion. We don't want to annotate
yet. At best it's a waste of work, and at worst the completion UI
and our UI might distractingly interfere with each other. Just do
nothing for now. If the user selects a completion candiate, that
buffer modification will cause us to run later -- which is
perfect. If they cancel completion, the annotation won't refresh
and might miss a change from before they even started completion
-- which is not great, but is better than making a mistake
rescheduling an idle-timer with an amount <= the amount of idle
time that has already elapsed: see #504."
  (with-current-buffer buffer
    (unless (racket--xp-completing-p)
      (racket--xp-annotate))))

(defun racket--xp-completing-p ()
  "Is completion underway?
This is ad hoc and forensic."
  (or (get-buffer-window "*Completions*")
      (and (boundp 'company-pseudo-tooltip-overlay)
           company-pseudo-tooltip-overlay)))

;;; Annotation

(defun racket-xp-annotate-all-buffers ()
  "Call `racket-xp-annotate' in all `racket-xp-mode' buffers."
  (interactive)
  (let ((buffers (seq-filter (lambda (buffer)
                               (with-current-buffer buffer
                                 racket-xp-mode))
                             (buffer-list))))
    (when (y-or-n-p
           (format "Request re-annotation of %s racket-xp-mode buffers?"
                   (length buffers)))
      (message "")
      (with-temp-message "Working..."
        (dolist (buffer buffers)
          (with-current-buffer buffer
            (racket-xp-annotate)))))))

(defun racket-xp-annotate ()
  "Request the buffer to be analyzed and annotated.

If you have set `racket-xp-after-change-refresh-delay' to nil --
or to a very large amount -- you can use this command to annotate
manually."
  (interactive)
  (when (and racket-xp-mode
             (or (< (buffer-size) racket-xp-buffer-size-limit)
                 (yes-or-no-p "The buffer is so large Emacs will probably 'freeze'! Are you sure you want to continue? ")))
    (racket--xp-annotate
     (let ((windows (get-buffer-window-list (current-buffer) nil t)))
       (lambda ()
         (dolist (window windows)
           (racket-xp--force-redisplay window)))))))

(defvar racket--xp-imenu-index nil)

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
           (imenu       . ,imenu)
           (annotations . ,annotations))
         (racket--xp-clear)
         (setq-local racket--xp-binding-completions completions)
         (setq-local racket--xp-imenu-index imenu)
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

        (`(target/tails ,target ,calls)
         (let ((target (copy-marker target t))
               (calls  (mapcar (lambda (tail) (copy-marker tail t))
                               calls)))
           (put-text-property (marker-position target)
                              (1+ (marker-position target))
                              'racket-xp-tail-target
                              calls)
           (dolist (call calls)
             (put-text-property (marker-position call)
                                (1+ (marker-position call))
                                'racket-xp-tail-position
                                target))))
        (`(jump ,beg ,end ,path ,subs ,ids)
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
      (setq-local racket--xp-imenu-index nil)
      (racket--remove-overlays-in-buffer racket-xp-def-face
                                         racket-xp-use-face
                                         racket-xp-unused-face
                                         racket-xp-tail-position-face
                                         racket-xp-tail-target-face)
      (remove-text-properties (point-min) (point-max)
                              (list 'racket-xp-def           nil
                                    'racket-xp-use           nil
                                    'racket-xp-tail-position nil
                                    'racket-xp-tail-target   nil
                                    'racket-xp-visit         nil
                                    'racket-xp-doc           nil)))))

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

(defun racket-xp-imenu-create-index-function ()
  "A function for the variable `imenu-create-index-function'.

Builds the index from syncheck:add-definition-target annotations,
which seem to correspond to module bindings -- but not lexical
bindings, which seems about right for imenu."
  racket--xp-imenu-index)

(provide 'racket-xp)

;; racket-xp.el ends here
