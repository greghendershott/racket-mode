;;; racket-xp.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-custom)
(require 'racket-scribble-anchor)
(require 'racket-browse-url)
(require 'racket-doc)
(require 'racket-eldoc)
(require 'racket-repl)
(require 'racket-describe)
(require 'racket-imenu)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-show)
(require 'racket-xp-complete)
(require 'racket-back-end)
(require 'easymenu)
(require 'imenu)
(require 'rx)
(require 'seq)
(require 'xref)

(declare-function racket-complete-at-point "racket-edit" ())
(declare-function racket-browse-file-url "racket-browse-url" (path anchor))

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
     ("N" ,#'next-error)
     ("P" ,#'previous-error))))

(defvar racket-xp-mode-map
  (racket--easy-keymap-define
   `(("C-c #"     ,racket-xp-control-c-hash-keymap)
     ("M-."       ,#'xref-find-definitions)
     ("C-c C-."   ,#'racket-xp-describe)
     ("C-c C-d"   ,#'racket-xp-documentation)
     ("C-c C-s"   ,#'racket-describe-search))))

(easy-menu-define racket-xp-mode-menu racket-xp-mode-map
  "Menu for `racket-xp-mode'."
  '("Racket-XP"
    ["Next Error" next-error]
    ["Previous Error" previous-error]
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
    ["Describe Search" racket-describe-search]
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
    (require \\='racket-xp)
    (add-hook \\='racket-mode-hook #\\='racket-xp-mode)
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
  (require \\='racket-xp)
  (add-hook \\='racket-xp-mode-hook
            (lambda ()
              (remove-hook \\='pre-redisplay-functions
                           #\\='racket-xp-pre-redisplay
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

If you have one or more syntax errors, `next-error' and
`previous-error' navigate among them. Although most languages
will stop after the first syntax error, some like Typed Racket
will try to collect and report multiple errors.

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
  (racket--assert-edit-mode (lambda () (setq racket-xp-mode nil)))
  (setq-local text-property-default-nonsticky
              (append text-property-default-nonsticky
                      '((racket-xp-def . t)
                        (racket-xp-use . t)
                        (racket-xp-tail-position . t)
                        (racket-xp-tail-target . t)
                        (racket-xp-visit . t)
                        (racket-xp-doc . t)
                        (racket-xp-require . t))))
  (cond (racket-xp-mode
         (if (< (buffer-size) racket-xp-buffer-size-limit)
             (racket--xp-annotate)
           (setq-local racket-xp-after-change-refresh-delay nil)
           (message "Extremely large buffer; racket-xp-after-change-refresh-delay locally set to nil"))
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
                              (racket--set-xp-module-completions result)))
         (add-hook 'xref-backend-functions
                   #'racket-xp-xref-backend-function
                   nil t)
         (setq-local imenu-create-index-function #'racket-xp-imenu-create-index-function)
         (setq-local next-error-function #'racket-xp-next-error-function)
         (add-hook 'pre-redisplay-functions
                   #'racket-xp-pre-redisplay
                   nil t)
         (when (boundp 'eldoc-documentation-functions)
           (add-hook 'eldoc-documentation-functions
                     #'racket-xp-eldoc-sexp-app
                     nil t)
           (add-hook 'eldoc-documentation-functions
                     #'racket-xp-eldoc-point
                     nil t))
         (when (boundp 'eldoc-box-buffer-setup-function)
           (setq-local eldoc-box-buffer-setup-function
                       #'racket-xp-eldoc-box-buffer-setup-function)))
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
         (setq-local next-error-function nil)
         (setq-local imenu-create-index-function #'racket-imenu-create-index-function)
         (remove-hook 'xref-backend-functions
                      #'racket-xp-xref-backend-function
                      t)
         (remove-hook 'pre-redisplay-functions
                      #'racket-xp-pre-redisplay
                      t)
         (when (boundp 'eldoc-documentation-functions)
           (dolist (hook (list #'racket-xp-eldoc-sexp-app
                               #'racket-xp-eldoc-point))
            (remove-hook 'eldoc-documentation-functions
                         hook
                         t)))
         (when (and (boundp 'eldoc-box-buffer-setup-function))
           (kill-local-variable eldoc-box-buffer-setup-function)))))

;;; Change hook and idle timer

(defvar-local racket--xp-annotate-idle-timer nil)

(defvar-local racket--xp-edit-generation 0
  "A counter to detect check-syntax command responses we should ignore.
Example scenario: User edits. Timer set. Timer expires; we
request annotations. While waiting for that response, user makes
more edits. When the originally requested annotations arrive, we
can see they're out of date and should be ignored. Instead just wait
for the annotations resulting from the user's later edits.")

(defvar-local racket--xp-inhibit-after-change-hook nil)

(defun racket--xp-after-change-hook (_beg _end _len)
  (unless racket--xp-inhibit-after-change-hook
    (cl-incf racket--xp-edit-generation)
    (when (timerp racket--xp-annotate-idle-timer)
      (cancel-timer racket--xp-annotate-idle-timer))
    (racket--xp-set-status 'outdated)
    (when racket-xp-after-change-refresh-delay
      (racket--xp-start-idle-timer (current-buffer)))))

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
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (racket--xp-completing-p)
        (racket--xp-annotate)))))

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
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   racket-xp-mode)))
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

(defvar-local racket--xp-imenu-index nil)

(defun racket--xp-buffer-file-name ()
  "Allow racket-xp-mode to work in buffers with no buffer-file-name.
Although no file need exist, our back end check-syntax command
needs /some/ path-string? as a unique index."
  (or (racket--buffer-file-name)
      (expand-file-name (buffer-name))))

(defun racket--xp-annotate (&optional after-thunk)
  (racket--xp-set-status 'running)
  (let ((generation-of-our-request racket--xp-edit-generation))
    (racket--cmd/async
     nil
     `(check-syntax ,(racket-file-name-front-to-back
                      (racket--xp-buffer-file-name))
                    ,(save-restriction
                       (widen)
                       (buffer-substring-no-properties (point-min) (point-max))))
     (lambda (response)
       (when (= generation-of-our-request racket--xp-edit-generation)
         (racket-show "")
         (racket--xp-clear-errors)
         (pcase response
           (`(check-syntax-ok
              (completions . ,completions)
              (imenu       . ,imenu)
              (annotations . ,annotations))
            ;; We have no errors; allow `next-error-find-buffer' to
            ;; pick some other buffer, such as a `racket-repl-mode'
            ;; buffer that set this when it ran.
            (when (equal next-error-last-buffer (current-buffer))
              (setq next-error-last-buffer nil))
            (racket--xp-clear)
            (racket--set-xp-binding-completions completions)
            (setq racket--xp-imenu-index imenu)
            (racket--xp-insert annotations)
            (racket--xp-set-status 'ok)
            (when (and annotations after-thunk)
              (funcall after-thunk)))
           (`(check-syntax-errors
              (errors      . ,errors)
              (annotations . ,annotations))
            ;; Set this so `next-error-find-buffer' chooses us.
            (setq next-error-last-buffer (current-buffer))
            ;; Don't do full `racket--xp-clear': The old completions and
            ;; some old annotations may be helpful to user while editing
            ;; to correct the error. However do clear things related to
            ;; previous _errors_.
            (racket--xp-clear t)
            (racket--xp-insert errors)
            (racket--xp-insert annotations)
            (racket--xp-set-status 'err)
            (when (and annotations after-thunk)
              (funcall after-thunk)))))))))

(defun racket--xp-insert (xs)
  "Insert text properties."
  (with-silent-modifications
    (save-restriction
      (widen)
      (overlay-recenter (point-max))
      (dolist (x xs)
        (pcase x
          (`(error ,path ,beg ,end ,str)
           (let ((path (racket-file-name-back-to-front path)))
             (racket--xp-add-error path beg str)
             (when (equal path (racket--xp-buffer-file-name))
               (remove-text-properties
                beg end
                (list 'help-echo     nil
                      'racket-xp-def nil
                      'racket-xp-use nil))
               (racket--add-overlay beg end racket-xp-error-face)
               (put-text-property beg end
                                  'help-echo
                                  (racket--error-message-sans-location-prefix str)))))
          (`(info ,beg ,end ,str)
           (put-text-property beg end 'help-echo str)
           (when (and (string-equal str "no bound occurrences")
                      (string-match-p racket-xp-highlight-unused-regexp
                                      (buffer-substring beg end)))
             (racket--add-overlay beg end racket-xp-unused-face)))
          (`(unused-require ,beg ,end)
           (put-text-property beg end 'help-echo "unused require")
           (racket--add-overlay beg end racket-xp-unused-face))
          (`(require ,beg ,end ,file)
           (put-text-property beg end 'racket-xp-require file))
          (`(def/uses ,def-beg ,def-end ,req ,id ,uses)
           (let ((def-beg (copy-marker def-beg t))
                 (def-end (copy-marker def-end t))
                 (uses    (mapcar (lambda (use)
                                    (mapcar (lambda (pos)
                                              (copy-marker pos t))
                                            use))
                                  uses)))
             (put-text-property (marker-position def-beg)
                                (marker-position def-end)
                                'racket-xp-def (list req id uses))
             (when racket-xp-add-binding-faces
               (racket--xp-add-def-face (marker-position def-beg)
                                        (marker-position def-end)
                                        req))
             (dolist (use uses)
               (pcase-let* ((`(,use-beg ,use-end) use))
                 (put-text-property (marker-position use-beg)
                                    (marker-position use-end)
                                    'racket-xp-use (list def-beg def-end))
                 (when racket-xp-add-binding-faces
                   (racket--xp-add-use-face (marker-position use-beg)
                                            (marker-position use-end)
                                            req))))))
          (`(target/tails ,target ,calls)
           (let ((target (copy-marker target t))
                 (calls  (mapcar (lambda (call)
                                   (copy-marker call t))
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
            (list 'racket-xp-visit
                  (list (racket-file-name-back-to-front path) subs ids))))
          (`(doc ,beg ,end ,path ,anchor ,tag)
           (add-text-properties
            beg end
            (list 'racket-xp-doc
                  (list (racket-file-name-back-to-front path) anchor tag)))))))))

(defun racket--error-message-sans-location-prefix (str)
  "Remove \"/path/to/file.rkt:line:col: \" location prefix from an
error message, which is just noise for a help-echo at that
point."
  (save-match-data
    (if (string-match (rx bos
                          (+? anything) ?: (+ digit) (any ?: ?.) (+ digit) ?:
                          (+? space)
                          (group-n 1 (+? anything))
                          eos)
                      str)
        (match-string 1 str)
      str)))

(defun racket--xp-add-binding-face (beg end face)
  (add-text-properties beg end
                       (list 'font-lock-face face)))

(defun racket--xp-add-def-face (beg end arrow-kind)
  (racket--xp-add-binding-face
   beg end
   (cl-case arrow-kind
     ((module-lang) racket-xp-binding-lang-face)
     ((import)      racket-xp-binding-import-face)
     ((local)       racket-xp-binding-local-face))))

(defun racket--xp-add-use-face (beg end arrow-kind)
  (racket--xp-add-binding-face
   beg end
   (cl-case arrow-kind
     ((module-lang) racket-xp-binding-lang-use-face)
     ((import)      racket-xp-binding-import-use-face)
     ((local)       racket-xp-binding-local-use-face))))

(defun racket--xp-clear (&optional only-errors-p)
  (with-silent-modifications
    (racket-show "")
    (racket--xp-clear-errors)
    (racket--remove-overlays-in-buffer racket-xp-error-face)
    (remove-text-properties (point-min) (point-max)
                            (list 'help-echo nil))
    (unless only-errors-p
      (racket--set-xp-binding-completions nil)
      (setq racket--xp-imenu-index nil)
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
                                    'racket-xp-doc           nil
                                    'racket-xp-require       nil
                                    ;; TODO: Shouldn't do this
                                    ;; unconditionally, need some
                                    ;; other prop to record spans
                                    ;; where WE added this, and
                                    ;; remove only those.
                                    'font-lock-face          nil)))))

(defun racket-xp-eldoc-point (callback &rest _more)
  "Call eldoc CALLBACK about the identifier at point.
A value for the variable `eldoc-documentation-functions'. Use
racket-xp-doc and help-echo text properties added by
`racket-xp-mode'. See `racket-xp-eldoc-level'."
  (when (racket--cmd-open-p)
    (racket--xp-eldoc callback (point))))

(defun racket-xp-eldoc-sexp-app (callback &rest _more)
  "Call eldoc CALLBACK about sexp application around point.
A value for the variable `eldoc-documentation-functions'. Use
racket-xp-doc and help-echo text properties added by
`racket-xp-mode'. See `racket-xp-eldoc-level'."
  (when (and (racket--cmd-open-p)
             (> (point) (point-min)))
    ;; Preserve point during the dynamic extent of the eldoc calls,
    ;; because things like eldoc-box may dismiss the UI if they notice
    ;; point has moved.
    (when-let (pos (condition-case _
                       (save-excursion
                         (backward-up-list)
                         (forward-char 1)
                         (point))
                     (scan-error nil)))
      ;; Avoid returning the same result as `racket-xp-eldoc-point',
      ;; in case `eldoc-documentation-strategy' composes multiple.
      (cl-flet* ((bounds (pos prop)
                   (cdr (racket--get-text-property/bounds pos prop)))
                 (same (a b prop)
                   (equal (bounds a prop) (bounds b prop))))
        (unless (and (boundp 'eldoc-documentation-functions)
                     (member #'racket-xp-eldoc-point
                                eldoc-documentation-functions)
                     (same pos (point) 'racket-xp-doc)
                     (same pos (point) 'help-echo))
          (racket--xp-eldoc callback pos))))))

(defun racket--xp-eldoc (callback pos)
  (pcase (racket--get-text-property/bounds pos 'racket-xp-doc)
    (`((,path ,anchor ,tag) ,beg ,end)
     (let ((thing (buffer-substring-no-properties beg end))
           (help-echo (if-let (s (get-text-property pos 'help-echo))
                           (concat s "\n")
                        "")))
       (let ((str
              (pcase racket-xp-eldoc-level
                ('summary (racket--cmd/await nil `(bluebox ,tag)))
                ('complete (racket--path+anchor->string path anchor)))))
         (when (or help-echo str)
           (racket--eldoc-do-callback callback thing
                                      (propertize
                                       (concat help-echo str)
                                       'racket-xp-eldoc t))))))
    (_
     (pcase (racket--get-text-property/bounds pos 'help-echo)
       (`(,str ,beg ,end)
        (let ((thing (buffer-substring-no-properties beg end)))
          (racket--eldoc-do-callback callback thing
                                     (propertize
                                      str
                                      'racket-xp-eldoc t))))))))

(defun racket--get-text-property/bounds (pos prop)
  "Like `get-text-property' but also returning the bounds."
  (when-let (val (get-text-property pos prop))
    (let* ((beg (if (not (get-text-property (1- pos) prop))
                    pos
                  (previous-single-property-change pos prop)))
           (end (or (next-single-property-change beg prop)
                    (point-max))))
      (list val beg end))))

(defun racket-xp-eldoc-function ()
  "A value for the obsolete variable `eldoc-documentation-function'.

Obsolete: Newer versions of Emacs instead use the variable
`eldoc-documentation-functions', plural."
  nil)

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
    (save-restriction
      (widen)
      (let ((point (window-point window)))
        (unless (equal point (window-parameter window 'racket-xp-point))
          (set-window-parameter window 'racket-xp-point point)
          ;; `racket-show' help-echo (unless eldoc might also do so).
          (unless (and eldoc-mode
                       (bound-and-true-p eldoc-documentation-functions))
            (pcase (get-text-property point 'help-echo)
              ((and s (pred racket--non-empty-string-p))
               (racket-show
                s
                ;; Because some `racket-show' flavors present a tooltip, a
                ;; position after the end of the span is preferable: less
                ;; likely to hide the target of the annotation.
                (pcase (or (next-single-property-change point 'help-echo)
                           (point-max))
                  ((and end (guard (pos-visible-in-window-p end window))) end)
                  ;; But if end isn't visible (#629) prefer beginning.
                  (end
                   (pcase (or (previous-single-property-change end 'help-echo)
                              (point-min))
                     ((and beg (guard (pos-visible-in-window-p beg window))) beg)
                     ;; But if neither beginning nor end are visible, just
                     ;; show starting at top line of window.
                     (_ (save-excursion
                          (goto-char (window-start window))
                          (forward-line -1)
                          (point))))))))
              (_ (racket-show ""))))
          ;; Add def and use overlays.
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
          ;; Add recursion tail and target overlays
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
                      (racket--add-overlay context (1+ context) 'racket-xp-tail-position-face 2)))))))))))))

(defun racket-xp--force-redisplay (window)
  (dolist (param '(racket-xp-point
                   racket-xp-use racket-xp-def
                   racket-xp-tail-target racket-xp-tail-position))
    (set-window-parameter window param nil))
  (racket-xp-pre-redisplay window))

(defun racket-xp-documentation (&optional prefix)
  "View documentation in an external web browser.

The command varies based on how many \\[universal-argument]
command prefixes you supply.
\\<racket-xp-mode-map>

- \\[racket-xp-documentation]

  Uses the symbol at point. Tries to find documentation for an
  identifer defined in the expansion of the current buffer.

  If no such identifer exists, opens the Search Manuals page. In
  this case, the variable `racket-documentation-search-location'
  determines whether the search is done locally as with `raco
  doc`, or visits a URL.

- \\[universal-argument] \\[racket-xp-documentation]

  Always prompts you to enter a symbol, defaulting to the symbol
  at point if any.

- \\[universal-argument] \\[universal-argument] \\[racket-xp-documentation]

  Always prompts you to enter anything, defaulting to the symbol
  at point if any.

  Proceeds directly to the Search Manuals page. Use this if you
  would like to see documentation for all identifiers named
  \"define\", for example."
  (interactive "P")
  (pcase (get-text-property (racket--point) 'racket-xp-doc)
    ((and `(,path ,anchor ,_tag) (guard (not prefix)))
     (racket-browse-file-url path anchor))
    (_
     (racket--doc prefix
                  (racket--xp-buffer-file-name)
                  racket--xp-completion-table-imports))))

;;; Navigation

(defun racket-xp--forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (racket--point) 'racket-xp-use)
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
    (_ (pcase (get-text-property (racket--point) 'racket-xp-def)
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
       (def-prop     (get-text-property (racket--point) 'racket-xp-def))
       (uses-prop    (get-text-property (racket--point) 'racket-xp-use))
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
       (new-id       (read-string (format "Rename %s to: " old-id) nil nil old-id))
       (marker-pairs (mapcar (lambda (loc)
                               (let ((beg (make-marker))
                                     (end (make-marker)))
                                 (set-marker beg (nth 0 loc) (current-buffer))
                                 (set-marker end (nth 1 loc) (current-buffer))
                                 (list beg end)))
                             locs))
       (point-marker (let ((m (make-marker)))
                       (set-marker m (point) (current-buffer)))))
    ;; Don't let our after-change hook run while we make changes,
    ;; otherwise check-syntax will find a syntax error. Note:
    ;; `inhibit-modification-hooks' is too strong here; inhibit just
    ;; our hook.
    (let ((racket--xp-inhibit-after-change-hook t))
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
    (pcase (and f (funcall f (racket--point) prop))
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
  (pcase (get-text-property (racket--point) 'racket-xp-tail-position)
    ((and (pred markerp) pos)
     (goto-char pos))
    (_ (user-error "Expression not in tail position"))))

(defun racket-xp-tail-down ()
  "Go \"down\" to the first tail position enclosed by the current expression."
  (interactive)
  (pcase (get-text-property (racket--point) 'racket-xp-tail-target)
    (`(,pos . ,_) (goto-char pos))
    (_ (user-error "Expression does not enclose an expression in tail position"))))

(defun racket-xp--forward-tail (amt)
  "When point is on a tail, go AMT tails forward. AMT may be negative.

Moving before/after the first/last tail wraps around."
  (pcase (get-text-property (racket--point) 'racket-xp-tail-position)
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

(defun racket-xp-next-error-function (&optional amt reset)
  "Move AMT errors, if any.

A value for the variable `next-error-function'.

If there are any check-syntax errors, moves among them, wrapping
around at the first and last errors."
  (interactive)
  (let ((len (length racket--xp-errors)))
    (unless (zerop len)
      (if reset
          (setq racket--xp-errors-index 0)
        (setq racket--xp-errors-index
              (mod (+ racket--xp-errors-index amt)
                   len)))
      (pcase-let ((`(,path ,pos ,str)
                   (aref racket--xp-errors
                         racket--xp-errors-index)))
        (cond ((equal path (racket--xp-buffer-file-name))
               (goto-char pos))
              (t
               (find-file path)
               (goto-char pos)))
        (message "%s" str)))))

(make-obsolete 'racket-xp-next-error 'next-error "2023-11-20")
(defun racket-xp-next-error ()
  "An obsolete alias for `next-error'."
  (interactive)
  (next-error))

(make-obsolete 'racket-xp-previous-error 'previous-error "2023-11-20")
(defun racket-xp-previous-error ()
  "An obsolete alias for `previous-error'."
  (interactive)
  (previous-error))

;;; describe

(defun racket-xp-describe (&optional prefix)
  "Describe the identifier at point.

The command varies based on how many \\[universal-argument] command prefixes you supply.
\\<racket-xp-mode-map>

- \\[racket-xp-describe]

  Uses the symbol at point. If no such symbol exists, you are
  prompted enter the identifier, but in this case it only
  considers definitions or imports at the file's module level --
  not local bindings nor definitions in submodules.

  - If the identifier has installed Racket documentation, then a
    simplified version of the HTML is presented in the buffer,
    including the \"blue box\", documentation prose, and
    examples.

  - Otherwise, if the identifier is a function, then its
    signature is displayed, for example \"\(name arg-1-name
    arg-2-name\)\".

- \\[universal-argument] \\[racket-xp-describe]

  Always prompts you to enter a symbol, defaulting to the symbol
  at point if any.

- \\[universal-argument] \\[universal-argument] \\[racket-xp-describe]

  This is an alias for `racket-describe-search', which uses
  installed documentation in a `racket-describe-mode' buffer
  instead of an external web browser.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser.

This buffer is also displayed when you use `company-mode' and
press F1 or C-h in its pop up completion list."
  (interactive "P")
  (if (equal prefix '(16))
      (racket-describe-search)
    (pcase (racket--symbol-at-point-or-prompt
            prefix "Describe: "
            racket--xp-completion-table-all)
      ((and (pred stringp) str)
       ;; When user did /not/ supply command prefix to input an
       ;; arbitrary string, we can look for a racket-xp-doc property
       ;; at point. If available, use its path and anchor, because
       ;; that will be correct even for an identifier in a submodule
       ;; with different imports than the file module. Otherwise
       ;; supply the file's path, and the "describe" command will
       ;; treat str as a file module identifier.
       (let ((how (pcase (and (not prefix)
                              (get-text-property (racket--point) 'racket-xp-doc))
                    (`(,path ,anchor ,_tag) `(,path . ,anchor))
                    (_                      (racket--xp-buffer-file-name)))))
         (racket--do-describe how nil str))))))

;;; xref

(defconst racket--xp-props-for-xref
  '(racket-xp-require
    racket-xp-visit
    racket-xp-use
    racket-xp-def))

(defun racket--xp-props-for-xref-at (pos &optional object)
  (when-let (plist (text-properties-at pos object))
    (seq-some (lambda (p) (plist-member plist p))
              racket--xp-props-for-xref)))

(defun racket--xp-find-propertized-string (str)
  "Find string in buffer matching STR and having one of our properties.
When found, returns the buffer string and all its properties,
else returns STR."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (let ((result nil))
          (while (and (not result)
                      (< (point) (point-max)))
            (if (search-forward str nil t)
                (when (and (equal (thing-at-point 'symbol) str)
                           (racket--xp-props-for-xref-at (match-beginning 0)))
                  (setq result (match-string 0)))
             (goto-char (point-max))))
          (or result str))))))

(defun racket-xp-xref-backend-function ()
  'racket-xp-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-xp-xref)))
  (or (seq-some (lambda (prop)
                  (when (get-text-property (racket--point) prop)
                    (let* ((end (next-single-property-change (racket--point) prop))
                           (beg (previous-single-property-change end prop)))
                      (save-restriction (widen) (buffer-substring beg end)))))
                racket--xp-props-for-xref)
      (racket--thing-at-point 'symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-xp-xref)))
  racket--xp-completion-table-all)

(cl-defmethod xref-backend-definitions ((_backend (eql racket-xp-xref)) str)
  ;; If xref used `completing-read', then STR will have no text
  ;; properties, which limits what we can do with it. Try to find a
  ;; matching string in the buffer to use, so we can use one of its
  ;; `racket--xp-props-for-xref'.
  (let ((str (if (not (racket--xp-props-for-xref-at 0 str))
                 (racket--xp-find-propertized-string str)
               str)))
    (list
     (or
      ;; Something annotated as add-open-require-menu by drracket/check-syntax
      (when-let (path (get-text-property 0 'racket-xp-require str))
        (xref-make str (xref-make-file-location path 1 0)))
      ;; Something annotated for jump-to-definition by drracket/check-syntax
      (pcase (get-text-property 0 'racket-xp-visit str)
        (`(,path ,subs ,ids)
         (pcase (racket--cmd/await nil
                                   `(def/drr
                                      ,(racket-file-name-front-to-back
                                        (racket--xp-buffer-file-name))
                                      ,(racket-file-name-front-to-back path)
                                      ,subs
                                      ,ids))
           (`(,path ,line ,col)
            (xref-make str
                       (xref-make-file-location
                        (racket-file-name-back-to-front path) line col))))))
      (pcase (get-text-property 0 'racket-xp-use str)
        (`(,beg ,end)
         (xref-make (save-restriction (widen) (buffer-substring beg end))
                    (xref-make-buffer-location (current-buffer)
                                               (marker-position beg)))))
      (pcase (get-text-property 0 'racket-xp-def str)
        (`(local ,id ((,use-beg ,_use-end) . ,_))
         (when-let (def-beg (car (get-text-property use-beg 'racket-xp-use)))
           (xref-make id
                      (xref-make-buffer-location (current-buffer)
                                                 (marker-position def-beg)))))
        ;; Annotated by dr/cs as imported module; visit the module
        (`(import ,id . ,_)
         (xref-backend-definitions 'racket-xref-module id)))
      ;; Something that, for whatever reason, drracket/check-syntax
      ;; did not annotate. Use the back end `def` command (although it
      ;; can only find definitions imported at the file module level,
      ;; not submodules, since all we give it is a plain string and no
      ;; position.)
      (pcase (racket--cmd/await nil `(def ,(racket-file-name-front-to-back
                                            (racket--xp-buffer-file-name))
                                          ,(substring-no-properties str)))
        (`(,path ,line ,col)
         (xref-make str
                    (xref-make-file-location path line col)))
        (`kernel
         (xref-make str
                    (xref-make-bogus-location
                     "Defined in #%%kernel -- source not available")))
        (_
         (xref-make str
                    (xref-make-bogus-location
                     "Cannot find definition; maybe if identifier is imported in a submodule but not used"))))))))

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
         (save-restriction
           (widen)
           (mapcar (lambda (use)
                     (pcase-let*
                         ((`(,beg ,end) use)
                          (before (buffer-substring (save-excursion
                                                      (goto-char beg)
                                                      (line-beginning-position))
                                                    beg))
                          (match (propertize (buffer-substring beg end)
                                             'face 'match))
                          (after (buffer-substring end
                                                   (save-excursion
                                                     (goto-char end)
                                                     (line-end-position))))
                          (label (concat before match after))
                          (loc (xref-make-buffer-location
                                (current-buffer) (marker-position beg))))
                       (xref-make label loc)))
                   uses))))
      ;; As a fallback use the xref-backend-references default
      ;; implementation, which greps major-mode files within the
      ;; project. Be careful to strip properties because it is given
      ;; to grep. Also be careful with major-mode-alist regexps as
      ;; they're given to grep.
      (cl-call-next-method backend (substring-no-properties str))))

;;; eldoc-box

(defun racket-xp-eldoc-box-buffer-setup-function (original-buffer)
  "Called by eldoc-box with its child frame buffer current.

Although most of `eldoc-box-buffer-setup' is necessary for
eldoc-box to work correctly, we want to avoid two default
behaviors:

1. It runs `eldoc-buffer-hook' functions to massage markdown; N/A.

2. It enables `visual-line-mode', which might add line breaks.

To further avoid extra line breaks and continuation marks in the
fringe, we also want to enable `truncate-lines', as does
`racket-describe-mode'.

Extra line breaks are especially bad when `racket-xp-eldoc-level'
is \\='complete and we're showing a fragment of full Racket
documentation HTML, which makes heavy use of HTML tables, and
we've worked hard to help shr to convert these correctly."
  (when (and (fboundp 'eldoc-box-buffer-setup)
             (boundp 'eldoc-box-buffer-hook))
    ;; Call `eldoc-box-buffer-setup' for most of the work it does --
    ;; but during the dynamic extent of the call, "disable"
    ;; visual-line-mode and the hook.
    (cl-letf (((symbol-function #'visual-line-mode) #'ignore)
              ((default-value 'eldoc-box-buffer-hook) nil))
      (eldoc-box-buffer-setup original-buffer))
    ;; Although the above should suffice, belt+suspenders.
    (visual-line-mode -1)
    ;; Finally, enable `truncate-lines'.
    (setq-local truncate-lines t)))

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
