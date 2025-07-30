;;; racket-debug.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2025 by Greg Hendershott.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-back-end)
(require 'racket-repl)
(require 'compat) ;for seq-map-indexed, seq-sort-by
(require 'easymenu)
(require 'rx)
(require 'seq)

(defun racket-same-directory-files (file)
  "A suitable value for the variable `racket-debuggable-files'.

Return FILE plus absolute paths for all Racket files in the same
directory as FILE."
  (cons file
        (directory-files (file-name-directory file)
                         t
                         (rx "." (or "rkt" "ss" "scm" "scrbl") eos)
                         nil)))

(defun racket--debuggable-files (file-to-run)
  "Do the work described in doc str for variable `racket-debuggable-files'."
  (seq-map (lambda (file)
             (racket-file-name-front-to-back
              (expand-file-name file file-to-run)))
           (if (functionp racket-debuggable-files)
               (funcall racket-debuggable-files file-to-run)
             racket-debuggable-files)))

(defvar racket--debug-break-span nil)
(defvar racket--debug-breakable-positions nil)
(defvar racket--debug-break-locals nil)
(defvar racket--debug-break-info nil)
;; (U nil (cons break-id
;;              (U (list 'before)
;;                 (list 'after string-of-racket-write-values))))

(defvar racket--debug-point-overlays nil
  "A list of overlays for debug action points set by the user.

We need this variable because we support debugging across
multiple source files so these overlays may exist among various
buffers.")

;;;###autoload
(defun racket--debug-on-break (info)
  (pcase info
    (`((,src ,pos ,beg ,end) ,breakable-positions ,locals ,vals)
     (let ((src (racket-file-name-back-to-front src)))
       (if-let (buf (find-buffer-visiting src))
           (if (eq major-mode 'racket-repl-mode)
               (pop-to-buffer buf) ;other window
             (pop-to-buffer-same-window buf))
         (if (eq major-mode 'racket-repl-mode)
             (find-file-other-window src)
           (find-file src)))
       (goto-char pos)
       (setq racket--debug-break-span (cons beg end))
       (pcase vals
         (`(,_id before)
          (message "Break before expression; press ? for help"))
         (`(,_id after (,_ . ,s))
          (message "Break after expression: (values %s; press ? for help"
                   (substring s 1))))
       (setq racket--debug-breakable-positions
             (seq-map (lambda (path+positions)
                        (cons (racket-file-name-back-to-front (car path+positions))
                              (sort (cdr path+positions) #'<)))
                      breakable-positions))
       (setq racket--debug-break-locals locals)
       (setq racket--debug-break-info vals)
       (racket-debug-mode 1)))))

(defun racket--debug-resume (next-break value-prompt-p
                                        &optional extra-debug-points)
  (unless racket--debug-break-info (user-error "Not debugging"))
  (racket--debug-validate-points)
  (let* ((info (if value-prompt-p
                   (racket--debug-prompt-for-new-values)
                 racket--debug-break-info))
         (points (seq-map (lambda (o)
                            (list (with-current-buffer (overlay-buffer o)
                                    (racket-file-name-front-to-back
                                     (racket--buffer-file-name)))
                                  (overlay-start o)
                                  (or (overlay-get o 'racket-point-expression)
                                      "#t")))
                          racket--debug-point-overlays))
         (points (append extra-debug-points points)))
    (racket--cmd/async (racket--repl-session-id)
                       `(debug-resume ((,next-break . ,points) ,info))))
  (racket-debug-mode -1)
  (setq racket--debug-breakable-positions nil)
  (setq racket--debug-break-locals nil)
  (setq racket--debug-break-info nil))

(defun racket--debug-prompt-for-new-values ()
  (pcase racket--debug-break-info
    (`(,id before)
     (pcase (read-from-minibuffer "Skip step, substituting values: " "()")
       ((or `nil "" "()") `(,id before))
       (str               `(,id before ,str))))
    (`(,id after (t . ,orig))
     (pcase (read-from-minibuffer "Step, replacing result values: " orig)
       ((or `nil "" "()") `(,id after (t . ,orig)))
       (new               `(,id after (t . ,new)))))
    (v v)))

(defun racket-debug-step (&optional prefix)
  "Step to next breakable position.

With \\[universal-argument] substitute values."
  (interactive "P")
  (racket--debug-resume 'all prefix))

(defun racket-debug-step-over (&optional prefix)
  "Step over next expression.

With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'over prefix))

(defun racket-debug-step-out (&optional prefix)
  "Step out.

With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'out prefix))

(defun racket-debug-continue (&optional prefix)
  "Continue, utilizing `racket-debug-set-point' expressions.

Execution breaks at any position whose debug point expression evaluates
to a true, non-void value.

With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'some prefix))

(defun racket-debug-run-to-here (&optional prefix)
  "Run to point.

Equivalent to adding a temporary unconditional break at point,
followed by `racket-debug-continue'.

With \\[universal-argument], substitute values."
  (interactive)
  (let ((extra (list (list (racket-file-name-front-to-back
                            (racket--buffer-file-name))
                           (point)
                           "#t"
                           "(break)"))))
    (racket--debug-resume 'some prefix extra)))

(defun racket-debug-go (&optional prefix)
  "Go, ignoring all debug points.

Similar to continuing the program normally, without stepping or
evaluating any debug point expressions -- although code annotated for
debugging runs more slowly.

With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'none prefix))

(defun racket-debug-forward-breakable ()
  "Move to next breakable position in current buffer.

Useful followed by commands like `racket-debug-run-to-here' or
`racket-debug-set-point'."
  (interactive)
  (racket--debug-goto-breakable t))

(defun racket-debug-backward-breakable ()
  "Move to previous breakable position in current buffer.

Useful followed by commands like `racket-debug-run-to-here' or
`racket-debug-set-point'."
  (interactive)
  (racket--debug-goto-breakable nil))

(defun racket--debug-goto-breakable (forwardp)
  (pcase (assoc (racket--buffer-file-name) racket--debug-breakable-positions)
    (`(,_src . ,ps)
     (let ((ps   (if forwardp ps (reverse ps)))
           (pred (apply-partially (if forwardp #'< #'>) (point))))
       (goto-char (or (seq-find pred ps) (car ps)))))
    (_ (user-error "No breakable positions in this buffer"))))

(defun racket-debug-set-local ()
  "Set local variable to new value."
  (interactive)
  (let* ((candidates
          (seq-filter
           #'identity
           (seq-map-indexed
            (pcase-lambda (`(,src ,pos ,_span ,sym ,val) index)
              (when (equal src (racket-file-name-back-to-front
                                (racket--buffer-file-name)))
                (concat
                 (propertize (format "%s" sym)
                             'racket-affix (list val (format "%s::%s" src pos))
                             'racket-sort index)
                 ;; `completing-read' strips props so return this via
                 ;; appended invisible text
                 (propertize (format "\t%S" pos)
                             'display ""))))
            racket--debug-break-locals)))
         (affix (racket--make-affix [[8 font-lock-variable-name]
                                     [8 racket-debug-locals-face]
                                     0]))
         (display-sort (lambda (strs)
                         (seq-sort-by (lambda (v)
                                        (get-text-property 0 'racket-sort v))
                                      #'<
                                      strs)))
         (metadata `((category . racket-debug-local)
                     (affixation-function . ,affix)
                     (display-sort-function . ,display-sort)))
         (collection (racket--completion-table candidates metadata))
         (str (completing-read "Local binding to change: " collection nil t))
         (pos (progn
                (string-match (rx bos (+? any) "\t" (group-n 1 (+? any)) eos)
                              str)
                (read (match-string 1 str))))
         (o (seq-find (lambda (o)
                        (eq (overlay-get o 'name) 'racket-debug-local))
                      (overlays-at pos))))
    (unless o
      (error "No local variable found"))
    (let ((val (read-from-minibuffer "New value: "
                                     (overlay-get o 'racket-debug-local-value))))
      (when (and val (not (equal val "")))
        (overlay-put o
                     'after-string
                     (propertize val 'face racket-debug-locals-face))
        (overlay-put o
                     'racket-debug-local-value
                     val)
        (let* ((break-id (car racket--debug-break-info))
               (source (racket-file-name-front-to-back (racket--buffer-file-name)))
               (pos (overlay-start o))
               (info (list break-id source pos val)))
          (racket--cmd/async (racket--repl-session-id)
                             `(debug-set-local ,info)))))))

(defun racket-debug-disable ()
  "Disable `racket-debug-mode' and reset related variables."
  (interactive)
  (when (racket--cmd-open-p) ;otherwise no need
    (racket--cmd/async (racket--repl-session-id) `(debug-disable)))
  (racket-debug-mode -1)
  (setq racket--debug-breakable-positions nil)
  (setq racket--debug-break-locals nil)
  (setq racket--debug-break-info nil))

(add-hook 'racket--repl-before-run-hook #'racket-debug-disable)

;;; breakpoint/watchpoint overlays

(defun racket--debug-validate-points ()
  "Remove invalid overlays from the list."
  (setq racket--debug-point-overlays
        (seq-filter (lambda (o)
                      (if (bufferp (overlay-buffer o))
                          t
                        (delete-overlay o)
                        nil))
                    racket--debug-point-overlays)))

(defun racket-debug-clear-point ()
  "When a debug point exists at point, clear it and return true."
  ;; Note: Actually this defensively deletes multiple debug points at
  ;; point, in case we somehow mistakenly created more than one there.
  ;; But that detail is N/A for user doc string.
  (interactive)
  (let ((anyp nil)
        (os (overlays-at (point))))
    (dolist (o os)
      (when (eq (overlay-get o 'name) 'racket-debug-point)
        (delete-overlay o)
        (setq racket--debug-point-overlays
              (remove o racket--debug-point-overlays))
        (setq anyp t)))
    anyp))

(defun racket-debug-set-point (expression)
  "Set a debug point expression at a breakable position.

Debug point expressions enable debugger features like conditional
breakpoints and watchpoints.

Each debug point consists of a a Racket expression, which will be
evaluated in a context where local variables exist.

Unless the expression evaluates to Racket false or void, execution will
break there. In other words, this is a \"conditional breakpoint\".

In addition, the expression may invoke `#%dump`, which displays
information about all locals (and for after-breaks, the result values)
to both the REPL and the racket-mode-debugger logger topic. In other
words, this is a \"watchpoint\". Although `racket-debug-mode' already
shows these values /in situ/ when at a break, this may be useful if you
want a history.

For example, if the code around the point is something like
`(for ([n 100]) ___)`, then:

  - `#t` means break always.

  - `(zero? (modulo n 10))` breaks every 10 times through the loop.

  - `(when (even? n) (#%dump))` dumps watch information every other time
    through the loop, but never breaks.

The expression may consist of any Racket sub-expressions that evaluate
without error in that local context.

Each debug point is displayed using the customization variables
`racket-debug-point-string' and `racket-debug-point-face'.

Note: If you're warned that point isn't known to be a breakable
position, that might be because it truly isn't, or, just because
`racket-debug-mode' is inactive therefore the breakable positions aren
uknown. Worst case, if you set a debug point someplace that is not
breakable, it is ignored. With a few exceptions -- such as close paren
positions that are tail calls -- most open parens and close parens are
breakble positions in s-expression languages. See the commands
`racket-debug-forward-breakable' and `racket-debug-backward-breakable'."
  (interactive
   (progn
     (unless (or (pcase (assoc (racket--buffer-file-name)
                               racket--debug-breakable-positions)
                   (`(,_src . ,ps) (memq (point) ps)))
                 (y-or-n-p "Point not known to be a breakable position; set anyway "))
       (keyboard-quit))
     (list
      (read-string "Expression [RET for \"#t\"]: "
                   nil
                   'racket-debug-point-expressions
                   "#t"))))
  (racket-debug-clear-point)
  (let ((o (make-overlay (point) (1+ (point)) (current-buffer) t nil)))
    (overlay-put o 'name 'racket-debug-point)
    (overlay-put o 'before-string (propertize
                                   racket-debug-point-string
                                   'face 'racket-debug-point-face
                                   'help-echo expression))
    (overlay-put o 'evaporate t)
    (overlay-put o 'racket-point-expression expression)
    (push o racket--debug-point-overlays)))

(defun racket-debug-toggle-point ()
  "`racket-debug-clear-point' or `racket-debug-set-point'."
  (interactive)
  (unless (racket-debug-clear-point)
    (call-interactively #'racket-debug-set-point)))

(defun racket-debug-forward-point ()
  "Move to next `racket-debug-set-point' location."
  (interactive)
  (racket--debug-goto-point 'forward))

(defun racket-debug-backward-point ()
  "Move to previous `racket-debug-set-point' location."
  (interactive)
  (racket--debug-goto-point 'backward))

(defun racket--debug-goto-point (dir)
  "Move among debug overlays, in `buffer-list' order."
  (let ((by-buffer (seq-group-by #'overlay-buffer
                                 racket--debug-point-overlays)))
    (pcase (seq-some (lambda (buffer)
                       (when-let (overlays (cdr (assoc buffer by-buffer)))
                         (let* ((old-pos (if (equal buffer (current-buffer))
                                             (point)
                                           (with-current-buffer buffer
                                             (if (eq dir 'forward)
                                                 (point-min)
                                               (point-max)))))
                                (new-pos (racket--debug-find-point old-pos
                                                                   overlays
                                                                   dir)))
                           (and new-pos
                                (cons buffer new-pos)))))
                     (buffer-list))
      (`(,buffer . ,pos)
       (pop-to-buffer buffer)
       (goto-char pos))
      (_ (user-error "No debug action point found %s" dir)))))

(defun racket--debug-find-point (orig-pos overlays dir)
  (let ((cmp (if (eq dir 'forward) #'< #'>)))
    (seq-reduce (lambda (pos ov)
                  (let ((beg (overlay-start ov)))
                    (if (and (funcall cmp orig-pos beg)
                             (or (not pos)
                                 (funcall cmp beg pos)))
                        beg
                      pos)))
                overlays
                nil)))

(defun racket-debug-help ()
  (interactive)
  (describe-function 'racket-debug-mode))

(defvar racket--debug-overlays nil)

(define-minor-mode racket-debug-mode
  "Minor mode enabled during step debugging breaks.

How to step debug:

1. \"Instrument\" code for step debugging.

   Use two \\[universal-argument] command prefixes -- for either
   `racket-run' or `racket-run-module-at-point' -- to instrument
   and run the file with step debugging.

   Imported files are also instrumented if they are in the
   variable `racket-debuggable-files'.

   Execution will pause at the first breakable position.

2. Whenever a break occurs:

   - In the `racket-repl-mode' buffer, the prompt changes. In
     this debug REPL, local variables are available for you to
     use and even to `set!`.

   - In the `racket-mode' buffer where the break is located,
     `racket-debug-mode' is enabled. This minor mode makes the
     buffer read-only, provides visual feedback -- about the
     break position, local variable values, and result values --
     and provides shortcut keys:

\\{racket-debug-mode-map}

Tip: After your program runs to completion and returns to a
normal REPL prompt, the code remains instrumented. As a result,
in the REPL if you enter expressions that evaluate instrumented
code, you can debug those, too."
  :lighter " RacketDebug"
  :keymap (racket--easy-keymap-define
           '(("SPC" racket-debug-step)
             ("o"   racket-debug-step-over)
             ("u"   racket-debug-step-out)
             ("n"   racket-debug-forward-breakable)
             ("p"   racket-debug-backward-breakable)
             ("h"   racket-debug-run-to-here)
             ("!"   racket-debug-toggle-point)
             ("="   racket-debug-set-local)
             ("N"   racket-debug-forward-point)
             ("P"   racket-debug-backward-point)
             ("c"   racket-debug-continue)
             ("g"   racket-debug-go)
             ("?"   racket-debug-help)))
  (racket--assert-edit-mode (lambda () (setq racket-debug-mode nil)))
  (cond
   (racket-debug-mode
    (racket--debug-make-overlay
     (car racket--debug-break-span) (cdr racket--debug-break-span)
     'name 'racket-debug-break-expression
     'priority 90
     'face racket-debug-break-expression-face)
    (racket--debug-make-overlay
     (point) (1+ (point))
     'name 'racket-debug-break
     'priority 99
     'face racket-debug-break-face
     'after-string
     (when-let (str (cdr (nth 2 racket--debug-break-info)))
       (propertize (substring str 1 -1)
                   'face racket-debug-locals-face)))
    (dolist (local racket--debug-break-locals)
      (pcase-let ((`(,_src ,pos ,span ,_name ,val) local))
        (racket--debug-make-overlay
         pos (+ pos span)
         'name 'racket-debug-local
         'racket-debug-local-value val
         'after-string (propertize val 'face racket-debug-locals-face))))
    (read-only-mode 1))
   (t
    (read-only-mode -1)
    (dolist (o racket--debug-overlays)
      (delete-overlay o))
    (setq racket--debug-overlays nil))))

(defun racket--debug-make-overlay (beg end &rest props)
  (let ((o (make-overlay beg end)))
    (push o racket--debug-overlays)
    (overlay-put o 'name 'racket-debug-overlay)
    (overlay-put o 'priority 100)
    (while props
      (overlay-put o (pop props) (pop props)))
    o))

(provide 'racket-debug)

;; racket-debug.el ends here
