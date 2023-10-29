;;; racket-debug.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2022 by Greg Hendershott.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-back-end)
(require 'racket-repl)
(require 'easymenu)
(require 'cl-lib)
(require 'rx)

(defun racket-same-directory-files (file)
  "A suitable value for the variable `racket-debuggable-files'.
Return FILE plus absolute paths for all Racket files in the same
directory as FILE."
  (cons file
        (directory-files (file-name-directory file)
                         t
                         (rx "." (or "rkt" "ss" "scm" "scrbl") eos)
                         nil)))

(defvar racket-debuggable-files #'racket-same-directory-files
  "Used to tell `racket-run' what files may be instrumented for debugging.

This isn't yet a defcustom becuase the debugger status is still
\"experimental\".

Must be either a list of file name strings, or, a function that
takes the name of the file being run and returns a list of file
names.

Each file name in the list is made absolute using
`expand-file-name' with respect to the file being run and given
to `racket-file-name-front-to-back'.")

(defun racket--debuggable-files (file-to-run)
  "Do the work described in doc str for variable `racket-debuggable-files'."
  (mapcar (lambda (file)
            (racket-file-name-front-to-back
             (expand-file-name file file-to-run)))
          (if (functionp racket-debuggable-files)
              (funcall racket-debuggable-files file-to-run)
            racket-debuggable-files)))

(defvar racket--debug-breakable-positions nil)
(defvar racket--debug-break-locals nil)
(defvar racket--debug-break-info nil)
;; (U nil (cons break-id
;;              (U (list 'before)
;;                 (list 'after string-of-racket-write-values))))

(defvar racket--debug-breakpoints nil
  "A list of overlays for breakpoints the user has set.")

;;;###autoload
(defun racket--debug-on-break (response)
  (pcase response
    (`((,src . ,pos) ,breakable-positions ,locals ,vals)
     (let ((src (racket-file-name-back-to-front src)))
       (pcase (find-buffer-visiting src)
         (`nil (other-window 1) (find-file src))
         (buf  (pop-to-buffer buf)))
       (goto-char pos)
       (pcase vals
         (`(,_id before)          (message "Break before expression"))
         (`(,_id after (,_ . ,s)) (message "Break after expression: (values %s"
                                           (substring s 1))))
       (setq racket--debug-breakable-positions
             (mapcar (lambda (path+positions)
                       (cons (racket-file-name-back-to-front (car path+positions))
                             (sort (cdr path+positions) #'<)))
                     breakable-positions))
       (setq racket--debug-break-locals locals)
       (setq racket--debug-break-info vals)
       (racket-debug-mode 1)))))

(defun racket--debug-resume (next-break value-prompt-p)
  (unless racket--debug-break-info (user-error "Not debugging"))
  (let ((info (if value-prompt-p
                  (racket--debug-prompt-for-new-values)
                racket--debug-break-info)))
    (racket--cmd/async (racket--repl-session-id)
                       `(debug-resume (,next-break ,info))))
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
  "Step to next breakable position. With \\[universal-argument] substitute values."
  (interactive "P")
  (racket--debug-resume 'all prefix))

(defun racket-debug-step-over (&optional prefix)
  "Step over next expression. With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'over prefix))

(defun racket-debug-step-out (&optional prefix)
  "Step out. With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'out prefix))

(defun racket-debug-continue (&optional prefix)
  "Continue to next breakpoint. With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-validate-breakpoints)
  (racket--debug-resume (seq-map (lambda (o)
                                   (list (with-current-buffer (overlay-buffer o)
                                          (racket-file-name-front-to-back
                                           (racket--buffer-file-name)))
                                         (overlay-start o)
                                         (or (overlay-get o 'racket-breakpoint-condition)
                                             "#t")
                                         (or (overlay-get o 'racket-breakpoint-actions)
                                             "(break)")))
                                 racket--debug-breakpoints)
                        prefix))

(defun racket--debug-validate-breakpoints ()
  "Remove invalid overlays from the list."
  (setq racket--debug-breakpoints
        (seq-filter (lambda (o)
                      (if (bufferp (overlay-buffer o))
                          t
                        (delete-overlay o)
                        nil))
                    racket--debug-breakpoints)))

(defun racket-debug-go (&optional prefix)
  "Go, don't break anymore. With \\[universal-argument], substitute values."
  (interactive "P")
  (racket--debug-resume 'none prefix))

(defun racket-debug-run-to-here (&optional prefix)
  "Resume until point (if possible). With \\[universal-argument], substitute values."
  (interactive)
  ;; i.e. Act as if the only breakpoint is here.
  (racket--debug-resume (list (list (racket-file-name-front-to-back
                                     (racket--buffer-file-name))
                                    (point)
                                    "#t"
                                    "(break)"))
                        prefix))

(defun racket-debug-next-breakable ()
  "Move point to next breakable position."
  (interactive)
  (racket--debug-goto-breakable t))

(defun racket-debug-prev-breakable ()
  "Move point to previous breakable position."
  (interactive)
  (racket--debug-goto-breakable nil))

(defun racket--debug-goto-breakable (forwardp)
  (pcase (assoc (racket--buffer-file-name) racket--debug-breakable-positions)
    (`(,_src . ,ps)
     (let ((ps   (if forwardp ps (reverse ps)))
           (pred (apply-partially (if forwardp #'< #'>) (point))))
       (goto-char (or (cl-find-if pred ps) (car ps)))))
    (_ (user-error "No breakable positions in this buffer"))))

(defun racket--debug-breakpoint-overlay-equal (o)
  (and (equal (overlay-buffer o) (current-buffer))
       (equal (overlay-start o)  (point))))

(defvar racket-debug-breakpoint-conditions '("#t"))
(defvar racket-debug-breakpoint-actions '("(break)" "(print)" "(log)"))
(defun racket-debug-toggle-breakpoint ()
  "Add or remove a breakpoint.

Each breakpoint has a condition and a list of actions.

The condition is a Racket expression that is evaluated in a
context where local variables exist. Examples:

  - \"#t\" means break always.

  - If the code around the breakpoint is something like
     \"(for ([n 100]) _)\", then a condition like
     \"(zero? (modulo n 10))\" is every 10 times through the
     loop.

Actions is a list of symbols; you may specify one or more. The
action symbols are:

  - \"break\" causes a break, enabling `racket-debug-mode'.

  - \"log\" and \"print\" display information about local
    variables to the logger or REPL output, respectively.
    Although `racket-debug-mode' already shows these values \"in
    situ\" when you reach a break, this may be useful if you want
    a history. Specifying \"log\" or \"print\", but not
    \"break\", is equivalent to what many debuggers call a
    watchpoint instead of a breakpoint: Output some information
    and automatically resume.

Note: Although `racket-debug-mode' provides a convenient
keybinding, you may invoke this command anytime using M-x.

Note: If you're warned that point isn't known to be a breakable
position, that might be because it truly isn't, or, just because
you are not in `racket-debug-mode' and the breakable positions
aren't yet known. Worst case, if you set a breakpoint someplace
that is not breakable, it is ignored. With a few exceptions --
such as close paren positions that are tail calls -- most open
parens and close parens are breakble positions."
  (interactive)
  (if-let (o (seq-find #'racket--debug-breakpoint-overlay-equal
                        racket--debug-breakpoints))
      (progn
        (delete-overlay o)
        (setq racket--debug-breakpoints
              (seq-remove #'racket--debug-breakpoint-overlay-equal
                          racket--debug-breakpoints)))
    (when (or (pcase (assoc (racket--buffer-file-name) racket--debug-breakable-positions)
                (`(,_src . ,ps) (memq (point) ps)))
              (y-or-n-p "Point not known to be a breakable position; set anyway "))
      (let* ((condition (read-string "Condition expression [RET for \"#t\"]: "
                                     nil
                                     'racket-debug-breakpoint-conditions
                                     "#t"))
             (actions   (read-string "Actions list [RET for \"(break)\"]: "
                                     nil
                                     'racket-debug-breakpoint-actions
                                     "(break)"))
             (o (make-overlay (point) (1+ (point)) (current-buffer) t nil)))
        (overlay-put o 'name 'racket-debug-breakpoint)
        (overlay-put o 'before-string (propertize
                                       "⦿"
                                       'face 'racket-debug-breakpoint-face))
        (overlay-put o 'evaporate t)
        (overlay-put o 'racket-breakpoint-condition condition)
        (overlay-put o 'racket-breakpoint-actions actions)
        (push o racket--debug-breakpoints)
        (setq racket-debug-breakpoint-conditions
              (seq-uniq racket-debug-breakpoint-conditions))
        (setq racket-debug-breakpoint-actions
              (seq-uniq racket-debug-breakpoint-actions))))))

(defun racket-debug-next-breakpoint ()
  "Move point to the next breakpoint in this buffer."
  (interactive)
  (racket--goto-breakpoint 'next))

(defun racket-debug-prev-breakpoint ()
  "Move point to the previous breakpoint in this buffer."
  (interactive)
  (racket--goto-breakpoint 'previous))

(defun racket--goto-breakpoint (dir)
  (if-let (p (seq-find (if (eq dir 'next)
                           (lambda (pos) (< (point) pos))
                         (lambda (pos) (< pos (point))))
                       (sort (seq-map #'overlay-start
                                      (seq-filter (lambda (o)
                                                    (equal (overlay-buffer o)
                                                           (current-buffer)))
                                                  racket--debug-breakpoints))
                             (if (eq dir 'next)
                                 #'<
                               #'>))))
      (goto-char p)
    (user-error (format "No %s breakpoint in this buffer" dir))))

(defun racket-debug-disable ()
  (interactive)
  (when (racket--cmd-open-p) ;otherwise no need
    (racket--cmd/async (racket--repl-session-id) `(debug-disable)))
  (racket-debug-mode -1)
  (setq racket--debug-breakable-positions nil)
  (setq racket--debug-break-locals nil)
  (setq racket--debug-break-info nil))

(add-hook 'racket--repl-before-run-hook #'racket-debug-disable)

(defun racket-debug-help ()
  (interactive)
  (describe-function 'racket-debug-mode))

(defvar racket--debug-overlays nil)

(define-minor-mode racket-debug-mode
  "Minor mode for debug breaks.

This feature is **EXPERIMENTAL**!!! It is likely to have
significant limitations and bugs. You are welcome to open an
issue to provide feedback. Please understand that this feature
might never be improved -- it might even be removed someday if it
turns out to have too little value and/or too much cost.

How to debug:

1. \"Instrument\" code for step debugging.

   Use two \\[universal-argument] command prefixes for either
   `racket-run' or `racket-run-module-at-point'.

   The file will be instrumented for step debugging before it is
   run. Any imported files are also instrumented if they are in
   the variable `racket-debuggable-files'.

   The run will break at the first breakable position.

   Tip: After you run to completion and return to a normal
   REPL prompt, the code remains instrumented. You may enter
   expressions that evaluate instrumented code and it will
   break so you can step debug again.

2. When a break occurs, the `racket-repl-mode' prompt changes. In
   this debug REPL, local variables are available for you to use
   and even to `set!`.

   Also, in the `racket-mode' buffer where the break is located,
   `racket-debug-mode' is enabled. This minor mode makes the
   buffer read-only, provides visual feedback -- about the break
   position, local variable values, and result values -- and
   provides shortcut keys:

\\{racket-debug-mode-map}
"
  :lighter " RacketDebug"
  :keymap (racket--easy-keymap-define
           '(("SPC" racket-debug-step)
             ("o"   racket-debug-step-over)
             ("u"   racket-debug-step-out)
             ("c"   racket-debug-continue)
             ("g"   racket-debug-go)
             ("n"   racket-debug-next-breakable)
             ("p"   racket-debug-prev-breakable)
             ("N"   racket-debug-next-breakpoint)
             ("P"   racket-debug-prev-breakpoint)
             ("!"   racket-debug-toggle-breakpoint)
             ("h"   racket-debug-run-to-here)
             ("?"   racket-debug-help)))
  (racket--assert-edit-mode (lambda () (setq racket-debug-mode nil)))
  (cond
   (racket-debug-mode
    (racket--debug-make-overlay
     (point) (1+ (point))
     'face racket-debug-break-face
     'priority 99)
    (dolist (local racket--debug-break-locals)
      (pcase-let ((`(,_src ,pos ,span ,_name ,val) local))
        (racket--debug-make-overlay
         pos (+ pos span)
         'after-string (propertize val 'face racket-debug-locals-face))))
    (pcase racket--debug-break-info
      (`(,_id after (,_ . ,str))
       (let ((eol (line-end-position)))
         (racket--debug-make-overlay
          (1- eol) eol
          'after-string (propertize (concat "⇒ (values " (substring str 1))
                                    'face racket-debug-result-face)))))
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
