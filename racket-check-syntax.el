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

(require 'racket-repl)

(defvar racket--highlight-overlays nil)

(defun racket--highlight (beg end defp)
  ;; Unless one of our highlight overlays already exists there...
  (let ((os (overlays-at beg)))
    (unless (cl-some (lambda (o) (member o racket--highlight-overlays)) os)
      (let ((o (make-overlay beg end)))
        (setq racket--highlight-overlays (cons o racket--highlight-overlays))
        (overlay-put o 'name 'racket-check-syntax-overlay)
        (overlay-put o 'priority 100)
        (overlay-put o 'face (if defp
                                 racket-check-syntax-def-face
                               racket-check-syntax-use-face))))))

(defun racket--unhighlight-all ()
  (while racket--highlight-overlays
    (delete-overlay (car racket--highlight-overlays))
    (setq racket--highlight-overlays (cdr racket--highlight-overlays))))

(defun racket--non-empty-string-p (v)
  (and (stringp v)
       (not (string-match-p "\\`[ \t\n\r]*\\'" v)))) ;`string-blank-p'

(defun racket--point-entered (_old new)
  (pcase (get-text-property new 'help-echo)
    ((and s (pred racket--non-empty-string-p))
     (if (and (boundp 'tooltip-mode)
              tooltip-mode
              (fboundp 'window-absolute-pixel-position))
         (pcase (window-absolute-pixel-position new)
           (`(,left . ,top)
            (let ((tooltip-frame-parameters `((left . ,left)
                                              (top . ,top)
                                              ,@tooltip-frame-parameters)))
              (tooltip-show s))))
       (message "%s" s))))
  (pcase (get-text-property new 'racket-check-syntax-def)
    ((and uses `((,beg ,_end) . ,_))
     (pcase (get-text-property beg 'racket-check-syntax-use)
       (`(,beg ,end) (racket--highlight beg end t)))
     (dolist (use uses)
       (pcase use (`(,beg ,end) (racket--highlight beg end nil))))))
  (pcase (get-text-property new 'racket-check-syntax-use)
    (`(,beg ,end)
     (racket--highlight beg end t)
     (dolist (use (get-text-property beg 'racket-check-syntax-def))
       (pcase use (`(,beg ,end) (racket--highlight beg end nil)))))))

(defun racket--point-left (_old _new)
  (racket--unhighlight-all))

(defun racket-check-syntax-mode-quit ()
  (interactive)
  (racket-check-syntax-mode -1))

(defun racket-check-syntax-mode-goto-def ()
  "When point is on a use, go to its definition."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,_end) (goto-char beg))))

(defun racket-check-syntax-mode-forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,_end)
     (pcase (get-text-property beg 'racket-check-syntax-def)
       (uses (let* ((pt (point))
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
         (`((,beg ,_end) . ,_) (goto-char beg))))))

(defun racket-check-syntax-mode-goto-next-use ()
  "When point is on a use, go to the next (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use 1))

(defun racket-check-syntax-mode-goto-prev-use ()
  "When point is on a use, go to the previous (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use -1))

(defun racket-check-syntax-mode-help ()
  (interactive)
  (describe-function #'racket-check-syntax-mode))

(defun racket-check-syntax-mode-rename ()
  (interactive)
  ;; If we're on a def, get its uses. If we're on a use, get its def.
  (let* ((pt (point))
         (uses (get-text-property pt 'racket-check-syntax-def))
         (def  (get-text-property pt 'racket-check-syntax-use)))
    ;; If we got one, get the other.
    (when (or uses def)
      (let* ((uses (or uses (get-text-property (car def)   'racket-check-syntax-def)))
             (def  (or def  (get-text-property (caar uses) 'racket-check-syntax-use)))
             (locs (cons def uses))
             (strs (mapcar (lambda (loc)
                             (apply #'buffer-substring-no-properties loc))
                           locs)))
        ;; Proceed only if all the strings are the same. (They won't
        ;; be for e.g. import bindings.)
        (when (cl-every (lambda (s) (equal (car strs) s))
                        (cdr strs))
          (let ((new (read-from-minibuffer (format "Rename %s to: " (car strs))))
                (marker-pairs
                 (mapcar (lambda (loc)
                           (let ((beg (make-marker))
                                 (end (make-marker)))
                             (set-marker beg (nth 0 loc) (current-buffer))
                             (set-marker end (nth 1 loc) (current-buffer))
                             (list beg end)))
                         locs))
                (point-marker (let ((m (make-marker)))
                                (set-marker m (point) (current-buffer)))))
            (racket-check-syntax-mode -1)
            (dolist (marker-pair marker-pairs)
              (let ((beg (marker-position (nth 0 marker-pair)))
                    (end (marker-position (nth 1 marker-pair))))
                (delete-region beg end)
                (goto-char beg)
                (insert new)))
            (goto-char (marker-position point-marker))
            (racket-check-syntax-mode 1)))))))

(defun racket-check-syntax-mode-goto-next-def ()
  (interactive)
  (let ((pos (next-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (next-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(defun racket-check-syntax-mode-goto-prev-def ()
  (interactive)
  (let ((pos (previous-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (previous-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(define-minor-mode racket-check-syntax-mode
  "Analyze the buffer and annotate with information.

The buffer becomes read-only until you exit this minor mode.
However you may navigate the usual ways. When point is on a
definition or use, related items are highlighted and
information is displayed in the echo area. You may also use
special commands to navigate among the definition and its uses.

\\{racket-check-syntax-mode-map}
"
  :lighter " CheckSyntax"
  :keymap (racket--easy-keymap-define
           '(("q"               racket-check-syntax-mode-quit)
             ("h"               racket-check-syntax-mode-help)
             (("j" "TAB")       racket-check-syntax-mode-goto-next-def)
             (("k" "<backtab>") racket-check-syntax-mode-goto-prev-def)
             ("."               racket-check-syntax-mode-goto-def)
             ("n"               racket-check-syntax-mode-goto-next-use)
             ("p"               racket-check-syntax-mode-goto-prev-use)
             ("r"               racket-check-syntax-mode-rename)))
  (unless (eq major-mode 'racket-mode)
    (setq racket-check-syntax-mode nil)
    (user-error "racket-check-syntax-mode only works with Racket Mode buffers"))
  (racket--check-syntax-stop)
  (when racket-check-syntax-mode
    (racket--check-syntax-start)))

(defun racket--check-syntax-start ()
  (let ((buf (current-buffer)))
    (racket--save-if-changed)
    (message "Running check-syntax analysis...")
    (racket--cmd/async-raw
     `(check-syntax ,(racket--buffer-file-name))
     (lambda (response)
       (with-current-buffer buf
        (pcase response
          (`(error ,m)
           (racket-check-syntax-mode -1)
           (error m))
          (`(ok ())
           (racket-check-syntax-mode -1)
           (user-error "No bindings found"))
          (`(ok ,xs)
           (message "Marking up buffer...")
           (racket--check-syntax-insert xs)
           (message ""))))))))

(defun racket--check-syntax-insert (xs)
  (with-silent-modifications
    (dolist (x xs)
      (pcase x
        (`(,`info ,beg ,end ,str)
         (put-text-property beg end 'help-echo str))
        (`(,`def/uses ,def-beg ,def-end ,uses)
         (add-text-properties def-beg
                              def-end
                              (list 'racket-check-syntax-def uses
                                    'point-entered #'racket--point-entered
                                    'point-left    #'racket--point-left))
         (dolist (use uses)
           (pcase-let* ((`(,use-beg ,use-end) use))
             (add-text-properties use-beg
                                  use-end
                                  (list 'racket-check-syntax-use (list def-beg
                                                                       def-end)
                                        'point-entered #'racket--point-entered
                                        'point-left    #'racket--point-left)))))))
    (setq buffer-read-only t)
    (setq header-line-format
          "Check Syntax. Buffer is read-only. Press h for help, q to quit.")
    ;; Make 'point-entered and 'point-left work in Emacs 25+. Note
    ;; that this is somewhat of a hack -- I spent a lot of time trying
    ;; to Do the Right Thing using the new cursor-sensor-mode, but
    ;; could not get it to work satisfactorily. See:
    ;; http://emacs.stackexchange.com/questions/29813/point-motion-strategy-for-emacs-25-and-older
    (setq-local inhibit-point-motion-hooks nil)
    ;; Go to next definition, as an affordance/hint what this does:
    (racket-check-syntax-mode-goto-next-def)))

(defun racket--check-syntax-stop ()
  (setq header-line-format nil)
  (with-silent-modifications
    (remove-text-properties (point-min)
                            (point-max)
                            '(help-echo nil
                              racket-check-syntax-def nil
                              racket-check-syntax-use nil
                              point-entered
                              point-left))
    (racket--unhighlight-all)
    (setq buffer-read-only nil)))

(provide 'racket-check-syntax)

;; racket-check-syntax.el ends here
