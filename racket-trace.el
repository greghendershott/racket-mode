;;; racket-trace.el -*- lexical-binding: t; -*-

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

(require 'semantic/symref/grep)
(require 'xref)
(require 'pulse)
(require 'racket-util)
(require 'racket-show)

(defvar racket-trace-mode-map
  (racket--easy-keymap-define
   '((("." "RET") xref-find-definitions)
     ("n"         racket-trace-next)
     ("p"         racket-trace-previous)
     ("u"         racket-trace-up-level)
     ("/"         racket-trace-only-this-thread)
     ("a"         racket-trace-all-threads))))

(defvar racket--trace-known-threads nil)

(define-derived-mode racket-trace-mode special-mode "Racket-Trace"
  "Major mode for trace output.
\\<racket-trace-mode-map>

Shows items logged to the racket-mode-trace topic, for example by
the \"vestige\" package, which is like racket/trace but supports
source location information.

\\{racket-trace-mode-map}"
  (setq-local buffer-undo-list t) ;disable undo
  (setq-local window-point-insertion-type t)
  (add-hook 'before-change-functions #'racket-trace-before-change-function)
  (add-hook 'kill-buffer-hook #'racket-trace-delete-all-overlays nil t)
  (setq-local revert-buffer-function #'racket-trace-revert-buffer-function)
  (setq buffer-invisibility-spec nil)
  ;; xref
  (add-hook 'xref-backend-functions
            #'racket-trace-xref-backend-function
            nil t)
  (add-to-list 'semantic-symref-filepattern-alist
               '(racket-trace-mode "*.rkt" "*.rktd" "*.rktl")))

(defun racket-trace-revert-buffer-function (_ignore-auto noconfirm)
  (when (or noconfirm
            (y-or-n-p "Clear buffer?"))
    (with-silent-modifications
      (erase-buffer))
    (racket-trace-delete-all-overlays)
    (setq buffer-invisibility-spec nil)
    (setq racket--trace-known-threads nil)))

(defconst racket--trace-buffer-name "*Racket Trace*")

(defun racket--trace-get-buffer-create ()
  "Create buffer if necessary."
  (unless (get-buffer racket--trace-buffer-name)
    (with-current-buffer (get-buffer-create racket--trace-buffer-name)
      (racket-trace-mode)))
  (get-buffer racket--trace-buffer-name))

(defun racket--trace-on-notify (data)
  (with-current-buffer (racket--trace-get-buffer-create)
    (let* ((inhibit-read-only  t)
           (original-point     (point))
           (point-was-at-end-p (equal original-point (point-max))))
      (goto-char (point-max))
      (pcase data
        (`(,callp ,tailp ,show ,name ,level ,def ,sig ,call ,ctx ,thread ,msec)
         (let* ((xref (cons name (racket--trace-srcloc-line+col def)))
                (sig (cons show (racket--trace-srcloc-beg+end sig)))
                (call (cons show (racket--trace-srcloc-beg+end call)))
                (ctx (cons show (racket--trace-srcloc-beg+end ctx)))
                (thread (intern thread))
                (prefix (if callp
                            (if tailp "⤑ " "↘ ")
                          "   ⇒ "))
                (common-props (list 'racket-trace-callp     callp
                                    'racket-trace-tailp     tailp
                                    'racket-trace-level     level
                                    'racket-trace-xref      xref
                                    'racket-trace-signature sig
                                    'racket-trace-caller    call
                                    'racket-trace-context   ctx
                                    'racket-trace-thread    thread
                                    'racket-trace-msec      msec
                                    'invisible              thread))
                (new-thread-p (save-excursion
                                (not (eq (and (zerop (forward-line -1))
                                              (get-text-property (point)
                                                                 'racket-trace-thread))
                                         thread))))
                ;; The base face for the entire line. The main feature
                ;; here is to use :overline when this line's thread
                ;; differs from the previous line.
                (face `(:inherit
                        default
                        :overline
                        ,(if new-thread-p "black" nil))))
           (add-to-list 'racket--trace-known-threads thread)
           ;; For an "inset boxes" effect, we start the line by
           ;; drawing a space for each parent level, in its background
           ;; color.
           (cl-loop for n to (1- level)
                    do
                    (insert
                     (apply #'propertize
                            "  "
                            'face
                            (append face
                                    `(:background
                                      ,(racket--trace-level-color n)))
                            common-props))
                    ;; Finally draw the interesting information for
                    ;; this line.
                    finally
                    (let ((face (append face `(:background
                                               ,(racket--trace-level-color level)))))
                      (insert
                       (concat
                        (apply #'propertize
                               (concat prefix show)
                               'face face
                               common-props)
                        (apply #'propertize
                               (format "  %s" thread)
                               'face (append face (if new-thread-p
                                                      `(:height 0.8)
                                                    `(:height 0.8 :foreground "gray")))
                               common-props)
                        (apply #'propertize
                               (format "  %s" msec)
                               'face (append face `(:height 0.8 :foreground "gray"))
                               common-props)
                        (apply #'propertize
                               "\n"
                               'face face
                               common-props))))))))
      (unless point-was-at-end-p
        (goto-char original-point)))))

(defun racket--trace-srcloc-line+col (v)
  "Extract the line and col from a srcloc."
  (pcase v
    (`(,path ,line ,col ,_pos ,_span)
     `(,path ,line ,col))))

(defun racket--trace-srcloc-beg+end (v)
  "Extract the pos and span from a srcloc and convert to beg and end."
  (pcase v
    (`(,path ,_line ,_col ,pos ,span)
     `(,path ,pos ,(+ pos span)))))

(defun racket--trace-level-color (level)
  ;; TODO: Make an array of deffaces for customization
  (let ((colors ["cornsilk1" "cornsilk2" "LightYellow1" "LightYellow2" "LemonChiffon1" "LemonChiffon2"]))
    (aref colors (mod level (length colors)))))

;;; xref

(defun racket-trace-xref-backend-function ()
  'racket-trace-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-trace-xref)))
  (pcase (get-text-property (point) 'racket-trace-xref)
    ((and v `(,name . ,_)) (propertize name 'racket-trace-xref v))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-trace-xref)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql racket-trace-xref)) str)
  (pcase (get-text-property 0 'racket-trace-xref str)
    (`(,_name ,path ,line ,col)
     (list (xref-make str (xref-make-file-location path line col))))))

;;; Commands

;;;###autoload
(defun racket-trace ()
  "Create the `racket-trace-mode' buffer and select it in a window."
  (interactive)
  (select-window
   (display-buffer (racket--trace-get-buffer-create)
                   '((display-buffer-reuse-window
                      display-buffer-below-selected
                      display-buffer-use-some-window)
                     (inhibit-same-window . t)))))

(defun racket-trace-only-this-thread ()
  "Filter to show only traces for the thread at point."
  (interactive)
  (let ((thread (get-text-property (point) 'racket-trace-thread)))
    (unless thread (user-error "No thread found"))
    (dolist (thd racket--trace-known-threads)
      (unless (eq thd thread)
        (add-to-invisibility-spec thd)))
    (message "Showing only thread %s" thread)))

(defun racket-trace-all-threads ()
  "Remove filtering and show traces for all threads."
  (interactive)
  (dolist (thd racket--trace-known-threads)
      (remove-from-invisibility-spec thd))
  (message "Showing all threads"))

(defun racket-trace-next ()
  "Move to next line and show caller and definition sites.
Ignores i.e. skips invisible lines."
  (interactive)
  (when (racket--trace-forward-line 1)
    (racket--trace-back-to-indentation)
    (racket--trace-show-sites)))

(defun racket-trace-previous ()
  "Move to previous line and show caller and definition sites.
Ignores i.e. skips invisible lines."
  (interactive)
  (when (racket--trace-forward-line -1)
    (racket--trace-back-to-indentation)
    (racket--trace-show-sites)))

(defun racket--trace-forward-line (amt)
  "Like `forward-line' but ignores invisible lines and returns a boolean."
  (let ((orig (point))
        (result nil))
    (while (not (or result
                    (if (< amt 0) (bobp) (eobp))))
      (when (and (zerop (forward-line amt))
                 (not (invisible-p (point)))
                 ;; At eob invisible-p can return nil so confirm:
                 (not (if (< amt 0) (bobp) (eobp))))
        (setq result t)))
    (or result
        (progn (goto-char orig) nil))))

(defun racket-trace-up-level ()
  "Move up one level and show caller and definition sites."
  (interactive)
  (unless (and (racket--trace-up-level)
               (not (invisible-p (point))))
    (user-error "Cannot find parent level"))
  (racket--trace-back-to-indentation)
  (racket--trace-show-sites))

(defun racket--trace-up-level ()
  "Try to move up one level for same thread, returning boolean whether moved."
  (let ((orig (point))
        (level (1- (get-text-property (point) 'racket-trace-level)))
        (thread (get-text-property (point) 'racket-trace-thread))
        (result nil))
    (while (not (or result (bobp)))
      (when (and (zerop (forward-line -1))
                 (eq level (get-text-property (point) 'racket-trace-level))
                 (eq thread (get-text-property (point) 'racket-trace-thread)))
        (setq result t)))
    (or result
        (progn (goto-char orig) nil))))

(defun racket--trace-back-to-indentation ()
  "Move to the start of information for a line, based on its indent, skipping any prefix."
  (back-to-indentation)
  (forward-char 2))

;;; Showing call values in situ

(defvar racket--trace-overlays nil
  "List of overlays we've added in various buffers.")

(defun racket-trace-delete-all-overlays ()
  (dolist (o racket--trace-overlays)
    (delete-overlay o))
  (setq racket--trace-overlays nil))

(defun racket-trace-before-change-function (_beg _end)
  "When a buffer is modified, hide all overlays we have in it.
We don't actually delete them, just move them \"nowhere\"."
  (dolist (o racket--trace-overlays)
    (when (equal (overlay-buffer o) (current-buffer))
      (with-temp-buffer (move-overlay o 1 1)))))

(defun racket--trace-show-sites ()
  "Show caller and definition sites for all parent levels and current level."
  (racket-trace-delete-all-overlays)
  ;; Show sites for parent levels, in reverse order
  (let ((here    (point))
        (parents (cl-loop until (not (racket--trace-up-level))
                          collect (point))))
    (cl-loop for pt in (nreverse parents)
             do
             (goto-char pt)
             (racket--trace-show-sites-at-point nil))
    ;; Show sites for current level, last.
    (goto-char here)
    (racket--trace-show-sites-at-point t)))

(defun racket--trace-show-sites-at-point (display-buffer-for-caller-site-p)
  (let ((level (get-text-property (point) 'racket-trace-level))
        (callp (get-text-property (point) 'racket-trace-callp)))
    ;; Caller: Always create buffer if necessary. Maybe display-buffer
    ;; and move window point.
    (pcase (get-text-property (point) 'racket-trace-caller)
      (`(,show ,file ,beg ,end)
       (with-current-buffer (or (get-file-buffer file)
                                (let ((find-file-suppress-same-file-warnings t))
                                  (find-file-noselect file)))
         (when display-buffer-for-caller-site-p
           (let ((win (display-buffer (current-buffer)
                                      '((display-buffer-reuse-window
                                         display-buffer-below-selected
                                         display-buffer-use-some-window)
                                        (inhibit-same-window . t)))))
             (save-selected-window
               (select-window win)
               (goto-char beg))))
         ;; For nested trace-expressions, we might need to make an
         ;; overlay "on top of" an existing one, but that doesn't
         ;; work, so hide any existing trace overlay(s) here. (We
         ;; don't try to delete the overlay and remove it from
         ;; `racket--trace-overlays' here; just move it "nowhere".)
         (dolist (o (overlays-in beg end))
           (when (eq (overlay-get o 'name) 'racket-trace-overlay)
             (with-temp-buffer (move-overlay o 1 1))))
         (let ((o (make-overlay beg end))
               (face `(:inherit default :background ,(racket--trace-level-color level))))
           (push o racket--trace-overlays)
           (overlay-put o 'priority (+ 100 level))
           (overlay-put o 'name 'racket-trace-overlay)
           (overlay-put o 'display (if callp show t))
           (unless callp
             (overlay-put o 'after-string (propertize (concat " ⇒ " show)
                                                      'face face)))
           (overlay-put o 'face face)))))
    ;; Signature at definition site. Only show overlay for calls (not
    ;; results), i.e. "in" the function. If there is not already a
    ;; buffer, don't create one. If the buffer is not already shown in
    ;; a window, don't show it. If already overlay here with exact
    ;; same beg/end, it's probably from "caller", above, for some
    ;; syntactic form where the caller and signature are identical;
    ;; don't create another overlay next to it.
    (pcase (get-text-property (point) 'racket-trace-signature)
      (`(,show ,file ,beg ,end)
       (let ((buf (get-file-buffer file)))
         (when (and callp
                    buf
                    (cl-notany (lambda (o)
                                 (and (eq (overlay-get o 'name) 'racket-trace-overlay)
                                      (eq (overlay-start o) beg)
                                      (eq (overlay-end o) end)))
                               (with-current-buffer buf (overlays-in beg end))))
           (with-current-buffer buf
             (let ((o (make-overlay beg end))
                   (face `(:inherit default :background ,(racket--trace-level-color level))))
               (push o racket--trace-overlays)
               (overlay-put o 'name 'racket-trace-overlay)
               (overlay-put o 'priority 100)
               (overlay-put o 'display show)
               (overlay-put o 'face face)))))))))

(provide 'racket-trace)

;;; racket-trace.el ends here
