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

(defvar racket-trace-mode-map
  (racket--easy-keymap-define
   '((("." "RET") xref-find-definitions-other-window)
     ("n"         racket-trace-next)
     ("p"         racket-trace-previous)
     ("u"         racket-trace-up-level))))

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
  ;; xref
  (add-hook 'xref-backend-functions
            #'racket-trace-xref-backend-function
            nil t)
  (add-to-list 'semantic-symref-filepattern-alist
               '(racket-trace-mode "*.rkt" "*.rktd" "*.rktl")))

(defconst racket--trace-buffer-name "*Racket Trace*")

(defun racket--trace-get-buffer-create ()
  "Create buffer if necessary. Display it only when created."
  (unless (get-buffer racket--trace-buffer-name)
    (with-current-buffer (get-buffer-create racket--trace-buffer-name)
      (racket-trace-mode)
      (display-buffer (current-buffer)
                      '((display-buffer-below-selected
                         display-buffer-use-some-window)
                        (inhibit-same-window . t)))))
  (get-buffer racket--trace-buffer-name))

(defun racket--trace-on-notify (data)
  (with-current-buffer (racket--trace-get-buffer-create)
    (let* ((inhibit-read-only  t)
           (original-point     (point))
           (point-was-at-end-p (equal original-point (point-max))))
      (goto-char (point-max))
      (pcase data
        (`(,callp ,show ,name ,level
                  (,def-path ,def-line ,def-col ,_def-pos ,_def-span)
                  (,sig-path ,_sig-line ,_sig-col ,sig-pos ,sig-span)
                  (,call-path ,_call-line ,_call-col ,call-pos ,call-span)
                  (,ctx-path ,_ctx-line ,_ctx-col ,ctx-pos ,ctx-span))
         (racket--trace-insert callp
                               (if callp show (concat " ⇒ " show))
                               level
                               `(,name ,def-path ,def-line ,def-col)
                               `(,show ,sig-path ,sig-pos ,(+ sig-pos sig-span))
                               `(,show ,call-path ,call-pos ,(+ call-pos call-span))
                               `(,show ,ctx-path ,ctx-pos ,(+ ctx-pos ctx-span)))))
      (unless point-was-at-end-p
        (goto-char original-point)))))

(defun racket--trace-insert (callp str level xref signature caller context)
  (cl-loop for n to (1- level)
           do
           (insert
            (propertize "  "
                        'face `(:inherit default :background ,(racket--trace-level-color n))
                        'racket-trace-callp callp
                        'racket-trace-level level
                        'racket-trace-xref xref
                        'racket-trace-signature signature
                        'racket-trace-caller caller
                        'racket-trace-context context))
           finally
           (insert
            (propertize (concat str "\n")
                        'face `(:inherit default :background ,(racket--trace-level-color level))
                        'racket-trace-callp callp
                        'racket-trace-level level
                        'racket-trace-xref xref
                        'racket-trace-signature signature
                        'racket-trace-caller caller
                        'racket-trace-context context))))

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

(defun racket-trace ()
  "Create the `racket-trace-mode' buffer and select it in a window."
  (interactive)
  (select-window (get-buffer-window (racket--trace-get-buffer-create))))

(defun racket-trace-next ()
  (interactive)
  (forward-line 1)
  (racket--trace-back-to-sexp)
  (racket--trace-show-sites))

(defun racket-trace-previous ()
  (interactive)
  (forward-line -1)
  (racket--trace-back-to-sexp)
  (racket--trace-show-sites))

(defun racket-trace-up-level ()
  (interactive)
  (when (racket--trace-up-level)
    (racket--trace-back-to-sexp)
    (racket--trace-show-sites)))

(defun racket--trace-up-level ()
  (let ((orig (point)))
    (pcase (get-text-property (point) 'racket-trace-level)
      ((and (pred numberp) this-level)
       (let ((desired-level (1- this-level)))
         (cl-loop until (or (not (zerop (forward-line -1)))
                            (equal (get-text-property (point) 'racket-trace-level)
                                   desired-level)))
         (if (equal (get-text-property (point) 'racket-trace-level)
                    desired-level)
             t
           (goto-char orig)
           nil))))))

(defun racket--trace-back-to-sexp ()
  (back-to-indentation)
  (forward-sexp)
  (backward-sexp))

;;; Showing call values in situ

(defvar racket--trace-overlays nil
  "List of overlays we've added in various buffers.")

(defun racket-trace-before-change-function (_beg _end)
  (dolist (o racket--trace-overlays)
    (when (equal (overlay-buffer o) (current-buffer))
      (with-temp-buffer (move-overlay o 1 1)))))

(defun racket--trace-show-sites ()
  (dolist (o racket--trace-overlays)
    (delete-overlay o))
  (setq racket--trace-overlays nil)
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

(defun racket--trace-show-sites-at-point (winp)
  (let ((level (get-text-property (point) 'racket-trace-level))
        (callp (get-text-property (point) 'racket-trace-callp)))
    ;; Caller: Always create buffer if necessary. Always display its
    ;; buffer in a window, and move window point to call point. Just
    ;; check whether a signature overlay already exists at the
    ;; location, and if so, don't duplicate.
    (pcase (get-text-property (point) 'racket-trace-caller)
      (`(,show ,file ,beg ,end)
       (with-current-buffer (or (get-file-buffer file)
                                (let ((find-file-suppress-same-file-warnings t))
                                  (find-file-noselect file)))
         (when winp
           (let ((win (display-buffer (current-buffer)
                                      '((display-buffer-reuse-window
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
    ;; a window, don't show it. If already overlay here with same
    ;; beg/end, it's probably from "caller", above; don't create
    ;; another overlay next to it.
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
