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
                         display-buffer-pop-up-window
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
        (`(,kind ,show ,name ,level
                 (,def-path ,def-line ,def-col ,_def-pos ,_def-span)
                 (,call-path ,_call-line ,_call-col ,call-pos ,call-span))
         (racket--trace-insert kind
                               (if (equal kind "call") show (concat " ⇒ " show))
                               level
                               `(,name ,def-path ,def-line ,def-col)
                               `(,show ,call-path ,call-pos ,(+ call-pos call-span)))))
      (unless point-was-at-end-p
        (goto-char original-point)))))

(defun racket--trace-insert (kind str level xref caller)
  (cl-loop for n to (1- level)
           do
           (insert
            (propertize "  "
                        'face `(:inherit default :background ,(racket--trace-level-color n))
                        'racket-trace-kind kind
                        'racket-trace-level level
                        'racket-trace-xref xref
                        'racket-trace-caller caller))
           finally
           (insert
            (propertize (concat str "\n")
                        'face `(:inherit default :background ,(racket--trace-level-color level))
                        'racket-trace-kind kind
                        'racket-trace-level level
                        'racket-trace-xref xref
                        'racket-trace-caller caller))))

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
  (racket--trace-next-or-previous 1))

(defun racket-trace-previous ()
  (interactive)
  (racket--trace-next-or-previous -1))

(defun racket-trace-up-level ()
  (interactive)
  (pcase (get-text-property (point) 'racket-trace-level)
    ((and (pred numberp) level)
     (while (and (= 0 (forward-line -1))
                 (pcase (get-text-property (point) 'racket-trace-level)
                   ((and (pred numberp) n)
                    (not (= n (1- level))))))
       nil)))
  (racket--trace-back-to-sexp))

(defvar racket--trace-overlay (with-temp-buffer (make-overlay 1 1)))

(defun racket--trace-next-or-previous (amt)
  (forward-line amt)
  (racket--trace-back-to-sexp)
  (pcase (get-text-property (point) 'racket-trace-caller)
    (`(,show ,file ,beg ,end)
     (let ((callp (equal (get-text-property (point) 'racket-trace-kind)
                         "call")))
       (with-current-buffer (or (get-file-buffer file)
                                (let ((find-file-suppress-same-file-warnings t))
                                  (find-file-noselect file)))
         (let ((win (display-buffer (current-buffer)
                                    '((display-buffer-reuse-window
                                       display-buffer-below-selected
                                       display-buffer-use-some-window)
                                      (inhibit-same-window . t)))))
           (save-selected-window
             (select-window win)
             (goto-char beg)))
         (let ((o racket--trace-overlay))
           (move-overlay o beg end)
           (overlay-put o 'display (if callp "" t))
           (overlay-put o 'after-string
                        (propertize (if callp show (concat "⇒ " show))
                                    'face '(:inherit default :box (:line-width -1))))
           (overlay-put o 'face '(:inherit default :box (:line-width -1))))
         (pulse-momentary-highlight-region beg end))))))

(defun racket--trace-back-to-sexp ()
  (back-to-indentation)
  (forward-sexp)
  (backward-sexp))

(provide 'racket-trace)

;;; racket-trace.el ends here
