;;; racket-show.el -*- lexical-binding: t -*-

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

(require 'racket-util)
(require 'racket-custom)
(require 'pos-tip)
(require 'cl-macs)

(defun racket-show (val &optional pos)
  "See the variable `racket-show-functions' for information about VAL and POS."
  (dolist (f racket-show-functions)
    (funcall f val pos)))

(defun racket-show-echo-area (v &optional _pos)
  "Show things in the echo area.

A value for the variable `racket-show-functions'."
  (let ((message-log-max nil))
    (if v
        (message "%s" v)
      (message ""))))

(defun racket-show-header-line (v &optional _pos)
  "Show things using a buffer header line.

A value for the variable `racket-show-functions'.

When there is nothing to show, keep a blank header-line. That
way, the buffer below doesn't \"jump up and down\" by a line as
messages appear and disappear. Only when V is nil do we remove
the header line."
  (setq-local header-line-format
              (and v (format "%s" (racket--only-first-line v)))))

(defun racket--only-first-line (str)
  (save-match-data
    (string-match (rx (group (* (not (any ?\n))))) str)
    (match-string 1 str)))

(defun racket-show-pos-tip (v &optional pos)
  "Show things using `pos-tip-show' if available.

A value for the variable `racket-show-functions'."
  (when (racket--pos-tip-available-p)
    (if (racket--non-empty-string-p v)
        (pos-tip-show v nil pos)
      (pos-tip-hide))))

(defun racket--pos-tip-available-p ()
  "Is `pos-tip' available and expected to work on current frame?"
  (and (fboundp 'x-hide-tip)
       (fboundp 'x-show-tip)
       (not (memq window-system (list nil 'pc)))))

(defvar-local racket--pseudo-tooltip-overlays nil)

(defun racket-show-pseudo-tooltip (v &optional pos)
  "Show using an overlay that resembles a tooltip.

This is nicer than `racket-show-pos-tip' because it:

  - Doesn't flicker while navigating.
  - Doesn't disappear after a timeout.
  - Performs well when `x-gtk-use-system-tooltips' is nil.

On the other hand, this does not look as nice when displaying
text that spans multiple lines. In that case, we simply
left-justify everything and do not draw any border."
  (racket--delete-pseudo-tooltip-overlays)
  (when (racket--non-empty-string-p v)
    (setq-local racket--pseudo-tooltip-overlays
                (racket--make-pseudo-tooltip-overlays v pos))))

(defun racket--delete-pseudo-tooltip-overlays ()
  (dolist (ov racket--pseudo-tooltip-overlays)
    (delete-overlay ov))
  (setq-local racket--pseudo-tooltip-overlays nil))

(defun racket--make-pseudo-tooltip-overlays (text pos)
  "Create one or more overlays for a pseudo tooltip, returning them in a list."
  (if (string-match-p "\n" text)
      ;; When text is multi-line, we don't try to simulate a tooltip,
      ;; exactly. Instead we simply "insert" the multiple lines left
      ;; justified, before the next line.
      (let* ((text (propertize (concat text "\n")
                               'face
                               `(:inherit default
                                 :foreground ,(face-foreground 'tooltip)
                                 :background ,(face-background 'tooltip))))
             (eol (save-excursion (goto-char pos) (point-at-eol)))
             (ov (make-overlay eol (1+ eol))))
        (overlay-put ov 'after-string text)
        (list ov))
    ;; Otherwise we simulate a tooltip displayed one line below pos,
    ;; and one column right (although it might start further left
    ;; depending on window-width) "over" any existing text.
    (pcase-let*
        ((text     (propertize (concat " " text " ")
                               'face
                               `(:inherit default
                                 :foreground ,(face-foreground 'tooltip)
                                 :background ,(face-background 'tooltip)
                                 :box (:line-width -1))))
         (text-len (length text))
         (bol      (save-excursion (goto-char pos) (point-at-bol)))
         (eol      (save-excursion (goto-char pos) (point-at-eol)))
         ;; Position the tooltip on the next line, indented to `pos'
         ;; -- but not so far it ends off right edge.
         (indent   (max 0 (min (- pos bol)
                               (- (window-width) text-len))))
         (beg      (+ eol indent 1))
         (next-eol (save-excursion (goto-char (1+ eol)) (point-at-eol))))
      ;; If the tip starts before next-eol, create an overlay with the
      ;; 'display property, covering the span of the tooltip text but
      ;; not beyond next-eol.
      ;;
      ;; As a further wrinkle, when the overlay does not cover the
      ;; entire rest of the line, our new text might not be exactly
      ;; the same pixel width as the text we replace -- causing the
      ;; remaining text to shift. This can happen e.g. due to Unicode
      ;; characters like λ. Furthermore, our replacement text can be
      ;; two pixels wider because :box (:line-width -1) doesn't seem
      ;; to work as advertised.
      ;;
      ;; To avoid this, we add _another_ overlay simply to replace the
      ;; character following our tooltip with a space of the necessary
      ;; pixel width to keep things aligned. Although covering the
      ;; character with a space isn't great -- even if you justify it
      ;; as a sort of "shadow" (?) -- it's better than having the
      ;; remainder of the line jiggle as the tooltip apears and
      ;; disappears.
      (if (< beg next-eol)
          (cl-labels ((text-pixel-width
                       (beg end)
                       (car (window-text-pixel-size nil beg end))))
            (let* ((end  (min next-eol (+ beg text-len)))
                   (ov   (make-overlay beg end))
                   (old  (text-pixel-width (1+ eol) end))
                   (_    (overlay-put ov 'display text))
                   (new  (text-pixel-width (1+ eol) end))
                   (diff (- new old)))
              (cons
               ov
               (when (and (not (zerop diff))
                          (< end next-eol))
                 (let* ((ov-spacer   (make-overlay end (1+ end)))
                        (width       (text-pixel-width end (1+ end)))
                        (space-width (abs (- width diff))))
                   (overlay-put ov-spacer
                                'display
                                `(space
                                  :width (,space-width)))
                   (list ov-spacer))))))
        ;; Else the tip starts after next-eol. So, create an overlay
        ;; on the newline, and use an after-string, where we prefix
        ;; enough blank spaces before the tooltip text itself to get
        ;; the desired indent.
        (let* ((ov (make-overlay (1- next-eol) next-eol))
               (blanks (make-string (- beg next-eol) 32)))
          (overlay-put ov 'after-string (concat blanks text))
          (list ov))))))

(provide 'racket-show)

;; racket-show.el ends here
