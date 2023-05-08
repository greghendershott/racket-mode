;;; racket-show.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-util)
(require 'racket-custom)
(require 'pos-tip nil t) ;noerror
(require 'cl-macs)
(require 'face-remap)

(defun racket-show (str &optional pos transient-p)
  "Apply STR and POS to functions in the variable `racket-show-functions'.

See that for meaning of STR and POS.

When TRANSIENT-P, we automatically hide before the next command
runs. Otherwise, the UI might remain visible indefinitely --
depending on how a racket-show function displays --- until a
subsequent call to `racket-show' to hide or to show a new value.
Either behavior could be desirable depending on the caller's use
case. For example `racket-xp-mode' wants the display to remain
visible, if possible, even when the user chooses a command to
select another window; only point motion hides or shows a
different annotation."
  (unless (string-or-null-p str)
    (signal 'wrong-type-argument `(string-or-null-p ,str)))
  (when (racket--non-empty-string-p str)
    (unless (number-or-marker-p pos)
      (signal 'wrong-type-argument `(number-or-marker-p ,pos))))
  (run-hook-with-args 'racket-show-functions str pos)
  (if transient-p
      (add-hook 'pre-command-hook #'racket-show--pre-command-hook nil t)
    (remove-hook 'pre-command-hook #'racket-show--pre-command-hook t)))

(defun racket-show--pre-command-hook ()
  "Hide and remove ourselves as a pre-command-hook."
  (run-hook-with-args 'racket-show-functions "" nil)
  (remove-hook 'pre-command-hook #'racket-show--pre-command-hook t))

(defun racket-show-echo-area (str &optional _pos)
  "Show things in the echo area.

A value for the variable `racket-show-functions'.

This does /not/ add STR to the \"*Messages*\" log buffer."
  (when str
    (let ((message-log-max nil)) ;don't log
     (message "%s" str))))

(defun racket-show-header-line (str &optional _pos)
  "Show things using a buffer header line.

A value for the variable `racket-show-functions'.

When there is nothing to show, keep a blank header-line. That
way, the buffer below doesn't \"jump up and down\" by a line as
messages appear and disappear. Only when V is nil do we remove
the header line."
  (setq-local header-line-format
              (and str
                   (format "%s" (racket--only-first-line str)))))

(defun racket--only-first-line (str)
  (save-match-data
    (string-match (rx (group (* (not (any ?\n))))) str)
    (match-string 1 str)))

(defun racket-show-pos-tip (str &optional pos)
  "Show things using `pos-tip-show' if available.

A value for the variable `racket-show-functions'."
  (when (and (fboundp 'x-hide-tip)
             (fboundp 'x-show-tip)
             (not (memq window-system (list nil 'pc)))
             (fboundp 'pos-tip-show)
             (fboundp 'pos-tip-hide))
    (if (racket--non-empty-string-p str)
        (pos-tip-show str nil pos)
      (pos-tip-hide))))

(defvar-local racket--pseudo-tooltip-overlays nil)

(defun racket-show-pseudo-tooltip (str &optional pos)
  "Show using an overlay that resembles a tooltip.

This is nicer than `racket-show-pos-tip' because it:

  - Doesn't flicker while navigating.
  - Doesn't disappear after a timeout.
  - Performs well when `x-gtk-use-system-tooltips' is nil.

On the other hand, this does not look as nice when displaying
text that spans multiple lines or is too wide to fit the window.
In that case, we simply left-justify everything and do not draw
any border."
  (racket--delete-pseudo-tooltip-overlays)
  (when (racket--non-empty-string-p str)
    (setq-local racket--pseudo-tooltip-overlays
                (racket--make-pseudo-tooltip-overlays str pos))))

(defun racket--delete-pseudo-tooltip-overlays ()
  (dolist (ov racket--pseudo-tooltip-overlays)
    (delete-overlay ov))
  (setq-local racket--pseudo-tooltip-overlays nil))

(defun racket--make-pseudo-tooltip-overlays (text pos)
  "Create one or more overlays for a pseudo tooltip, returning them in a list."
  (if (or (string-match-p "\n" text)
          (< (window-width) (+ (string-width text) 2))
          (and text-scale-mode (< 0 text-scale-mode-amount)))
      ;; When text is multi-line or too wide, we don't try to simulate
      ;; a tooltip, exactly. Instead we simply "insert" left
      ;; justified, before the next line.
      (let* ((text (propertize (concat text "\n")
                               'face
                               `(:inherit default
                                 :foreground ,(face-foreground 'tooltip)
                                 :background ,(face-background 'tooltip))))
             (eol (racket--eol pos))
             (ov (make-overlay eol (1+ eol))))
        (overlay-put ov 'after-string text)
        (list ov))
    ;; Else we simulate a tooltip. The only question is where, and the
    ;; overlay(s) necessary to achieve that.
    (let*
        ((text (propertize (concat " " text " ")
                           'face
                           `(:inherit default
                             :foreground ,(face-foreground 'tooltip)
                             :background ,(face-background 'tooltip)
                             :box (:line-width -1))))
         (text-width (string-width text))
         (bol (racket--bol pos))
         (eol (racket--eol pos)))
      ;; If there is room after end of same line, show there.
      (if (< (+ text-width 1) (- (window-width) (- eol bol)))
          (let ((ov (make-overlay (1- eol) eol)))
            (overlay-put ov 'after-string (concat " " text))
            (list ov))
        ;; Otherwise we simulate a tooltip displayed one line below
        ;; pos, and one column right (although it might start further
        ;; left depending on window-width) "over" any existing text.
        (let*
            (;; Position the tooltip on the next line, indented to `pos'
             ;; -- but not so far it ends off right edge.
             (indent   (max 0 (min (- pos bol)
                                   (- (window-width) text-width 2))))
             (beg      (+ eol indent 1))
             (next-eol (racket--eol (1+ eol))))
          ;; If the tip starts before next-eol, create an overlay with
          ;; the 'display property, covering the span of the tooltip
          ;; text but not beyond next-eol.
          ;;
          ;; As a further wrinkle, when the overlay does not cover the
          ;; entire rest of the line, our new text might not be
          ;; exactly the same pixel width as the text we replace --
          ;; causing the remaining text to shift. This can happen e.g.
          ;; due to Unicode characters like λ. Furthermore, our
          ;; replacement text can be two pixels wider because :box
          ;; (:line-width -1) doesn't seem to work as advertised.
          ;;
          ;; To avoid this, we add _another_ overlay simply to replace
          ;; the character following our tooltip with a space of the
          ;; necessary pixel width to keep things aligned. Although
          ;; covering the character with a space isn't great -- even
          ;; if you justify it as a sort of "shadow" (?) -- it's
          ;; better than having the remainder of the line jiggle as
          ;; the tooltip apears and disappears.
          (if (< beg next-eol)
              (cl-flet ((text-pixel-width
                         (beg end)
                         (car (window-text-pixel-size nil beg end))))
                (let* ((end  (min next-eol (+ beg text-width)))
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
              (list ov))))))))

(defun racket--bol (pos)
  "Given POS return line beginning position."
  (save-excursion
    (goto-char pos)
    (if visual-line-mode
        (beginning-of-visual-line)
      (beginning-of-line))
    (point)))

(defun racket--eol (pos)
  "Given POS return line ending position."
  (save-excursion
    (goto-char pos)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))
    (point)))

(provide 'racket-show)

;; racket-show.el ends here
