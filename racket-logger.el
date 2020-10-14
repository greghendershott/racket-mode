;;; racket-logger.el -*- lexical-binding: t; -*-

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

(require 'easymenu)
(require 'rx)
(require 'racket-custom)
(require 'racket-repl)

;; Need to define this before racket-logger-mode
(defvar racket-logger-mode-map
  (racket--easy-keymap-define
   '(("l"       racket-logger-topic-level)
     ("w"       toggle-truncate-lines)
     ("n"       racket-logger-next-item)
     ("p"       racket-logger-previous-item)
     ("g"       racket-logger-clear))))

(easy-menu-define racket-logger-mode-menu racket-logger-mode-map
  "Menu for Racket logger mode."
  '("Racket-Logger"
    ["Configure Topic and Level" racket-logger-topic-level]
    ["Toggle Truncate Lines" toggle-truncate-lines]
    "---"
    ["Clear" racket-logger-clear]))

(defconst racket-logger-font-lock-keywords
  (eval-when-compile
    `((,#'racket--font-lock-config . racket-logger-config-face)
      (,(rx bol "[  fatal]")       . racket-logger-fatal-face)
      (,(rx bol "[  error]")       . racket-logger-error-face)
      (,(rx bol "[warning]")       . racket-logger-warning-face)
      (,(rx bol "[   info]")       . racket-logger-info-face)
      (,(rx bol "[  debug]")       . racket-logger-debug-face)
      (,(rx bol ?\[ (+? anything) ?\] space
            (group (+? anything) ?:) space)
       1 racket-logger-topic-face))))

(defconst racket--logger-print-config-prefix
  "racket-logger-config:\n")

(defun racket--font-lock-config (limit)
  "Handle multi-line font-lock of the configuration info."
  (ignore-errors
    (when (re-search-forward (concat "^" racket--logger-print-config-prefix) limit t)
      (let ((md (match-data)))
        (goto-char (match-end 0))
        (forward-sexp 1)
        (setf (elt md 1) (point)) ;; set (match-end 0)
        (set-match-data md)
        t))))

(define-derived-mode racket-logger-mode special-mode "Racket-Logger"
  "Major mode for Racket logger output.
\\<racket-logger-mode-map>

The customization variable `racket-logger-config' determines the
levels for topics. During a session you may change topic levels
using `racket-logger-topic-level'.

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

\\{racket-logger-mode-map}
"
  (setq-local font-lock-defaults (list racket-logger-font-lock-keywords))
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t) ;disable undo
  (setq-local window-point-insertion-type t))

(defconst racket--logger-buffer-name "*Racket Logger*")

(defun racket--logger-get-buffer-create ()
  "Create buffer if necessary. Do not display or select it."
  (unless (get-buffer racket--logger-buffer-name)
    (with-current-buffer (get-buffer-create racket--logger-buffer-name)
      (racket-logger-mode)
      (racket--logger-activate-config)))
  (get-buffer racket--logger-buffer-name))

(defun racket--logger-on-notify (str)
 (when noninteractive ;emacs --batch
    (princ (format "{racket logger}: %s" str)))
  (with-current-buffer (racket--logger-get-buffer-create)
    (let* ((inhibit-read-only  t)
           (original-point     (point))
           (point-was-at-end-p (equal original-point (point-max))))
      (goto-char (point-max))
      (insert str)
      (unless point-was-at-end-p
        (goto-char original-point)))))

(defun racket--logger-activate-config ()
  "Send config to logger and display it in the buffer."
  (racket--cmd/async nil
                     `(logger ,racket-logger-config))
  (with-current-buffer (get-buffer-create racket--logger-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (concat racket--logger-print-config-prefix
                                  (pp-to-string racket-logger-config))
                          'font-lock-multiline t))
      (goto-char (point-max)))))

(defun racket--logger-set (topic level)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (unless (symbolp level) (error "LEVEL must be symbolp"))
  (pcase (assq topic racket-logger-config)
    (`() (add-to-list 'racket-logger-config (cons topic level)))
    (v   (setcdr v level)))
  (racket--logger-activate-config))

(defun racket--logger-unset (topic)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (when (eq topic '*)
    (user-error "Cannot unset the level for the '* topic"))
  (setq racket-logger-config
        (assq-delete-all topic racket-logger-config))
  (racket--logger-activate-config))

(defun racket--logger-topics ()
  "Effectively (sort (dict-keys racket-logger-config))."
  (sort (mapcar (lambda (x) (format "%s" (car x)))
                racket-logger-config)
        #'string<))

(defun racket--logger-topic-level (topic not-found)
  "Effectively (dict-ref racket-logger-config topic not-found)."
  (or (cdr (assq topic racket-logger-config))
      not-found))

;;; commands

(defun racket-logger ()
  "Create the `racket-logger-mode' buffer."
  (interactive)
  (racket--logger-get-buffer-create)
  ;; Give it a window if necessary
  (unless (get-buffer-window racket--logger-buffer-name)
    (display-buffer (get-buffer racket--logger-buffer-name)))
  ;; Select the window
  (select-window (get-buffer-window racket--logger-buffer-name)))

(defun racket-logger-clear ()
  "Clear the buffer and reconnect."
  (interactive)
  (when (y-or-n-p "Clear buffer? ")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (racket--logger-activate-config)))

(defconst racket--logger-item-rx
  (rx bol ?\[ (0+ space) (or "fatal" "error" "warning" "info" "debug") ?\] space))

(defun racket-logger-next-item (&optional count)
  "Move point N items forward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item forward."
  (interactive "P")
  (forward-char 1)
  (if (re-search-forward racket--logger-item-rx nil t count)
      (beginning-of-line)
    (backward-char 1)))

(defun racket-logger-previous-item (&optional count)
  "Move point N items backward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item backward."
  (interactive "P")
  (re-search-backward racket--logger-item-rx nil t count))

(defun racket-logger-topic-level ()
  "Set or unset the level for a topic.

For convenience, input choices using `ido-completing-read'.

The topic labeled \"*\" is the level to use for all topics not
specifically assigned a level.

The level choice \"*\" means the topic will no longer have its
own level, therefore will follow the level specified for the
\"*\" topic."
  (interactive)
  (let* ((topic  (ido-completing-read
                  "Topic: "
                  (racket--logger-topics)))
         (topic  (pcase topic
                   ("" "*")
                   (v  v)))
         (topic  (intern topic))
         (levels (list "fatal" "error" "warning" "info" "debug"))
         (levels (if (eq topic '*) levels (cons "*" levels)))
         (level  (ido-completing-read
                  (format "Level for topic `%s': " topic)
                  levels
                  nil t nil nil
                  (format "%s" (racket--logger-topic-level topic "*"))))
         (level  (pcase level
                   (""  nil)
                   ("*" nil)
                   (v   (intern v)))))
    (if level
        (racket--logger-set topic level)
      (racket--logger-unset topic))))

(provide 'racket-logger)

;;; racket-logger.el ends here
