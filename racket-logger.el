;;; racket-logger.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'compat) ;for text-property-search-{forward backward}
(require 'easymenu)
(require 'rx)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-back-end)

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
  (setq-local font-lock-defaults (list nil t)) ;no font lock
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t) ;disable undo
  (setq-local window-point-insertion-type t))

(defun racket--logger-buffer-name (&optional back-end-name)
  (format "*Racket Logger <%s>*" (or back-end-name
                                     (racket-back-end-name))))

(defun racket--logger-get-buffer-create (&optional back-end-name)
  "Create buffer if necessary. Do not display or select it."
  (let ((name (racket--logger-buffer-name back-end-name)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (racket-logger-mode)
        (racket--logger-activate-config)))
    (get-buffer name)))

(defun racket--logger-on-notify (back-end-name v)
  "This is called from `racket--cmd-dispatch-response'.

As a result, we might create this buffer before the user does a
`racket-logger-mode' command."
  (when noninteractive ;emacs --batch
    (princ (format "{logger %s}: %s"
                   (racket-back-end-name)
                   v)))
  (with-current-buffer (racket--logger-get-buffer-create back-end-name)
    (pcase-let* ((`(,level ,topic ,message) v)
                 (`(,level-str . ,level-face)
                  (pcase level
                    ('fatal   (cons "[  fatal]" racket-logger-fatal-face))
                    ('error   (cons "[  error]" racket-logger-error-face))
                    ('warning (cons "[warning]" racket-logger-warning-face))
                    ('info    (cons "[   info]" racket-logger-info-face))
                    ('debug   (cons "[  debug]" racket-logger-debug-face))))
                 (inhibit-read-only  t)
                 (original-point     (point))
                 (point-was-at-end-p (equal original-point (point-max))))
      (goto-char (point-max))
      (insert (propertize level-str
                          'face level-face
                          'racket-logger-item-level t)
              " "
              (propertize (symbol-name topic)
                          'face racket-logger-topic-face)
              ": "
              message
              "\n")
      (unless point-was-at-end-p
        (goto-char original-point)))))

(defun racket--logger-activate-config ()
  "Send config to logger and display it in the buffer."
  (racket--cmd/async nil
                     `(logger ,racket-logger-config))
  (with-current-buffer (racket--logger-get-buffer-create)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (concat "racket-logger-config:\n"
                                  (pp-to-string racket-logger-config))
                          'face racket-logger-config-face))
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
  (unless (get-buffer-window (racket--logger-buffer-name))
    (display-buffer (get-buffer (racket--logger-buffer-name))))
  ;; Select the window
  (select-window (get-buffer-window (racket--logger-buffer-name))))

(defun racket-logger-clear ()
  "Clear the buffer and reconnect."
  (interactive)
  (when (eq major-mode 'racket-logger-mode)
    (when (y-or-n-p "Clear buffer? ")
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (racket--logger-activate-config))))

(defun racket-logger-next-item (&optional count)
  "Move point forward COUNT logger output items.

Interactively, COUNT is the numeric prefix argument. If COUNT is
omitted or nil, move point 1 item forward."
  (interactive "p")
  (let* ((count (or count 1))
         (step (if (< 0 count) -1 1))
         (search (if (< 0 count)
                     #'text-property-search-forward
                   #'text-property-search-backward)))
    (while (not (zerop count))
      (let ((match (funcall search 'racket-logger-item-level t t t)))
        (if (not match)
            (setq count 0)
          (goto-char (prop-match-beginning match))
          (setq count (+ count step)))))))

(defun racket-logger-previous-item (&optional count)
  "Move point backward COUNT logger output items.

Interactively, COUNT is the numeric prefix argument. If COUNT is
omitted or nil, move point 1 item backward."
  (interactive "p")
  (racket-logger-next-item (if count (- count) -1)))

(defun racket-logger-topic-level ()
  "Set or unset the level for a topic.

The topic labeled \"*\" is the level to use for all topics not
specifically assigned a level.

The level choice \"*\" means the topic will no longer have its
own level, therefore will follow the level specified for the
\"*\" topic."
  (interactive)
  (let* ((topic  (completing-read
                  "Topic: "
                  (racket--logger-topics)))
         (topic  (pcase topic
                   ("" "*")
                   (v  v)))
         (topic  (intern topic))
         (levels (list "fatal" "error" "warning" "info" "debug"))
         (levels (if (eq topic '*) levels (cons "*" levels)))
         (level  (completing-read
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
