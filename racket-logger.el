;;; racket-logger.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
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
     ("g"       racket-logger-clear)
     ("x"       racket-logger-exit)
     ("C-c C-z" racket-repl))))

(easy-menu-define racket-logger-mode-menu racket-logger-mode-map
  "Menu for Racket logger mode."
  '("Racket"
    ["Configure Topic and Level" racket-logger-topic-level]
    ["Toggle Truncate Lines" toggle-truncate-lines]
    "---"
    ["Switch to REPL" racket-repl]
    "---"
    ["Clear and Reconnect" racket-logger-clear]
    ["Exit Logger" racket-logger-exit]))

(define-derived-mode racket-logger-mode special-mode "Racket-Logger"
  "Major mode for Racket logger output.
\\<racket-logger-mode-map>
The customization variable `racket-logger-config' determines the
levels for topics. During a session you may change topic levels
using `racket-logger-topic-level', bound to
\"\\[racket-logger-topic-level]\".

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

```
\\{racket-logger-mode-map}
```
"
  (setq-local font-lock-defaults (list racket-logger-font-lock-keywords))
  (setq-local truncate-lines t))

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

(defun racket--font-lock-config (limit)
  "Handle multi-line font-lock of the configuration info."
  (ignore-errors
    (when (re-search-forward (concat "^" racket-logger--print-config-prefix) limit t)
      (let ((md (match-data)))
        (goto-char (match-end 0))
        (forward-sexp 1)
        (setf (elt md 1) (point)) ;; set (match-end 0)
        (set-match-data md)
        t))))

(defvar racket-logger--buffer-name "*Racket Logger*")
(defvar racket-logger--process nil)
(defvar racket-logger--connect-timeout 3)

(defun racket-logger--connect ()
  (unless racket-logger--process
   (with-temp-message "Connecting to logger process..."
     (with-timeout (racket-logger--connect-timeout
                    (error "Could not connect; try `racket-run' first"))
       (while (not racket-logger--process)
         (condition-case ()
             (setq racket-logger--process
                   (let ((process-connection-type nil)) ;use pipe not pty
                     (open-network-stream "racket-logger"
                                          (get-buffer-create racket-logger--buffer-name)
                                          "127.0.0.1"
                                          (1+ racket-command-port))))
           (error (sit-for 0.1)))))
     (racket-logger--activate-config)
     (set-process-sentinel racket-logger--process
                           #'racket-logger--process-sentinel))))

(defun racket-logger--process-sentinel (proc change)
  (funcall (process-filter proc) proc change) ;display in buffer
  (unless (memq (process-status proc) '(run open connect))
    (setq racket-logger--process nil)))

(defun racket-logger--disconnect ()
  (when racket-logger--process
    (with-temp-message "Disconnecting from logger process..."
      (set-process-sentinel racket-logger--process (lambda (_p _c)))
      (delete-process racket-logger--process)
      (setq racket-logger--process nil))))

(defconst racket-logger--print-config-prefix
  "racket-logger-config:\n")

(defun racket-logger--activate-config ()
  "Send config to Racket process, and, display it in the buffer."
  (process-send-string racket-logger--process
                       (format "%S" racket-logger-config))
  (funcall (process-filter racket-logger--process)
           racket-logger--process
           (propertize (concat racket-logger--print-config-prefix
                               (pp-to-string racket-logger-config))
                       'font-lock-multiline t)))

(defun racket-logger--set (topic level)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (unless (symbolp level) (error "LEVEL must be symbolp"))
  (pcase (assq topic racket-logger-config)
    (`() (add-to-list 'racket-logger-config (cons topic level)))
    (v   (setcdr v level)))
  (racket-logger--activate-config))

(defun racket-logger--unset (topic)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (when (eq topic '*)
    (user-error "Cannot unset the level for the '* topic"))
  (setq racket-logger-config
        (assq-delete-all topic racket-logger-config))
  (racket-logger--activate-config))

(defun racket-logger--topics ()
  "Effectively (sort (dict-keys racket-logger-config))."
  (sort (mapcar (lambda (x) (format "%s" (car x)))
                racket-logger-config)
        #'string<))

(defun racket-logger--topic-level (topic not-found)
  "Effectively (dict-ref racket-logger-config topic not-found)."
  (or (cdr (assq topic racket-logger-config))
      not-found))

;;; commands

(defun racket-logger ()
  "Create the `racket-logger-mode' buffer and connect to logger output.

If the `racket-repl-mode' buffer is displayed in a window, split
that window and put the logger in the bottom window. Otherwise,
use `pop-to-buffer'."
  (interactive)
  ;; Create buffer if necessary
  (unless (get-buffer racket-logger--buffer-name)
    (with-current-buffer (get-buffer-create racket-logger--buffer-name)
      (racket-logger-mode))
    (racket-logger--connect))
  ;; Give it a window if necessary
  (unless (get-buffer-window racket-logger--buffer-name)
    (pcase (get-buffer-window racket--repl-buffer-name)
      (`() (pop-to-buffer (get-buffer racket-logger--buffer-name)))
      (win (set-window-buffer (split-window win)
                              (get-buffer racket-logger--buffer-name)))))
  ;; Select the window
  (select-window (get-buffer-window racket-logger--buffer-name)))

(defun racket-logger-exit ()
  "Disconnect, kill the buffer, and delete the window."
  (interactive)
  (when (y-or-n-p "Disconnect and kill buffer? ")
    (racket-logger--disconnect)
    (kill-buffer)
    (delete-window)))

(defun racket-logger-clear ()
  "Clear the buffer and reconnect."
  (interactive)
  (when (y-or-n-p "Clear buffer and reconnect? ")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (racket-logger--disconnect)
    (racket-logger--connect)))

(defconst racket-logger--item-rx
  (rx bol ?\[ (0+ space) (or "fatal" "error" "warning" "info" "debug") ?\] space))

(defun racket-logger-next-item (&optional count)
  "Move point N items forward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item forward."
  (interactive "P")
  (forward-char 1)
  (if (re-search-forward racket-logger--item-rx nil t count)
      (beginning-of-line)
    (backward-char 1)))

(defun racket-logger-previous-item (&optional count)
  "Move point N items backward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item backward."
  (interactive "P")
  (re-search-backward racket-logger--item-rx nil t count))

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
                  (racket-logger--topics)))
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
                  (format "%s" (racket-logger--topic-level topic "*"))))
         (level  (pcase level
                   (""  nil)
                   ("*" nil)
                   (v   (intern v)))))
    (if level
        (racket-logger--set topic level)
      (racket-logger--unset topic))))

(provide 'racket-logger)

;;; racket-logger.el ends here
