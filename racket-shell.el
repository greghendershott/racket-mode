;;; racket-shell.el -*- lexical-binding: t -*-

;; Copyright (c) 2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-custom)
(require 'racket-util)
(require 'shell)
(require 'subr-x)
(require 'term)

(defun racket-racket ()
  "Use command-line racket to run the file.

Uses a shell or terminal buffer as specified by the configuration
variable `racket-shell-or-terminal-function'."
  (interactive)
  (racket--shell-or-terminal
   (concat (shell-quote-argument (racket--buffer-file-name)))))

(defun racket-raco-test ()
  "Use command-line raco test to run the \"test\" submodule.

Uses a shell or terminal buffer as specified by the configuration
variable `racket-shell-or-terminal-function'."
  (interactive)
  (racket--shell-or-terminal
   (concat "-l raco test -x "
           (shell-quote-argument (racket--buffer-file-name)))))

(defun racket--shell-or-terminal (args)
  (racket--save-if-changed)
  (let* ((command (if (stringp racket-program)
                      (list racket-program)
                    racket-program))
         (program (car command))
         (exe (shell-quote-argument
               (if (file-name-absolute-p program)
                   (expand-file-name program) ;handle e.g. ~/
                 program)))
         (flags (mapcar (lambda (x) (shell-quote-argument x))
                        (cdr command)))
         (cmd (concat exe " " (string-join flags " ") args))
         (win (selected-window)))
    (funcall racket-shell-or-terminal-function cmd)
    (select-window win)))

(defun racket-shell (cmd)
  "Run CMD using `shell'.

A value for the variable `racket-shell-or-terminal-function'."
  (let ((buf (shell)))
    (comint-simple-send buf cmd)))

(defun racket-term (cmd)
  "Run CMD using `term'.

A value for the variable `racket-shell-or-terminal-function'."
  (let ((buf (term (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))))
    (term-simple-send buf cmd)))

(defun racket-ansi-term (cmd)
  "Run CMD using `ansi-term'.

A value for the variable `racket-shell-or-terminal-function'."
  (let ((buf (ansi-term (or explicit-shell-file-name
                            (getenv "ESHELL")
                            (getenv "SHELL")
                            "/bin/sh"))))
    (term-simple-send buf cmd)))

(declare-function vterm "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-send-string "ext:vterm")

(defun racket-vterm (cmd)
  "Run CMD using `vterm', if that package is installed.

A value for the variable `racket-shell-or-terminal-function'."
  (unless (require 'vterm nil 'noerror)
    (error "Package 'vterm' is not available"))
  (vterm)
  (vterm-send-string cmd)
  (vterm-send-return))

(provide 'racket-shell)

;; racket-shell.el ends here
