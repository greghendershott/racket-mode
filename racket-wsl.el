;;; racket-wsl.el -*- lexical-binding: t -*-

;; Copyright (c) 2020-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(defvar racket--wslpath (and (eq system-type 'gnu/linux)
                             (executable-find "wslpath")))

(defun racket--call-wsl-path (pathname flag)
  "Wrapper for wslpath.

When variable `racket--wslpath' is not nil, use it to convert
PATHNAME using FLAG.

  wslpath usage:
    -a  force result to absolute path format
    -u  translate from a Windows path to a WSL path (default)
    -w  translate from a WSL path to a Windows path
    -m  translate from a WSL path to a Windows path, with \"/\" instead of \"\\\"
"
  (if racket--wslpath
      (with-temp-buffer
        (let ((code (call-process racket--wslpath
                                  nil                    ;infile
                                  (list (current-buffer) ;output
                                        nil)             ;stderr
                                  nil                    ;display
                                  flag
                                  pathname)))
          (unless (zerop code)
            (error "%s %s %s exit code %s" racket--wslpath flag pathname code)))
        (buffer-substring-no-properties (point-min) (1- (point-max))))
    pathname))

(defun racket-wsl-to-windows (pathname)
  (racket--call-wsl-path pathname "-w"))

(defun racket-windows-to-wsl (pathname)
  (racket--call-wsl-path pathname "-u"))

(provide 'racket-wsl)

;; racket-wsl.el ends here
