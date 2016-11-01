;;; racket-repl.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.
;; Image portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

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

(require 'racket-custom)
(require 'racket-common)
(require 'racket-util)
(require 'comint)
(require 'compile)
(require 'easymenu)

(defconst racket--repl-buffer-name/raw
  "Racket REPL"
  "The base buffer name, NOT surrounded in *stars*")
(defconst racket--repl-buffer-name
  (concat "*" racket--repl-buffer-name/raw "*")
  "The actual buffer name as created by comint-mode")

(defmacro with-racket-repl-buffer (&rest body)
  "Execute the forms in BODY with `racket-repl-mode' temporarily current.
The value returned is the value of the last form in BODY --
unless no `racket-repl-mode' buffer exists, in which case no BODY
forms are evaluated and nil is returned. See also
`with-current-buffer'."
  (declare (indent 0) (debug t))
  (let ((repl-buffer (make-symbol "repl-buffer")))
    `(let ((,repl-buffer (get-buffer racket--repl-buffer-name)))
       (when ,repl-buffer
         (with-current-buffer ,repl-buffer
           ,@body)))))

(defun racket--get-repl-buffer-process ()
  (get-buffer-process racket--repl-buffer-name))

(defun racket-repl--input-filter (str)
  "Don't save anything matching `racket-history-filter-regexp'."
  (not (string-match racket-history-filter-regexp str)))

(defun racket--get-old-input ()
  "Snarf the sexp ending at point."
  (if (looking-back comint-prompt-regexp (line-beginning-position))
      ""
    (save-excursion
      (let ((end (point)))
        (backward-sexp)
        (buffer-substring (point) end)))))

(defun racket-repl-eval-or-newline-and-indent ()
  "If complete sexpr, eval in Racket. Else do `racket-newline-and-indent'."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (cond ((not proc) (user-error "Current buffer has no process"))
          ((not (eq "" (racket--get-old-input)))
           (condition-case nil
               (let* ((beg (marker-position (process-mark proc)))
                      (end (save-excursion
                             (goto-char beg)
                             (forward-list) ;scan-error unless complete sexpr
                             (point))))
                 (comint-send-input)
                 ;; Remove comint-highlight-input face applied to
                 ;; input. I don't like how that looks.
                 (remove-text-properties beg end '(font-lock-face comint-highlight-input)))
             (scan-error (newline-and-indent)))))))

;;;###autoload
(defun racket-repl (&optional noselect)
  "Run the Racket REPL and display its buffer in some window.

If the Racket process is not already running, it is started.

If NOSELECT is not nil, does not select the REPL
window (preserves the originally selected window).

Commands that don't want the REPL to be displayed can instead use
`racket--repl-ensure-buffer-and-process'."
  (interactive "P")
  (racket--repl-ensure-buffer-and-process t)
  (unless noselect
    (select-window (get-buffer-window racket--repl-buffer-name t))))

(defconst racket--minimum-required-version "6.0"
  "The minimum version of Racket required by run.rkt.

Although some functionality may require an even newer version of
Racket, run.rkt will handle that via `dynamic-require` and
fallbacks. The version number here is a baseline for run.rkt to
be able to load at all.")

(defvar racket--run.rkt
  (expand-file-name "run.rkt"
                    (file-name-directory (or load-file-name
                                             (racket--buffer-file-name))))
  "Path to run.rkt")

(defun racket--repl-live-p ()
  "Does the Racket REPL buffer exist and have a live Racket process?"
  (comint-check-proc racket--repl-buffer-name))

(defun racket--repl-ensure-buffer-and-process (&optional display)
  "Ensure Racket REPL buffer exists and has live Racket process.

If the Racket process is not already running, it is started and
the buffer is put in `racket-repl-mode'.

Non-nil DISPLAY means `display-buffer'.

Never changes selected window."
  (if (comint-check-proc racket--repl-buffer-name)
      (when display
        (display-buffer racket--repl-buffer-name))
    (racket--require-version racket--minimum-required-version)
    (with-current-buffer
        (with-temp-message "Starting Racket process..."
         (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                      racket-racket-program
                      nil
                      racket--run.rkt
                      (number-to-string racket-command-port)))
      ;; Display now so users see startup and banner sooner.
      (when display
        (display-buffer (current-buffer)))
      ;; The following is needed to make e.g. λ work when pasted
      ;; into the comint-buffer, both directly by the user and via
      ;; the racket--repl-eval functions.
      (set-process-coding-system (get-buffer-process racket--repl-buffer-name)
                                 'utf-8 'utf-8)
      (racket-repl-mode)
      (racket--repl-command-connect))))

(defun racket--version ()
  "Get the `racket-racket-program' version as a string."
  (with-temp-message "Checking Racket version..."
    (with-temp-buffer
      (call-process racket-racket-program
                    nil                  ;infile: none
                    t                    ;destination: current-buffer
                    nil                  ;redisplay: no
                    "-e"
                    "(version)")
      (eval (read (buffer-substring (point-min) (point-max)))))))

(defun racket--require-version (at-least)
  "Raise a `user-error' unless Racket is version AT-LEAST."
  (let ((have (racket--version)))
    (unless (version<= at-least have)
      (user-error "racket-mode requires at least Racket version %s but you have %s"
                  at-least have))
    t))

(defvar racket--repl-command-process nil)
(defvar racket--repl-command-connect-timeout 30)

(defun racket--repl-command-connect ()
  "Connect to the Racket command server.
If already connected, disconnects then connects again."
  (racket--repl-command-disconnect)
  (with-temp-message "Connecting to command server..."
    ;; The command server may not be ready -- Racket itself and our
    ;; backend are still starting up -- so retry until timeout.
    (with-timeout (racket--repl-command-connect-timeout
                   (error "Could not connect to command server"))
      (while (not racket--repl-command-process)
        (condition-case ()
            (setq racket--repl-command-process
                  (let ((process-connection-type nil)) ;use pipe not pty
                    (open-network-stream "racket-command"
                                         (get-buffer-create "*racket-command-output*")
                                         "127.0.0.1"
                                         racket-command-port)))
          (error (sit-for 0.1)))))))

(defun racket--repl-command-disconnect ()
  "Disconnect from the Racket command server. "
  (when racket--repl-command-process
    (with-temp-message "Deleting existing connection to command server..."
      (delete-process racket--repl-command-process)
      (setq racket--repl-command-process nil))))

(defun racket--repl-command (fmt &rest xs)
  "Send command to the Racket process and return the response sexp.
Do not prefix the command with a `,'. Not necessary to append \n."
  (racket--repl-ensure-buffer-and-process)
  (let ((proc racket--repl-command-process))
    (unless proc
      (error "Command server process is nil"))
    (with-current-buffer (process-buffer proc)
      (delete-region (point-min) (point-max))
      (process-send-string proc
                           (concat (apply #'format (cons fmt xs))
                                   "\n"))
      (with-timeout (racket-command-timeout
                     (error "Command server timeout"))
        ;; While command server running and not yet complete sexp
        (while (and (memq (process-status proc) '(open run))
                    (or (= (point) (point-min))
                        (condition-case ()
                            (progn (scan-lists (point-min) 1 0) nil)
                          (scan-error t))))
          (accept-process-output nil 0.1)))
      (cond ((not (memq (process-status proc) '(open run)))
             (error "Racket command process: died"))
            ((= (point-min) (point))
             (error "Racket command process: Empty response"))
            (t
             (let ((result (buffer-substring (point-min) (point-max))))
               (delete-region (point-min) (point-max))
               (eval (read result))))))))

(defun racket-repl-file-name ()
  "Return the file running in the buffer, or nil.

The result can be nil if the REPL is not started, or if it is
running no particular file as with the `,top` command."
  (when (comint-check-proc racket--repl-buffer-name)
    (racket--repl-command "path")))

(defun racket--in-repl-or-its-file-p ()
  "Is current-buffer `racket-repl-mode' or buffer for file active in it?"
  (or (eq (current-buffer) (get-buffer racket--repl-buffer-name))
      (string= (buffer-file-name) (racket-repl-file-name))))

(defun racket-repl-switch-to-edit ()
  "Switch to the window for the buffer of the file running in the REPL.

If no buffer is visting the file, `find-file' it in `other-window'.

If the REPL is running no file -- if the prompt is `>` -- use the
most recent `racket-mode' buffer, if any."
  (interactive)
  (let ((path (racket-repl-file-name)))
    (if path
        (let ((buffer (find-buffer-visiting path)))
          (if buffer
              (pop-to-buffer buffer t)
            (other-window 1)
            (find-file path)))
      (let ((buffer (racket--most-recent-racket-mode-buffer)))
        (unless buffer
          (user-error "There are no racket-mode buffers"))
        (pop-to-buffer buffer t)))))

(defun racket--most-recent-racket-mode-buffer ()
  (cl-some (lambda (b)
             (with-current-buffer b
               (and (eq major-mode 'racket-mode) b)))
           (buffer-list)))

(defun racket--repl-eval (fmt &rest vs)
  "Eval expression in the *Racket REPL* buffer.
Allow Racket process output to be displayed, and show the window.
Intended for use by things like ,run command."
  (racket-repl t)
  (racket--repl-forget-errors)
  (comint-send-string (racket--get-repl-buffer-process)
                      (apply #'format (cons fmt vs)))
  (racket--repl-show-and-move-to-end))

;;; send to REPL

(defun racket--send-region-to-repl (start end)
  "Internal function to send the region to the Racket REPL.

Before sending the region, call `racket-repl' and
`racket--repl-forget-errors'. Also insert a ?\n at the process
mark so that output goes on a fresh line, not on the same line as
the prompt.

Afterwards call `racket--repl-show-and-move-to-end'."
  (when (and start end)
    (racket-repl t)
    (racket--repl-forget-errors)
    (let ((proc (racket--get-repl-buffer-process)))
      (with-racket-repl-buffer
        (save-excursion
          (goto-char (process-mark proc))
          (insert ?\n)
          (set-marker (process-mark proc) (point))))
      (comint-send-region proc start end)
      (comint-send-string proc "\n"))
    (racket--repl-show-and-move-to-end)))

(defun racket-send-region (start end)
  "Send the current region (if any) to the Racket REPL."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (racket--send-region-to-repl start end))

(defun racket-send-definition ()
  "Send the current definition to the Racket REPL."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (racket--send-region-to-repl (point) end))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the Racket REPL.

When the previous sexp is a sexp comment the sexp itself is sent,
without the #; prefix."
  (interactive)
  (racket--send-region-to-repl (save-excursion
                                 (backward-sexp)
                                 (if (save-match-data (looking-at "#;"))
                                     (+ (point) 2)
                                   (point)))
                               (point)))

(defun racket--repl-forget-errors ()
  "Forget existing errors in the REPL.

Although they remain clickable they will be ignored by
`next-error' and `previous-error'"
  (with-racket-repl-buffer
    (compilation-forget-errors)
    ;; `compilation-forget-errors' may have just set
    ;; `compilation-messages-start' to a marker at position 1. But in
    ;; that case process output (including error messages) will be
    ;; inserted ABOVE the marker, in which case `next-error' won't see
    ;; them. Instead use a non-marker position like 1 or use nil.
    (when (and (markerp compilation-messages-start)
               (equal (marker-position compilation-messages-start) 1)
               (equal (marker-buffer compilation-messages-start) (current-buffer)))
      (setq compilation-messages-start nil))))

(defun racket--repl-show-and-move-to-end ()
  "Make the Racket REPL visible, and move point to end.
Keep original window selected."
  (display-buffer racket--repl-buffer-name)
  (save-selected-window
    (select-window (get-buffer-window racket--repl-buffer-name t))
    (comint-show-maximum-output)))

;;; Inline images in REPL

(defvar racket-image-cache-dir nil)

(defun racket-repl--list-image-cache ()
  "List all the images in the image cache."
  (and racket-image-cache-dir
       (file-directory-p racket-image-cache-dir)
       (let ((files (directory-files-and-attributes
                     racket-image-cache-dir t "racket-image-[0-9]*.png")))
         (mapcar 'car
                 (sort files (lambda (a b)
                               (< (float-time (nth 6 a))
                                  (float-time (nth 6 b)))))))))

(defun racket-repl--clean-image-cache ()
  "Clean all except for the last `racket-images-keep-last'
images in 'racket-image-cache-dir'."
  (interactive)
  (dolist (file (butlast (racket-repl--list-image-cache)
                         racket-images-keep-last))
    (delete-file file)))

(defun racket-repl--replace-images ()
  "Replace all image patterns with actual images"
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\"#<Image: \\([-+./_0-9a-zA-Z]+\\)>\"" nil t)
        ;; can't pass a filename to create-image because emacs might
        ;; not display it before it gets deleted (race condition)
        (let* ((file (match-string 1))
               (begin (match-beginning 0))
               (end (match-end 0)))
          (delete-region begin end)
          (goto-char begin)
          (if (and racket-images-inline (display-images-p))
              (insert-image (create-image file) "[image]")
            (goto-char begin)
            (insert "[image] ; use M-x racket-view-last-image to view"))
          (setq racket-image-cache-dir (file-name-directory file))
          (racket-repl--clean-image-cache))))))

(defun racket-view-last-image (n)
  "Open the last displayed image using `racket-images-system-viewer'.

With prefix arg, open the N-th last shown image."
  (interactive "p")
  (let ((images (reverse (racket-repl--list-image-cache))))
    (if (>= (length images) n)
        (start-process "Racket image view"
                       nil
                       racket-images-system-viewer
                       (nth (- n 1) images))
      (error "There aren't %d recent images" n))))

(defun racket-repl--output-filter (txt)
  (racket-repl--replace-images))

;;; racket-repl-mode

(defvar racket-repl-mode-map
  (racket--easy-keymap-define
   '(("RET"             racket-repl-eval-or-newline-and-indent)
     ("TAB"             indent-for-tab-command)
     ("M-C-u"           racket-backward-up-list)
     ("C-a"             comint-bol)
     ("C-w"             comint-kill-region)
     ("[C-S-backspace]" comint-kill-whole-line)
     ("["               racket-smart-open-bracket)
     (")"               racket-insert-closing)
     ("]"               racket-insert-closing)
     ("}"               racket-insert-closing)
     ("C-c C-e x"       racket-expand-definition)
     ("C-c C-e e"       racket-expand-last-sexp)
     ("C-c C-e r"       racket-expand-region)
     ("C-c C-e a"       racket-expand-again)
     ("M-C-y"           racket-insert-lambda)
     ("C-c C-d"         racket-doc)
     ("C-c C-."         racket-describe)
     ("M-."             racket-visit-definition)
     ("C-M-."           racket-visit-module)
     ("M-,"             racket-unvisit)
     ("C-c C-z"         racket-repl-switch-to-edit)))
  "Keymap for Racket REPL mode.")

(easy-menu-define racket-repl-mode-menu racket-repl-mode-map
  "Menu for Racket REPL mode."
  '("Racket"
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    ("Macro Expand"
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp]
     "---"
     ["Again" racket-expand-again])
    "---"
    ["Visit Definition" racket-visit-definition]
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Racket Documentation" racket-doc]
    ["Describe" racket-describe]
    "---"
    ["Switch to Edit Buffer" racket-repl-switch-to-edit]))

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.
\\{racket-repl-mode-map}"
  (racket--variables-for-both-modes)
  (setq-local comint-prompt-regexp (rx (regexp "^[^>\n]*") "\ufeff> "))
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-read-only nil)
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter #'racket-repl--input-filter)
  (add-hook 'comint-output-filter-functions #'racket-repl--output-filter nil t)
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   '(;; error
     ("^;?[ ]*\\([^ :]+\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" 1 2 3)
     ;; contract
     ("^;?[ ]*at:[ ]+\\([^ :]+\\):\\([0-9]+\\)[.]\\([0-9]+\\)$" 1 2 3)
     ;; rackunit check-xxx
     ("#<path:\\([^>]+\\)> \\([0-9]+\\) \\([0-9]+\\)" 1 2 3)
     ;;rackunit/text-ui test-suite
     ("^location:[ ]+\\(\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\)" 2 3 4 2 1)
     ;; path struct
     ("#<path:\\([^>]+\\)>" 1 nil nil 0)
     ))
  (setq-local comint-get-old-input #'racket--get-old-input))

(provide 'racket-repl)

;; racket-repl.el ends here
