;;; racket-repl.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2018 by Greg Hendershott.
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
(require 'tq)

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

(defconst rx-whitespace-or-comment
  (rx (1+ (or (syntax whitespace)
              (syntax comment-start)
              (syntax comment-end)))))

(defun racket-repl-eval-or-newline-and-indent ()
  "Evaluate or `newline-and-indent'.

If you've supplied a complete s-expression, it is submitted to
Racket to be evaluated. Otherwise, this does
`newline-and-indent`.

- This is most useful when you are _not_ using something like
  Paredit: Until you type the closing paren, you can use RET to
  `newline-and-indent'.

- If you _are_ using something like Paredit: You will probably
  always have a complete s-expression. To enter one s-expression
  over multiple lines, use something like C-j.

If you supply more than one s-expression, only the first is
evaluated by Racket. The remainder move down to the next prompt,
where you can evaluate the next one by pressing RET again."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless proc (user-error "Current buffer has no process"))
    (condition-case nil
        (let* ((beg (marker-position (process-mark proc)))
               (end (save-excursion
                      (goto-char beg)
                      (forward-list 1) ;scan-error unless complete sexpr
                      (point))))
          (unless (or (equal beg end)
                      (all-space-p beg end))
            (goto-char end)
            (let ((comint-eol-on-send nil))
              (comint-send-input))
            ;; Remove comint-highlight-input face applied to
            ;; input. I don't like how that looks.
            (remove-text-properties beg end '(font-lock-face comint-highlight-input))
            ;; If there was any trailing space after the sexpr just
            ;; sent, it's now sitting at the prompt, possibly before
            ;; more sexprs. Delete it.
            (goto-char (marker-position (process-mark proc)))
            (save-match-data
              (when (looking-at rx-whitespace-or-comment)
                (delete-region (match-beginning 0) (match-end 0))))))
      (scan-error (newline-and-indent)))))

(defun all-space-p (beg end)
  (save-excursion
    (goto-char beg)
    (save-match-data
      (equal end (re-search-forward rx-whitespace-or-comment end t)))))

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
        (with-temp-message (format "Starting %s to run racket-mode %s ..."
                                   racket-program
                                   racket--run.rkt)
         (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                      racket-program
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
      (racket--repl-command-connect-start))))

(defun racket--version ()
  "Get the `racket-program' version as a string."
  (with-temp-message "Checking Racket version ..."
    (with-temp-buffer
      (call-process racket-program nil t nil "--version")
      (goto-char (point-min))
      ;; Welcome to Racket v6.12.
      ;; Welcome to Racket v7.0.0.6.
      (save-match-data
        (re-search-forward "[0-9]+\\(?:\\.[0-9]+\\)*")
        (match-string 0)))))

(defun racket--require-version (at-least)
  "Raise a `user-error' unless Racket is version AT-LEAST."
  (let ((have (racket--version)))
    (unless (version<= at-least have)
      (user-error "racket-mode requires at least Racket version %s but you have %s"
                  at-least have))
    t))

(defvar racket--repl-command-tq nil
  "A `tq' object when connection to the command server is established.")
(defvar racket--repl-command-connecting-timer nil
  "For `run-with-timer' to retry connection attempts.")
(defvar racket--repl-command-connect-timeout 30
  "How long should `racket--repl-command-finish' wait.")

(defun racket--repl-command-connect-start ()
  "Start to connect to the Racket command process.

If already connected, disconnects first.

The command server may might not be ready to accept connections,
because Racket itself and our backend are still starting up. As a
result, this will keep attempting to connect \"in the
background\" using `run-with-timer'. So after calling this, call
`racket--repl-command-connect-finish' to wait for the connection
to be established."
  (racket--repl-command-disconnect)
  (setq
   racket--repl-command-connecting-timer
   (run-with-timer
    0.5                                 ;initial delay
    0.1                                 ;repeat interval
    (lambda ()
      ;; If `open-network-stream' fails, we return from this
      ;; repeating timer lambda. Else if it succeeds, we
      ;; `cancel-timer'.
      (ignore-errors
        (let ((proc
               (let ((process-connection-type nil)) ;use pipe not pty
                 (open-network-stream "racket-command"
                                      nil
                                      "127.0.0.1"
                                      racket-command-port))))
          (setq racket--repl-command-tq (tq-create proc))
          (set-process-filter proc
                              (lambda (_proc string)
                                (racket--repl-command-process-filter
                                 racket--repl-command-tq
                                 string)))
          (cancel-timer racket--repl-command-connecting-timer)
          (setq racket--repl-command-connecting-timer nil)))))))

(defun racket--repl-command-connect-finish ()
  "Waits until connection established."
  (with-timeout (racket--repl-command-connect-timeout
                 (cancel-timer racket--repl-command-connecting-timer)
                 (setq racket--repl-command-connecting-timer nil)
                 (error "Could not connect to racket-mode command process"))
   (while (not racket--repl-command-tq)
     (message "Still trying to connect to racket-mode command process on port %s ..."
              racket-command-port)
     (sit-for 0.2))))

(defun racket--repl-command-disconnect ()
  "Disconnect from the Racket command process."
  (when racket--repl-command-tq
    (with-temp-message "Deleting existing connection to command process ..."
      (tq-close racket--repl-command-tq)
      (setq racket--repl-command-tq nil))))

(defun racket--repl-command-process-filter (tq string)
  "Like `tq-filter' and `tq-process-buffer' but using sexps not regexps."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (tq-queue-empty tq)
            (delete-region (point-min) (point-max)) ;spurious
          (goto-char (point-max))
          (insert string)
          (goto-char (point-min))
          (condition-case ()
              (progn
                (forward-sexp 1)
                (let ((sexp (buffer-substring (point-min) (point))))
                  (delete-region (point-min) (point))
                  (unwind-protect
                      (ignore-errors
                        (funcall (tq-queue-head-fn tq)
                                 (eval (read sexp))))
                    (tq-queue-pop tq))))
            (scan-error nil)))))))

(defun racket--repl-command-async (callback fmt &rest xs)
  "Send command to the Racket process and give `callback' the response sexp.
Do not prefix the command with a `,'. Not necessary to append \n."
  (racket--repl-ensure-buffer-and-process nil)
  (racket--repl-command-connect-finish)
  (tq-enqueue racket--repl-command-tq
              (concat (apply #'format (cons fmt xs)) "\n")
              'n/a
              'n/a
              callback
              t))

(defun racket--repl-command (fmt &rest xs)
  "Send command to the Racket process, await and return the response sexp.
Do not prefix the command with a `,'. Not necessary to append \n.

Really just a wrapper around `racket--repl-command-async'."
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (result awaiting))
    (apply #'racket--repl-command-async
           (lambda (v) (setq result v))
           fmt
           xs)
    (with-timeout (racket-command-timeout
                   (error "Command process timeout"))
      (while (eq result awaiting)
        (sit-for 0.1))
      result)))

(defun racket-repl-file-name ()
  "Return the file running in the buffer, or nil.

The result can be nil if the REPL is not started, or if it is
running no particular file as with the `,top` command.

On Windows this will replace \ with / in an effort to match the
Unix style names used by Emacs on Windows."
  (when (comint-check-proc racket--repl-buffer-name)
    (let ((path (racket--repl-command "path")))
      (and path
           (cl-case system-type
             (windows-nt (subst-char-in-string ?\\ ?/ path))
             (otherwise  path))))))

(defun racket--in-repl-or-its-file-p ()
  "Is current-buffer `racket-repl-mode' or buffer for file active in it?"
  (or (eq (current-buffer)
          (get-buffer racket--repl-buffer-name))
      (string-equal (racket--buffer-file-name)
                    (racket-repl-file-name))))

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

Before sending the region, calls `racket-repl' and
`racket--repl-forget-errors'. Also inserts a ?\n at the process
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
  (racket--send-region-to-repl (racket--repl-last-sexp-start)
                               (point)))

(defun racket-eval-last-sexp ()
  "Eval the previous sexp asynchronously and `message' the result."
  (interactive)
  (racket--repl-command-async
   #'message
   (concat "eval-sexp "
           (buffer-substring-no-properties (racket--repl-last-sexp-start)
                                           (point)))))

(defun racket--repl-last-sexp-start ()
  (save-excursion
    (condition-case ()
        (progn
          (backward-sexp)
          (if (save-match-data (looking-at "#;"))
              (+ (point) 2)
            (point)))
      (scan-error (user-error "There isn't a complete s-expression before point")))))

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
      (while (re-search-forward  "\"#<Image: \\(.+racket-image-.+\\.png\\)>\"" nil t)
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

(defun racket-repl--output-filter (_txt)
  (racket-repl--replace-images))

;;; racket-repl-mode

(defvar racket-repl-mode-map
  (racket--easy-keymap-define
   '(("RET"             racket-repl-eval-or-newline-and-indent)
     ("TAB"             indent-for-tab-command)
     ("C-M-u"           racket-backward-up-list)
     ("C-M-q"           prog-indent-sexp)
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
     ("C-c C-z"         racket-repl-switch-to-edit)
     ("C-c C-l"         racket-logger)))
  "Keymap for Racket REPL mode.")

(easy-menu-define racket-repl-mode-menu racket-repl-mode-map
  "Menu for Racket REPL mode."
  '("Racket"
    ["Insert Lambda" racket-insert-lambda] ;λ in string breaks menu
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
  (racket--common-variables)
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
