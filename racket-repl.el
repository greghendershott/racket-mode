;;; racket-repl.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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

(require 'racket-common)
(require 'comint)
(require 'compile)

(defconst racket--repl-buffer-name/raw
  "Racket REPL"
  "The base buffer name, NOT surrounded in *stars*")
(defconst racket--repl-buffer-name
  (concat "*" racket--repl-buffer-name/raw "*")
  "The actual buffer name as created by comint-mode")
(defun racket--get-repl-buffer-process ()
  (get-buffer-process racket--repl-buffer-name))

(defvar racket-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("RET"     racket-repl-eval-or-newline-and-indent)
            ("TAB"     racket-indent-or-complete)
            ("M-C-u"   racket-backward-up-list)
            (")"       racket-insert-closing-paren)
            ("]"       racket-insert-closing-bracket)
            ("}"       racket-insert-closing-brace)
            ("M-C-y"   racket-insert-lambda)
            ("C-c C-d" racket-doc)
            ("C-c C-." racket-describe)
            ("M-."     racket-visit-definition)
            ("C-M-."   racket-visit-module)))
    m)
  "Keymap for Racket REPL mode.")

(defcustom racket-repl-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :tag "History filter regexp"
  :type 'regexp
  :group 'racket)

(defun racket-repl--input-filter (str)
  "Don't save anything matching `racket-repl-filter-regexp'."
  (not (string-match racket-repl-filter-regexp str)))

(defun racket--get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;; I don't want comint-mode clobbering our font-lock with
;; comint-highlight-input face. Changing that _face_ to be non-bold
;; isn't sufficient, it simply clobbers everything non-bold.
;;
;; So far, the least-pukey way I can figure out how to do this is to
;; copy-pasta much of comint-send-input, and modify the one tiny
;; offending bit.  Blech. If anyone reading this knows a better way,
;; please let me know!
;;
;; Update: I went on to slim down the copy -- deleted the `no-newline`
;; and `artificial` args we don't use, and the code that could only
;; execute if they were non-nil.
(defun racket--comint-send-input ()
  "Like `comint-send-input` but doesn't use face `comint-highlight-input'."
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send (end-of-line))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        ;; This is messy 'cos ultimately the original
                        ;; functions used do insertion, rather than return
                        ;; strings.  We have to expand, then insert back.
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))
        (insert ?\n)
        (comint-add-to-input-history history)
        (run-hook-with-args 'comint-input-filter-functions
                            (concat input "\n"))
        (let ((beg (marker-position pmark))
              (end (1- (point)))
              (inhibit-modification-hooks t))
          (when (> end beg)
            ;;;; The bit from comint-send-input that we DON'T want:
            ;; (add-text-properties beg end
            ;;                      '(front-sticky t
            ;;                        font-lock-face comint-highlight-input))
            (unless comint-use-prompt-regexp
              ;; Give old user input a field property of `input', to
              ;; distinguish it from both process output and unsent
              ;; input.  The terminating newline is put into a special
              ;; `boundary' field to make cursor movement between input
              ;; and output fields smoother.
              (add-text-properties
               beg end
               '(mouse-face highlight
                 help-echo "mouse-2: insert after prompt as new input"))))
          (unless comint-use-prompt-regexp
            ;; Cover the terminating newline
            (add-text-properties end (1+ end)
                                 '(rear-nonsticky t
                                   field boundary
                                   inhibit-line-move-field-capture t))))
        (comint-snapshot-last-prompt)
        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (funcall comint-input-sender proc input)
        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (run-hook-with-args 'comint-output-filter-functions "")))))

(defun racket-repl-eval-or-newline-and-indent ()
  "If complete sexpr, eval in Racket. Else do `racket-newline-and-indent'."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (user-error "Current buffer has no process")
      (condition-case nil
          (progn
            (save-excursion
              (goto-char (process-mark proc))
              (forward-list)) ;will error unless complete sexpr
            (racket--comint-send-input))
        (error (racket-newline-and-indent))))))

(defvar racket--run.rkt
  (expand-file-name "run.rkt"
                    (file-name-directory (or load-file-name (buffer-file-name))))
  "Path to run.rkt")

;;;###autoload
(defun racket-repl ()
  "Run a Racket REPL in a comint buffer.

When starting Racket, `racket--repl-wait-for-prompt'.

The selected window remains the same.

Does not necessarily cause the REPL buffer to be shown in a window.

Runs `comint-mode-hook' and `racket-repl-mode-hook'."
  (interactive)
  (let ((original-window (selected-window)))
    ;; What window to use? If REPL buffer already visible in a window,
    ;; keep using that. Otherwise (other-window -1).
    (let ((rw (get-buffer-window racket--repl-buffer-name)))
      (if rw
          (select-window rw)
        (other-window 1)))
    ;; If REPL buffer doesn't have a live process, start one.
    (unless (comint-check-proc racket--repl-buffer-name)
      (message "Starting Racket...")
      (set-buffer (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                               racket-program
                               nil
                               racket--run.rkt))
      ;; The following is needed to make e.g. λ work when pasted into the
      ;; comint-buffer, both directly by the user and via the racket--eval
      ;; functions.
      (set-process-coding-system (get-buffer-process racket--repl-buffer-name)
                                 'utf-8 'utf-8)
      (racket-repl-mode)
      (racket--repl-wait-for-prompt)
      (message ""))
    (select-window original-window)))

(defcustom racket--wait-for-prompt-timeout 30
  "When REPL starts Racket process, how long to wait for Racket prompt."
  :tag "REPL startup timeout (seconds):"
  :type 'number
  :group 'racket)

(defun racket--repl-wait-for-prompt ()
  "Wait up to `racket--wait-for-prompt-timeout' seconds for
`racket--repl-has-prompt-p' to be t."
  (message "Waiting for Racket prompt...")
  (let ((deadline (+ (float-time) racket--wait-for-prompt-timeout)))
    (while (and (not (racket--repl-has-prompt-p))
                (< (float-time) deadline))
      (accept-process-output (get-buffer-process racket--repl-buffer-name)
                             (- deadline (float-time)))))
  (unless (racket--repl-has-prompt-p)
    (error "Timeout waiting for Racket REPL prompt")))

(defun racket--repl-has-prompt-p ()
  "Is the REPL process alive and is the Racket prompt the last thing in the buffer?"
  (and (comint-check-proc racket--repl-buffer-name)
       (with-current-buffer racket--repl-buffer-name
         (save-excursion
           (goto-char (point-max))
           (and (re-search-backward "\n" nil t)
                (re-search-forward comint-prompt-regexp nil t)
                (= (point) (point-max)))))))

;;;

(defun racket--send-region-to-repl (start end)
  "Internal function to send the region to the Racket REPL.
Before sending the region, calls `racket-repl' and
`racket--repl-forget-errors'. Afterwards calls
`racket--repl-show-and-move-to-end'."
  (when (and start end)
    (racket-repl)
    (racket--repl-forget-errors)
    (comint-send-region (racket--get-repl-buffer-process) start end)
    (comint-send-string (racket--get-repl-buffer-process) "\n")
    (racket--repl-show-and-move-to-end)))

(defun racket-send-region (start end)
  "Send the current region (if any) to the Racket REPL."
  (interactive "r")
  (if (region-active-p)
      (racket--send-region-to-repl start end)
    (beep)
    (message "No region.")))

(defun racket-send-definition ()
  "Send the current definition to the Racket REPL."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (racket--send-region-to-repl (point) end))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the Racket REPL."
  (interactive)
  (racket--send-region-to-repl (save-excursion (backward-sexp) (point))
                               (point)))

(defun racket--repl-forget-errors ()
  "Forget existing compilation mode errors in the REPL.
Although they remain clickable, `next-error' and `previous-error'
will ignore them."
  (with-current-buffer racket--repl-buffer-name
    (compilation-forget-errors)))

(defun racket--repl-show-and-move-to-end ()
  "Make the Racket REPL visible, and move point to end.
Keep original window selected."
  (let ((w (selected-window)))
    (pop-to-buffer racket--repl-buffer-name t)
    (select-window (get-buffer-window racket--repl-buffer-name))
    (with-current-buffer racket--repl-buffer-name
      (goto-char (point-max)))
    (select-window w)))

;;; Inline images in REPL

(defcustom racket-repl--inline-images t
  "Whether to display inline images in the REPL."
  :type 'boolean
  :group 'racket)

(defcustom racket--system-image-viewer "display"
  "Which system image viewer program to invoke upon M-x
 `racket-view-last-image'."
  :type 'string
  :group 'racket)

(defcustom racket--image-cache-keep-last 100
  "How many images to keep in the image cache."
  :type 'integer
  :group 'racket)

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
  "Clean all except for the last `racket--image-cache-keep-last'
images in 'racket-image-cache-dir'."
  (interactive)
  (dolist (file (butlast (racket-repl--list-image-cache)
                         racket--image-cache-keep-last))
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
          (if (and racket-repl--inline-images (display-images-p))
              (insert-image (create-image file) "[image]")
            (goto-char begin)
            (insert "[image] ; use M-x racket-view-last-image to view"))
          (setq racket-image-cache-dir (file-name-directory file))
          (racket-repl--clean-image-cache))))))

(defun racket-view-last-image (n)
  "Open the last displayed image in the system's image viewer.

With prefix arg, open the N-th last shown image in the system's image viewer."
  (interactive "p")
  (let ((images (reverse (racket-repl--list-image-cache))))
    (if (>= (length images) n)
        (start-process "Racket image view"
                       nil
                       racket--system-image-viewer
                       (nth (- n 1) images))
      (error "There aren't %d recent images" n))))

(defun racket-repl--output-filter (txt)
  (racket-repl--replace-images))

;;; racket-repl-mode

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.
\\{racket-repl-mode-map}"
  (racket--variables-for-both-modes)
  ;; comint-prompt-regexp is important to get exactly right for
  ;; comint-redirect-send-command-to-process as used by
  ;; racket--eval/buffer and friends. Bad things happen if this regexp
  ;; accidentally matches process output other than the prompt. After
  ;; fighting with this for awhile, it occurred to me to use "≺file≻"
  ;; (instead of "file>"). Much less likely to occur in process
  ;; output. Also it lets the regexp become simpler and more reliably
  ;; handle things such as file names with spaces in them. Also when
  ;; process output doesn't end with a newline (e.g. `display` not
  ;; `displayln`) the opening ≺ helps the user visually delimit the
  ;; output from the following prompt.
  (setq-local comint-prompt-regexp "^≺.*?≻ +")
  ;; (setq-local comint-use-prompt-regexp t)
  ;; (setq-local comint-prompt-read-only t)
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter (function racket-repl--input-filter))
  (add-hook 'comint-output-filter-functions 'racket-repl--output-filter nil t)
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   '(("^;?[ ]*\\([^ :]+\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" 1 2 3) ;errs, defns
     ("^;?[ ]*at:[ ]+\\([^ :]+\\):\\([0-9]+\\)[.]\\([0-9]+\\)$" 1 2 3) ;contract
     ("#<path:\\([^>]+\\)> \\([0-9]+\\) \\([0-9]+\\)" 1 2 3)   ;rackunit
     ("#<path:\\([^>]+\\)>" 1 nil nil 0)                       ;path struct
     ))
  (setq-local comint-get-old-input (function racket--get-old-input)))

(provide 'racket-repl)

;; racket-repl.el ends here
