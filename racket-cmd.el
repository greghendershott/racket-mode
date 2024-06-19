;;; racket-cmd.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Back end: process and commands

(require 'racket-back-end)
(require 'racket-custom)
(require 'racket-util)
(require 'tramp)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function  racket--debug-on-break "racket-debug" (response))
(autoload         'racket--debug-on-break "racket-debug")

(declare-function  racket--logger-on-notify "racket-logger" (back-end-name str))
(autoload         'racket--logger-on-notify "racket-logger")

(declare-function  racket--hash-lang-on-notify "racket-hash-lang" (id v))
(autoload         'racket--hash-lang-on-notify "racket-hash-lang")

(declare-function  racket--repl-on-output "racket-repl" (session-id kind value))
(autoload         'racket--repl-on-output "racket-repl")

(declare-function  racket--package-on-notify "racket-package" (v))
(autoload         'racket--package-on-notify "racket-package")

;;;###autoload
(defvar racket-start-back-end-hook nil
  "Hook run after `racket-start-back-end' finishes successfully.")

;;;###autoload
(defvar racket-stop-back-end-hook nil
  "Hook run before `racket-stop-back-end'.")

;;;###autoload
(defun racket-start-back-end ()
  "Start a back end process used by Racket Mode.

If a process is already started, this command will stop and restart it.

When successful runs the hook `racket-start-back-end-hook'."
  (interactive)
  (racket--back-end-validate (racket-back-end))
  (racket--cmd-open))

;;;###autoload
(defun racket-stop-back-end ()
  "Stop a back end process used by Racket Mode.

Before doing anything runs the hook `racket-stop-back-end-hook'."
  (interactive)
  (racket--cmd-close))

(defun racket--cmd-open-p ()
  "Does a running process exist for `racket-back-end-name'?"
  (pcase (get-process (racket--back-end-process-name (racket-back-end)))
    ((and (pred (processp)) proc)
     (eq 'run (process-status proc)))))

(make-obsolete-variable
 'racket-adjust-run-rkt
 "This is no longer supported."
 "2021-08-16")

(defun racket--cmd-open ()
  ;; Avoid excess processes/buffers like "racket-process<1>".
  (when (racket--cmd-open-p)
    (racket--cmd-close))
  ;; Give the process buffer the current values of some vars; see
  ;; <https://github.com/purcell/envrc/issues/22>.
  (cl-letf* (((default-value 'process-environment) process-environment)
             ((default-value 'exec-path)           exec-path))
    (let* ((back-end (racket-back-end))
           (_ (when noninteractive
                (princ (format "back end is %S\n" back-end))))
           (process-name (racket--back-end-process-name back-end))
           (process-name-stderr (racket--back-end-process-name-stderr back-end))
           (stderr (make-pipe-process
                    :name     process-name-stderr
                    :buffer   (concat " " process-name-stderr)
                    :noquery  t
                    :coding   'utf-8
                    :filter   #'racket--cmd-process-stderr-filter
                    :sentinel #'ignore))
           (local-p (racket--back-end-local-p back-end))
           (main-dot-rkt (expand-file-name
                          "main.rkt"
                          (if local-p
                              racket--rkt-source-dir
                            (racket--ensure-updated-back-end-on-remote))))
           (svg-flag (if (and (not racket-images-do-not-use-svg)
                              (boundp 'image-types)
                              (fboundp 'image-type-available-p)
                              (or (and (memq 'svg image-types)
                                       (image-type-available-p 'svg))
                                  (and (memq 'imagemagick image-types)
                                       (image-type-available-p 'imagemagick))))
                         "--use-svg"
                       "--do-not-use-svg"))
           (args    (list main-dot-rkt svg-flag))
           (command (racket--back-end-args->command back-end args))
           (process
            (make-process
             :name            process-name
             :connection-type 'pipe
             :noquery         t
             :coding          'utf-8
             :buffer          (concat " " process-name)
             :stderr          stderr
             :command         command
             :filter          #'racket--cmd-process-filter
             :sentinel        #'racket--cmd-process-sentinel))
           (status (process-status process)))
      (process-put process 'racket-back-end-name (racket-back-end-name back-end))
      (unless (eq status 'run)
        (error "%s process status is not \"run\", instead it is %s"
               process-name
               status))
      (run-hooks 'racket-start-back-end-hook))))

(defun racket--cmd-close ()
  "Delete back end's main process/buffer and stderr process/buffer."
  (cl-flet ((delete-process/buffer
             (lambda (process-name)
               (when-let (process (get-process process-name))
                 (when-let (buffer (get-buffer (process-buffer process)))
                   (kill-buffer buffer))
                 (delete-process process)))))
    (when-let (back-end (racket-back-end))
      (run-hooks 'racket-stop-back-end-hook)
      (delete-process/buffer (racket--back-end-process-name        back-end))
      (delete-process/buffer (racket--back-end-process-name-stderr back-end)))))

(defun racket--cmd-process-sentinel (proc event)
  (unless (eq 'open (process-status proc))
    (when (string-match-p "exited abnormally" event)
      (run-hooks 'racket-stop-back-end-hook))
    (message "{%s} %s" (process-name proc) (substring event 0 -1))))

(defun racket--cmd-process-stderr-filter (proc string)
  "Show back end process stderr via `message'.
Won't show noise like \"process finished\" if stderr process
sentinel is `ignore'."
  (message "{%s} %s\n" proc string))

(defun racket--cmd-process-filter (proc string)
  "Read and dispatch sexprs as they become available from process output."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (racket--cmd-read (apply-partially
                           #'racket--cmd-dispatch
                           (process-get proc 'racket-back-end-name)))))))

;; The process filter inserts text as it arrives in chunks. So the
;; challenge here is to read whenever the buffer accumulates one or
;; more /complete/ top-level sexps. Although it's simple to call
;; `read' and let it succeed or fail, when a top-level sexp is long
;; (as for check-syntax) and not yet complete, it's wasteful to
;; read/allocate sub-expressions, only to fail and discard that work
;; -- perhaps repeatedly as the long sexp grows in the buffer but
;; remains incomplete. Using `scan-lists' to check for a complete sexp
;; is better, but still wasteful to do from `point-min' every time.
;; Instead we use `parse-partial-sexp' to parse/check incrementally,
;; saving its parse state between calls, and resuming the parse for
;; newly added text. We tell it to stop when the depth reaches zero,
;; meaning we have a complete top-level sexp that can be read.
(defvar-local racket--cmd-read-state nil)
(defvar-local racket--cmd-read-from 1)
(defconst racket--cmd-read-whitespace " \n\r\t")
(defun racket--cmd-read (on-top-level-sexp)
  ;; Note: Because top-level sexps from the back end are always nested
  ;; in parens, all we need is a syntax-table to give them that
  ;; char-syntax, as does even `fundamental-mode'.
  (while
      (when (< racket--cmd-read-from (point-max))
        (setq racket--cmd-read-state
              (parse-partial-sexp racket--cmd-read-from
                                  (point-max)
                                  0     ;target depth
                                  nil   ;stop before
                                  racket--cmd-read-state))
        (setq racket--cmd-read-from (point))
        (when (zerop (elt racket--cmd-read-state 0))
          (goto-char (point-min))
          (skip-chars-forward racket--cmd-read-whitespace)
          (when (< (point) (point-max))
            (funcall on-top-level-sexp (read (current-buffer)))
            (skip-chars-forward racket--cmd-read-whitespace)
            (delete-region (point-min) (point))
            (setq racket--cmd-read-state nil)
            (setq racket--cmd-read-from (point-min))
            t)))))

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from command nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

(defun racket--cmd-dispatch (back-end response)
  "Do something with a sexpr sent to us from the command server.
Although mostly these are 1:1 responses to command requests, some
like \"logger\", \"debug-break\", and \"hash-lang\" are
notifications."
  (pcase response
    (`(logger ,str)
     (run-at-time 0.001 nil #'racket--logger-on-notify back-end str))
    (`(debug-break . ,response)
     (run-at-time 0.001 nil #'racket--debug-on-break response))
    (`(hash-lang ,id . ,vs)
     (run-at-time 0.001 nil #'racket--hash-lang-on-notify id vs))
    (`(repl-output ,session-id ,kind ,v)
     (run-at-time 0.001 nil #'racket--repl-on-output session-id kind v))
    (`(pkg-op-notify . ,v)
     (run-at-time 0.001 nil #'racket--package-on-notify v))
    (`(,nonce . ,response)
     (when-let (callback (gethash nonce racket--cmd-nonce->callback))
       (remhash nonce racket--cmd-nonce->callback)
       (run-at-time 0.001 nil callback response)))
    (_ nil)))

(defun racket--assert-readable (sexp)
  "Sanity check that SEXP is readable by Racket.

For example if a command sexp supplies a marker -- which prints
as unreadable #<marker ...> -- instead of an integer, we will
error here in Emacs with a more helpful error message and
backtrace. Besides which, a read error in the back end's command
loop could cause the entire back end to abend."
  (cl-labels
      ((check
        (v)
        (cond
         ((numberp v) t)
         ((stringp v) t)
         ((symbolp v) t)
         ((consp v)   (and (check (car v)) (check (cdr v))))
         (t
          (let ((print-length nil)
                (print-level nil))
            (error "invalid s-expression for Racket reader\n value: %S\n in: %S"
                   v sexp))))))
    (check sexp)))

(defun racket--cmd/async-raw (repl-session-id command-sexpr &optional callback)
  "Send COMMAND-SEXPR and return. Later call CALLBACK with the response sexp.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace.

If CALLBACK is not supplied or nil, defaults to `ignore'.

Otherwise CALLBACK is called after the command server returns a
response. Because command responses are obtained from the dynamic
extent of a `set-process-filter' proc -- which may have
limitations on what it can or should do -- CALLBACK is not called
immediately but instead using `run-at-time' with a very small
delay.

Important: Do not assume that `current-buffer' is the same when
CALLBACK is called, as it was when the command was sent. If you
need to do something to do that original buffer, save the
`current-buffer' in a `let' and use it in a `with-current-buffer'
form. See `racket--restoring-current-buffer'."
  (racket--assert-readable command-sexpr)
  (unless (racket--cmd-open-p)
    (racket--cmd-open))
  (cl-incf racket--cmd-nonce)
  (when (and callback
             (not (equal callback #'ignore)))
    (puthash racket--cmd-nonce callback racket--cmd-nonce->callback))
  (process-send-string
   (get-process (racket--back-end-process-name))
   (let ((print-length nil) ;for %S
         (print-level nil))
     (format "%S\n" `(,racket--cmd-nonce ,repl-session-id . ,command-sexpr)))))

(defun racket--cmd/async (repl-session-id command-sexpr &optional callback)
  "You probably want to use this instead of `racket--cmd/async-raw'.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace.

CALLBACK is only called for \"ok\" responses, with (ok v ...)
unwrapped to (v ...).

\"error\" responses are handled here. Note: We use `message' not
`error' here because:

  1. It would show \"error running timer:\" which, although true,
     is confusing or at best N/A for end users.

  2. More simply, we don't need to escape any call stack, we only
     need to ... not call the callback!

\"break\" responses are handled here, too. This is used when a
command is somehow canceled, with no useful response except the
indication we should clean up the pending callback as usual.

The original value of `current-buffer' is set for the dynamic
extent of CALLBACK, because neglecting to do so is a likely
mistake."
  (let ((buf (current-buffer))
        (name (racket--back-end-process-name)))
    (racket--cmd/async-raw
     repl-session-id
     command-sexpr
     (if callback
         (lambda (response)
           (pcase response
             (`(ok ,v)    (when (buffer-live-p buf)
                            (with-current-buffer buf (funcall callback v))))
             (`(error ,m) (let ((print-length nil) ;for %S
                                (print-level nil))
                            (message "Exception for command %S with repl-id %S from %S to %S:\n%s"
                                     command-sexpr repl-session-id buf name m)))
             (`(break)    nil)
             (v           (let ((print-length nil) ;for %S
                                (print-level nil))
                            (message "Unknown response to command %S with repl-id %S from %S to %S:\n%S"
                                     command-sexpr repl-session-id buf name v)))))
       #'ignore))))

(defun racket--cmd/await (repl-session-id command-sexpr)
  "Send COMMAND-SEXPR. Await and return an \"ok\" response value, or raise `error'.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace."
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (response awaiting)
         (buf (current-buffer))
         (name (racket--back-end-process-name)))
    (racket--cmd/async-raw repl-session-id
                           command-sexpr
                           (lambda (v) (setq response v)))
    (with-timeout (racket-command-timeout
                   (let ((print-length nil) ;for %S
                         (print-level nil))
                     (error "Command %S from %S to %S timed out after %s seconds"
                            command-sexpr buf name racket-command-timeout)))
      (while (eq response awaiting)
        (accept-process-output nil 0.001))
      (pcase response
        (`(ok ,v)    v)
        (`(error ,m) (let ((print-length nil) ;for %S
                           (print-level nil))
                       (error "Exception for command %S from %S to %S:\n%s"
                              command-sexpr buf name m)))
        (v           (let ((print-length nil) ;for %S
                           (print-level nil))
                       (error "Unknown response to command %S from %S to %S:\n%S"
                              command-sexpr buf name v)))))))

(provide 'racket-cmd)

;; racket-cmd.el ends here
