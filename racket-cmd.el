;;; racket-cmd.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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

;;; Back end command server process

(require 'racket-custom)
(require 'racket-util)
(require 'tramp)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function  racket--debug-on-break "racket-debug" (response))
(autoload         'racket--debug-on-break "racket-debug")

(declare-function  racket--logger-on-notify "racket-logger" (str))
(autoload         'racket--logger-on-notify "racket-logger")

(defvar racket-back-end nil
  "A property list to describe a Racket Mode back end.

End users or other packages should NOT set this variable
directly. Instead they should supply a value for the
customization variable `racket-back-end-function'. See its
documentation for a description of the properties.

Initialized to nil so that failures to set this buffer-locally
will result in explicit error messages instead of silently using
the wrong back end.

Racket Mode major mode initialization functions should use
`racket--get-back-end' to get the value they should set
buffer-locally:

  (setq-local racket-back-end (`racket--get-back-end'))

Futhermore, commands that create buffers that do not visit a file
should use `(plist-get racket-back-end 'name)` as a suffix in the
buffer name -- permitting a unique buffer per back end as well as
making it easy for users to distinguish them. See the source for
`racket-describe-mode' as an example.

Note that values of the customization variable
`racket-repl-buffer-name-function' need to be \"host-aware\" --
e.g. even `racket-repl-buffer-name-shared' needs to return
different names for different hosts -- because a REPL on a back
end cannot run files hosted on another.")

(defsubst racket--back-end-process-name ()
  (concat "racket-back-end-" (plist-get racket-back-end 'name)))

(defsubst racket--back-end-process-name-stderr ()
  (concat (racket--back-end-process-name) "-stderr"))

(defun racket--back-end-local-p ()
  (equal (plist-get racket-back-end 'host-name) "127.0.0.1"))

(defun racket--back-end-validate (v)
  (cl-macrolet ((check (pred var)
                       `(unless (,pred ,var)
                          (error "%s should be %s but is `%S` in `%S` in %S"
                                 ',var
                                 ',pred
                                 ,var
                                 racket-back-end
                                 (current-buffer)))))
    (check stringp (plist-get v 'name))
    (when (plist-get v 'racket-program)
      (check stringp (plist-get v 'racket-program)))
    (check stringp (plist-get v 'host-name))
    (check stringp (plist-get v 'user-name))
    (check numberp (plist-get v 'ssh-port))
    (check stringp (plist-get v 'repl-tcp-accept-host))
    (check stringp (plist-get v 'remote-source-dir)))
  v)

(defun racket--get-back-end ()
  "Return a suitable value for the variable `racket-back-end'.

See `racket-back-end-function' for more information."
  (racket--back-end-validate
   (funcall (if (functionp racket-back-end-function)
                racket-back-end-function
              #'identity)
            (racket--back-end-default))))

(defun racket--back-end-default ()
  "Create a `racket-back-end' plist with default values."
  ;; If you change these defaults, update the description in the doc
  ;; string of the customization variable `racket-back-end-function'.
  (pcase-let* ((`(,user ,host ,port) (racket--back-end-filename-to-user+host+port
                                      default-directory))
               (local-p (equal host "127.0.0.1")))
    (list 'name                 host
          'racket-program       nil
          'remote-source-dir    "/tmp/racket-mode-back-end"
          'host-name            host
          'user-name            user
          'ssh-port             port
          'repl-tcp-accept-host (if local-p "127.0.0.1" "0.0.0.0")
          'repl-tcp-port        (if local-p 0 55555))))

(defun racket--back-end-filename-to-user+host+port (filename)
  (let* ((tfns (and (tramp-tramp-file-p filename)
                    (tramp-dissect-file-name filename)))
         (user (or (and tfns
                        (equal "ssh" (tramp-file-name-method tfns))
                        (tramp-file-name-user tfns))
                   (user-login-name)))
         (host (or (and tfns
                        (equal "ssh" (tramp-file-name-method tfns))
                        (tramp-file-name-host tfns))
                   "127.0.0.1"))
         (port (or (and tfns
                        (equal "ssh" (tramp-file-name-method tfns))
                        (fboundp 'tramp-file-name-port)
                        (tramp-file-name-port tfns))
                   22)))
    (list user host port)))

(defun racket-file-name-front-to-back (file)
  "Make a front end file name usable to give to the back end.

When a tramp file name, extract the \"localname\" portion of a
tramp file name."
  (if (tramp-tramp-file-p file)
      (tramp-file-name-localname (tramp-dissect-file-name file))
    file))

(defun racket-how-front-to-back (how)
  "Convenience for back end commands that have a \"how\" argument.

These \"how\" arguments can be a path name, or a pair where the
car is a path name, or the symbol namespace. Apply
`racket-file-name-front-to-back' in the path name cases."
  (pcase how
    ((and (pred stringp) path)
     (racket-file-name-front-to-back path))
    (`(,(and (pred stringp) path) . ,anchor)
     (cons (racket-file-name-front-to-back path) anchor))
    (v v)))

(defun racket-file-name-back-to-front (file)
  "Make a file name from the back end usable on the front end.

When the back end is local: When on Windows, replace back slashes
with forward slashes; else return FILE as is.

When the back end is remote: Treat FILE as the \"localname\"
portion of a tramp file name, and make a tramp file name using
various racket-back-end-xxx variables for the remaining
components."
  (if (racket--back-end-local-p)
      (if racket--winp
          (subst-char-in-string ?\\ ?/ file)
        file)
    (racket--make-tramp-file-name (plist-get racket-back-end 'user-name)
                                  (plist-get racket-back-end 'host-name)
                                  (plist-get racket-back-end 'ssh-port)
                                  file)))

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
  (racket--back-end-validate racket-back-end)
  (racket--cmd-open))

;;;###autoload
(defun racket-stop-back-end ()
  "Stop a back end process used by Racket Mode.

Before doing anything runs the hook `racket-stop-back-end-hook'."
  (interactive)
  (racket--cmd-close))

(defun racket--cmd-open-p ()
  "Does a running process exist for `racket-back-end'?"
  (and racket-back-end
       (stringp (racket--back-end-process-name))
       (pcase (get-process (racket--back-end-process-name))
         ((and (pred (processp)) proc)
          (eq 'run (process-status proc))))))

(make-obsolete-variable
 'racket-adjust-run-rkt
 "This is no longer supported."
 "2021-08-16")

(defvar racket--back-end-auth-token (format "token-%x" (random))
  "A value used to start a REPL in a back end process.
We share this among back ends, which is fine. Keep in mind this
does get freshly initialized each time this .el file is loaded --
even from compiled bytecode.")

(defun racket--cmd-open ()
  ;; Avoid excess processes/buffers like "racket-process<1>".
  (racket--cmd-close)
  ;; Give the process buffer the current values of some vars; see
  ;; <https://github.com/purcell/envrc/issues/22>.
  (cl-letf* (((default-value 'process-environment) process-environment)
             ((default-value 'exec-path)           exec-path))
    (when noninteractive
      (princ (format "racket-back-end-name is %s\n"
                     (plist-get racket-back-end 'name))))
    (let* ((local-p (racket--back-end-local-p))
           (main-dot-rkt (expand-file-name
                          "main.rkt"
                          (if local-p
                              racket--rkt-source-dir
                            (racket--ensure-updated-back-end-on-remote))))
           (svg-flag (if (and (boundp 'image-types)
                              (fboundp 'image-type-available-p)
                              (or (and (memq 'svg image-types)
                                       (image-type-available-p 'svg))
                                  (and (memq 'imagemagick image-types)
                                       (image-type-available-p 'imagemagick))))
                         "--use-svg"
                       "--do-not-use-svg"))
           (process-name (racket--back-end-process-name))
           (process-name-stderr (racket--back-end-process-name-stderr))
           (stderr (make-pipe-process
                    :name     process-name-stderr
                    :buffer   (concat " *" process-name-stderr "*")
                    :noquery  t
                    :coding   'utf-8
                    :filter   #'racket--cmd-process-stderr-filter
                    :sentinel #'ignore))
           (command (list (or (plist-get racket-back-end 'racket-program)
                              racket-program)
                          main-dot-rkt
                          "--auth"        racket--back-end-auth-token
                          "--accept-host" (plist-get racket-back-end
                                                     'repl-tcp-accept-host)
                          "--port"        (format "%s"
                                                  (plist-get racket-back-end
                                                             'repl-tcp-port))
                          svg-flag))
           (command (if local-p
                        command
                      (cons "ssh"
                            (cons (format "%s@%s"
                                          (plist-get racket-back-end 'user-name)
                                          (plist-get racket-back-end 'host-name))
                                  command))))
           (process
            (make-process
             :name            process-name
             :connection-type 'pipe
             :noquery         t
             :coding          'utf-8
             :buffer          (get-buffer-create
                               (concat " *" process-name "*"))
             :stderr          stderr
             :command         command
             :filter          #'racket--cmd-process-filter
             :sentinel        #'racket--cmd-process-sentinel))
           (status (process-status process)))
      ;; Give the process buffer the same buffer-local value for
      ;; `racket-back-end', so that we can retrieve for the
      ;; logger-notify handling below.
      (let ((v racket-back-end))
        (with-current-buffer (process-buffer process)
          (setq-local racket-back-end v)))
      (unless (eq status 'run)
        (error "%s process status is not \"run\", instead it is %s"
               (racket--back-end-process-name)
               status))
      (run-hooks 'racket-start-back-end-hook))))

(defun racket--cmd-close ()
  "Delete back end main process/buffer and stderr process/buffer."
  (cl-flet ((delete-process-and-buffer
             (lambda (process-name)
               (pcase (and (stringp process-name)
                           (get-process process-name))
                 ((and (pred (processp)) proc)
                  (delete-process proc)
                  (pcase (get-buffer (process-buffer proc))
                    ((and (pred (bufferp)) buf)
                     (kill-buffer buf))))))))
    (when racket-back-end
      (run-hooks 'racket-stop-back-end-hook)
      (delete-process-and-buffer (racket--back-end-process-name))
      (delete-process-and-buffer (racket--back-end-process-name-stderr)))))

(defun racket--cmd-process-sentinel (proc event)
  (when (string-match-p "exited abnormally|failed|connection broken" event)
    (message "{%s} %s" (process-name proc) (substring event 0 -1))))

(defun racket--cmd-process-stderr-filter (proc string)
  "Show back end process stderr via `message'.
Won't show noise like \"process finished\" if stderr process
sentinel is `ignore'."
  (message "{%s} %s\n" proc string))

(defun racket--cmd-process-filter (proc string)
  "Parse complete sexprs from the process output and give them to
`racket--cmd-dispatch-response'."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (goto-char (point-min))
        (while (pcase (ignore-errors (read buffer))
                 (`nil `nil)
                 (sexp (delete-region (point-min)
                                      (if (eq (char-after) ?\n)
                                          (1+ (point))
                                        (point)))
                       (racket--cmd-dispatch-response racket-back-end
                                                      sexp)
                       t)))))))

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

(defun racket--cmd-dispatch-response (back-end response)
  "Do something with a sexpr sent to us from the command server.
Mostly these are responses to command requests. Strictly speaking
'logger and 'debug-break are \"notifications\", i.e. /not/ in
direct response to one command request."
  (pcase response
    (`(logger ,str)
     (run-at-time 0.001 nil #'racket--logger-on-notify back-end str))
    (`(debug-break . ,response)
     (run-at-time 0.001 nil #'racket--debug-on-break response))
    (`(,nonce . ,response)
     (let ((callback (gethash nonce racket--cmd-nonce->callback)))
       (when callback
         (remhash nonce racket--cmd-nonce->callback)
         (run-at-time 0.001 nil callback response))))
    (_ nil)))

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
  (unless racket-back-end
    (error "racket-back-end is nil"))
  (unless (stringp (racket--back-end-process-name))
    (error "racket--back-end-process-name is not a string"))
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

CALLBACK is only called for 'ok responses, with (ok v ...)
unwrapped to (v ...).

'error responses are handled here. Note: We use `message' not
`error' here because:

  1. It would show \"error running timer:\" which, although true,
     is confusing or at best N/A for end users.

  2. More simply, we don't need to escape any call stack, we only
     need to ... not call the callback!

'break responses are handled here, too. This is used when a
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
             (`(error ,m) (message "%s command exception:\n%s" name m))
             (`(break)    nil)
             (v           (let ((print-length nil) ;for %S
                                (print-level nil))
                            (message "%s unknown command response:\n%S" name v)))))
       #'ignore))))

(defun racket--cmd/await (repl-session-id command-sexpr)
  "Send COMMAND-SEXPR. Await and return an 'ok response value, or raise `error'.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace."
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (response awaiting))
    (racket--cmd/async-raw repl-session-id
                           command-sexpr
                           (lambda (v) (setq response v)))
    (with-timeout (racket-command-timeout
                   (error "racket-command process timeout"))
      (while (eq response awaiting)
        (accept-process-output nil 0.001))
      (pcase response
        (`(ok ,v)    v)
        (`(error ,m) (error "%s" m))
        (v           (let ((print-length nil) ;for %S
                           (print-level nil))
                       (error "Unknown command response: %S" v)))))))

;;; Tramp remote back end source files

;; Adapted from PR #553 -- for which, big thanks!!

(defun racket--ensure-updated-back-end-on-remote ()
  "Ensure back end files on remote, return the directory localname.

Take the sha-1 digest for `racket--rkt-source-dir' files. Look
for a \"digest\" file on the remote. If it doesn't exist or its
contents don't match, then we copy a new \"digest\" file as well
as the entire `racket--rkt-source-dir' tree to the remote.
Otherwise assume the files exist there and are current, from the
last time we needed to copy.

This is the most efficient way I can think of to handle this over
a possibly slow remote connection."
  (let* ((user (plist-get racket-back-end 'user-name))
         (host (plist-get racket-back-end 'host-name))
         (port (plist-get racket-back-end 'ssh-port))
         (dir  (plist-get racket-back-end 'remote-source-dir))
         (tramp-dir (racket--make-tramp-file-name user host port dir))
         (digest-here
          (sha1
           (string-join
            (mapcar (lambda (file-name)
                      (with-temp-buffer
                        (insert-file-contents-literally file-name)
                        (sha1 (current-buffer))))
                    (directory-files-recursively racket--rkt-source-dir ".+")))))
         (digest-file-there (expand-file-name "digest" tramp-dir))
         (digest-there
          (with-temp-buffer
            (let ((tramp-verbose 0))
              (ignore-errors            ;OK if it doesn't exist yet
                (insert-file-contents-literally digest-file-there)))
            (buffer-substring (point-min) (point-max)))))
    (unless (equal digest-here digest-there)
      ;; We need to create a digest file on the remote. The simplest
      ;; way to do so is create the file locally, then let `copy-file'
      ;; use tramp automatically.
      ;;
      ;; Don't create a digest file in `rkt--source-dir' -- it would
      ;; be one more thing to .gitignore, and might interfere with
      ;; people using e.g. straight.el -- instead make a temp file.
      (let* ((temp-digest-file-here (make-temp-file "racket-mode-digest")))
        (with-temp-buffer
          (insert digest-here)
          (write-region (point-min) (point-max) temp-digest-file-here))
        (let ((tramp-verbose 2)) ;avoid "encoding"/"decoding" messages
          ;; Copy the back end directory to the remote.
          ;;
          ;;`copy-directory' likes to create symlinks when the source
          ;; is a symlink (e.g. straight.el keeps package repos in a
          ;; symlinked dir). This will never work when copying to a
          ;; remote host. Fortunately we can replace every symlink
          ;; create with a file copy.
          (cl-flet ((make-symbolic-link (src dest _x) (copy-file src dest t nil)))
            (copy-directory racket--rkt-source-dir
                            tramp-dir
                            nil t t))
          ;; Now that we're sure the directory there is created, copy
          ;; our digest file.
          (copy-file temp-digest-file-here digest-file-there t)
          (delete-file temp-digest-file-here))
        (message "Racket Mode back end copied to remote back end at %s" host)))
    dir))

(defun racket--make-tramp-file-name (user host port localname)
  ;; Using `tramp-make-tramp-file-name' across versions of Emacs is a
  ;; PITA because it has had three different signatures. Although the
  ;; newest version supports the middle signature for backward
  ;; compatibility, it doesn't support the oldest signature.
  ;;
  ;; First try the middle signature (METHOD USER DOMAIN HOST PORT
  ;; LOCALNAME &optional HOP):
  (condition-case nil
      (with-no-warnings ;from byte compiler
        (tramp-make-tramp-file-name "ssh"
                                    (or user "")
                                    ""  ;domain
                                    host
                                    (format "%s" port)
                                    localname))
    (wrong-number-of-arguments
     ;; Otherwise try the oldest signature (METHOD USER HOST
     ;; LOCALNAME). Note this means needing to ignore `port'.
     (condition-case nil
         (with-no-warnings ;from byte compiler
           (tramp-make-tramp-file-name "ssh"
                                       (or user "")
                                       host
                                       localname))
       (wrong-number-of-arguments
        (error "Unsupported flavor of `tramp-make-tramp-file-name'."))))))

(provide 'racket-cmd)

;; racket-cmd.el ends here
