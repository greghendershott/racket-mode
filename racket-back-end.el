;;; racket-back-end.el -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Back end: configuration

(require 'cl-macs)
(require 'cl-lib)
(require 'filenotify)
(require 'racket-custom)
(require 'racket-util)
(require 'subr-x)
(require 'tramp)

;; A Racket Mode "back end" is a process running our racket/main.rkt.
;; The process could be local or (via ssh) remote. The remote process
;; could even be something like "ssh xvfb-run racket racket/main.rkt".
;; But for most people it's simply a local process, running on the
;; same machine as Emacs.
;;
;; The back end accepts commands and returns responses, as well as
;; giving non-command-response notifications (logging, debugging),
;; which is handled in racket-cmd.el.
;;
;; When some buffer needs a back end, which back end does it use?
;; That's the concern of the back end configuration code in this file.

;; Commands that create buffers that do not visit a file should use
;; `racket-back-end-name' as a suffix in the buffer name -- enabling a
;; unique buffer per back end as well as making it easy for users to
;; distinguish them. See the source for `racket-describe-mode' as an
;; example.

;; Values for the variable `racket-repl-buffer-name-function' need to
;; be aware of host names. For example, even
;; `racket-repl-buffer-name-shared' needs to return different REPL
;; buffer names for different back end names -- because a REPL on a
;; back end cannot run files hosted on another.

;; Note: In various places we use `file-remote-p', which, despite the
;; "-p" isn't just a predicate; it returns the remote prefix before
;; the localname, expanded.
;;
;; Although `file-remote-p' has an optional argument to extract
;; pieces, only 'localname (and perhaps 'user?) are reliable at least
;; in Emacs 25. Instead see `racket--back-end-host+user+port'.

;; Note that we disregard the tramp method (if any) for both
;; `default-directory' and the :directory item for back end
;; configurations. The user may have used methods like ssh, sshx, scp,
;; scpx, or rsync with `find-file', and of course tramp will use those
;; for file transfers for those buffers. But in all cases our back end
;; process is started using ssh. Example: If the user has one buffer
;; with ssh method but another buffer with scp method, for the same
;; host, we do /not/ want two different back ends on that same host,
;; solely due to those differing methods; nor do we want the user to
;; need to configure both.

(defvar racket-back-end-configurations nil
  "A list of property lists, each of which has a unique :directory.

Instead of modifying this directly, users should
`racket-add-back-end' and `racket-remove-back-end'.")

(defun racket-back-end ()
  "Return a back end configuration plist for current buffer.

If a configuration does not already exist, automatically add
one for \"/\" on the host/user/port."
  (let ((default-directory (racket--file-name-sans-remote-method default-directory)))
    (or (cl-find default-directory
                 racket-back-end-configurations
                 :test
                 (lambda (dd back-end)
                   ;; This assumes `racket-add-back-end' keeps the
                   ;; list sorted from longest to shortest :directory
                   ;; patterns.
                   (file-in-directory-p dd (plist-get back-end :directory))))
        (racket-add-back-end (if-let (str (file-remote-p default-directory))
                                 (substring-no-properties str)
                               "/")))))

(defun racket-add-back-end (directory &rest plist)
  "Add a description of a Racket Mode back end.

Racket Mode supports one or more back ends, which are Racket
processes supporting REPLs as well as various other Racket Mode
features.

DIRECTORY is a string describing a `file-name-absolute-p'
directory on some local or remote server.

When a back end's DIRECTORY is the longest matching prefix of a
buffer's `default-directory', that back end is used for the
buffer.

DIRECTORY can be a local directory like \"/\" or
\"/path/to/project\", or a `file-remote-p' directory like
\"/user@host:\" or \"/user@host:/path/to/project\".

Note that you need not include a method -- such as the \"ssh\" in
\"/ssh:user@host:\" -- and if you do it is stripped: A back end
process is always started using SSH. Even if multiple buffers for
the same user+host+port use different methods, they will share
the same back end.

Practically speaking, DIRECTORY is a path you could give to
`find-file' to successfully find some local or remote file, but
omitting any method. (Some remote file shorthand forms get
expanded to at least \"/method:host:\". When in doubt check
`buffer-file-name' and follow its example.)

In addition to being used as a pattern to pick a back end for a
buffer, DIRECTORY determines:

- Whether the back end is local or remote.

- When remote, any explicit user and port used to make SSH
  connections (as opposed to relying on values from
  ~/.ssh/config).

- Other properties get reasonable defaults based on whether the
  back end is local or remote, as described below.

After DIRECTORY, the remainining arguments are optional; they are
alternating :keywords and values describing some other properties
of a back end:

- :racket-program

  When not nil this is used instead of the value of the
  customization variable `racket-program'.

- :remote-source-dir

  Where on a remote host to copy the back end's *.rkt files when
  they do not exist or do not match the digest of the local
  files. This must be `file-name-absolute-p' on the remote. Only
  supply the localname there (not a full `file-remote-p'). The
  default value is \"/tmp/racket-mode-back-end\".

- :windows

  Whether the back end uses Windows style path names. Used to
  translate betwen slashes and backslashes between the Emacs
  front end (which uses slashes even on Windows) and the Racket
  back end (which expects native backslashes on Windows).

- :restart-watch-directories

  A list of `directory-name-p' strings. Each directory, and
  recursively its subdirectories, will be watched for file system
  changes. After any changes are detected, the next
  `racket-run' (or `racket-run-module-at-point' etc.) command
  will ask you if it should restart the back end for you. This
  may be helpful when you are changing source files used by the
  back end.

The default property values are appropriate for whether
DIRECTORY is local or remote:

- When DIRECTORY is remote, :windows defaults to nil.

- Otherwise, :windows defaults to a value based on `system-type'.

Although the default values usually \"just work\" for local and
remote back ends, you might want a special configuration. Here
are a few examples.

#+BEGIN_SRC lisp
    ;; 1. A back end configuration for \"/\" is
    ;; created automatically and works fine as a default
    ;; for buffers visiting local files, so we don't need
    ;; to add one here.

    ;; 2. However assume we want buffers under /var/tmp/8.0
    ;; instead to use Racket 8.0.
    (racket-add-back-end \"/var/tmp/8.0\"
                         :racket-program \"~/racket/8.0/bin/racket\")

    ;; 3. A back end configuration will be created
    ;; automatically for buffers visiting file names like
    ;; \"/ssh:user@linode\", so we don't need to add one here.
    ;;
    ;; If ~/.ssh/config defines a Host alias named \"linode\",
    ;; with HostName and User settings, a file name as simple as
    ;; \"/linode:\" would work fine with tramp -- and the
    ;; automatically created back end configuration would work
    ;; fine, too.

    ;; 4. For example's sake, assume for buffers visiting
    ;; /ssh:headless:~/gui-project/ we want :racket-program instead
    ;; to be \"xvfb-run racket\".
    (racket-add-back-end \"/ssh:headless:~/gui-project/\"
                         :racket-program \"xvfb-run racket\")
#+END_SRC
"
  (unless (and (stringp directory) (file-name-absolute-p directory))
    (error "racket-add-back-end: directory must be file-name-absolute-p"))
  (let* ((local-p (not (file-remote-p directory)))
         (directory (racket--file-name-sans-remote-method directory))
         (plist
          (list
           :directory            directory
           :racket-program       (plist-get plist :racket-program)
           :remote-source-dir    (or (plist-get plist :remote-source-dir)
                                     (unless local-p
                                       "/tmp/racket-mode-back-end"))
           :restart-watch-directories (plist-get plist :restart-watch-directories)
           ;; These booleanp things need to distinguish nil meaning
           ;; "user specififed false" from "user did not specify
           ;; anything".
           :windows              (if (memq :windows plist)
                                     (plist-get plist :windows)
                                   (and local-p racket--winp)))))
    (racket--back-end-validate plist)
    (racket-remove-back-end directory 'no-refresh-watches)
    ;; Keep configs sorted from longest :directory pattern to shortest.
    (setq racket-back-end-configurations
          (sort (cons plist racket-back-end-configurations)
                (lambda (a b)
                  (> (length (plist-get a :directory))
                     (length (plist-get b :directory))))))
    (racket--back-end-refresh-watches)
    plist))

(defun racket--back-end-validate (plist)
  (cl-flet ((check
             (type key)
             (let ((v (plist-get plist key)))
               (unless (funcall type v)
                 (signal 'wrong-type-argument (list type key v)))))
            (number-or-null-p (n) (or (not n) (numberp n))))
    (check #'stringp :directory)
    (check #'string-or-null-p :racket-program)
    (when (file-remote-p (plist-get plist :directory))
      (check #'stringp :remote-source-dir)
      (check #'file-name-absolute-p :remote-source-dir))
    (check #'booleanp :windows)
    (dolist (dir (plist-get plist :restart-watch-directories))
      (unless (file-directory-p dir)
        (signal 'wrong-type-argument (list #'file-directory-p :restart-watch-directories dir)))))
  plist)

(defun racket-remove-back-end (directory &optional no-refresh-watches-p)
  (setq racket-back-end-configurations
        (cl-remove-if (lambda (plist)
                        (and (string-equal (plist-get plist :directory)
                                           directory)))
                      racket-back-end-configurations))
  (unless no-refresh-watches-p
    (racket--back-end-refresh-watches)))

(defun racket-back-end-name (&optional back-end)
  "Return the \"name\" of a back end.

This is the back-end :directory. It can be used as suffix to use
in the name of a buffer not visiting a file. It can also be used
in situations where you want to refer to the back end indirectly,
by \"id\" instead of by value."
  (plist-get (or back-end (racket-back-end)) :directory))

(defun racket--back-end-process-name (&optional back-end)
  (concat "racket-back-end-" (racket-back-end-name back-end)))

(defun racket--back-end-process-name-stderr (&optional back-end)
  (concat (racket--back-end-process-name back-end) "-stderr"))

(defun racket--file-name->host+user+port+name (file-name)
  "Although it would be wonderful simply to use `file-remote-p',
it is unreliable for \"host\" or \"port\", at least on Emacs 25.
Instead need the following."
  (let* ((tfns (and (tramp-tramp-file-p file-name)
                    (tramp-dissect-file-name file-name)))
         (host (or (and tfns
                        (if (fboundp 'tramp-file-name-real-host)
                            (tramp-file-name-real-host tfns) ;older tramp
                          (tramp-file-name-host tfns)))
                   "127.0.0.1"))
         (user (and tfns
                    (tramp-file-name-user tfns)))
         (port (and tfns
                    (let ((p (tramp-file-name-port tfns)))
                      (and (not (equal p 22))
                           p))))
         (name (or (and tfns
                        (tramp-file-name-localname tfns))
                   file-name)))
    (list host user port name)))

(defun racket--host+user+port+name->file-name (v)
  "Like `tramp-make-tramp-file-name' but Emacs version independent."
  (pcase-let ((`(,host ,user ,port ,localname) v))
    (let ((port (and port (format "%s" port))))
      (concat tramp-prefix-format
              user
              (unless (zerop (length user))
                tramp-postfix-user-format)
              (if (string-match-p tramp-ipv6-regexp host)
                  (concat
                   tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
                host)
              (unless (zerop (length port))
                (concat tramp-prefix-port-format port))
              tramp-postfix-host-format
              localname))))

(defun racket--file-name-sans-remote-method (file-name)
  (if (file-remote-p file-name)
      (racket--host+user+port+name->file-name
       (racket--file-name->host+user+port+name
        file-name))
    file-name))
;;(racket--file-name-sans-remote-method "/ssh:host:/path/to/foo.rkt")
;;(racket--file-name-sans-remote-method "/ssh:user@host:/path/to/foo.rkt")
;;(racket--file-name-sans-remote-method "/ssh:user@host#123:/path/to/foo.rkt")

(defun racket--back-end-local-p (&optional back-end)
  (not (file-remote-p (plist-get (or back-end (racket-back-end))
                                 :directory))))

(defun racket-file-name-front-to-back (file)
  "Make a front end file name usable on the back end.

When a remote file name, extract the \"localname\" portion.

When Windows back end, substitute slashes with backslashes."
  (let* ((file (or (file-remote-p file 'localname)
                   file))
         (file (if (plist-get (racket-back-end) :windows)
                   (subst-char-in-string ?/ ?\\ file)
                 file)))
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

When Windows back end, replace back slashes with forward slashes.

When remote back end, treat FILE as the \"localname\" portion of
a remote file name, and form a remote file name by prepending to
FILE the back end's remote prefix."
  (let* ((back-end (racket-back-end))
         (file (if (plist-get back-end :windows)
                   (subst-char-in-string ?\\ ?/ file)
                 file))
         (file (if-let (prefix (file-remote-p (plist-get back-end :directory)))
                   (concat (substring-no-properties prefix) file)
                 file)))
    file))

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
  (let* ((back-end (racket-back-end))
         (back-end-dir (plist-get back-end :directory))
         (remote-source-dir (plist-get back-end :remote-source-dir))
         (tramp-dir (concat (file-remote-p back-end-dir)
                            remote-source-dir))
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
          ;;`copy-directory' creates symlinks when the source is a
          ;; symlink (and e.g. straight.el keeps package repos in a
          ;; symlinked dir), which won't work on a remote host. Change
          ;; `make-symbolic-link' to `copy-file' during the dynamic
          ;; extent of our call to `copy-directory'. Note that
          ;; `cl-flet' is /not/ the right thing to use here; see e.g.
          ;; <http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html>
          (cl-letf (((symbol-function 'make-symbolic-link)
                     (lambda (src dest ok-if-already-exists-p)
                       (copy-file src dest ok-if-already-exists-p nil))))
            (copy-directory racket--rkt-source-dir
                            tramp-dir
                            nil t t))
          ;; Now that we're sure the directory there is created, copy
          ;; our digest file.
          (copy-file temp-digest-file-here digest-file-there t)
          (delete-file temp-digest-file-here))
        (message "Racket Mode back end copied to remote back end at %s"
                 tramp-dir)))
    remote-source-dir))

(defun racket--back-end-args->command (back-end racket-command-args)
  "Given RACKET-COMMAND-ARGS, prepend path to racket for BACK-END."
  (if (racket--back-end-local-p back-end)
      `(,(or (plist-get back-end :racket-program)
             (executable-find racket-program)
             (user-error
              "Cannot find Racket executable\nracket-program: %S\nexec-path: %S"
              racket-program
              exec-path))
        ,@racket-command-args)
    (pcase-let ((`(,host ,user ,port ,_name)
                 (racket--file-name->host+user+port+name
                  (plist-get back-end :directory))))
      `("ssh"
        ,@(when port
            `("-p" ,(format "%s" port)))
        ,(if user
             (format "%s@%s"
                     user
                     host)
           host)
        ,(or (plist-get back-end :racket-program)
             racket-program) ;can't use `executable-find' remotely
        ,@racket-command-args))))

;;; File system watches

(defvar racket--back-end-watch-descriptors nil)

(defun racket--back-end-refresh-watches ()
  ;; Remove all our existing watches.
  (mapc #'file-notify-rm-watch racket--back-end-watch-descriptors)
  (setq racket--back-end-watch-descriptors nil)
  ;; Create new watches.
  (dolist (plist racket-back-end-configurations)
    (let ((back-end-dir (plist-get plist :directory)))
      (dolist (watch-dir (plist-get plist :restart-watch-directories))
        (dolist (file (cons watch-dir
                            (directory-files-recursively watch-dir ".+" t)))
          (when (file-directory-p file)
            (push (file-notify-add-watch (directory-file-name file)
                                         '(change)
                                         (apply-partially
                                          #'racket--back-end-watch-callback
                                          back-end-dir))
                  racket--back-end-watch-descriptors)))))))

(defvar racket--back-end-watch-changes (make-hash-table :test #'equal))

(defun racket--back-end-watch-callback (back-end-dir event)
  (pcase-let ((`(,_descriptor ,action ,file . _more) event))
    (unless (or (eq action 'stopped)
                (string-match-p "^[.]#" (file-name-base file)))
      (puthash back-end-dir
               (cl-remove-duplicates
                (cons file (gethash back-end-dir
                                    racket--back-end-watch-changes))
                :test #'equal)
               racket--back-end-watch-changes))))

(defun racket--back-end-watch-read/reset ()
  (let ((key (racket-back-end-name)))
    (prog1
        (gethash key
                 racket--back-end-watch-changes)
      (puthash key
               nil
               racket--back-end-watch-changes))))

(provide 'racket-back-end)

;; racket-back-end.el ends here
