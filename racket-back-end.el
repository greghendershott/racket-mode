;;; racket-back-end.el -*- lexical-binding: t; -*-

;; Copyright (c) 2021 by Greg Hendershott.
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

;;; Back end: configuration

(require 'racket-custom)
(require 'racket-util)
(require 'subr-x)
(require 'tramp)

;; A Racket Mode "back end" is a process running our racket/main.rkt.
;; The process could be local or (via ssh) remote. The remote process
;; could even be something like "ssh xvfb-run racket racket/main.rkt".
;; But for most people it's simply a local process, running on the
;; same machine as Emacs. The back end accepts commands and returns
;; responses, as well as giving non-command-response notifications
;; (logging, debugging). The back end also accepts connections on a
;; TCP port for one or more REPL sessions.
;;
;; When some buffer needs a back end, which back end does it use?
;;
;; For efficiency there are two levels of mapping:
;;
;; 1. From a buffer to a back end name.
;;
;; The simple case is the back end name is the same as the host name.
;; The default is to use the host-name portion of `default-directory'
;; when `tramp-tramp-file-p', else "127.0.0.1".
;;
;; Users can customize to use more than back end on the same host,
;; such as to run different versions of Racket for different projects.
;;
;; 2. Given a back end name:
;;
;; - It is trivial to get the names of its process and process-buffer.
;;   If these already exist they may be used to send commands. If they
;;   do not already exist then `racket--cmd/async' automatically calls
;;   `racket--cmd-open' to create them.
;;
;; - It can be found in `racket-back-end-configurations', a list of
;;   property lists. Each includes information necessary to:
;;
;;   - Create an Emacs process to run the back end locally or
;;     remotely.
;;
;;   - Make a TCP connection to start a REPL session.
;;
;;   - Decide how to translate file names between front and back end
;;   - (e.g. is the back end running on Windows).

;; Commands that create buffers that do not visit a file should use
;; (racket-back-end-name) as a suffix in the buffer name -- enabling a
;; unique buffer per back end as well as making it easy for users to
;; distinguish them. See the source for `racket-describe-mode' as an
;; example.

;; Values for the variable `racket-repl-buffer-name-function' need to
;; be aware of host names. For example, even
;; `racket-repl-buffer-name-shared' needs to return different REPL
;; buffer names for different back end names -- because a REPL on a
;; back end cannot run files hosted on another.

(defun racket-back-end-name ()
  "Return the name of a back end to use for `current-buffer'.

Consults `racket-back-end-name-hook' for special configurations,
but otherwise simply uses the host-name of `default-directory' as
the name of the back end configuration to use."
  (or (run-hook-with-args-until-success 'racket-back-end-name-hook)
      (let ((tfns (and (tramp-tramp-file-p default-directory)
                       (tramp-dissect-file-name default-directory))))
        (or (and tfns
                 (equal "ssh" (tramp-file-name-method tfns))
                 (if (fboundp 'tramp-file-name-real-host)
                     (tramp-file-name-real-host tfns) ;older tramp
                   (tramp-file-name-host tfns)))
            "127.0.0.1"))))

(defun racket--back-end-process-name (&optional name)
  (concat "racket-back-end-" (or name (racket-back-end-name))))

(defun racket--back-end-process-name-stderr (&optional name)
  (concat (racket--back-end-process-name name) "-stderr"))

(defvar racket-back-end-configurations nil
  "A list of property lists.

Instead of modifying this directly, use `racket-add-back-end' and
`racket-remove-back-end'.")

(defun racket-back-end (&optional name)
  "Return a back end configuration plist having NAME.

When NAME is nil, uses `racket-back-end-name'. If a configuration
does not already exist, automatically add one created by
`racket-back-end-default'."
  (let ((name (or name (racket-back-end-name))))
    (or (cl-find name
                 racket-back-end-configurations
                 :test #'string-equal
                 :key (lambda (plist) (plist-get plist :name)))
        (racket-add-back-end default-directory
                             :name name))))

(defun racket-add-back-end (example-file-name &rest plist)
  "Add a description of a Racket Mode back end configuration.

When a configuration with the same `:name` already exists it is
replaced.

Given some buffer, the hook `racket-back-end-name-hook' says the
name of the back end it should it use. The named back end
configurations are added by this function. If you don't specify a
back end configuration for a host, one is created automatically.
As a result, you may not need to use this function in your Emacs
init file; things may \"just work\" unless you have special
needs.

EXAMPLE-FILE-NAME is an example of the sort of file name you
would use to visit a file on the host. This could be a local file
name like \"/\" or a `tramp-tramp-file-p' like \"/ssh:host:\",
\"/ssh:user@host:\", \"/host:\", and so on.

EXAMPLE-FILE-NAME is used as a convenience to supply default
values for properties if do do not specify them. In addition to
extracting things like the host-name, other properties get
reasonable defaults based on whether it is local or remote.

After EXAMPLE-FILE-NAME, the remainining arguments are
alternating :keywords and values describing a back end. The only
required property is `:name`.

- :name

  A unique name for the back end.

  This name is distinct from :host-name to allow for multiple
  back ends on the same host (for example each running a
  different version of Racket).

  The name is used as a suffix for the names of buffers that are
  not visiting a file. That way there is a unique buffer per back
  end, and, users can easily distinguish them. For example
  `*Racket Describe <foo>` and `Racket Describe <bar>*` buffers.

  The name is also combined with a \"racket-back-end-\" prefix to
  make the name of the Emacs process used to run the back end.

- :racket-program

  When not nil this is used instead of the value of the
  customization variable `racket-program'.

- :remote-source-dir

  When :host-name is not \"127.0.0.1\", this is where on a remote
  host to copy the back end's *.rkt files when they do not exist
  or do not match the digest of the local files. This must be
  `file-name-absolute-p' on the remote --- and just the file
  name, a.k.a. localname, /not/ a full `tramp-tramp-file-p'. The
  default value is \"/tmp/racket-mode-back-end\".

- :host-name

  When :host-name is not \"127.0.0.1\", used to start a back end
  on a host via SSH.

  Always used to make TCP/IP connections to a back end for REPL
  sesssions.

  This may be a Host alias from ~/.ssh/config with a HostName, in
  which case the latter is used as the actual host-name.

- :user-name

  When :host-name is not \"127.0.0.1\", used to make an SSH
  connection.

  It may be nil, meaning to use a value from ~/.ssh/config.

- :ssh-port

  When :host-name is not \"127.0.0.1\", used to make an SSH
  connection.

  Note that this is nil or `numberp' --- not `stringp'.

  It may be nil, meaning to use a value from ~/.ssh/config or the
  SSH protocol default value. If you wouldn't include a port when
  typing a tramp file name for `find-file', then don't do so in
  EXAMPLE-FILE-NAME, and, don't supply a :ssh-port value of 22;
  just let it be nil.

- :repl-tcp-accept-host

  Host from which the back end TCP REPL server will accept
  connections. \"127.0.0.1\" means it will accept only local
  connections. \"0.0.0.0\" means it will accept connections from
  anywhere --- which usually is risky unless the remote is behind
  a firewall that limits connections!

- :repl-tcp-port

  The port number the back end TCP REPL server uses to listen for
  connections.

  Note that this is `numberp' --- not `stringp'.

  When 0, this means the back end chooses an available port --- a
  so-called \"ephemeral\" port. Usually that is practical only on
  a local host. Otherwise a specific port number should be used,
  and, remember to allow that in the remote's firewall.

- :gui

  When this is false: The back end will /not/ attempt to load
  racket/gui/base eagerly. This can make sense for a remote back
  end running on a headless server, where the Racket gui-lib
  package is installed, making racket/gui/base available, but you
  do not want to use it. Keep in mind that when `gui` is false,
  if you `racket-run' a program that /does/ require
  racket/gui/base (directly or indirectly), you cannot run a
  second such program without Racket complaining that
  racket/gui/base cannot be instantiated more than once. If that
  happens, you must use `racket-start-back-end' to restart the
  back end process.

  When this is true: The back end /will/ attempt to load
  racket/gui/base eagerly. This can make sense for a remote back
  end running on a headless server, where the Racket gui-lib
  package is installed, making racket/gui/base available, and you
  do want to use it. In this case the headless server should have
  the xvfb package installed, and you should set the
  :racket-program property to something like \"xvfb-run racket\".
  Now your programs can use modules that require racket/gui/base,
  including obvious things like plot, as well as some
  unfortunately less-obvious things.

- :windows

  Whether the back end uses Windows style path names. Used to do
  translation betwen slashes and backslashes between Emacs and
  Racket.

The default property values are appropriate for whether
EXAMPLE-FILE-NAME is local or remote:

- When EXAMPLE-FILE-NAME satisfies `tramp-tramp-file-p', it is
  dissected to set :user-name, :host-name, and :ssh-port.
  Furthermore, :repl-tcp-port is set to 55555,
  :repl-tcp-accept-host is set to \"0.0.0.0\" \(accepts
  connections from anywhere), :gui is nil, and :windows is nil.

  When working with back ends on remote hosts, *remember to check
  your remote host firewall*. The goal here is to make sure
  things work for you --- and only you. Probably you want the
  firewall to limit from where it accepts SSH connections on
  :ssh-port. Also you need the firewall to accept connections on
  :repl-tcp-port, but again, limiting from where --- either in
  the firewall or by setting :repl-tcp-accept-host to a value
  that is /not/ \"0.0.0.0\".

- Otherwise, reasonable defaults are used for a local back end.
  For example :host-name is set to \"127.0.0.1\",
  :repl-tcp-port is set to 0 \(meaning the back end picks an
  ephemeral port), :repl-tcp-accept-host is set to \"127.0.0.1\"
  \(meaning the back end only accept TCP connections locally),
  :gui is true, and :windows is set based on `system-type'.

Although the default values usually \"just work\" for local and
remote back ends, you might want a special configuration. Here
are a few examples.

#+BEGIN_SRC lisp
    ;; Set `racket-program' to Racket built from source
    (setq racket-program \"~/src/racket-lang/racket/bin/racket\")

    ;; 1. A back end configuration named \"127.0.0.1\" is
    ;; created automatically and works fine as a default
    ;; for buffers visiting local files, so we don't need
    ;; to define one here.

    ;; 2. However assume we want buffers under /var/tmp/8.0
    ;; instead to use Racket 8.0. We say they should use a
    ;; back end that we'll name \"local-8.0\" ...
    (add-hook 'racket-back-end-name-hook
              (lambda ()
                (when (string-match-p \"^/var/tmp/8.0\" default-directory)
                  \"local-8.0\")))
    ;; ... which we define to have the usual settings for
    ;; a local back end, except :racket-program is where
    ;; we have installed Racket 8.0.
    (racket-add-back-end \"/\"
                         :name           \"local-8.0\"
                         :racket-program \"~/racket/8.0/bin/racket\")

    ;; 3. A back end configuration named \"linode\" will be created
    ;; automatically for buffers vising tramp file names like
    ;; \"/ssh:user@linode\" so we need not specify one here.
    ;;
    ;; If ~/.ssh/config defines a Host alias named \"linode\",
    ;; with HostName and User settings, a tramp file name as simple as
    ;; \"/linode:\" would work fine with tramp -- and its automatically
    ;; created back end configuration would work fine, too.

    ;; 4. For the sake example, assume for buffers visiting
    ;; /ssh:headless:* we want :racket-program instead to be
    ;; \"xvfb-run racket\" and :gui to be true. In that case we
    ;; can provide the example file name, and set just the
    ;; properties we want to be different.
    (racket-add-back-end \"/ssh:headless:\"
                         :name           \"headless\"
                         :racket-program \"xvfb-run racket\"
                         :gui            t)
#+END_SRC
"
  (unless (and (stringp example-file-name)
               (file-name-absolute-p example-file-name))
    (signal 'wrong-type-argument `((and stringp file-name-absolute-p)
                                   example-file-name
                                   ,example-file-name)))
  (unless (memq :name plist)
    (user-error "racket-add-back-end: You must supply a :name"))
  (let* ((tfns (and (tramp-tramp-file-p example-file-name)
                    (tramp-dissect-file-name example-file-name)))
         (user (and tfns
                    (equal "ssh" (tramp-file-name-method tfns))
                    (tramp-file-name-user tfns)))
         (host (or (and tfns
                        (equal "ssh" (tramp-file-name-method tfns))
                        (if (fboundp 'tramp-file-name-real-host)
                            (tramp-file-name-real-host tfns) ;older tramp
                          (tramp-file-name-host tfns)))
                   "127.0.0.1"))
         (port (and tfns
                    (equal "ssh" (tramp-file-name-method tfns))
                    (let ((p (tramp-file-name-port tfns)))
                      (and (not (equal p 22))
                           p))))
         (local-p (equal host "127.0.0.1"))
         (plist
          (list
           :name                 (or (plist-get plist :name)
                                     host)
           :racket-program       (or (plist-get plist :racket-program)
                                     nil)
           :remote-source-dir    (or (plist-get plist :remote-source-dir)
                                     "/tmp/racket-mode-back-end")
           :host-name            (or (plist-get plist :host)
                                     host)
           :user-name            (or (plist-get plist :user)
                                     user)
           :ssh-port             (or (plist-get plist :ssh-port)
                                     port)
           :repl-tcp-accept-host (or (plist-get plist :repl-tcp-accept-host)
                                     (if local-p "127.0.0.1" "0.0.0.0"))
           :repl-tcp-port        (or (plist-get plist :repl-tcp-port)
                                     (if local-p 0 55555))
           ;; These booleanp things need to distinguish nil meaning
           ;; "user specififed false" from "user did not specify
           ;; anything".
           :gui                  (if (memq :gui plist)
                                     (plist-get plist :gui)
                                   local-p)
           :windows              (if (memq :windows plist)
                                     (plist-get plist :windows)
                                   (and local-p racket--winp)))))
    (racket--back-end-validate plist)
    (racket-remove-back-end (plist-get plist :name))
    (push plist racket-back-end-configurations)
    plist))

(defun racket--back-end-validate (plist)
  (cl-flet ((check
             (type key)
             (let ((v (plist-get plist key)))
               (unless (funcall type v)
                 (signal 'wrong-type-argument (list type key v)))))
            (number-or-null-p (n) (or (not n) (numberp n))))
    (check #'stringp :name)
    (check #'string-or-null-p :racket-program)
    (check #'stringp :host-name)
    (check #'string-or-null-p :user-name)
    (check #'number-or-null-p :ssh-port)
    (check #'stringp :repl-tcp-accept-host)
    (check #'numberp :repl-tcp-port)
    (unless (string-equal (plist-get plist :host-name) "127.0.0.1")
      (check #'stringp :remote-source-dir)
      (check #'file-name-absolute-p :remote-source-dir))
    (check #'booleanp :gui)
    (check #'booleanp :windows))
  plist)

(defun racket-remove-back-end (name)
  (setq racket-back-end-configurations
        (cl-remove-if (lambda (plist)
                        (string-equal (plist-get plist :name)
                                      name))
                      racket-back-end-configurations)))

(defun racket--back-end-local-p (&optional back-end)
  (equal (plist-get (or back-end (racket-back-end)) :host-name)
         "127.0.0.1"))

(defun racket-file-name-to-user+host+port (filename)
  (let* ((tfns (and (tramp-tramp-file-p filename)
                    (tramp-dissect-file-name filename)))
         (user (and tfns
                    (equal "ssh" (tramp-file-name-method tfns))
                    (tramp-file-name-user tfns)))
         (host (or (and tfns
                        (equal "ssh" (tramp-file-name-method tfns))
                        (if (fboundp 'tramp-file-name-real-host)
                            (tramp-file-name-real-host tfns) ;older tramp
                          (tramp-file-name-host tfns)))
                   "127.0.0.1"))
         (port (and tfns
                    (equal "ssh" (tramp-file-name-method tfns))
                    (let ((p (tramp-file-name-port tfns)))
                      (and (not (equal p 22))
                           p)))))
    (list user host port)))

(defun racket--back-end-actual-host ()
  "Return actual host name, considering possible ~/.ssh/config HostName.

The user may have supplied a tramp file name using a Host defined
in ~/.ssh/config, which has a HostName option that is the actual
host name. The ssh command of course uses that config so we can
start a back end process just fine. However `racket-repl-mode'
needs to open a TCP connection at the same host. This function
lets it know the HostName if any."
  (let ((host (plist-get (racket-back-end) :host-name)))
    (condition-case nil
        (with-temp-buffer
         (insert-file-contents-literally "~/.ssh/config")
         (goto-char (point-min))
         ;; Dumb parsing to find a HostName within the Host block.
         ;; Does not handle Match blocks except to recognize them
         ;; ending the desired Host block.
         (let ((case-fold-search t))
           (search-forward-regexp (concat "host[ ]+" host "[ \n]"))
           (let ((limit (save-excursion
                          (or (search-forward-regexp "\\(host|match\\) " nil t)
                              (point-max)))))
             (search-forward-regexp "hostname[ ]+\\([^ \n]+\\)" limit)
             (match-string 1))))
      (error host))))

(defun racket-file-name-front-to-back (file)
  "Make a front end file name usable to give to the back end.

When a tramp file name, extract the \"localname\" portion of a
tramp file name.

When Windows back end, substitute slashes with backslashes."
  (let* ((file (if (tramp-tramp-file-p file)
                   (tramp-file-name-localname
                    (tramp-dissect-file-name file))
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
a tramp file name, and make a tramp file name using various back
end property list values for the remaining components."
  (let* ((back-end (racket-back-end))
         (file (if (plist-get back-end :windows)
                   (subst-char-in-string ?\\ ?/ file)
                 file))
         (file (if (racket--back-end-local-p back-end)
                   file
                 (racket--make-tramp-file-name (plist-get back-end :user-name)
                                               (plist-get back-end :host-name)
                                               (plist-get back-end :ssh-port)
                                               file))))
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
  (let* ((tramp-dir (racket--make-tramp-file-name
                     (plist-get (racket-back-end) :user-name)
                     (plist-get (racket-back-end) :host-name)
                     (plist-get (racket-back-end) :ssh-port)
                     (plist-get (racket-back-end) :remote-source-dir)))
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
        (message "Racket Mode back end copied to remote back end at %s"
                 (plist-get (racket-back-end) :host-name))))
    (plist-get (racket-back-end) :remote-source-dir)))

(defun racket--make-tramp-file-name (user host port localname)
  (unless (or (not user) (stringp user)) (error "user must be nil or stringp"))
  (unless (or (not port) (numberp port)) (error "port must be nil or numberp"))
  (unless (stringp host) (error "host must be stringp"))
  (unless (stringp localname) (error "localname must be stringp"))
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
                                    port
                                    localname))
    (wrong-number-of-arguments
     ;; Otherwise try the oldest signature (METHOD USER HOST
     ;; LOCALNAME). Note this means needing to ignore `port'.
     (condition-case nil
         (with-no-warnings ;from byte compiler
           (tramp-make-tramp-file-name "ssh"
                                       (or user "")
                                       (if (or (not port) (= port 22))
                                           host
                                         (format "%s#%s" host port))
                                       localname))
       (wrong-number-of-arguments
        (error "Unsupported flavor of `tramp-make-tramp-file-name'."))))))

(provide 'racket-back-end)

;; racket-back-end.el ends here
