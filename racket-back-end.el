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

;; Commands that create buffers that do not visit a file should use
;; `(plist-get (racket-back-end) 'name)` as a suffix in the buffer
;; name -- enabling a unique buffer per back end as well as making it
;; easy for users to distinguish them. See the source for
;; `racket-describe-mode' as an example.

;; Values for the variable `racket-repl-buffer-name-function' need to
;; be aware of host names. For example, even
;; `racket-repl-buffer-name-shared' needs to return different names
;; for different back end names -- because a REPL on a back end cannot
;; run files hosted on another.

(defun racket-back-end ()
  "Use the variable `racket-back-end-functions' to return a property list.

Note that such functions may use the buffer-local value of the
variable `default-directory' to decided what to return."
  (racket--back-end-validate
   (or (cl-some #'funcall racket-back-end-functions)
       (racket-back-end-default))))

(defsubst racket--back-end-process-name ()
  (concat "racket-back-end-" (plist-get (racket-back-end) 'name)))

(defsubst racket--back-end-process-name-stderr ()
  (concat (racket--back-end-process-name) "-stderr"))

(defun racket--back-end-local-p ()
  (equal (plist-get (racket-back-end) 'host-name) "127.0.0.1"))

(defun racket--back-end-validate (v)
  (cl-macrolet ((check (pred var)
                       `(unless (,pred ,var)
                          (error "%s should be %s but is `%S` in `%S` in %S"
                                 ',var
                                 ',pred
                                 ,var
                                 v
                                 (current-buffer)))))
    (check stringp (plist-get v 'name))
    (when (plist-get v 'racket-program)
      (check stringp (plist-get v 'racket-program)))
    (check stringp (plist-get v 'host-name))
    (when (plist-get v 'user-name)
      (check stringp (plist-get v 'user-name)))
    (when (plist-get v 'ssh-port)
      (check numberp (plist-get v 'ssh-port)))
    (check stringp (plist-get v 'repl-tcp-accept-host))
    (check stringp (plist-get v 'remote-source-dir)))
  v)

(defun racket-back-end-default ()
  "Create a back end property list with default values."
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
          'repl-tcp-port        (if local-p 0 55555)
          'gui                  local-p
          'windows              (and local-p racket--winp))))

(defun racket--back-end-filename-to-user+host+port (filename)
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
  (let ((host (plist-get (racket-back-end) 'host-name)))
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
         (file (if (plist-get (racket-back-end) 'windows)
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
  (let ((back-end (racket-back-end)))
    (racket--make-tramp-file-name (plist-get back-end 'user-name)
                                  (plist-get back-end 'host-name)
                                  (plist-get back-end 'ssh-port)
                                  (if (plist-get back-end 'windows)
                                      (subst-char-in-string ?\\ ?/ file)
                                    file))))

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
                     (plist-get (racket-back-end) 'user-name)
                     (plist-get (racket-back-end) 'host-name)
                     (plist-get (racket-back-end) 'ssh-port)
                     (plist-get (racket-back-end) 'remote-source-dir)))
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
                 (plist-get (racket-back-end) 'host-name))))
    (plist-get (racket-back-end) 'remote-source-dir)))

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
