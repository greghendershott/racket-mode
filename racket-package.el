;;; racket-package.el -*- lexical-binding: t -*-

;; Copyright (c) 2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-macs)
(require 'seq)
(require 'url-parse)
(require 'racket-custom)

(defun racket--package-raco-pkg (command k)
  (with-temp-buffer
    (let ((raco (if (file-name-absolute-p racket-program)
                    (expand-file-name racket-program) ;handle e.g. ~/
                  racket-program)))
      (call-process-shell-command (concat raco " -l raco pkg " command) nil t))
    (goto-char (point-min))
    (funcall k)))

(defun racket--package-get-installed ()
  (append
   (racket--package-raco-pkg "show --all --installation --long --dir"
                             (racket--package-installed-parse 'installation))
   (racket--package-raco-pkg "show --all --user --long --dir"
                             (racket--package-installed-parse 'user))))

(defun racket--package-installed-parse (scope)
  (lambda ()
    (let ((results nil))
      (goto-char (point-min))
      (when (search-forward-regexp (rx (group "Package" (? "[*=auto]") (1+ space))
                                       (group "Checksum" (1+ space))
                                       (group "Source" (1+ space))
                                       (group "Directory" (0+ space))
                                       "\n")
                                   nil t)
        (let ((package-ofs (1- (match-beginning 1)))
              (checksum-ofs (1- (match-beginning 2)))
              (source-ofs (1- (match-beginning 3)))
              (dir-ofs (1- (match-beginning 4))))
          (while (and (not (eobp))
                      (not (looking-at (rx "[" (1+ digit) " auto-installed packages not shown]"))))
            (cl-flet ((get (beg end)
                           (let ((str (buffer-substring-no-properties (+ (point) beg)
                                                                      (+ (point) end))))
                             ;; string-trim not available in older Emacs, so...
                             (string-match (rx bos
                                               (*? (any " "))
                                               (group (*? anything))
                                               (*? (any " "))
                                               eos)
                                           str)
                             (match-string 1 str))))
              (pcase-let* ((name* (get package-ofs checksum-ofs))
                           (`(,name . ,status) (if (string-match-p (rx (1+ any) "*" eos)
                                                                   name*)
                                                   (cons (substring name* 0 -1)
                                                         'dependency)
                                                 (cons name* 'manual)))
                           (checksum (get checksum-ofs source-ofs))
                           (source (get source-ofs dir-ofs))
                           (dir (get dir-ofs (- (save-excursion (forward-line 1) (point))
                                                (point)
                                                1)))
                           (dir (if (string-match-p (rx bos "\"" (group (+ any)) "\"" eos)
                                                    dir)
                                    (substring dir 1 -1)
                                  dir)))
                (push (list name scope status checksum source dir)
                      results)))
            (forward-line 1))))
      results)))

(defvar racket--package-catalog-cache nil)
(defun racket--package-get-catalog (&optional refresh-p)
  (when refresh-p
    (setq racket--package-catalog-cache nil))
  (unless racket--package-catalog-cache
    (racket--package-raco-pkg
     "catalog-show --all"
     (lambda ()
       (goto-char (point-min))
       (while (search-forward-regexp (rx (*? "\n") "Package name: " (group (+? (not (any "\n")))) "\n")
                                     nil t)
         (let ((name (match-string 1))
               (author "")
               (description "")
               (checksum "")
               (deps nil)
               (source "")
               (tags ""))
           (while (and (not (looking-at "\nPackage name:"))
                       (search-forward-regexp (rx " " (group (+? (not (any ":")))) ":" (* " ") (group (*? (not (any "\n")))) "\n")
                                              nil t))
             (let ((key (match-string 1))
                   (val (match-string 2)))
               (pcase key
                 ("Author" (setq author val))
                 ("Description" (setq description val))
                 ("Checksum"    (setq checksum val))
                 ("Dependencies" (while (and (not (looking-at "\nPackage name:"))
                                             (search-forward-regexp (rx "  " (group (+? (not (any "\n")))) "\n")
                                                                    nil t))
                                   (push (match-string 1) deps)))
                 ("Source" (setq source val))
                 ("Tags" (setq tags val)))))
           (push (list name description checksum (nreverse deps) author source tags)
                 racket--package-catalog-cache))))))
  racket--package-catalog-cache)

(defvar racket--package-details
  (make-hash-table :test 'equal)
  "Hash-table from packge name to package details property list;
includes all packages, both installed and from catalog.")

(defun racket--package-refresh-details (&optional refresh-catalog-p)
  (let ((installed (racket--package-get-installed))
        (catalog (racket--package-get-catalog refresh-catalog-p)))
    (clrhash racket--package-details)
    ;; Eveything from catalog -- some of which may be installed, in
    ;; which case we want "merge" information.
    (mapc (pcase-lambda (`(,name ,description ,checksum ,deps ,author ,source ,tags))
            (pcase-let* ((`(,scope ,status ,checksum ,source ,dir)
                          (pcase (assoc name installed)
                            (`(,_name ,scope ,status ,checksum ,source ,dir)
                             (list (symbol-name scope) (symbol-name status) checksum source dir))
                            (_
                             (list nil "available" checksum source nil))))
                         (details (list :name name
                                        :description description
                                        :checksum checksum
                                        :deps deps
                                        :author author
                                        :source source
                                        :tags tags
                                        :scope scope
                                        :status status
                                        :dir dir)))
              (puthash name details racket--package-details)))
          catalog)
    ;; Any additional installed, such as links, not handled above
    ;; because not also from catalog.
    (mapc (pcase-lambda (`(,name ,scope ,status ,checksum ,source ,dir))
            (unless (assoc name catalog)
              (let* ((status (symbol-name status))
                     (scope (symbol-name scope))
                     (details (list :name name
                                    :scope scope
                                    :status status
                                    :checksum checksum
                                    :source source
                                    :dir dir)))
                (puthash name details racket--package-details))))
          installed)))

(defun racket--package-tabulated-list-revert-hook ()
  (racket--package-refresh-details t))

(defun list-racket-packages ()
  "Uses raco pkg commands to populate a `racket-packages-mode' buffer.

Press RET on a package to `describe-racket-package', when opens a
buffer where you can view details, and use buttons to
install/update/remove the package."
  (interactive)
  (with-current-buffer (get-buffer-create "*Racket Packages*")
    (unless (eq major-mode 'racket-packages-mode)
      (racket-packages-mode))
    (with-silent-modifications
      (erase-buffer)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (pop-to-buffer (current-buffer))))

(defun racket-package-menu-describe ()
  "Describe the package at point in a `racket-packages-mode' buffer."
  (interactive)
  (describe-racket-package (tabulated-list-get-id)))

(defun describe-racket-package (&optional name-or-button)
  "Describe details of a Racket package.

Buttons allow you to install/update/remove the package, depending
on its status. "
  (interactive "sRacket package name: ")
  (let* ((name (if name-or-button
                   (if (stringp name-or-button)
                       name-or-button
                     (button-label name-or-button))
                 (tabulated-list-get-id)))
         (details (gethash name racket--package-details)))
    (unless details (user-error "no package"))
    (help-setup-xref (list #'describe-racket-package (plist-get details :name))
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (racket--package-insert-details details)))))

(defun racket--package-insert-details (details)
  (let ((name (plist-get details :name))
        (status (plist-get details :status)))
    (insert (propertize name
                        'font-lock-face 'bold))
    (pcase status
      ("available"
       (insert " is available to ")
       (insert (propertize (format "raco pkg install --auto %s" name)
                           'button '(t)
                           'face '(button bold)
                           'category 'default-button
                           'action #'racket--raco-pkg-mutate)))
      ("manual"
       (insert " was manually installed\n\n")
       (insert (propertize (format "raco pkg update --auto %s" name)
                           'button '(t)
                           'face '(button bold)
                           'category 'default-button
                           'action #'racket--raco-pkg-mutate))
       (insert " or ")
       (insert (propertize (format "raco pkg remove --auto %s" name)
                           'button '(t)
                           'face '(button bold)
                           'category 'default-button
                           'action #'racket--raco-pkg-mutate)))
      ("dependency"
       (insert " was automatically installed as a dependency")))
    (newline)
    (newline)
    (let ((lks `(("   Directory" :dir)
                 ("       Scope" :scope)
                 ("      Source" :source)
                 ("    Checksum" :checksum)
                 ("      Author" :author)
                 ("        Tags" :tags)
                 ("Dependencies" :deps)
                 (" Description" :description))))
      (dolist (lk lks)
        (pcase-let* ((`(,l ,k) lk)
                     (v (plist-get details k)))
          (when v
            (insert (propertize (concat l ":")
                                'font-lock-face 'package-help-section-name))
            (pcase k
              (:deps
               (let ((firstp t))
                 (dolist (name v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n              "))
                   (let ((details (gethash (car (split-string name))
                                           racket--package-details)))
                     (if details
                         (insert (propertize name
                                             'button '(t)
                                             'category 'default-button
                                             'follow-link t
                                             'action #'describe-racket-package))
                       (insert name)))))
               (newline))
              (:source
               (insert " ")
               (insert (racket--package-linkify-source v))
               (newline))
              (_ (insert (format " %s\n" v))))))))))

(defun racket--raco-pkg-mutate (&optional button)
  (interactive)
  (unless button (error "no raco pkg button here"))
  (let ((cmd (button-label button))
        (inhibit-read-only t)
        (_ (goto-char (point-max)))
        (end-details (point)))
    (newline)
    (insert (propertize cmd 'font-lock-face 'bold))
    (newline)
    (call-process-shell-command cmd nil t t)
    (newline)
    (insert "Done.")
    ;; Fully refresh *Racket Packages* install details because
    ;; "--auto" commands can install/remove/update multiple,
    ;; dependency packages. (But needn't refresh catalog details).
    (let ((id nil))
      (with-current-buffer "*Racket Packages*"
        (setq id (tabulated-list-get-id))
        (racket--package-refresh-details nil)
        (tabulated-list-print t)
        (let ((win (get-buffer-window (current-buffer))))
          (when win
            (set-window-point win (point)))))
      ;; Also refresh the status for this package, at the top of this
      ;; detail buffer.
      (delete-region (point-min) end-details)
      (goto-char (point-min))
      (let ((details (gethash id racket--package-details)))
        (when details
          (racket--package-insert-details details)))
      (goto-char (point-min)))))

(defun racket--package-linkify-source (str)
  ;; See "Package Sources" in docs esp re git and github source
  ;; schemes. The idea here is to get a URL that is likely to work in
  ;; a web browser for the repo, without trying to be too clever about
  ;; things like revisions etc.
  (save-match-data
    (cl-labels ((parse
                 (str)
                 (cond
                  ((string-match (rx bos "("
                                     "catalog "
                                     "\"" (+? (not (any "\""))) "\""
                                     " "
                                     "\"" (group (+? (not (any "\"")))) "\""
                                     ")" eos)
                                 str)
                   (parse (match-string 1 str)))
                  ((string-match (rx bos "("
                                     (or "link" "static-link")
                                     " "
                                     "\"" (group (+? (not (any "\"")))) "\""
                                     ")" eos)
                                 str)
                   (parse (match-string 1 str)))
                  ((string-match (rx bos "file://" (group (+? any)) (? "?type=" (+? any)) eos)
                                 str)
                   (concat "file://" (match-string 1 str)))
                  ;; git flavors: Use https and simplify the
                  ;; path+query to just user and repo elements.
                  ((string-match-p (rx bos (or "github://" "git://" "git+http://" "git+https://")) str)
                   (pcase-let* ((u (url-generic-parse-url str))
                                (`(,path . ,_query) (url-path-and-query u))
                                (`(,_ ,user ,repo . ,_) (split-string path "/"))
                                (path (concat "/" user "/" repo)))
                     (setf (url-type u) "https")
                     (setf (url-filename u) path)
                     (url-recreate-url u)))
                  ;; Local file path; make file: url
                  ((string-match (rx bos "/" (not (any "/")))
                                 str)
                   (concat "file://" str))
                  (t str))))
      (let ((url (parse str)))
        (propertize str
                    'button '(t)
                    'category 'default-button
                    'action #'racket-browse-package-url
                    'racket-package-url url)))))
;; (racket--package-linkify-source "(static-link \"/path/to/file\")")
;; (racket--package-linkify-source "(catalog \"2d\" \"github://github.com/racket/2d?path=2d\")")

(defun racket-browse-package-url (button)
  (browse-url (button-get button 'racket-package-url)))

(defvar racket-packages-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          `(("RET" ,#'racket-package-menu-describe)))
    m)
  "Keymap for `racket-packages-mode'.")

(define-derived-mode racket-packages-mode tabulated-list-mode
  "RacketPackages"
  "Major mode for Racket package management.

\\{racket-package-mode-map}
"
  (setq show-trailing-whitespace nil)
  (setq tabulated-list-sort-key '("Name" . nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format
        `[("Name"      20 t)
          ("Status"    10 t)
          ("Checksum"   8 nil)
          ("Source"    15 t)
          ("Description" 15 t)])
  (racket--package-refresh-details t)
  (setq tabulated-list-entries #'racket--package-tabulated-list-entries)
  (add-hook 'tabulated-list-revert-hook #'racket--package-tabulated-list-revert-hook nil t))


(defun racket--package-tabulated-list-entries ()
  (let ((result nil))
    (maphash
     (lambda (name details)
       (let ((status-face (pcase (plist-get details :status)
                            ("available" 'package-status-available)
                            (_           'package-status-installed))))
         (push
          (list name
                (vector (propertize name
                                    'face 'package-name
                                    'font-lock-face 'package-name
                                    'button '(t)
                                    'category 'default-button
                                    'follow-link t
                                    'action #'describe-racket-package)
                        (propertize (plist-get details :status)
                                    'font-lock-face status-face)
                        (propertize (or (plist-get details :checksum) "")
                                    'font-lock-face status-face)
                        (or (plist-get details :source) "")
                        (or (plist-get details :description) "")))
          result)))
     racket--package-details)
    result))

(provide 'racket-package)

;; racket-package.el ends here
