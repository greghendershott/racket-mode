;;; racket-package.el -*- lexical-binding: t -*-

;; Copyright (c) 2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'seq)
(require 'url-parse)
(require 'racket-complete)
(require 'racket-custom)
(require 'racket-browse-url)
(require 'racket-back-end)
(require 'racket-cmd)
(require 'xref)

;;; racket-package-mode summary list of packages

(defun racket--package-buffer-name ()
   (format "*Racket Packages <%s>*" (racket-back-end-name)))

;;;###autoload
(defun list-racket-packages ()
  "Open a `racket-package-mode' buffer for the active back end."
  (interactive)
  (with-current-buffer (get-buffer-create (racket--package-buffer-name))
    (unless (eq major-mode 'racket-package-mode)
      (racket-package-mode))
    (pop-to-buffer (current-buffer))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defvar racket-package-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          `(("RET" ,#'racket-package-describe)))
    m)
  "Keymap for `racket-package-mode'.")

(define-derived-mode racket-package-mode tabulated-list-mode
  "Racket Package List"
  "Major mode for Racket package management.

The list of packages is equivalent to \"raco pkg show -all\" on
the active back end -- that is, all packages installed manually
or as dependencies -- plus packages available from your
configured catalogs, assuming you have run the command
`racket-package-refresh'.

On each row you can press RET to `describe-racket-package', which
opens a buffer where you can view details, and use buttons to
install/update/remove the package.

\\{racket-package-mode-map}
"
  (setq show-trailing-whitespace nil)
  (setq tabulated-list-sort-key '("Name" . nil))
  (setq tabulated-list-padding 0)
  (setq tabulated-list-format
        `[("Name"      20 t)
          ("Status"    10 t)
          ("Description" 15 t)])
  (setq tabulated-list-entries
        #'racket-package-tabulated-list-entries))

(defun racket-package-tabulated-list-entries ()
  (seq-map (lambda (summary)
             (pcase-let* ((`(,name ,status ,desc) summary)
                          (status-face
                           (pcase status
                             ("available" 'package-status-available)
                             (_           'package-status-installed))))
               (list name
                     (vector (list name
                                   :type 'describe-racket-package
                                   'face 'package-name)
                             (propertize status
                                         'font-lock-face status-face)
                             desc))))
           (racket--cmd/await nil `(pkg-list))))

;;; help buffer of details about a single package, and button actions

(defun racket-package-describe ()
  "`describe-racket-package' the package at point."
  (interactive)
  (describe-racket-package (tabulated-list-get-id)))

(define-button-type 'describe-racket-package
  'action #'describe-racket-package)

;;;###autoload
(defun describe-racket-package (&optional name-or-button)
  "Describe details of a Racket package.

Depending on the package status, buttons let you install, update,
and/or remove the package and its dependencies. These convenience
buttons are equivalent to using the command line on the active
back end to do \"raco pkg {install update remove} --auto\". For
other operations, you still need to use \"raco pkg\" yourself;
see <https://docs.racket-lang.org/pkg/cmdline.html>.

Detail values are links when possible:

- The /Catalog/ (when \"https://pkgs.racket-lang.org\") links to
  the package's web page, which may have additional details not
  available locally.

- The /Source/ links to the repo's web page or local filesystem.

- The /Directory/ for an installed package opens a dired buffer.

- Each /Dependencies/ name links to details about that package.

- For installed packages, each /Modules/ item links to the local
  file. There is also a button to each module's locally installed
  documentation, if any.

If the package is available from a catalog, additional details
will be shown, assuming you have run the command
`racket-package-refresh'."
  (interactive (racket--package-completing-read))
  (let ((name (if name-or-button
                  (if (stringp name-or-button)
                      name-or-button
                    (button-label name-or-button))
                (tabulated-list-get-id))))
    (unless name (user-error "no package"))
    (racket--cmd/async
     nil
     `(pkg-details ,name)
     (lambda (details)
       (help-setup-xref (list #'describe-racket-package (alist-get 'name details))
                        (called-interactively-p 'interactive))
       (with-help-window (help-buffer)
         (with-current-buffer standard-output
           (racket--package-insert-details details)))))))

(defvar racket-package-history nil
  "History for `describe-racket-package'.")

(defconst racket-package-completion-category 'racket-package
  "Completion category for Racket package names.
Allows users to customize via `completion-category-overrides'.")

(defun racket--package-completing-read ()
  "Arrange for :category and :affixation-function to show metadata."
  (pcase-let*
      ((pkgs (racket--cmd/await nil `(pkg-list)))
       (pkgs (seq-map (pcase-lambda (`(,name ,stat ,desc))
                        (let* ((stat-face
                                (pcase stat
                                  ("installed"  'font-lock-escape-face)
                                  ("dependency" 'font-lock-keyword-face)
                                  ("available"  'completions-annotations)))
                               (stat (propertize stat 'face stat-face))
                               (desc (propertize desc 'face 'font-lock-doc-face)))
                         (propertize name
                                     'racket-affix (list stat desc))))
                      pkgs))
       (affix (racket--make-affix [16 [11 nil] [0 nil]]))
       (val (completing-read "Describe Racket package: "
                             (racket--completion-table
                              pkgs
                              `((category . ,racket-package-completion-category)
                                (affixation-function . ,affix)))
                             nil nil nil
                             'racket-package-history nil)))
    (list (and (> (length val) 0) val))))

(defconst racket--package-main-catalog
  "https://pkgs.racket-lang.org")

(defun racket--package-insert-details (details)
  (let ((name (alist-get 'name details))
        (status (alist-get 'status details)))
    (insert (propertize name
                        'font-lock-face 'bold))
    (pcase status
      ("available"
       (insert " is available to ")
       (racket--package-insert-raco-pkg-op-button 'install name)
       (when (equal (alist-get 'catalog details)
                    racket--package-main-catalog)
         (insert "; ")
         (insert-text-button "documentation"
                             :type 'racket-package-check-doc
                             'racket-package-name name)))
      ("installed"
       (insert " was manually installed: ")
       (racket--package-insert-raco-pkg-op-button 'update name)
       (insert " or ")
       (racket--package-insert-raco-pkg-op-button 'remove name))
      ("dependency"
       (insert " was automatically installed as a dependency"))
      (_
       (insert " is ")
       (insert status)))
    (newline)
    (newline)
    (let ((lks `((" Description" description)
                 ("   Directory" dir)
                 ("       Scope" scope)
                 ("      Source" source)
                 ("     Catalog" catalog)
                 ("    Checksum" checksum)
                 ("      Author" author)
                 ("        Tags" tags)
                 ("Dependencies" deps)
                 ("     Modules" modules)
                 ;; configuration
                 ("          Name" config-name)
                 (" Default Scope" default-scope)
                 ("      Catalogs" config-catalogs))))
      (dolist (lk lks)
        (pcase-let ((`(,l ,k) lk))
          (when-let (v (alist-get k details))
            (when (eq k 'config-name)
              (insert (propertize "\n--- raco pkg configure ------------------\n"
                                  'font-lock-face 'font-lock-comment-face)))

            (insert (propertize (concat l ":")
                                'font-lock-face 'package-help-section-name))
            (pcase k
              ('deps
               (let ((firstp t))
                 (dolist (dep v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n              "))
                   (insert-text-button (car dep)
                                       :type 'describe-racket-package)
                   (insert " ")
                   (insert (cdr dep))))
               (newline))
              ('catalog
               (insert " ")
               (if (equal v "https://pkgs.racket-lang.org")
                   (insert-text-button v
                                       :type 'racket-package-browse-url
                                       'url (concat v "/package/" name))
                 (insert v))
               (newline))
              ('modules
               (let ((firstp t))
                 (dolist (mod v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n              "))
                   (let* ((label           (if (listp mod) (nth 0 mod) mod))
                          (mod-path        (if (listp mod) (nth 1 mod) nil))
                          (doc-path+anchor (if (listp mod) (nth 2 mod) nil))
                          (private-p (string-match-p "/private/" label)))
                     (if mod-path
                         (insert-text-button label
                                             :type 'racket-package-visit-path
                                             'path (racket-file-name-back-to-front
                                                    mod-path)
                                             'face (if private-p
                                                       'font-lock-comment-face
                                                     'button))
                       (insert (propertize label
                                           'face (if private-p
                                                     'font-lock-comment-face
                                                   'default))))
                     (when doc-path+anchor
                       (let ((path (racket-file-name-back-to-front (car doc-path+anchor)))
                             (anchor (cdr doc-path+anchor)))
                         (insert " ")
                         (insert-text-button "documentation"
                                             :type 'racket-package-browse-file-url
                                             'face 'custom-button
                                             'path path
                                             'anchor anchor))))))
               (newline))
              ('tags
               (insert " ")
               (insert (string-join v " "))
               (newline))
              ('dir
               (insert " ")
               (insert-text-button v
                                   :type 'racket-package-visit-path
                                   'path (racket-file-name-back-to-front v))
               (newline))
              ('source
               (insert " ")
               (pcase v
                 (`(,label url ,url)
                  (insert-text-button label
                                      :type 'racket-package-browse-url
                                      'url url))
                 (`(,label path ,path)
                  (insert-text-button label
                                      :type 'racket-package-visit-path
                                      'path (racket-file-name-back-to-front path))))
               (newline))
              ('config-catalogs
               (let ((firstp t))
                 (dolist (cat v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n                "))
                   (insert-text-button cat
                                       :type 'racket-package-browse-url
                                       'url cat)))
               (newline))
              (_ (insert (format " %s\n" v))))))))))

(define-button-type 'racket-package-browse-url
  'action #'racket-package-browse-url)

(defun racket-package-browse-url (button)
  (browse-url (button-get button 'url)))

(define-button-type 'racket-package-visit-path
  'action #'racket-package-visit-path)

(defun racket-package-visit-path (button)
  (xref-push-marker-stack)
  (find-file (button-get button 'path)))

(define-button-type 'racket-package-browse-file-url
  'action #'racket-package-browse-file-url 'custom-face)

(defun racket-package-browse-file-url (button)
  (racket-browse-file-url (button-get button 'path)
                          (button-get button 'anchor)))

(define-button-type 'racket-package-check-doc
  'action #'racket-package-check-doc
  'face 'custom-button)

(defun racket-package-check-doc (&optional button)
  (interactive)
  (let ((name (button-get button 'racket-package-name)))
    (racket--cmd/async
     nil
     `(pkg-doc-link ,name)
     (lambda (result)
       (pcase result
         (`()
          (message "No rendered documentation found for %s at %s"
                  name
                  racket--package-main-catalog))
         (`((,_name ,url))
          (browse-url url))
         (choices
          (racket-package-choose-docs name choices)))))))

(defun racket-package-choose-docs (name choices)
  (help-setup-xref (list #'racket-package-choose-docs name choices)
                   nil)
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (insert (format "Multiple documentation links available for package %s:"
                      name))
      (newline)
      (dolist (choice choices)
        (pcase-let ((`(,label ,url) choice))
          (insert
           (propertize label
                       'button '(t)
                       'category 'default-button
                       'action #'racket-package-browse-url
                       'racket-package-url url))
          (newline))))))

(defun racket-package-refresh ()
  "Refresh the local copy of package catalogs.

Will make HTTP requests to remote catalog servers. May take a few
seconds to complete."
  (interactive)
  (help-setup-xref (list #'ignore)
                   (called-interactively-p 'interactive))
  (racket--do-pkg-op 'refresh nil))

(defun racket--package-insert-raco-pkg-op-button (verb name)
  (insert-text-button (symbol-name verb)
                      :type 'racket-package-op
                      'raco-pkg-verb verb
                      'raco-pkg-name name))

(define-button-type 'racket-package-op
  'action #'racket-raco-pkg-op
  'face 'custom-button)

(defun racket-raco-pkg-op (&optional button)
  (interactive)
  (unless button (error "no raco pkg button here"))
  (racket--do-pkg-op (button-get button 'raco-pkg-verb)
                     (button-get button 'raco-pkg-name)))

(defun racket--do-pkg-op (verb name)
  (pop-to-buffer (racket--package-notify-buffer-name)
                 '(display-buffer-below-selected))
  (racket--cmd/async nil `(pkg-op ,verb ,name)))

(defun racket--package-notify-buffer-name ()
  (format "*Racket Package Operations <%s>*" (racket-back-end-name)))

(defun racket--package-on-notify (v)
  (with-current-buffer (get-buffer-create (racket--package-notify-buffer-name))
    (unless (eq major-mode 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (pcase v
        ('done
         (insert (propertize "<done>\n\n"
                             'face 'compilation-mode-line-exit))
         (quit-window)
         ;; Fully refresh *Racket Packages* list because "--auto" commands
         ;; can install/remove/update multiple, dependent packages.
         (when-let (buf (get-buffer (racket--package-buffer-name)))
           (with-current-buffer buf
             (tabulated-list-revert)
             (let ((win (get-buffer-window buf)))
               (when win
                 (set-window-point win (point))))))
         ;; Also refresh the status for this package in the *Help*
         ;; buffer.
         (with-current-buffer (help-buffer)
           (help-mode-revert-buffer nil nil)))
        (`(error ,message)
         (insert (propertize message
                             'face 'compilation-error)))
        (str
         (insert (propertize str
                             'face 'compilation-info))))
      (goto-char (point-max)))))

(provide 'racket-package)

;; racket-package.el ends here
