;;; racket-package.el -*- lexical-binding: t -*-

;; Copyright (c) 2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'seq)
(require 'url-parse)
(require 'racket-custom)
(require 'racket-back-end)
(require 'racket-cmd)

;;; racket-package-mode summary list of packages

(defun racket--package-buffer-name ()
   (format "*Racket Packages <%s>*" (racket-back-end-name)))

;;;###autoload
(defun list-racket-packages ()
  "Uses raco pkg commands to populate a `racket-package-mode' buffer.

On each package you can press RET to `describe-racket-package',
which opens a buffer where you can view details, and use buttons
to install/update/remove the package."
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
                          (status-face (pcase status
                                         ("available" 'package-status-available)
                                         (_           'package-status-installed))))
               (list name
                     (vector (propertize name
                                         'face 'package-name
                                         'font-lock-face 'package-name
                                         'button '(t)
                                         'category 'default-button
                                         'follow-link t
                                         'action #'describe-racket-package)
                             (propertize status
                                         'font-lock-face status-face)
                             desc))))
           (racket--cmd/await nil `(pkg-list))))

(defun racket-package-describe ()
  "Describe the package at point."
  (interactive)
  (describe-racket-package (tabulated-list-get-id)))

;;; help buffer of details about a single package, and button actions

;;;###autoload
(defun describe-racket-package (&optional name-or-button)
  "Describe details of a Racket package.

Buttons allow you to install/update/remove the package, depending
on its status. "
  (interactive "sRacket package name: ")
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
       (help-setup-xref (list #'describe-racket-package (plist-get details :name))
                        (called-interactively-p 'interactive))
       (with-help-window (help-buffer)
         (with-current-buffer standard-output
           (racket--package-insert-details details)))))))

(defun racket--package-insert-details (details)
  (let ((name (plist-get details :name))
        (status (plist-get details :status)))
    (insert (propertize name
                        'font-lock-face 'bold))
    (pcase status
      ("available"
       (insert " is available to ")
       (racket--package-insert-raco-pkg-op-button 'install name))
      ("manual"
       (insert " was manually installed: ")
       (racket--package-insert-raco-pkg-op-button 'update name)
       (insert " or ")
       (racket--package-insert-raco-pkg-op-button 'remove name))
      ("dependency"
       (insert " was automatically installed as a dependency")))
    (newline)
    (newline)
    (let ((lks `((" Description" :description)
                 ("   Directory" :dir)
                 ("       Scope" :scope)
                 ("     Catalog" :catalog)
                 ("      Source" :source)
                 ("    Checksum" :checksum)
                 ("      Author" :author)
                 ("        Tags" :tags)
                 ("Dependencies" :deps)
                 ("     Modules" :modules))))
      (dolist (lk lks)
        (pcase-let* ((`(,l ,k) lk)
                     (v (plist-get details k)))
          (when v
            (insert (propertize (concat l ":")
                                'font-lock-face 'package-help-section-name))
            (pcase k
              (:deps
               (let ((firstp t))
                 (dolist (dep v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n              "))
                   (insert (propertize (car dep)
                                       'button '(t)
                                       'category 'default-button
                                       'follow-link t
                                       'action #'describe-racket-package))
                   (insert " ")
                   (insert (cdr dep))))
               (newline))
              (:modules
               (let ((firstp t))
                 (dolist (mod v)
                   (if firstp
                       (progn (setq firstp nil) (insert " "))
                     (insert "\n              "))
                   (insert mod)))
               (newline))
              (:tags
               (insert " ")
               (insert (string-join v " "))
               (newline))
              (:dir
               (insert " ")
               (insert
                  (propertize v
                              'button '(t)
                              'category 'default-button
                              'action #'racket-package-browse-url
                              'racket-package-url (concat "file://" v)))
               (newline))
              (:source
               (let ((label (format "%s" (car v)))
                     (url (cdr v)))
                 (insert " ")
                 (insert
                  (propertize label
                              'button '(t)
                              'category 'default-button
                              'action #'racket-package-browse-url
                              'racket-package-url url))
                 (newline)))
              (_ (insert (format " %s\n" v)))))))))
  (put-text-property (point-min) (point) 'racket-package-details t))

(defun racket-package-browse-url (button)
  (browse-url (button-get button 'racket-package-url)))

(defun racket--package-insert-raco-pkg-op-button (verb name)
  (insert (propertize (symbol-name verb)
                      'button '(t)
                      'face 'custom-button
                      'category 'default-button
                      'action #'racket--raco-pkg-op
                      'raco-pkg-verb verb
                      'raco-pkg-name name)))

(defun racket--raco-pkg-op (&optional button)
  (interactive)
  (unless button (error "no raco pkg button here"))
  (let ((verb (button-get button 'raco-pkg-verb))
        (name (button-get button 'raco-pkg-name))
        (inhibit-read-only t))
    (pop-to-buffer (racket--package-notify-buffer-name)
                   '(display-buffer-below-selected))
    (racket--cmd/async nil `(pkg-op ,verb ,name))))

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
         (with-current-buffer (racket--package-buffer-name)
           (tabulated-list-revert)
           (let ((win (get-buffer-window (current-buffer))))
             (when win
               (set-window-point win (point)))))
         ;; Also refresh the status for this package in the *Help*
         ;; buffer.
         (with-current-buffer (help-buffer)
           (revert-buffer)))
        (`(error ,message)
         (insert (propertize message
                             'face 'compilation-error)))
        (str
         (insert (propertize str
                             'face 'compilation-info))))
      (goto-char (point-max)))))

(provide 'racket-package)

;; racket-package.el ends here
