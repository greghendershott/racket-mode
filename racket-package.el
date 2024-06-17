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

;;; summary

(defun racket--package-buffer-name ()
   (format "*Racket Packages <%s>*" (racket-back-end-name)))

(defun list-racket-packages ()
  "Uses raco pkg commands to populate a `racket-packages-mode' buffer.

On each package you can press RET to `describe-racket-package',
which opens a buffer where you can view details, and use buttons
to install/update/remove the package."
  (interactive)
  (with-current-buffer (get-buffer-create (racket--package-buffer-name))
    (unless (eq major-mode 'racket-packages-mode)
      (racket-packages-mode))
    (pop-to-buffer (current-buffer))
    (tabulated-list-init-header)
    (tabulated-list-print)))

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

;;; details

(defun racket-package-menu-describe ()
  "Describe the package at point in a `racket-packages-mode' buffer."
  (interactive)
  (describe-racket-package (tabulated-list-get-id)))

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
       (racket--package-insert-raco-pkg-mutate-button "install" name))
      ("manual"
       (insert " was manually installed: ")
       (racket--package-insert-raco-pkg-mutate-button "update" name)
       (insert " or ")
       (racket--package-insert-raco-pkg-mutate-button "remove" name))
      ("dependency"
       (insert " was automatically installed as a dependency")))
    (newline)
    (newline)
    (let ((lks `((" Description" :description)
                 ("   Directory" :dir)
                 ("       Scope" :scope)
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
              (_ (insert (format " %s\n" v))))))))))

(defun racket--package-insert-raco-pkg-mutate-button (verb name)
  (let* ((cmd-list (racket--back-end-args->command
                    (racket-back-end)
                    (list "-l" "raco" "pkg" verb "--auto" name)))
         (cmd-str (string-join cmd-list " ")))
   (insert (propertize verb
                       'button '(t)
                       'face '(button bold)
                       'category 'default-button
                       'action #'racket--raco-pkg-mutate
                       'raco-pkg-command cmd-str
                       'raco-pkg-name name))))

(defun racket--raco-pkg-mutate (&optional button)
  (interactive)
  (unless button (error "no raco pkg button here"))
  (let ((cmd (button-get button 'raco-pkg-command))
        (name (button-get button 'raco-pkg-name))
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
    ;; dependent packages.
    (with-current-buffer (racket--package-buffer-name)
      (tabulated-list-revert)
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (set-window-point win (point)))))
    ;; Also refresh the status for this package, at the top of this
    ;; detail buffer.
    (delete-region (point-min) end-details)
    (goto-char (point-min))
    (let ((details (racket--cmd/await nil `(pkg-details ,name))))
      (when details
        (racket--package-insert-details details)))
    (goto-char (point-min))))

(defun racket-package-browse-url (button)
  (browse-url (button-get button 'racket-package-url)))

(provide 'racket-package)

;; racket-package.el ends here
