;;; racket-package.el -*- lexical-binding: t -*-

;; Copyright (c) 2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-macs)
(require 'seq)
(require 'racket-custom)

(defun racket--package-raco-pkg (command k)
  (with-temp-buffer
    (let ((raco (if (file-name-absolute-p racket-program)
                    (expand-file-name racket-program) ;handle e.g. ~/
                  racket-program)))
      (call-process-shell-command (concat raco " -l raco pkg " command) nil t))
    (goto-char (point-min))
    (funcall k)))

(defvar racket--package-installed nil)

(defun racket--package-installed ()
  (setq racket--package-installed nil)
  (racket--package-raco-pkg "show --all --installation --long --dir"
                            (racket--package-installed-parse 'inst))
  (racket--package-raco-pkg "show --all --user --long --dir"
                            (racket--package-installed-parse 'user)))

(defun racket--package-installed-parse (scope)
  (lambda ()
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
                         (string-trim
                          (buffer-substring-no-properties (+ (point) beg)
                                                          (+ (point) end)))))
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
                    racket--package-installed)))
          (forward-line 1))))))

(defvar racket--package-catalog nil)

(defun racket--package-catalog ()
  (setq racket--package-catalog nil)
  (racket--package-raco-pkg
   "catalog-show --all"
   (lambda ()
     (goto-char (point-min))
     (while (search-forward-regexp (rx (*? "\n") "Package name: " (group (+? (not "\n"))) "\n")
                                   nil t)
       (let ((name (match-string 1))
             (author "")
             (description "")
             (checksum "")
             (deps nil)
             (source "")
             (tags ""))
         (while (and (not (looking-at "\nPackage name:"))
                     (search-forward-regexp (rx " " (group (+? (not ":"))) ":" (* " ") (group (*? (not "\n"))) "\n")
                                            nil t))
           (let ((key (match-string 1))
                 (val (match-string 2)))
             (pcase key
               ("Author" (setq author val))
               ("Description" (setq description val))
               ("Checksum"    (setq checksum val))
               ("Dependencies" (while (and (not (looking-at "\nPackage name:"))
                                           (search-forward-regexp (rx "  " (group (+? (not "\n"))) "\n")
                                                                  nil t))
                                 (push (match-string 1) deps)))
               ("Source" (setq source val))
               ("Tags" (setq tags val)))))
         (push (list name description checksum (nreverse deps) author source tags)
               racket--package-catalog)))
     racket--package-catalog)))

(defun racket-package-config ()
  nil)

(defvar racket-packages-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("q"   quit-window)
            ("RET" racket-package-details)))
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
        `[("Name"      15 t)
          ("Status"    10 t)
          ("Scope"      6 t)
          ("Checksum"   8 nil)
          ("Source"    15 t)
          ("Description" 15 t)]))

(defun racket--package-installed+catalog ()
  (racket--package-installed)
  (racket--package-catalog)
  (append
   ;; Eveything from catalog, possibly also installed
   (seq-map (pcase-lambda (`(,name ,description ,checksum ,_deps ,_author ,source ,_tags))
              (pcase-let ((`(,scope ,status ,status-face ,checksum ,source)
                           (pcase (assoc name racket--package-installed)
                             (`(,_name ,scope ,status ,checksum ,source ,_dir)
                              (list (symbol-name scope) (symbol-name status) 'package-status-installed checksum source))
                             (_
                              (list "" "available" 'package-status-available checksum source)))))
                (list (cons name checksum)
                      (vector (propertize name
                                          'font-lock-face 'bold)
                              (propertize status
                                          'font-lock-face status-face)
                              (propertize scope
                                          'font-lock-face status-face)
                              (propertize checksum
                                          'font-lock-face status-face)
                              source
                              description))))
            racket--package-catalog)
   ;; Any additional installed, not handled above because not also from catalog.
   (seq-filter
    #'identity
    (seq-map (pcase-lambda (`(,name ,scope ,status ,checksum ,source ,_dir))
               (unless (assoc name racket--package-catalog)
                 (list (cons name checksum)
                       (vector (propertize name
                                           'font-lock-face 'bold)
                               (propertize (symbol-name status)
                                           'font-lock-face 'package-status-installed
                                           'font-lock-face 'package-status-installed)
                               (propertize (symbol-name scope)
                                           'font-lock-face 'package-status-installed)
                               (propertize checksum)
                               source
                               ""))))
             racket--package-installed))))

(defun racket-packages ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Racket Packages*")
    (unless (eq major-mode 'racket-packages-mode)
      (racket-packages-mode))
    (with-silent-modifications
      (erase-buffer)
      (setq tabulated-list-entries
            (racket--package-installed+catalog))
      (tabulated-list-init-header)
      (tabulated-list-print))
    (pop-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode racket-package-details-mode special-mode
  "RacketPackageDetails"
  "Major mode for Racket packege details.")

(defun racket-package-details ()
  (interactive))

(provide 'racket-package)

;; racket-package.el ends here
