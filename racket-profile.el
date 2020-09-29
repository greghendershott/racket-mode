;;; racket-profile.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
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

(require 'cl-lib)
(require 'racket-repl)
(require 'racket-util)

(defvar racket--profile-project-root nil)
(defvar racket--profile-results nil)
(defvar racket--profile-sort-col 1) ;0=Calls, 1=Msec
(defvar racket--profile-show-zero nil)
(defvar racket--profile-show-non-project nil)
(defvar racket--profile-overlay-this nil)
(defvar racket--profile-overlay-that nil)

(defun racket-profile ()
  "Like `racket-run-module-at-point' but with profiling.

Results are presented in a `racket-profile-mode' buffer, which
also lets you quickly view the source code.

You may evaluate expressions in the REPL. They are also profiled.
Use `racket-profile-refresh' to see the updated results. In
other words a possible workflow is: `racket-profile' a .rkt file,
call one its functions in the REPL, and refresh the profile
results.

Caveat: Only source files are instrumented. You may need to
delete compiled/*.zo files."
  (interactive)
  (unless (eq major-mode 'racket-mode)
    (user-error "Works only in a racket-mode buffer"))
  (message "Running with profiling instrumentation...")
  (let ((what-to-run (racket--what-to-run)))
    (racket--repl-run
     what-to-run
     '()
     'profile
     (lambda ()
       (message "Getting profile results...")
       (racket--cmd/async
        (racket--repl-session-id)
        `(get-profile)
        (lambda (results)
          (message "Profile results ready")
          (with-current-buffer (get-buffer-create "*Racket Profile*")
            (setq racket--profile-results results)
            (setq racket--profile-sort-col 1)
            (setq racket--profile-project-root
                  (racket-project-root (car what-to-run)))
            (racket-profile-mode)
            (racket--profile-draw)
            (pop-to-buffer (current-buffer)))))))))

(defun racket--profile-refresh ()
  (interactive)
  (setq racket--profile-results
        (racket--cmd/await (racket--repl-session-id)
                           `(get-profile)))
  (racket--profile-draw))

(defun racket--profile-draw ()
  (setq truncate-lines t) ;let run off right edge
  ;; TODO: Would be nice to set the Calls and Msec column widths based
  ;; on max values.
  (setq header-line-format
        (format " %8s %6s %-30.30s %s"
                (if (= 0 racket--profile-sort-col) "CALLS" "Calls")
                (if (= 1 racket--profile-sort-col) "MSEC" "Msec")
                "Name (inferred)"
                "File"))
  (with-silent-modifications
    (erase-buffer)
    (let* ((copied   (cl-copy-list racket--profile-results))
           (filtered (cl-remove-if-not
                      (lambda (x)
                        (cl-destructuring-bind (calls msec _name file _beg _end) x
                          (and (or racket--profile-show-zero
                                   (not (and (zerop calls) (zerop msec))))
                               (or racket--profile-show-non-project
                                   (equal (racket-project-root file)
                                          racket--profile-project-root)))))
                      copied))
           (xs  (sort filtered
                           (lambda (a b) (> (nth racket--profile-sort-col a)
                                            (nth racket--profile-sort-col b))))))
      (dolist (x xs)
        (cl-destructuring-bind (calls msec name file beg end) x
          (let ((simplified-file
                 (if (equal (racket-project-root file)
                            racket--profile-project-root)
                     (file-relative-name file racket--profile-project-root)
                   file)))
            (insert
             (propertize (format "%8d %6d %-30.30s %s\n"
                                 calls msec (or name "") simplified-file)
                         'racket-profile-location
                         (and file beg end
                              (list file beg end))))))))
    (newline)
    (insert (concat (if racket--profile-show-zero "Not h" "H")
                    "iding 0 calls and 0 msec. Press z to toggle."))
    (newline)
    (insert (concat (if racket--profile-show-non-project "Not h" "H")
                    "iding non-project files. Press f to toggle.")))
  (goto-char (point-min)))

(defun racket-profile-sort ()
  "Toggle sort between Calls and Msec."
  (interactive)
  (setq racket--profile-sort-col (if (= racket--profile-sort-col 0) 1 0))
  (racket--profile-draw))

(defun racket-profile-show-zero ()
  "Toggle between showing results with zero Calls or Msec."
  (interactive)
  (setq racket--profile-show-zero (not racket--profile-show-zero))
  (racket--profile-draw))

(defun racket-profile-show-non-project ()
  "Toggle between showing results for files only in the project.

The \"project\" is determined by `racket-project-root'."
  (interactive)
  (setq racket--profile-show-non-project (not racket--profile-show-non-project))
  (racket--profile-draw))

(defun racket--profile-visit ()
  (interactive)
  (let ((win (selected-window)))
    (pcase (get-text-property (point) 'racket-profile-location)
      (`(,file ,beg ,end)
       (setq racket--profile-overlay-this
             (make-overlay (save-excursion (beginning-of-line) (point))
                           (save-excursion (end-of-line) (point))
                           (current-buffer)))
       (overlay-put racket--profile-overlay-this 'face 'next-error)
       (find-file-other-window file)
       (setq racket--profile-overlay-that (make-overlay beg end (current-buffer)))
       (overlay-put racket--profile-overlay-that 'face 'next-error)
       (goto-char beg)
       (add-hook 'pre-command-hook #'racket--profile-remove-overlay)
       (select-window win)))))

(defun racket--profile-remove-overlay ()
  (delete-overlay racket--profile-overlay-this)
  (delete-overlay racket--profile-overlay-that)
  (remove-hook 'pre-command-hook #'racket--profile-remove-overlay))

(defun racket-profile-next ()
  "Do `forward-line' and show the source in other window."
  (interactive)
  (forward-line 1)
  (racket--profile-visit))

(defun racket-profile-prev ()
  "Do `previous-line' and show the source in other window."
  (interactive)
  (forward-line -1)
  (racket--profile-visit))

(defvar racket-profile-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("q"   quit-window)
            ("g"   racket-profile-refresh)
            ("n"   racket-profile-next)
            ("p"   racket-profile-prev)
            ("z"   racket-profile-show-zero)
            ("f"   racket-profile-show-non-project)
            (","   racket-profile-sort)))
    m)
  "Keymap for Racket Profile mode.")

(define-derived-mode racket-profile-mode special-mode
  "RacketProfile"
  "Major mode for results of `racket-profile'.

\\{racket-profile-mode-map}
"
  (setq show-trailing-whitespace nil))

(provide 'racket-profile)

;; racket-profile.el ends here
