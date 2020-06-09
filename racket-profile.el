;;; racket-profile.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-repl)
(require 'racket-util)
(require 'racket-back-end)

(defvar-local racket--profile-project-root nil)
(defvar-local racket--profile-results nil)
(defvar-local racket--profile-show-zero nil)
(defvar-local racket--profile-show-non-project nil)

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
  (racket--assert-edit-mode)
  (message "Running with profiling instrumentation...")
  (let ((buf-name (format "*Racket Profile <%s>*"
                          (racket-back-end-name)))
        (what-to-run (racket--what-to-run)))
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
          (message "Preparing profile results to display...")
          (with-current-buffer
              (get-buffer-create buf-name)
            (racket-profile-mode)
            (setq racket--profile-results results)
            (setq racket--profile-project-root
                  (racket-project-root (car what-to-run)))
            (racket--profile-draw)
            (pop-to-buffer (current-buffer)))))))))

(defun racket-profile-refresh ()
  (interactive)
  (racket--cmd/async (racket--repl-session-id)
                     `(get-profile)
                     (lambda (results)
                       (setq racket--profile-results
                             results)
                       (racket--profile-draw))))

(defun racket--profile-draw ()
  (setq truncate-lines t) ;let run off right edge
  (with-silent-modifications
    (erase-buffer)
    (pcase-let* ((filtered (seq-filter
                            (pcase-lambda (`(,calls ,msec ,_name ,file ,_beg ,_end))
                              (and (or racket--profile-show-zero
                                       (not (and (zerop calls) (zerop msec))))
                                   (or racket--profile-show-non-project
                                       (equal (racket-project-root
                                               (racket-file-name-back-to-front file))
                                              racket--profile-project-root))))
                            racket--profile-results))
                 (`(,width-calls ,width-msec ,width-name)
                  (seq-reduce (pcase-lambda (`(,width-calls ,width-msec ,width-name)
                                             `(,calls ,msec ,name . ,_))
                                (list (max width-calls (length (format "%s" calls)))
                                      (max width-msec  (length (format "%s" msec)))
                                      (max width-name  (length name))))
                              filtered
                              `(5 5 4))))
      (cl-flet ((sort-pred (col) (lambda (a b)
                                   (< (string-to-number (aref (cadr a) col))
                                      (string-to-number (aref (cadr b) col))))))
        (setq tabulated-list-format
              `[("Calls"  ,width-calls ,(sort-pred 0) :right-align t)
                ("Msec"   ,width-msec  ,(sort-pred 1) :right-align t)
                ("Name"   ,width-name  t)
                ("Source" 99           t)]))
      (setq tabulated-list-entries
            (seq-map (pcase-lambda (`(,calls ,msec ,name ,file ,beg ,end))
                       (let* ((file (racket-file-name-back-to-front file))
                              (simplified-file
                               (if (equal (racket-project-root file)
                                          racket--profile-project-root)
                                   (file-relative-name file racket--profile-project-root)
                                 file)))
                         (list nil
                               (vector
                                (format "%s" calls)
                                (format "%s" msec)
                                (propertize (or name "")
                                            'face font-lock-function-name-face)
                                (if (and file beg end)
                                    (list simplified-file
                                          'racket-file file
                                          'racket-beg  beg
                                          'racket-end  end
                                          'action      #'racket-profile-button)
                                  simplified-file)))))
                     filtered))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (save-excursion
        (goto-char (point-max))
        (newline)
        (insert (concat (if racket--profile-show-zero "Showing" "Hiding")
                        " 0 calls and 0 msec. Press z to toggle."))
        (newline)
        (insert (concat (if racket--profile-show-non-project "Showing" "Hiding")
                        " non-project files. Press f to toggle."))))))

(defun racket-profile-button (button)
  (let ((file (button-get button 'racket-file))
        (beg  (button-get button 'racket-beg)))
    (xref-push-marker-stack)
    (find-file file)
    (goto-char beg)))

(defun racket-profile-visit ()
  "Visit the source of the profile item.

Use \\[xref-pop-marker-stack] -- `xref-pop-marker-stack' -- to return."
  (interactive)
  (pcase (tabulated-list-get-entry (point))
    (`[,_calls ,_msec ,_name (,_ racket-file ,file racket-beg ,beg . ,_)]
     (xref-push-marker-stack)
     (find-file file)
     (goto-char beg))))

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

(defvar racket-profile-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m nil)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("q"   quit-window)
            ("g"   racket-profile-refresh)
            ("z"   racket-profile-show-zero)
            ("f"   racket-profile-show-non-project)
            ("."   racket-profile-visit)
            ("RET" racket-profile-visit)))
    m)
  "Keymap for Racket Profile mode.")

(define-derived-mode racket-profile-mode tabulated-list-mode
  "RacketProfile"
  "Major mode for results of `racket-profile'.

\\{racket-profile-mode-map}
"
  (setq show-trailing-whitespace nil)
  (setq tabulated-list-sort-key '("Calls" . t)))

(provide 'racket-profile)

;; racket-profile.el ends here
