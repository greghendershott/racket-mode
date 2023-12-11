;;; racket-pdb.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-custom)
(require 'racket-browse-url)
(require 'racket-doc)
(require 'racket-repl)
(require 'racket-describe)
(require 'racket-eldoc)
(require 'racket-imenu)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-show)
(require 'racket-xp-complete)
(require 'racket-back-end)
(require 'easymenu)
(require 'imenu)
(require 'rx)
(require 'seq)
(require 'xref)

(declare-function racket-complete-at-point "racket-edit" ())
(declare-function racket-xp-mode "racket-xp" (&optional arg))

(defvar racket-pdb-control-c-hash-keymap
  (racket--easy-keymap-define
   `(("n" ,#'racket-pdb-next-use)
     ("p" ,#'racket-pdb-previous-use)
     ("." ,#'xref-find-definitions)
     ("?" ,#'xref-find-references)
     ("r" ,#'racket-pdb-rename)
     ;; ("^" ,#'racket-pdb-tail-up)
     ;; ("v" ,#'racket-pdb-tail-down)
     ;; (">" ,#'racket-pdb-tail-next-sibling)
     ;; ("<" ,#'racket-pdb-tail-previous-sibling)
     ("g" ,#'racket-pdb-analyze)
     ("N" ,#'racket-pdb-next-error)
     ("P" ,#'racket-pdb-previous-error))))

(defvar racket-pdb-mode-map
  (racket--easy-keymap-define
   `(("C-c #"     ,racket-pdb-control-c-hash-keymap)
     ("M-."       ,#'xref-find-definitions)
     ("C-c C-."   ,#'racket-pdb-describe)
     ("C-c C-d"   ,#'racket-pdb-documentation)
     ("C-c C-s"   ,#'racket-describe-search))))

;;;###autoload
(define-minor-mode racket-pdb-mode
  "Like `racket-xp-mode' but using the pdb package, which must be installed.

There are several advantages of `racket-pdb-mode', compared to
`racket-xp-mode':

- The `racket-pdb-rename' command works across mulitple (known,
  analyzed) files, and is smarter about what should be named than
  a text search approach. Similarly `xref-find-references' shows
  better results.

- The pdb database is queried only as needed to present things on
  screen, or to run commands like `xref-find-definition'.
  Performance is better for larger files. Although the underlying
  drracket/check-syntax analysis can still take a long time (e.g.
  ~10 seconds for something like private/class-internal.rkt),
  Emacs is never blocked by re-propertizing the entire buffer.
  Also, if the file hasn't changed since being last visited and
  analyzed, the previous results are reused, even if the buffer
  has been closed in Emacs or Emacs has been restarted.

The disadvantages:

- The pdb Racket package must be installed, and it depends on
  very new versions of the drracket-tool-text-lib package as well
  as Racket itself. The newer versions improve the ability to
  analyze code, especially to understand the set of sites that
  must be renamed together. If you need to work with older
  verions of Racket (which Racket Mode tries to support as much
  as possible) you'll need to use `racket-xp-mode'.

- The pdb \"database\" uses some disk space. In an extreme case,
  for an analysis of about 8,000 files in the Racket main
  distribution, this is about 100 MB. However you can limit the
  analysis to projects you actively work on, if that is the scope
  for which you expect multi-file renaming to work. In other
  words, you won't need such a large database unless you want to
  rename something like \"define\" in racket/base. :)

Until this doc string is further rewritten, please refer to the
documentation for `racket-xp-mode', most of which is relevant to
this mode."
  :lighter racket-pdb-mode-lighter
  :keymap racket-pdb-mode-map
  (racket--assert-edit-mode)
  ;; Enabling both `racket-pdb-mode' and `racket-xp-mode' isn't
  ;; supported so automatically disable latter.
  (when (bound-and-true-p racket-xp-mode)
    (racket-xp-mode -1))
  (cond
   (racket-pdb-mode
    (add-hook 'after-change-functions
              #'racket--pdb-after-change-hook
              t t)
    (remove-hook 'completion-at-point-functions
                 #'racket-complete-at-point
                 t)
    (add-hook 'completion-at-point-functions
              #'racket-pdb-complete-at-point
              t t)
    (add-hook 'xref-backend-functions
              #'racket-pdb-xref-backend-function
              nil t)
    (add-hook 'pre-redisplay-functions
              #'racket-pdb-pre-redisplay
              nil t)
    (racket--cmd/async
     nil '(pdb-available?)
     (lambda (response)
       (unless response
         (racket-pdb-mode -1)
         (user-error "The Racket package `pdb` is not available so racket-pdb-mode cannot be used; instead maybe use racket-xp-mode"))
       (racket-pdb-analyze)
       (setq-local racket-submodules-at-point-function
                   #'racket-pdb-submodules-at-point))))
   (t
    (setq-local racket-submodules-at-point-function nil)
    (racket-show nil)
    (racket--pdb-remove-all-face-overlays)
    (remove-hook 'after-change-functions
                 #'racket--pdb-after-change-hook
                 t)
    (remove-hook 'completion-at-point-functions
                 #'racket-pdb-complete-at-point
                 t)
    (add-hook 'completion-at-point-functions
              #'racket-complete-at-point
              t t)
    (remove-hook 'xref-backend-functions
                 #'racket-pdb-xref-backend-function
                 t)
    (remove-hook 'pre-redisplay-functions
                 #'racket-pdb-pre-redisplay
                 t))))

(defun racket-pdb-submodules-at-point ()
  (racket--cmd/await
   nil
   `(pdb-submodules
     ,(racket-file-name-front-to-back (racket--buffer-file-name))
     ,(point))))

(defvar-local racket--pdb-motion-timer nil)
(defvar-local racket--pdb-motion-generation 0
  "See similar `racket--pdb-change-generation' for rationale.")
(defvar-local racket--pdb-motion-def-and-use-sites nil
  "A sorted vector of def and use sites related to point, if any.")

(defun racket-pdb-pre-redisplay (window)
  (with-current-buffer (window-buffer window)
    ;; Note: We use window-height not window-end, because the latter
    ;; can shift due to multi-line overlay strings, such as used by
    ;; racket-show tooltips for error messages.
    (let ((point+start+height (list (window-point window)
                                    (window-start window)
                                    (window-height window))))
      (unless (equal point+start+height
                     (window-parameter window 'racket-pdb-point+start+height))
        (set-window-parameter window 'racket-pdb-point+start+height point+start+height)
        (cl-incf racket--pdb-motion-generation)
        (racket--pdb-remove-all-decorations)
        (when (timerp racket--pdb-motion-timer)
          (cancel-timer racket--pdb-motion-timer))
        (setq racket--pdb-motion-timer
              (run-with-idle-timer racket-pdb-after-motion-refresh-delay
                                   nil  ;no repeat
                                   #'racket--pdb-on-motion-idle-timer
                                   window
                                   point+start+height))))))

(defun racket--pdb-on-motion-idle-timer (window point+start+height)
  (pcase-let ((generation-of-our-request racket--pdb-motion-generation)
              (`(,point ,start ,height) point+start+height))
    (with-current-buffer (window-buffer window)
      (when racket-pdb-mode
        (racket--cmd/async
         nil
         `(pdb-point-info ,(racket-file-name-front-to-back (racket--buffer-file-name))
                          ,point
                          ,start
                          ,(window-end window))
         (lambda (response)             ;called later
           (when (and (= generation-of-our-request racket--pdb-motion-generation)
                      (racket--pdb-analysis-is-up-to-date-p))
             (with-current-buffer (window-buffer window)
               ;; Still same point+start+height now that the response arrived?
               (when (and (= point (window-point window))
                          (= start (window-start window))
                          (= height (window-height window)))
                 (let ((inhibit-modification-hooks t))
                   ;; 1. Do racket-show of mouse-over.
                   (pcase (cdr (assq 'point-mouse-over response))
                     (`(,beg ,end ,text)
                      (racket-show text
                                   (cond
                                    ((pos-visible-in-window-p end window)
                                     end)
                                    ((pos-visible-in-window-p beg window)
                                     beg)
                                    ((save-excursion
                                       (goto-char (window-start window))
                                       (forward-line -1)
                                       (point))))))
                     (_ (racket-show "")))

                   ;; 2. Def and use sites
                   (pcase-let ((`(,def ,import-p ,uses)
                                (cdr (assq 'point-def-and-use-sites response))))
                     ;; Make sorted vector for `racket--pdb-forward-use'.
                     (setq racket--pdb-motion-def-and-use-sites
                           (seq-sort (lambda (a b) (< (car a) (car b)))
                                     (apply #'vector (if def (cons def uses) uses))))
                     ;; Add overlays unless import (which would be too
                     ;; noisy; also consistent with racket-xp-mode
                     ;; which lets you nav among these but doens't
                     ;; highlight them).
                     (unless import-p
                       (when def
                         (racket--pdb-add-face-overlay (car def) (cdr def) racket-xp-def-face))
                       (dolist (use uses)
                         (racket--pdb-add-face-overlay (car use) (cdr use) racket-xp-use-face))))

                   ;; 3. Add overlays to highight tail positions.
                   ;; TODO.


                   ;; 4. Add overlays for unused requires and bindings.
                   (dolist (v (cdr (assq 'unused-requires response)))
                     (racket--pdb-add-face-overlay (car v) (cdr v) racket-xp-unused-face))
                   (dolist (v (cdr (assq 'unused-def-sites response)))
                     (pcase-let ((`(,beg . ,end) v))
                       (when (string-match-p racket-pdb-highlight-unused-regexp
                                             (buffer-substring beg end))
                         (racket--pdb-add-face-overlay beg end racket-xp-unused-face))))

                   ;; 5. Experimental: Maybe set font-lock-face for
                   ;; "semantic highlighting". Do so only when
                   ;; font-lock-defaults is nil, which is the case
                   ;; e.g. for `racket-hash-lang-mode', which does
                   ;; font-lock only for the lang's lexer tokens.
                   ;;
                   ;; Note that font-lock-face -- not face -- is
                   ;; preferred here because that way if user disables
                   ;; font-lock-mode, this won't show.
                   (unless font-lock-defaults
                     (remove-text-properties (window-start window)
                                             (window-end window)
                                             `(font-lock-face nil))

                     ;; Emacs has font-lock-function-name-face and
                     ;; font-lock-variable-name-face. Some people like
                     ;; to set these differently, e.g. maybe both same
                     ;; color, but bold function names. Alas from
                     ;; check-syntax we get no distinction for variables
                     ;; whose value is a function. (Although Racket is a
                     ;; "Lisp-1", an analysis of fully-expanded syntax
                     ;; could distinguish define-values where the rhs is
                     ;; lambda; see e.g. inferred value names.)
                     (dolist (v (cdr (assq 'def-sites response)))
                       (put-text-property (car v) (cdr v)
                                          'font-lock-face
                                          'font-lock-variable-name-face))
                     ;; Not sure about this one, but in practice so far
                     ;; it seems to work well to highlight documented
                     ;; items as "keywords". TODO: Distinguish syntax as
                     ;; "keywords" and non-syntax as "builtints", much
                     ;; like racket-keywords-and-builtins.el?
                     (dolist (v (cdr (assq 'doc-sites response)))
                       (put-text-property (car v) (cdr v)
                                          'font-lock-face
                                          'font-lock-keyword-face)))))))))))))

(defun racket--pdb-show-after-motion (window)
  "Useful when a command doesn't move but wants to force showing."
  (racket--pdb-on-motion-idle-timer window
                                    (list (window-point window)
                                          (window-start window)
                                          (window-height window))))

;;; Face overlays

(defun racket--pdb-add-face-overlay (beg end face &optional priority)
  (let ((o (make-overlay beg end
                         nil ;current-buffer
                         t   ;inserts in front do not go into overlay
                         nil ;inserts at back do not go into overlay
                         )))
    (overlay-put o 'priority (or priority 0)) ;below other overlays e.g. isearch
    (overlay-put o 'face face)
    o))

(defun racket--pdb-remove-face-overlays (&rest faces)
  (save-restriction
    (widen)
    (dolist (face faces)
      (remove-overlays (point-min) (point-max) 'face face))))

(defun racket--pdb-remove-all-face-overlays ()
  (racket--pdb-remove-face-overlays racket-xp-def-face
                                    racket-xp-use-face
                                    racket-xp-unused-face
                                    racket-xp-error-face))

(defun racket--pdb-remove-all-decorations ()
  (racket--pdb-remove-all-face-overlays)
  (racket-show ""))

;;; Errors

(defvar-local racket--pdb-errors (vector)
  "[(list beg end path str) ...], sorted by beg")
(defvar-local racket--pdb-errors-index -1)

(defun racket--pdb-reset-errors (errors)
  (racket--pdb-remove-face-overlays racket-xp-error-face)
  (let ((errors (sort errors (lambda (a b) (< (car a) (car b))))))
    (dolist (e errors)
      (pcase-let ((`(,beg ,end . ,_) e))
        (racket--pdb-add-face-overlay beg end racket-xp-error-face)))
    (setq racket--pdb-errors (apply #'vector errors))
    (setq racket--pdb-errors-index -1)))

(defun racket--pdb-next-error (&optional amt reset)
  "Move AMT errors, if any.

If there are any check-syntax errors, moves among them, wrapping
around at the first and last errors.

Otherwise delegate to `next-error'. That way, things still work
as you would want when using `racket-run' -- e.g. for runtime
evaluation errors that won't be found merely from expansion -- or
`compilation-mode'."
  (interactive)
  (let ((len (length racket--pdb-errors)))
    (if (zerop len)
        (next-error amt reset)
      (if reset
          (setq racket--pdb-errors-index 0)
        (setq racket--pdb-errors-index
              (mod (+ racket--pdb-errors-index amt)
                   len)))
      (pcase-let ((`(,beg ,_end ,path ,str)
                   (aref racket--pdb-errors
                         racket--pdb-errors-index)))
        (cond ((equal path (racket--buffer-file-name))
               (goto-char beg))
              (t
               (find-file path)
               (goto-char beg)))
        (message "%s" str)))))

(defun racket-pdb-next-error ()
  "Go to the next error."
  (interactive)
  (racket--pdb-next-error 1 nil))

(defun racket-pdb-previous-error ()
  "Go to the previous error."
  (interactive)
  (racket--pdb-next-error -1 nil))

;;; Change hook and idle timer

(defvar-local racket--pdb-change-idle-timer nil)

(defvar-local racket--pdb-change-generation 0
  "A counter to detect pdb-analyze command responses we should ignore.
Example scenario: User edits. Timer set. Timer expires; we
request annotations. While waiting for that response, user makes
more edits. When the originally requested annotations arrive, we
can see they're out of date and should be ignored. Instead just wait
for the annotations resulting from the user's later edits.")

(defvar-local racket--pdb-change-response-generation 0)
(defun racket--pdb-analysis-is-up-to-date-p ()
  (= racket--pdb-change-generation
     racket--pdb-change-response-generation))

(defun racket--pdb-after-change-hook (_beg _end _len)
  (cl-incf racket--pdb-change-generation)
  (racket--pdb-remove-all-decorations)
  (when (timerp racket--pdb-change-idle-timer)
    (cancel-timer racket--pdb-change-idle-timer))
  (racket--pdb-set-status 'outdated)
  (when racket-pdb-after-change-refresh-delay
    (setq racket--pdb-change-idle-timer
          (run-with-idle-timer racket-pdb-after-change-refresh-delay
                               nil      ;no repeat
                               #'racket--pdb-on-change-idle-timer
                               (selected-window)
                               (current-buffer)))))

(defun racket--pdb-on-change-idle-timer (window buffer)
  "Handle after-change-hook => idle-timer expiration.

One scenario to keep in mind: The user has typed a few characters
-- which are likely to be a syntax error -- and is in the process
of using manual or auto completion. We don't want to analyze
yet. At best it's a waste of work, and at worst the completion UI
and our UI might distractingly interfere with each other. Just do
nothing for now. If the user selects a completion candiate, that
buffer modification will cause us to run later -- which is
perfect. If they cancel completion, the annotation won't refresh
and might miss a change from before they even started completion
-- which is not great, but is better than making a mistake
rescheduling an idle-timer with an amount <= the amount of idle
time that has already elapsed: see #504."
  (when (and (buffer-live-p buffer)
             (equal buffer (window-buffer window)))
    (with-current-buffer buffer
      (unless (racket--pdb-completing-p)
        (with-selected-window window
          (racket-pdb-analyze))))))

(defun racket--pdb-completing-p ()
  "Is completion underway?
This is ad hoc and forensic."
  (or (get-buffer-window "*Completions*")
      (and (boundp 'company-pseudo-tooltip-overlay)
           company-pseudo-tooltip-overlay)))

(defun racket-pdb-analyze ()
  (interactive)
  (racket--pdb-set-status 'running)
  (let ((generation-of-our-request racket--pdb-change-generation)
        (window (selected-window))) ;might change before response arrives
    (racket--cmd/async
     nil
     `(pdb-analyze-path ,(racket-file-name-front-to-back
                          (or (racket--buffer-file-name) (buffer-name)))
                        ,(buffer-substring-no-properties (point-min) (point-max)))
     (lambda (response) ;arrives later
       (when (= generation-of-our-request racket--pdb-change-generation)
         (setq racket--pdb-change-response-generation generation-of-our-request)
         (pcase response
           (`((errors . ,errors))
            (racket--pdb-remove-all-decorations)
            (racket--pdb-reset-errors errors)
            (racket--pdb-set-status (if errors 'err 'ok))
            (racket--pdb-show-after-motion window))))))))

;;; Rename

(defun racket-pdb-rename ()
  "Rename the identifier at point.

Uses pdb to query for sites among multiple files."
  (interactive)
  (unless (racket--pdb-analysis-is-up-to-date-p)
    (user-error "Wait for analysis to finish"))
  (pcase-let*
      ((back-end-path (racket-file-name-front-to-back (racket--buffer-file-name)))
       (files-and-sites (racket--cmd/await nil
                                           `(pdb-rename-sites ,back-end-path
                                                              ,(point))))
       (_ (unless files-and-sites
            (user-error "Not a definition or use site")))
       (`(,num-files . ,num-sites)
        (seq-reduce (lambda (ns file-and-sites)
                      (cons (+ (car ns) 1)
                            (+ (cdr ns) (length (cdr file-and-sites)))))
                    files-and-sites
                    (cons 0 0)))
       (old-id (seq-some (lambda (site)
                           (and (<= (car site) (point))
                                (< (point) (cdr site))
                                (buffer-substring-no-properties (car site) (cdr site))))
                         (cdr (assoc back-end-path files-and-sites))))
       (new-id (read-from-minibuffer
                (format "Rename `%s' at %s sites%s to: "
                        old-id
                        num-sites
                        (cond
                         ((<= num-files 1)
                          "")
                         ((<= num-files 6)
                          (format " in %s"
                                       (mapcar (lambda (f+s)
                                                 (file-name-nondirectory (car f+s)))
                                               files-and-sites)))
                         (t (format " in %s files" num-files)))))))
    (dolist (file-and-sites files-and-sites)
      (let ((file (racket-file-name-back-to-front (car file-and-sites)))
            (sites (cdr file-and-sites)))
        (with-current-buffer (save-selected-window (find-file file))
          (let ((point-marker (let ((m (make-marker)))
                                (set-marker m (point) (current-buffer))))
                (marker-pairs (mapcar (lambda (site)
                                        (let ((beg (make-marker))
                                              (end (make-marker)))
                                          (set-marker beg (car site) (current-buffer))
                                          (set-marker end (cdr site) (current-buffer))
                                          (cons beg end)))
                                      sites)))
            ;; Don't let our after-change hook run until all changes are
            ;; made, otherwise check-syntax will find a syntax error.
            (let ((inhibit-modification-hooks t))
              (dolist (marker-pair marker-pairs)
                (let ((beg (marker-position (car marker-pair)))
                      (end (marker-position (cdr marker-pair))))
                  (delete-region beg end)
                  (goto-char beg)
                  (insert new-id))))
            (goto-char (marker-position point-marker))
            ;; Save buffer, so that other, requiring files won't have
            ;; errors when they are re-analyzed.
            (save-buffer)))))
    ;; Now that all files are saved, re-analyze all their buffers.
    (dolist (file-and-sites files-and-sites)
      (let ((file (racket-file-name-back-to-front (car file-and-sites))))
        (with-current-buffer (save-selected-window (find-file file))
          (when racket-pdb-mode
            (racket--pdb-remove-all-decorations)
            (racket-pdb-analyze)))))))

;;; xref

(defun racket-pdb-xref-backend-function ()
  'racket-pdb-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-pdb-xref)))
  (propertize (thing-at-point 'symbol)
              'racket-pdb-xref-point (point)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-pdb-xref)))
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql racket-pdb-xref)) str)
  (pcase (racket--cmd/await nil
                            `(pdb-visit
                              ,(racket-file-name-front-to-back
                                (racket--buffer-file-name))
                              ,(point)))
    (`(,path ,beg ,_end)
     (let ((file (racket-file-name-back-to-front path)))
       (with-current-buffer
           (or (get-file-buffer file)
               (let ((find-file-suppress-same-file-warnings t))
                 (find-file-noselect file)))
         (save-restriction
           (widen)
           (save-excursion
             (goto-char beg)
             (list (xref-make str
                              (xref-make-file-location
                               file
                               (line-number-at-pos)
                               (current-column)))))))))))

(cl-defmethod xref-backend-references ((_backend (eql racket-pdb-xref)) str)
  (let* ((back-end-path (racket-file-name-front-to-back (racket--buffer-file-name)))
         (point (get-text-property 0 'racket-pdb-xref-point str))
         (files-and-sites (racket--cmd/await
                           nil
                           `(pdb-rename-sites ,back-end-path
                                              ,point)))
         (results nil))
    (dolist (file-and-sites files-and-sites)
      (let ((file (racket-file-name-back-to-front (car file-and-sites)))
            (sites (cdr file-and-sites)))
        (with-current-buffer
            (or (get-file-buffer file)
                (let ((find-file-suppress-same-file-warnings t))
                  (find-file-noselect file)))
          (save-restriction
            (widen)
            (save-excursion
              (dolist (site sites)
                (goto-char (car site))
                (push (xref-make (buffer-substring-no-properties (car site) (cdr site))
                                 (xref-make-file-location file
                                                          (line-number-at-pos)
                                                          (current-column)))
                      results)))))))
    (reverse results)))

;;; describe

(defun racket-pdb-describe (&optional prefix)
  "See documentation for similar command `racket-xp-describe'."
  (interactive "P")
  (if (equal prefix '(16))
      (racket-describe-search)
    (let ((path (racket--buffer-file-name)))
      (if (equal prefix '(4))
          (if-let (str (racket--symbol-at-point-or-prompt
                        t
                        "Describe: "
                        nil))
              (racket--do-describe path nil str)
            (message "No documentation available"))
        (racket--cmd/async nil `(pdb-doc-link ,path ,(point))
                           (lambda (path+anchor)
                             (if path+anchor
                                 (racket--do-describe path+anchor nil "")
                               (message "No documentation available"))))))))

(defun racket-pdb-documentation (&optional prefix)
  "See documentation for similar command `racket-xp-documentation'."
  (interactive "P")
  (let ((path (racket--buffer-file-name)))
    (if prefix
        (racket--doc prefix path nil)
      (racket--cmd/async nil `(pdb-doc-link ,path ,(point))
                         (lambda (path+anchor)
                           (if path+anchor
                               (racket-browse-file-url (car path+anchor)
                                                       (cdr path+anchor))
                             (message "No documentation available")))))))

;;; Next/previous use

(defun racket--pdb-forward-use (amt)
  (let ((vec racket--pdb-motion-def-and-use-sites)
        (pt (point)))
    (if-let (ix (seq-position vec
                              nil
                              (lambda (a _)
                                (and (<= (car a) pt) (< pt (cdr a))))))
        (let* ((new-ix  (mod (+ ix amt) (length vec)))
               (new-pos (car (aref vec new-ix))))
          (racket--pdb-remove-all-decorations)
          (goto-char new-pos))
      (user-error "No highlighted definition or use at point"))))

(defun racket-pdb-next-use (&optional amount)
  "When point is a highlighted definition or use, go to the next related site."
  (interactive "P")
  (racket--pdb-forward-use (if (numberp amount) amount 1)))

(defun racket-pdb-previous-use (&optional amount)
  "When point is a highlighted definition or use, go to the previous related site."
  (interactive "P")
  (racket--pdb-forward-use (if (numberp amount) amount -1)))

;;; Completion

(defun racket-pdb-complete-at-point ()
  (pcase-let
      ((`(,beg . ,end)
        ;; Stick to `forward-sexp' here, so should work for non-sexp
        ;; langs via `racket-hash-lang-mode' setting the variable
        ;; `forward-sexp-function' to use lang's grouping-position.
        (save-excursion
          (cons (condition-case ()
                    (progn (forward-sexp -1) (point))
                  (error (point)))
                (condition-case ()
                    (progn (forward-sexp 1) (point))
                  (error (point)))))))
    (list beg
          end
          (completion-table-dynamic
           (lambda (prefix)
             (all-completions prefix
                              (racket--cmd/await nil
                                                 `(pdb-completions
                                                   ,(racket-file-name-front-to-back
                                                     (racket--buffer-file-name))
                                                   ,(point))))))
          :predicate          #'identity
          :exclusive          'no
          ;; TODO: Change these not to use the "def" and "doc"
          ;; commands, if possible??
          :company-location   (racket--xp-make-company-location-proc)
          :company-doc-buffer (racket--xp-make-company-doc-buffer-proc))))

;;; Mode line status

(defvar-local racket--pdb-mode-status nil)

(defun racket--pdb-set-status (&optional which)
  (setq racket--pdb-mode-status which)
  (force-mode-line-update))

(defun racket--pdb-mode-lighter ()
  (let ((prefix "RktPdb"))
    (pcase-let*
        ((status (and (racket--cmd-open-p)
                      racket--pdb-mode-status))
         (`(,suffix ,face ,help-echo)
          (cl-case status
            ((ok)       '("✓" nil
                          "OK"))
            ((err)      `("✗" (face (:inherit error))
                          "One or more errors"))
            ((outdated) `("…" nil
                          "Outdated: Waiting for `racket-pdb-after-change-refresh-delay' or manual `racket-pdb-analyze'"))
            ((running)  '("λ" nil
                          "Getting analysis from pdb via Racket Mode back-end"))
            (otherwise  '("λ" (face (:strike-through t))
                          "Racket Mode back-end not available")))))
      `(" " (:propertize ,(concat prefix suffix)
                         ,@face
                         help-echo ,help-echo)))))

(provide 'racket-pdb)

;; racket-pdb.el ends here
