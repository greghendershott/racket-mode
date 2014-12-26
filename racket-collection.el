;;; racket-collection.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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

(require 'racket-util)
(require 'racket-eval)
(require 'ido)


;;; racket-find-collection

(defun racket-find-collection (&optional prefix)
  "Given a collection name, try to find its directory and files.

Takes a collection name from point (or, with a prefix, prompts you).

If only one directory is found, `ido-find-file-in-dir' lets you
pick a file there.

If more than one directory is found, `ido-completing-read' lets
you pick one, then `ido-find-file-in-dir' lets you pick a file
there.

Note: This requires the `raco-find-collection' package to be
installed. To install it, in `shell' enter:

    raco pkg install raco-find-collection

Tip: This works best with `ido-enable-flex-matching' set to t.
Also handy is the `flx-ido' package from MELPA.

See also: `racket-visit-module'."
  (interactive "P")
  (let* ((coll  (racket--symbol-at-point-or-prompt prefix "Collection name: "))
         (paths (racket--eval/sexpr (format ",find-collection \"%s\"\n" coll))))
    (cond ((eq 'find-collection-not-installed paths)
           ;; FIXME? Offer to run this for them?
           (error "Run `raco pkg install raco-find-collection'"))
          ((not paths)
           (error "Collection not found"))
          ((= 1 (length paths))
           (racket--find-file-in-dir (car paths)))
          (t
           (let ((done nil))
             (while (not done)
               ;; `(ido-find-file-in-dir (ido-completing-read paths))`
               ;; -- except we want to let the user press C-g inside
               ;; ido-find-file-in-dir to back up and pick a different
               ;; module path.
               (let ((dir (ido-completing-read "Directory: " paths)))
                 (condition-case ()
                     (progn (racket--find-file-in-dir dir)
                            (setq done t))
                   (quit nil)))))))))

(defun racket--find-file-in-dir (dir)
  "Like `ido-find-file-in-dir', but allows C-d to `dired' as does `ido-find-file'."
  (ido-file-internal ido-default-file-method nil dir))


;;; racket-open-require-path


;; From looking at ido-mode and ido-vertical-mode:
;;
;; Just use read-from-minibuffer.
;;
;; We're doing vertical mode, so we don't need var like ido-eoinput.
;; We can simply look for the first \n in the minibuffer -- that's the
;; end of user input.
;;
;; Everything after the input and first \n, is the candiates we
;; display, \n separated. The minibuffer automatically grows
;; vertically.
;;
;; Have some maximum number of candidates to display (10?). If > 10, print
;; last line 10 as "...", like ido-vertical-mode.
;;
;; Also use a keymap for commands:
;; - C-n and C-p, which move through the candidates
;; - ENTER
;;   - on a dir will add its contents to the candidates (like DrR's
;;    "Enter Subsellection" button.
;;   - on a file will exit and open the file.
;;
;; Can use
;;   `(run-with-timer seconds nil 'some-function (current-buffer))`
;; to run some-function that does
;;   `(buffer-substring-no-properties (minibuffer-prompt-end) first-\n-pos)`
;; retrieve the what the user has typed so far.
;; Then if changed, use Racket ,open-require to get new list of
;; candidates, and replace everything after eoinput with them.
;
;; Can do (catch 'ido (read-from-minibuffer ...)) and that way the ENTER
;; keymap command can (throw 'ido result) to escape.
;;
;; See also:
;; ido-tidy, a pre-command-hook
;; ido-exhibit, a post-command-hook
;;
;; Remember that typing a letter triggers `self-insert-command'.
;; Therefore the pre and post command hooks will run then, too.

(defvar racket--orp/active nil ;;FIXME: Use minibuffer-exit-hook instead?
  "Is `racket-open-require-path' using the minibuffer?")
(defvar racket--orp/input ""
  "The current user input. Unless user C-g's this persists, as with DrR.")
(defvar racket--orp/matches nil
  "The current user matches. Unless user C-g's this persists, as with DrR.")
(defvar racket--orp/match-index 0
  "The index of the current match selected by the user.")
(defvar racket--orp/input-timer nil
  "The `run-with-timer' ID.")
(defvar racket--orp/input-timer-amount 0.25
  "The delay after user typing ends, before fetching new matches.")
(defvar racket--orp/max-height 10
  "The maximum height of the minibuffer.")
(defvar racket--orp/keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'racket--orp/on-enter)
    (define-key map (kbd "C-j") 'racket--orp/on-enter)
    (define-key map (kbd "C-g") 'racket--orp/on-quit)
    (define-key map (kbd "C-n") 'racket--orp/next)
    (define-key map (kbd "C-p") 'racket--orp/prev)
    map))

(defun racket-open-require-path ()
  "Like Dr Racket's Open Require Path.

Type characters that are part of a module path name. When you
pause typing, a list of choices will appear. Type more (or delete
chars) to refine the search.

- C-n and C-p move among the choices. The current choice is at
  the top and marked with \"->\".
- RET on a file opens it.
- RET on a directory adds its contents to the choices.
- C-g aborts."
  (interactive)
  (unless (racket--eval/sexpr ",open-require begin\n")
    (error "Your version of Racket doesn't provide find-module-completion-path"))
  (setq racket--orp/active t)
  (setq racket--orp/match-index 0)
  ;; We do NOT initialize `racket--orp/input' or `racket--orp/matches'
  ;; here. Like DrR, we remember from last time invoked. We DO
  ;; initialize them in racket--orp/on-quit, if user pressed C-g.
  (add-hook 'minibuffer-setup-hook 'racket--orp/minibuffer-setup)
  (condition-case ()
      (progn
        (read-from-minibuffer "Prompt: "
                              racket--orp/input
                              racket--orp/keymap)
        (when racket--orp/matches
          (find-file (elt racket--orp/matches racket--orp/match-index))))
    (error (setq racket--orp/input "")
           (setq racket--orp/matches nil)))
  (setq racket--orp/active nil)
  (racket--eval/sexpr ",open-require end\n"))

(defun racket--orp/minibuffer-setup ()
  (add-hook 'pre-command-hook  'racket--orp/pre-command  nil t)
  (add-hook 'post-command-hook 'racket--orp/post-command nil t)
  (when racket--orp/active
    (racket--orp/draw-matches)))

(defun racket--orp/eoinput ()
  "Return position where user input ends, i.e. the first \n before the
candidates or (point-max)."
  (save-excursion
    (goto-char (point-min))
    (condition-case ()
        (1- (re-search-forward "\n"))
      (error (point-max)))))

(defun racket--orp/get-user-input ()
  "Get the user's input from the mini-buffer."
  (buffer-substring-no-properties (minibuffer-prompt-end)
                                  (racket--orp/eoinput)))

(defun racket--orp/pre-command ()
  (when racket--orp/input-timer
    (cancel-timer racket--orp/input-timer)
    (setq racket--orp/input-timer nil)))

(defun racket--orp/post-command ()
  "Checks if input has changed; if so, sets a timer to update later.
Intended to allow faster typing; update only after pauses."
  (when racket--orp/active
    (let ((input (racket--orp/get-user-input)))
      (when (not (string-equal input racket--orp/input))
        (setq racket--orp/input-timer
              (run-with-timer racket--orp/input-timer-amount
                              nil
                              'racket--orp/on-input-changed
                              (current-buffer)))))))

(defun racket--orp/on-input-changed (buffer)
  (setq racket--orp/input-timer nil)
  (when (and (buffer-live-p buffer)
             racket--orp/active)
    (with-current-buffer buffer
      (let ((input (racket--orp/get-user-input)))
        (when (not (string-equal input racket--orp/input))
          (let ((matches (cond ((string-equal input "") nil)
                               (t (racket--eval/sexpr
                                   (format ",open-require \"%s\"\n" input))))))
            (setq racket--orp/input input)
            (setq racket--orp/match-index 0)
            (setq racket--orp/matches matches)
            (racket--orp/draw-matches)))))))

(defun racket--orp/draw-matches ()
  (save-excursion
    (delete-region (racket--orp/eoinput) (point-max)) ;delete existing
    (let* ((len (length racket--orp/matches))
           (n   (min racket--orp/max-height len))
           (i   racket--orp/match-index))
      (while (> n 0)
        (insert "\n")
        (cond ((= i racket--orp/match-index) (insert "-> "))
              (t                             (insert "   ")))
        (insert (elt racket--orp/matches i))
        (setq n (1- n))
        (cond ((< (1+ i) len) (setq i (1+ i)))
              (t              (setq i 0))))
      (when (< racket--orp/max-height len)
        (insert "\n   ...")))))

(defun racket--orp/on-enter ()
  (interactive)
  (when racket--orp/active
    ;; Check for input changed (timer hasn't fired yet).
    (let ((input (racket--orp/get-user-input)))
      (if (not (string-equal input racket--orp/input))
          (progn (racket--trace "on-enter" 'input-changed)
                 (racket--orp/on-input-changed (current-buffer)))
        (let ((match (and racket--orp/matches
                          (elt racket--orp/matches racket--orp/match-index))))
          (cond (;; Pressing RET on a directory inserts its contents,
                 ;; just like "Enter subcollection" button in DrR.
                 (and match (file-directory-p match))
                 (racket--trace "on-enter" 'add-subdir)
                 (setq racket--orp/matches
                       (delete-dups ;if they RET same item more than once
                        (sort (append racket--orp/matches
                                      (directory-files match t "[^.]+$"))
                              #'string-lessp)))
                 (racket--orp/draw-matches))
                (t ;; Pressing ENTER on a file exits (selects it).
                 (racket--trace "on-enter" 'exit-minibuffer)
                 (exit-minibuffer))))))))

(defun racket--orp/on-quit ()
  (interactive)
  (when racket--orp/active
    (racket--trace "on-quit")
    (setq racket--orp/input "")
    (setq racket--orp/matches nil)
    (exit-minibuffer)))

(defun racket--orp/next ()
  (interactive)
  (when racket--orp/active
    (setq racket--orp/match-index (1+ racket--orp/match-index))
    (when (>= racket--orp/match-index (length racket--orp/matches))
      (setq racket--orp/match-index 0))
    (racket--orp/draw-matches)))

(defun racket--orp/prev ()
  (interactive)
  (when racket--orp/active
    (setq racket--orp/match-index (1- racket--orp/match-index))
    (when (< racket--orp/match-index 0)
      (setq racket--orp/match-index (max 0 (1- (length racket--orp/matches)))))
    (racket--orp/draw-matches)))

(provide 'racket-collection)

;; racket-collection.el ends here
