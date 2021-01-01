;;; racket-logger.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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

(require 'color)
(require 'easymenu)
(require 'rx)
(require 'racket-custom)
(require 'racket-repl)

;; Need to define this before racket-logger-mode
(defvar racket-logger-mode-map
  (racket--easy-keymap-define
   '(("l"       racket-logger-topic-level)
     ("w"       'toggle-truncate-lines)
     ("n"       racket-logger-next-item)
     ("p"       racket-logger-previous-item)
     ("RET"     racket-logger-show-sites)
     ("j"       racket-logger-next-item-and-show-sites)
     ("k"       racket-logger-previous-item-and-show-sites)
     ("L"       racket-logger-toggle-level-fields-visibility)
     ("O"       racket-logger-toggle-topic-fields-visibility)
     ("T"       racket-logger-toggle-thread-fields-visibility)
     ("I"       racket-logger-toggle-timing-fields-visibility)
     ("d"       racket-logger-show-only-this-thread)
     ("f"       racket-logger-show-all-threads))))

(easy-menu-define racket-logger-mode-menu racket-logger-mode-map
  "Menu for Racket logger mode."
  '("Racket-Logger"
    ["Configure Topic and Level" racket-logger-topic-level]
    ["Toggle Truncate Lines" toggle-truncate-lines]
    "---"
    ["Clear" racket-logger-clear]))

;; There are two "dimensions" of visibility:
;;
;; 1. Each entire line gets an 'invisibile property value which is a
;; symbol: the user program thread name. This allows filtering logger
;; output by thread.
;;
;; 2. Some portions of the line ("fields") also get an 'invisible
;; value, which allows toggling their display on/off. For this we use
;; magic numbers (they can't collide with symbols), which are
;; `defconst'ed just below.
;;
;; See `racket--logger-on-notify' for how we set the 'invisible
;; property. See various commands for how they call
;; `add-to-invisibility-spec' or `remove-from-invisibility-spec'.
(defconst racket--logger-invisible-level 0
  "A value for the 'invisible text property and the `add-to-invisibility-spec'.")
(defconst racket--logger-invisible-topic 1
  "A value for the 'invisible text property and the `add-to-invisibility-spec'.")
(defconst racket--logger-invisible-thread 2
  "A value for the 'invisible text property and the `add-to-invisibility-spec'.")
(defconst racket--logger-invisible-timing 3
  "A value for the 'invisible text property and the `add-to-invisibility-spec'.")
(defvar racket--logger-known-threads nil)


(define-derived-mode racket-logger-mode special-mode "Racket-Logger"
  "Major mode for Racket logger output.
\\<racket-logger-mode-map>

The customization variable `racket-logger-config' determines the
levels for topics. During a session you may change topic levels
using `racket-logger-topic-level'.

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

\\{racket-logger-mode-map}
"
  (setq-local font-lock-defaults nil)
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t) ;disable undo
  (setq-local window-point-insertion-type t)
  (setq buffer-invisibility-spec nil)
  (add-to-invisibility-spec racket--logger-invisible-timing)
  (racket--logger-add-overlay-arrow-variables)
  (add-hook 'before-change-functions #'racket-logger-before-change-function)
  (add-hook 'kill-buffer-hook #'racket-logger-delete-all-overlays nil t)
  (racket--logger-configure-depth-faces)
  (setq-local revert-buffer-function #'racket-logger-revert-buffer-function))

(defun racket-logger-revert-buffer-function (_ignore-auto noconfirm)
  (when (or noconfirm
            (y-or-n-p "Clear buffer?"))
    (with-silent-modifications
      (erase-buffer))
    ;;(racket-logger-delete-all-overlays)
    (dolist (thd racket--logger-known-threads)
      (remove-from-invisibility-spec thd))
    (setq racket--logger-known-threads nil)
    (racket--logger-activate-config)))

(defconst racket--logger-buffer-name "*Racket Logger*")

(defun racket--logger-get-buffer-create ()
  "Create buffer if necessary. Do not display or select it."
  (unless (get-buffer racket--logger-buffer-name)
    (with-current-buffer (get-buffer-create racket--logger-buffer-name)
      (racket-logger-mode)
      (racket--logger-activate-config)))
  (get-buffer racket--logger-buffer-name))

(defun racket--logger-on-notify (v)
  (when noninteractive ;emacs --batch
    (princ (format "{racket logger}: %s\n" v)))
  (with-current-buffer (racket--logger-get-buffer-create)
    (let* ((inhibit-read-only  t)
           (original-point     (point))
           (point-was-at-end-p (equal original-point (point-max))))
      (goto-char (point-max))
      (racket--logger-insert v)
      (unless point-was-at-end-p
        (goto-char original-point)))))

(cl-defstruct racket-logger
  depth caller context msec thread)

(defun racket--logger-get (&optional accessor)
  "Get our `racket-logger' struct from a 'racket-logger text
property at point, and apply the struct ACCESSOR."
  (pcase (get-text-property (point) 'racket-logger)
    ((and (pred racket-logger-p) v)
     (if accessor
         (funcall accessor v)
       v))))

(cl-defstruct racket-trace
  callp tailp name message show identifier formals header)

(defun racket--trace-get (&optional accessor)
  "Get our `racket-trace' struct from a 'racket-trace text
property at point, and apply the struct ACCESSOR."
  (pcase (get-text-property (point) 'racket-trace)
    ((and (pred racket-trace-p) v)
     (if accessor
         (funcall accessor v)
       v))))

(defconst racket--logger-unknown (intern "<unknown>"))

(defun racket--logger-insert (notify-data)
  (pcase-let*
      ((`(,level ,topic ,message ,depth ,caller ,context ,msec ,thread ,tracing)
        notify-data)
       (msec   (or msec racket--logger-unknown))
       (thread (or thread racket--logger-unknown))
       (logger-prop (make-racket-logger
                     :depth   depth
                     :caller  (racket--logger-srcloc-beg+end caller)
                     :context (racket--logger-srcloc-beg+end context)
                     :msec    msec
                     :thread  thread))
       ;; Possibly more things if tracing
       (`(,callp ,tailp ,trace-prop)
        (pcase tracing
          (`(,call ,tail ,name ,show ,identifier ,formals ,header)
           (list call
                 tail
                 (make-racket-trace
                  :callp      call
                  :tailp      tail
                  :name       name
                  :message    message
                  :show       show
                  :identifier (racket--logger-srcloc-line+col identifier)
                  :formals    (racket--logger-srcloc-beg+end formals)
                  :header     (racket--logger-srcloc-beg+end header))))))
       (new-thread-p (save-excursion
                       (ignore-errors (racket-logger-previous-item))
                       (not (eq (racket--logger-get #'racket-logger-thread)
                                thread))))
       (overline (when new-thread-p
                   `(:overline t)))
       (prefix (if trace-prop
                   (if callp
                       (if tailp
                           "⤑ "
                         "↘ ")
                     "   ⇒ ")
                 "• ")))
    (cl-pushnew thread racket--logger-known-threads)
    ;; We insert several separately-propertized strings because
    ;; some are "fields" that need their own face and
    ;; 'invisible property.
    (insert
     (concat
      (propertize (racket--logger-level->string level)
                  'racket-logger logger-prop
                  'racket-trace  trace-prop
                  'invisible (list thread
                                   racket--logger-invisible-level))
      (propertize (concat (substring topic 0 (min (length topic) 15))
                          (make-string (max 0 (- 15 (length topic))) ?\ )
                          " ")
                  'face racket-logger-topic-face
                  'racket-logger logger-prop
                  'racket-trace  trace-prop
                  'invisible (list thread
                                   racket--logger-invisible-topic))
      (propertize (concat (racket--logger-pad-string (format "%s" thread) 20)
                          " ")
                  'face          `(,@overline
                                   ,@(when new-thread-p
                                       (unless (eq thread '\?)
                                         `(:weight bold)))
                                   :height 0.8)
                  'racket-logger logger-prop
                  'racket-trace  trace-prop
                  'help-echo     (format "thread: %s" thread)
                  'invisible     (list thread
                                       racket--logger-invisible-thread))
      (propertize (concat (racket--logger-pad-string (format "%s" msec) 20)
                          " ")
                  'face          `(,@overline
                                   :height 0.8)
                  'racket-logger logger-prop
                  'racket-trace  trace-prop
                  'help-echo     (format "msec: %s" msec)
                  'invisible     (list thread
                                       racket--logger-invisible-timing))))
    ;; For an "inset boxes" effect, we start the line by
    ;; drawing a space for each parent level, in its background
    ;; color.
    (cl-loop for n to (1- depth)
             do
             (insert
              (propertize
               "  "
               'face          `(:inherit ,(racket--logger-depth-face-name n) ,@overline)
               'racket-logger logger-prop
               'racket-trace  trace-prop
               'invisible     thread)))
    ;; Finally draw the interesting information for this line.
    (let ((inherit `(:inherit ,(racket--logger-depth-face-name depth))))
      (insert
       (concat
        (propertize prefix
                    'face          `(,@inherit ,@overline)
                    'racket-logger logger-prop
                    'racket-trace  trace-prop
                    'invisible     thread)
        (propertize (racket--logger-limit-string message 4096)
                    'racket-logger-message
                    t ;message itself used by next/prev
                    'face          inherit
                    'racket-logger logger-prop
                    'racket-trace  trace-prop
                    'invisible     thread)
        (propertize "\n"
                    'face          inherit
                    'racket-logger logger-prop
                    'racket-trace  trace-prop
                    'invisible     thread))))))

(defun racket--logger-level->string (level)
  (cl-case level
    ('fatal   (propertize "[  fatal] " 'face racket-logger-fatal-face))
    ('error   (propertize "[  error] " 'face racket-logger-error-face))
    ('warning (propertize "[warning] " 'face racket-logger-warning-face))
    ('info    (propertize "[   info] " 'face racket-logger-info-face))
    ('debug   (propertize "[  debug] " 'face racket-logger-debug-face))))

;;; srclocs

(defun racket--logger-srcloc-line+col (v)
  "Extract the line and col from a srcloc."
  (pcase v
    (`(,path ,line ,col ,_pos ,_span)
     `(,path ,line ,col))))

(defun racket--logger-srcloc-beg+end (v)
  "Extract the pos and span from a srcloc and convert to beg and end."
  (pcase v
    (`(,path ,_line ,_col ,pos ,span)
     `(,path ,pos ,(+ pos span)))))

;;; Depth faces

(defface racket-logger-even-depth-face '((t (:inherit default)))
  "Face for even depths. Not for user customization.
The background color of this face is calculated as a change in
luminosity of the default face's background color."
  :group 'racket-faces)
(defface racket-logger-odd-depth-face '((t (:inherit default)))
  "Face for odd depths. Not for user customization.
The background color of this face is calculated as a change in
luminosity of the default face's background color."
  :group 'racket-faces)

(defun racket--logger-configure-depth-faces (&rest _ignored)
  (pcase (face-background 'default)
    ((and (pred color-defined-p) bg)
     (let ((sign (pcase-let* ((`(,_H ,_S ,L) (apply #'color-rgb-to-hsl
                                                    (color-name-to-rgb bg))))
                   (if (< L 0.5) 1 -1))))
       (set-face-background 'racket-logger-even-depth-face
                            (color-lighten-name bg (* 5 sign)))
       (set-face-background 'racket-logger-odd-depth-face
                            (color-lighten-name bg (* 10 sign)))))))

(advice-add 'load-theme    :after #'racket--logger-configure-depth-faces)
(advice-add 'disable-theme :after #'racket--logger-configure-depth-faces)

(defun racket--logger-depth-face-name (depth)
  (if (cl-evenp depth)
      'racket-logger-even-depth-face
    'racket-logger-odd-depth-face))

(defun racket--logger-depth-background (depth)
  `(:background ,(face-background (racket--logger-depth-face-name depth))))

;;; Logger topic configuration

(defun racket--logger-activate-config ()
  "Send config to back end and display it in the buffer."
  (racket--cmd/async nil
                     `(logger ,racket-logger-config))
  (with-current-buffer (get-buffer-create racket--logger-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (concat "racket-logger-config:\n"
                                  (let ((print-length nil)
                                        (print-level nil))
                                    (pp-to-string racket-logger-config)))
                          'face racket-logger-config-face))
      (goto-char (point-max)))))

(defun racket--logger-set (topic level)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (unless (symbolp level) (error "LEVEL must be symbolp"))
  (pcase (assq topic racket-logger-config)
    (`() (add-to-list 'racket-logger-config (cons topic level)))
    (v   (setcdr v level)))
  (racket--logger-activate-config))

(defun racket--logger-unset (topic)
  (unless (symbolp topic) (error "TOPIC must be symbolp"))
  (when (eq topic '*)
    (user-error "Cannot unset the level for the '* topic"))
  (setq racket-logger-config
        (assq-delete-all topic racket-logger-config))
  (racket--logger-activate-config))

(defun racket--logger-topics ()
  "Effectively (sort (dict-keys racket-logger-config))."
  (sort (mapcar (lambda (x) (format "%s" (car x)))
                racket-logger-config)
        #'string<))

(defun racket--logger-topic-level (topic not-found)
  "Effectively (dict-ref racket-logger-config topic not-found)."
  (or (cdr (assq topic racket-logger-config))
      not-found))

;;; General logger commands

(defun racket-logger ()
  "Select the `racket-logger-mode' buffer in a bottom side window."
  (interactive)
  (select-window
   (display-buffer-in-side-window (racket--logger-get-buffer-create)
                                  '((side . bottom)
                                    (slot . 1)
                                    (window-height . 15)))))

(defun racket--logger-start-of-message-span-p ()
  (and (get-text-property (point) 'racket-logger-message)
       (or (= (point) (point-min))
           (not (get-text-property (1- (point)) 'racket-logger-message)))))

(defun racket--logger-move-to-start-of-message-span (prev/next)
  (pcase (funcall prev/next (point) 'racket-logger-message)
    ((and (pred numberp) pos)
     (goto-char pos)
     (if (racket--logger-start-of-message-span-p)
         t
       (pcase (funcall prev/next (point) 'racket-logger-message)
         ((and (pred numberp) pos)
          (goto-char pos)
          t))))))

(defun racket--logger-do-move-prev-or-next (prev-or-next none-msg)
  (let ((orig (point)))
    (cl-loop while
             (and (racket--logger-move-to-start-of-message-span prev-or-next)
                  (invisible-p (point))))
    (unless (and (racket--logger-start-of-message-span-p)
                 (not (invisible-p (point))))
      (goto-char orig)
      (user-error none-msg))))

(defun racket-logger-next-item ()
  "Move point to start of next visible logger message."
  (interactive)
  (racket--logger-do-move-prev-or-next #'next-single-property-change
                                       "No visible next item"))

(defun racket-logger-previous-item ()
  "Move point to start of previous visible logger message."
  (interactive)
  (racket--logger-do-move-prev-or-next #'previous-single-property-change
                                       "No visible previous item"))

(defun racket-logger-next-item-and-show-sites ()
  (interactive)
  (racket-logger-next-item)
  (racket-logger-show-sites))

(defun racket-logger-previous-item-and-show-sites ()
  (interactive)
  (racket-logger-previous-item)
  (racket-logger-show-sites))

(defun racket-logger-topic-level ()
  "Set or unset the level for a topic.

For convenience, input choices using `ido-completing-read'.

The topic labeled \"*\" is the level to use for all topics not
specifically assigned a level.

The level choice \"*\" means the topic will no longer have its
own level, therefore will follow the level specified for the
\"*\" topic."
  (interactive)
  (let* ((topic  (ido-completing-read
                  "Topic: "
                  (racket--logger-topics)))
         (topic  (pcase topic
                   ("" "*")
                   (v  v)))
         (topic  (intern topic))
         (levels (list "fatal" "error" "warning" "info" "debug"))
         (levels (if (eq topic '*) levels (cons "*" levels)))
         (level  (ido-completing-read
                  (format "Level for topic `%s': " topic)
                  levels
                  nil t nil nil
                  (format "%s" (racket--logger-topic-level topic "*"))))
         (level  (pcase level
                   (""  nil)
                   ("*" nil)
                   (v   (intern v)))))
    (if level
        (racket--logger-set topic level)
      (racket--logger-unset topic))))

;; Visibility commands

(defun racket-logger-show-only-this-thread ()
  "Filter to show only items for the thread at point."
  (interactive)
  (let ((thread (racket--logger-get #'racket-logger-thread)))
    (unless thread (user-error "No thread found"))
    (dolist (thd racket--logger-known-threads)
      (unless (eq thd thread)
        (add-to-invisibility-spec thd)))
    (save-selected-window (other-window 1)) ;HACK: cause redisplay
    (message "Showing items only for thread %s" thread)))

(defun racket-logger-show-all-threads ()
  "Stop `racket-logger-show-only-this-thread' filtering."
  (interactive)
  (dolist (thd racket--logger-known-threads)
    (remove-from-invisibility-spec thd))
  (save-selected-window (other-window 1)) ;HACK: cause redisplay
  (message "Showing items for all threads"))

(defun racket-logger-toggle-level-fields-visibility ()
  (interactive)
  (if (memq racket--logger-invisible-level buffer-invisibility-spec)
      (remove-from-invisibility-spec racket--logger-invisible-level)
    (add-to-invisibility-spec racket--logger-invisible-level))
  (save-selected-window (other-window 1)))

(defun racket-logger-toggle-topic-fields-visibility ()
  (interactive)
  (if (memq racket--logger-invisible-topic buffer-invisibility-spec)
      (remove-from-invisibility-spec racket--logger-invisible-topic)
    (add-to-invisibility-spec racket--logger-invisible-topic))
  (save-selected-window (other-window 1)))

(defun racket-logger-toggle-thread-fields-visibility ()
  (interactive)
  (if (memq racket--logger-invisible-thread buffer-invisibility-spec)
      (remove-from-invisibility-spec racket--logger-invisible-thread)
    (add-to-invisibility-spec racket--logger-invisible-thread))
  (save-selected-window (other-window 1))) ;HACK: cause redisplay

(defun racket-logger-toggle-timing-fields-visibility ()
  (interactive)
  (if (memq racket--logger-invisible-timing buffer-invisibility-spec)
      (remove-from-invisibility-spec racket--logger-invisible-timing)
    (add-to-invisibility-spec racket--logger-invisible-timing))
  (save-selected-window (other-window 1))) ;HACK: cause redisplay

;;; Showing caller and formals sites

(defvar racket-logger-called-marker nil
  "A value for the variable `overlay-arrow-variable-list'.")
(defvar racket-logger-caller-marker nil
  "A value for the variable `overlay-arrow-variable-list'.")
(defvar racket-logger-context-marker nil
  "A value for the variable `overlay-arrow-variable-list'.")

(defun racket--logger-add-overlay-arrow-variables ()
  (cl-pushnew 'racket-logger-called-marker overlay-arrow-variable-list)
  (cl-pushnew 'racket-logger-caller-marker overlay-arrow-variable-list)
  (cl-pushnew 'racket-logger-context-marker overlay-arrow-variable-list))

(defvar racket--logger-overlays nil
  "List of overlays we've added in various buffers.")

(defun racket-logger-delete-all-overlays ()
  "Delete all overlays and overlay arrows in various buffers."
  (setq racket-logger-called-marker nil
        racket-logger-caller-marker nil
        racket-logger-context-marker nil)
  (dolist (o racket--logger-overlays) (delete-overlay o))
  (setq racket--logger-overlays nil))

(defun racket-logger-before-change-function (_beg _end)
  "When a buffer is modified, hide all overlays we have in it.
For speed we don't actually delete them, just move them \"nowhere\"."
  (setq racket-logger-called-marker nil
        racket-logger-caller-marker nil
        racket-logger-context-marker nil)
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (dolist (o racket--logger-overlays)
        (when (equal (overlay-buffer o) buf)
          (with-temp-buffer (move-overlay o 1 1)))))))

(defun racket-logger-show-sites ()
  (interactive)
  "Show caller and called sites for current level."
  (racket-logger-delete-all-overlays)
  (pcase (racket--trace-get)
    ((and (pred racket-trace-p) trace)
     ;; Draw called site first. That way it "wins" if the caller
     ;; site happens to intersect.
     (racket--logger-highlight-called-site trace)
     ;; Draw caller site, if any, and make it visible.
     (pcase (racket--logger-get)
       ((and (pred racket-logger-p) logger)
        (racket--logger-highlight-caller-site logger trace)
        (ignore-errors (racket-logger-goto-caller-site))))
     ;; Make called site visible, last, so it will be visible if the
     ;; caller site happens to be in same buffer.
     (ignore-errors (racket-logger-goto-called-site)))))

(defun racket--logger-highlight-called-site (trace)
  ;; This is slightly complicated because, for calls, normally we want
  ;; to replace only formals with actuals. However a thunk has no
  ;; formals, so in that case we replace the whole header with the
  ;; trace message which is the entire application (f args).
  (pcase-let ((`(,msg ,file ,beg ,end)
               (if (racket-trace-callp trace)
                   (pcase-let ((`(,file ,beg ,end) (racket-trace-formals trace)))
                     (if (/= beg end)
                         (cons (racket-trace-show trace) ;just `args`
                               (racket-trace-formals trace))
                       (cons (racket-trace-message trace) ;`(f args)`
                             (racket-trace-header trace))))
                 (cons (racket-trace-show trace) ;trace-message = -show for results
                       (racket-trace-header trace)))))
    (with-current-buffer (racket--logger-buffer-for-file file)
      (racket--logger-put-highlight-overlay (racket-trace-callp trace)
                                            msg
                                            beg end
                                            102))))

(defun racket--logger-highlight-caller-site (logger trace)
  (pcase (racket-logger-caller logger)
    (`(,file ,beg ,end)
     (with-current-buffer (racket--logger-buffer-for-file file)
       (racket--logger-put-highlight-overlay (racket-trace-callp trace)
                                             (racket-trace-message trace)
                                             beg end
                                             101)))))

(defun racket--logger-put-highlight-overlay (callp str beg end priority)
  ;; Avoid drawing overlays on top of each other, for example the
  ;; caller and called sites intersect. Whoever draws first wins.
  (unless (cl-some (lambda (o)
                     (eq (overlay-get o 'name) 'racket-trace-overlay))
                   (overlays-in beg end))
    (let* ((str (racket--logger-limit-string str 80))
           (o (make-overlay beg end))
           (face `(:inherit 'default :background ,(face-background 'match))))
      (push o racket--logger-overlays)
      (overlay-put o 'name 'racket-trace-overlay)
      (overlay-put o 'priority priority)
      (if callp
          ;; Replace
          (progn
            (overlay-put o 'face face)
            (overlay-put o 'display str))
        ;; `str' is results: display after. Avoid drawing redundant
        ;; results after-strings, which could happen with
        ;; trace-expression, because caller and called sites are the
        ;; same; overlay priorities won't help.
        (unless (cl-some (lambda (o)
                           (and (overlay-get o 'after-string)
                                (eq (overlay-get o 'name) 'racket-trace-overlay)))
                         (overlays-at beg))
          (overlay-put o 'display (buffer-substring beg end))
          (overlay-put o 'after-string (propertize (concat " ⇒ " str)
                                                   'face face)))))))

(defun racket-logger-goto-called-site ()
  (interactive)
  (pcase (racket--trace-get #'racket-trace-formals)
    (`(,file ,beg ,end)
     (setq racket-logger-called-marker
           (racket--logger-goto file beg end)))
    (_ (user-error "No called site information is available"))))

(defun racket-logger-goto-caller-site ()
  (interactive)
  (pcase (racket--logger-get #'racket-logger-caller)
    (`(,file ,beg ,end)
     (setq racket-logger-caller-marker
           (racket--logger-goto file beg end t)))
    (_ (user-error "No call site information is available"))))

(defun racket-logger-goto-context-site ()
  (interactive)
  (pcase (racket--logger-get #'racket-logger-context)
    (`(,file ,beg ,end)
     (setq racket-logger-context-marker
           (racket--logger-goto file beg end t)))
    (_ (user-error "No context site information is available"))))

(defun racket--logger-goto (file-or-buffer beg end &optional pulse-p)
  "Returns marker for BOL."
  (let ((buffer (if (bufferp file-or-buffer)
                    file-or-buffer
                  (racket--logger-buffer-for-file file-or-buffer))))
    (let ((win (display-buffer buffer
                               '((display-buffer-reuse-window
                                  display-buffer-below-selected
                                  display-buffer-use-some-window)
                                 (inhibit-same-window . t)))))
      (save-selected-window
        (select-window win)
        ;; When we move point and `racket-xp-mode' is active, its
        ;; point-motion thing kicks to display highlighting and
        ;; tooltips. Avoid by going to 1+ end, which is less likely to
        ;; be e.g. an identifier.
        (goto-char (1+ end))
        (when pulse-p
          (pulse-momentary-highlight-region beg end))
        (save-excursion (beginning-of-line) (point-marker))))))

(defun racket--logger-buffer-for-file (file)
  (or (get-file-buffer file)
      (let ((find-file-suppress-same-file-warnings t))
        (find-file-noselect file))))

;;; Misc

;; TODO: Refine this limiting
(defun racket--logger-limit-string (str &optional max)
  (let ((max (or max 80))
        (len (length str)))
    (if (< len max)
        str
      (concat (substring str 0 (min len (- max 3)))
              "..."))))

(defun racket--logger-pad-string (str &optional max)
  (let ((max (or max 80))
        (len (length str)))
    (if (< len max)
        (concat str (make-string (- max len) ?\ ))
      (concat (substring str 0 (min len (- max 3)))
              "..."))))

(provide 'racket-logger)

;;; racket-logger.el ends here
