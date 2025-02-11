;;; racket-describe.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'shr)
(require 'subr-x)
(require 'racket-browse-url)
(require 'racket-cmd)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-scribble)
(require 'racket-browse-url)
(require 'racket-back-end)
;; Don't (require 'racket-repl). Mutual dependency. Instead:
(declare-function racket--repl-session-id "racket-repl" ())
(autoload         'racket--repl-session-id "racket-repl")

(defvar-local racket--describe-here nil
  "The current navigation point. Either nil or (cons path point).")
(defvar-local racket--describe-stack-back nil
  "Back navigation list. Each item is (cons path point).")
(defvar-local racket--describe-stack-forward nil
  "Forward navigation list. Each item is (cons path point).")

(defun racket--call-with-describe-buffer (thunk)
  "Call THUNK in a blank `racket-describe-mode-buffer' and return
the buffer.

A helper for use by `racket--describe-path+anchor' and
`racket--describe-string'."
  (let ((buf-name (format "*Racket Describe <%s>*"
                          (racket-back-end-name))))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (eq major-mode 'racket-describe-mode)
        (racket-describe-mode))
      (setq racket--describe-stack-forward nil)
      (racket--describe-maybe-push-here 'back) ;before `erase-buffer'
      (let ((buffer-read-only nil))
        (erase-buffer))
      ;; shr-insert-document needs window width for formatting, and we
      ;; set window-point, so proactively give the window a buffer.
      (pop-to-buffer (current-buffer))
      (funcall thunk)
      (current-buffer))))

(defun racket--describe-path+anchor (path anchor)
  "Display/return `racket-describe-mode' buffer for PATH and ANCHOR."
  (racket--call-with-describe-buffer
   (lambda ()
     (racket--describe-insert-dom path
                                  anchor
                                  (racket--scribble-path->shr-dom path)))))

(defun racket--describe-string (str how &optional repl-session-id)
  "Display/return `racket-describe-mode' buffer for STR.

Use back end \"describe\" command to try to find documentation or
a description of STR, where HOW is either the source file
pathname or \\='namespace. When the latter, REPL-SESSION-ID must
be supplied."
  (racket--call-with-describe-buffer
   (lambda ()
     (setq header-line-format
           (propertize (format "Getting information from back end about %s ..." str)
                       'face 'italic))
     (racket--cmd/async
      repl-session-id
      `(describe ,(racket-how-front-to-back how) ,str)
      (lambda (result)
        (pcase result
          ;; STR has documentation at path and anchor.
          (`(,(and path (pred stringp)) . ,anchor)
           (let ((path (racket-file-name-back-to-front path)))
             (racket--describe-insert-dom path
                                          anchor
                                          (racket--scribble-path->shr-dom path))))
          ;; STR doesn't have documentation, but it does have a
          ;; signature and/or type, and here is a dom about that
          ;; we can insert.
          (`(shr-dom ,dom)
           (racket--describe-insert-dom nil     ;path
                                        str     ;anchor
                                        dom))
          ;; STR doesn't seem to be an identifier we can describe.
          (`()
           (racket--describe-insert-dom nil     ;path
                                        str     ;anchor
                                        (racket--describe-not-found-dom str)))))))))

(defun racket--describe-not-found-dom (str)
  `(div ()
        (p ()
           "No documentation, signature, or type found for "
           (racket-anchor ((name . ,str)))
           (em () ,str))
        (p () "If you came from a racket-xp-mode buffer, maybe it didn't finish annotating. You could press " (strong () "q") " return to that buffer, wait, then try again.")
        (p () "Otherwise you can type " (strong ()  "C-c C-s") " to search for " (em () ,str) " in the documentation index.")))

(defvar-local racket--describe-nav nil
  "The value of the racket-nav element extracted from a page.
Use `dom-attr' to extract the top, up, prev, next links, if any.")

(defun racket--describe-insert-dom (path goto dom)
  "Insert DOM into current buffer, add some buttons, and move point.

GOTO determines where point is moved: If stringp move to that
anchor. If numberp, move to that position."
  (setq racket--describe-here
        (if path (cons path goto) nil))
  (setq racket--describe-nav nil)
  (setq header-line-format
        (propertize
         (concat path (cond ((stringp goto) (concat " " goto))
                            ((numberp goto) (format " %s" goto))))
         'face '(:height 0.75)))
  ;; Although `shr' carefully fills to fit window width, if user
  ;; resizes window or changes text scaling, we don't want it to wrap.
  (setq truncate-lines t)
  ;; Modes that show line numbers in the buffer just eat up valuable
  ;; space; disable. (Also we'll set a text prop below.)
  (when (fboundp 'linum-mode)                (linum-mode -1))
  (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((shr-use-fonts nil)
          (shr-external-rendering-functions
           `((span              . ,#'racket-render-tag-span)
             (h1                . ,#'racket-render-tag-heading)
             (h2                . ,#'racket-render-tag-heading)
             (h3                . ,#'racket-render-tag-heading)
             (h4                . ,#'racket-render-tag-heading)
             (h5                . ,#'racket-render-tag-heading)
             (h6                . ,#'racket-render-tag-heading)
             (h7                . ,#'racket-render-tag-heading)
             (racket-doc-link   . ,#'racket-render-tag-racket-doc-link)
             (racket-ext-link   . ,#'racket-render-tag-racket-ext-link)
             (racket-anchor     . ,#'racket-render-tag-racket-anchor)
             (racket-nav        . ,#'racket-render-tag-racket-nav))))
      (shr-insert-document
       (racket--describe-handle-toc-nodes dom)))
    ;; See doc string for `racket--scribble-temp-nbsp'.
    (goto-char (point-min))
    (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
      (replace-match " " t t))
    ;; Just in case disabling `display-line-numbers-mode' doesn't
    ;; suffice (#678), as well as to cover e.g. user enabling
    ;; `global-display-line-numbers-mode' later:
    (put-text-property (point-min) (point-max) 'display-line-numbers-disable t)
    (racket--describe-goto goto)))

(defun racket--describe-goto (goto)
  "Move point to GOTO.

If `numberp', move to that position.

If `stringp' move to the position after the anchor that is not
anchor. There could be multiple anchors before some non-anchor
text. We want point left where `racket-search-describe' can use
`thing-at-point' to find a symbol."
  (set-window-point ;in case buffer window isnt' selected; #590
   (get-buffer-window (current-buffer))
   (cond
    ((numberp goto)
     goto)
    ((stringp goto)
     (or (racket--describe-anchor->position goto)
         (point-min)))
    (t (point-min))))
  (setq racket--describe-here
        (cons (car racket--describe-here) (point))))

(defun racket--describe-anchor->position (anchor)
  (let ((i nil)) ;silence byte-compiler warning...
    i            ;...on all versions of emacs
    (cl-loop for i being the intervals
             if (equal (get-text-property (car i) 'racket-anchor)
                       anchor)
             return (cl-loop for j from (car i) to (point-max)
                             if (not (get-text-property j 'racket-anchor))
                             return j))))

(defvar-local racket--describe-on-this-page nil)

(defun racket--describe-handle-toc-nodes (dom)
  "Handle nodes that render as a \"left nav panel\" in a web browser.

These aren't effective in a shr buffer, due to window width and
lack of independent scrolling columns. Instead:

- \"tocview\": Just delete it. User can nav up to see.

- \"tocsub\" a.k.a. \"On this page:\": Useful, but present via
  `imenu'.

Both are children of a \"tocscet\" div."
  (setq-local
   racket--describe-on-this-page
   (let* ((tocsublist-table (car (dom-by-class dom "tocsublist")))
          (trs (dom-children tocsublist-table)))
     (seq-map (lambda (tr)
                (let* ((td (car (dom-children tr)))
                       (num (car (dom-by-class td "tocsublinknumber")))
                       (link (dom-child-by-tag td 'racket-doc-link))
                       (label (concat (dom-texts num "")
                                      (dom-texts link "")))
                       (label (subst-char-in-string racket--scribble-temp-nbsp
                                                    32
                                                    label))
                       (anchor (dom-attr link 'anchor)))
                  (cons label anchor)))
              trs)))
  (pcase (dom-by-class dom "tocset")
    (`(,node . ,_) (dom-remove-node dom node)))
  dom)

(defun racket--describe-imenu-create-index ()
  (seq-map (lambda (v)
             (cons (car v)
                   (racket--describe-anchor->position (cdr v))))
           racket--describe-on-this-page))

(defconst racket--shr-faces
  '(("RktSym"                . font-lock-keyword-face)
    ("RktVal"                . font-lock-constant-face)
    ("RktCmt"                . font-lock-comment-face)
    ("RktErr"                . error)
    ("RktOut"                . racket-doc-output-face)
    ("RktRes"                . font-lock-constant-face)
    ("RktVar"                . font-lock-variable-name-face)
    ("RktInBG"               . racket-doc-litchar-face)
    ("RktModLink"            . font-lock-keyword-face)
    ("techinside"            . italic)
    ("RktValLink"            . font-lock-variable-name-face)
    ("RktStxLink"            . font-lock-keyword-face)
    ("RktValDef RktValLink"  . bold)
    ("RktStxDef RktStxLink"  . bold)))

(defun racket--describe-dom->face (dom)
  (let ((class (dom-attr dom 'class)))
    (if (equal class "RktPn")
        ;; Scribble gives keyword arguments "RktPn" style and CSS
        ;; conditionally adjusts. Ugh. Do similar hack here.
        (cond ((string-match-p "^#:" (dom-text dom)) 'racket-keyword-argument-face)
              ((facep 'parenthesis)                  'parenthesis)
              (t                                     'default))
      (cdr (assoc class racket--shr-faces)))))

(defun racket-render-tag-span (dom)
  "Handle some things shr-tag-span does not.

When span has a title attribute, set help-echo property.

When span has a RktXXX or techinside class, set the face."
  (let ((start (point)))
    (if-let (face (racket--describe-dom->face dom))
        (shr-fontize-dom dom face)
      (shr-generic dom))
    (when-let (title (dom-attr dom 'title))
      (put-text-property start (point) 'help-echo title))))

(defun racket-render-tag-heading (dom)
  (pcase-let ((`(,level . ,face)
               (pcase (car dom)
                 ('h1 '(1 . (variable-pitch (:height 2.00))))
                 ('h2 '(2 . (variable-pitch (:height 1.90))))
                 ('h3 '(3 . (variable-pitch (:height 1.75))))
                 ('h4 '(4 . (variable-pitch (:height 1.60))))
                 ('h5 '(5 . (variable-pitch (:height 1.45))))
                 ('h6 '(6 . (variable-pitch (:height 1.40))))
                 ('h7 '(7 . (variable-pitch (:height 1.15))))
                 (_ '(nil . (variable-pitch (:weight bold)))))))
    ;; Starting in Emacs 30.0.50, `shr-heading' assumes the face is a
    ;; symbol shr-hN so it can extract N to add an outline-level text
    ;; property. Avoid calling that; instead do equivalent. See #687.
    (shr-ensure-paragraph)
    (let ((start (point)))
      (shr-fontize-dom dom face)
      (when level
        (put-text-property start
                           (let ((inhibit-field-text-motion t))
                             (line-end-position))
                           'outline-level level)))
    (shr-ensure-paragraph)))

(define-button-type 'racket-doc-link
  'action #'racket-describe-doc-link-button)

(defun racket-render-tag-racket-doc-link (dom)
  (let ((path   (dom-attr dom 'path))
        (anchor (dom-attr dom 'anchor))
        (start  (point))
        (shr-start nil))
    (shr-generic dom) ;this will add faces to `dom' kids
    (unless (= start (point))
      (make-text-button
       start                   (point)
       'type                   'racket-doc-link
       'racket-doc-link-path   path
       'racket-doc-link-anchor anchor
       'face                   'racket-doc-link-face))))

(define-button-type 'racket-ext-link
  'action #'racket-describe-ext-link-button)

(defun racket-render-tag-racket-ext-link (dom)
  (let ((href   (dom-attr dom 'href))
        (start  (point))
        (shr-start nil))
    (shr-generic dom)
    (unless (= start (point))
      (make-text-button
       start                 (point)
       'type                 'racket-ext-link
       'face                 'racket-ext-link-face
       'racket-ext-link-href href))))

(defun racket-render-tag-racket-anchor (dom)
  "At least in Emacs 25.2 shr-tag-a isn't handling <a> anchors at all.
So we have our back end substitute these <racket-anchor> elements
for our custom shr handler."
  (let ((start (point))
        (id (or (dom-attr dom 'id) (dom-attr dom 'name))))
    (shr-generic dom)
    ;; How to attach a property to nothing? Make an invisible
    ;; something; insert a character with a 'display property value of
    ;; "". Although not displayed to the user, the character exists in
    ;; the buffer, therefore the choice of character matters. Don't
    ;; use a space because shr might eliminate it. Don't use something
    ;; that `thing-at-point' considers part of a symbol (in case user
    ;; inovkes `racket-describe-search' with point here).
    (when (= start (point))
      (insert ?^)
      (put-text-property (1- (point)) (point) 'display ""))
    (put-text-property start (1+ start) 'racket-anchor id)))

(defun racket-render-tag-racket-nav (dom)
  (setq racket--describe-nav dom))

(defun racket--describe-nav (which)
  (interactive)
  (let ((path (dom-attr racket--describe-nav which)))
    (unless path
      (user-error "There is no %s page available" which))
    (setq racket--describe-stack-forward nil)
    (racket--describe-maybe-push-here 'back)
    (racket--describe-fetch-and-show path nil)))

(defun racket-describe-nav-top ()
  (interactive)
  (racket--describe-nav 'top))

(defun racket-describe-nav-up ()
  (interactive)
  (racket--describe-nav 'up))

(defun racket-describe-nav-prev ()
  (interactive)
  (racket--describe-nav 'prev))

(defun racket-describe-nav-next ()
  (interactive)
  (racket--describe-nav 'next))

(defun racket--describe-fetch-and-show (path goto)
  "Insert shr dom for PATH and move point to GOTO.

PATH is doc path, as in the \"racket-doc-link-path\" button
property.

GOTO is as in `racket--describe-goto'."
  (if (equal path (car racket--describe-here))
      (racket--describe-goto goto) ;just move, same page
    (setq header-line-format
          (propertize
           (format "Waiting for documentation file %s"
                   path)
           'face 'italic))
    (condition-case e
        (racket--describe-insert-dom path
                                     goto
                                     (racket--scribble-path->shr-dom path))
      (error
       (setq header-line-format
             (propertize (format "%S" e)
                         'face 'error))
       (setq racket--describe-here nil)))))

(defun racket--describe-maybe-push-here (which)
  "When it is a path, push `racket--describe-here' to WHICH stack.

It might not be a path when for instance the back end describe
command does not find documentation."
  (pcase racket--describe-here
    (`(,(and path (pred stringp)) . ,_)
     (let ((v (cons path (point))))
       (pcase which
         ('back    (push v racket--describe-stack-back))
         ('forward (push v racket--describe-stack-forward))
         (_        (error "bad value for WHICH %s" which)))))))

(defun racket-describe-doc-link-button (button)
  "Action for racket-doc-link-button."
  (let ((path   (button-get button 'racket-doc-link-path))
        (anchor (button-get button 'racket-doc-link-anchor)))
    (when path
      (racket--describe-maybe-push-here 'back)
      (setq racket--describe-stack-forward nil)
      (racket--describe-fetch-and-show path anchor))))

(defun racket-describe-back ()
  "Go back to the previous topic, like in a web browser."
  (interactive)
  (unless racket--describe-stack-back
    (user-error "No backward history"))
  (racket--describe-maybe-push-here 'forward)
  (pcase-let ((`(,path . ,pos) (pop racket--describe-stack-back)))
    (racket--describe-fetch-and-show path pos)))

(defun racket-describe-forward ()
  "Go forward to the topic from where `racket-describe-back' came."
  (interactive)
  (unless racket--describe-stack-forward
    (user-error "No forward history"))
  (racket--describe-maybe-push-here 'back)
  (pcase-let ((`(,path . ,pos) (pop racket--describe-stack-forward)))
    (racket--describe-fetch-and-show path pos)))

(defun racket-describe-ext-link-button (button)
  "Action for racket-ext-link-button."
  (let ((href (button-get button 'racket-ext-link-href)))
    (racket-browse-url href)))

(defun racket-describe-mode-revert-buffer (_ignore-auto _noconfirm)
  (when-let (page (car racket--describe-here))
    (setq racket--describe-here nil)
    (racket--describe-fetch-and-show page (point))))

(defun racket-describe-browse-external ()
  "Open the current page using the variable `racket-browse-url-function'.

The anchor is the first one at or before point, if any."
  (interactive)
  (when-let (page (car racket--describe-here))
    (if-let (anchor (or (get-text-property (point) 'racket-anchor)
                        (when-let (pos (previous-single-property-change
                                        (point) 'racket-anchor))
                          (or (get-text-property pos 'racket-anchor)
                              (when (< (point-min) pos)
                                (get-text-property (1- pos) 'racket-anchor))))))
        (racket-browse-url (concat page "#" (url-hexify-string anchor)))
      (racket-browse-url page))))

(defun racket--describe-label-for-point ()
  "A bookmark name or org-link description for point.
Use the line containing a racket-anchor, starting with the
current line and looking back."
  (let* ((beg (or (previous-single-property-change (line-end-position)
                                                   'racket-anchor)
                  (point-min)))
         (beg (save-excursion (goto-char beg) (line-beginning-position)))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (text (buffer-substring beg end))
         (_ ;Delete invisible characters, as used for racket-anchor
          (while (when-let (ix (text-property-any 0 (length text)
                                                  'display "" text))
                   (setq text
                         (concat (substring text 0 ix)
                                 (substring text (1+ ix) (length text)))))))
         ;; Strip text properties
         (text (substring-no-properties text))
         ;; Strip leading/trailing space
         (text (save-match-data
                 (string-match (rx bos (* space) (group (*? any)) (* space) eos)
                               text)
                 (match-string 1 text)))
         ;; Replace runs of spaces with just one space
         (_ (save-match-data
              (while (string-match (rx space (+ space)) text)
                (setq text (replace-match " " t t text))))))
    text))

;; bookmark support

(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun racket-describe-bookmark-make-record ()
  "A value for `bookmark-make-record-function'."
  (when (boundp 'bookmark-current-bookmark)
    ;; Don't propose previous name as default
    (setq-local bookmark-current-bookmark nil))
  `(,(racket--describe-label-for-point)
    (filename . ,(car racket--describe-here))
    (position . ,(point))
    (last-modified . ,(current-time))
    (handler . racket-describe-bookmark-jump)))

(defun racket-describe-bookmark-jump (bmk)
  (set-buffer
   (racket--describe-path+anchor (bookmark-prop-get bmk 'filename)
                                 (bookmark-prop-get bmk 'position))))
(put 'racket-describe-bookmark-jump 'bookmark-handler-type "Racket docs")

;; org-link support

(declare-function org-link-types "ext:ol" ())
(declare-function org-link-store-props "ext:ol" (&rest plist))
(declare-function org-link-get-parameter "ext:ol" (type key))

(defconst racket--describe-org-link-type "racket-describe")

(defun racket--describe-add-support-for-org-links ()
  (when (and (fboundp 'org-link-set-parameters)
             (not (member racket--describe-org-link-type (org-link-types))))
    (org-link-set-parameters racket--describe-org-link-type
                             :store #'racket--describe-org-link-store
                             :follow #'racket--describe-org-link-follow)))

(defun racket--describe-org-link-store ()
  (when (derived-mode-p 'racket-describe-mode)
    (org-link-store-props :type racket--describe-org-link-type
                          :link (format "%s:%S"
                                        racket--describe-org-link-type
                                        (cons (car racket--describe-here)
                                              (point)))
                          :description (racket--describe-label-for-point))))

(defun racket--describe-org-link-follow (link)
  (pcase-let ((`(,path . ,anchor) (read link)))
    (racket--describe-path+anchor path anchor)))

;; keymap and major mode

(defvar racket-describe-mode-map
  (let ((map (racket--easy-keymap-define
              `(("<tab>"             ,#'forward-button)
                ("<backtab>"         ,#'backward-button)
                (("l" "b" "C-c C-b") ,#'racket-describe-back)
                (("r" "f" "C-c C-f") ,#'racket-describe-forward)
                (("C-c C-s" "i")     ,#'racket-describe-search)
                ("n"                 ,#'racket-describe-nav-next)
                ("p"                 ,#'racket-describe-nav-prev)
                ("^"                 ,#'racket-describe-nav-up)
                ("C-^"               ,#'racket-describe-nav-top)
                ("x"                 ,#'racket-describe-browse-external)))))
    (define-key map [XF86Back]    'racket-describe-back)
    (define-key map [XF86Forward] 'racket-describe-back)
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for Racket Describe mode.")

(define-derived-mode racket-describe-mode special-mode
  "RacketDescribe"
  "Major mode for viewing Racket documentation.

Many of the default key bindings are similar to `Info-mode', as
listed below.

To see \"On this page\" links, use \\[imenu] for `imenu', or,
when `context-menu-mode' is enabled, right click the mouse.

Supports bookmarks: `bookmark-set'.

Supports org links: `org-store-link', `org-insert-link', and
`org-open-at-point'.

Internal, intra-doc links -- which go to other doc pages in the
same `racket-describe-mode' buffer in Emacs -- are given
`racket-doc-link-face' unless the documentation specifies some
non-default face.

External links -- which are opened using the variable
`racket-browse-url-function', by default in an external web
browser program -- are given `racket-ext-link-face'.

\\{racket-describe-mode-map}"
  (setq show-trailing-whitespace nil)
  (setq-local revert-buffer-function #'racket-describe-mode-revert-buffer)
  (buffer-disable-undo)
  ;; imenu
  (setq-local imenu-create-index-function
              #'racket--describe-imenu-create-index)
  (when (boundp 'imenu-auto-rescan)
    (setq-local imenu-auto-rescan t))
  (when (boundp 'imenu-max-items)
    (setq-local imenu-max-items 999))
  (imenu-add-to-menubar "On this page")
  ;; bookmark
  (when (boundp 'bookmark-make-record-function)
    (setq-local bookmark-make-record-function
                #'racket-describe-bookmark-make-record))
  ;; org-link
  (racket--describe-add-support-for-org-links))

;;; Search local docs

(defun racket-describe-search ()
  "Search installed documentation; view using `racket-describe-mode'."
  (interactive)
  (pcase (racket--describe-search-completing-read)
    (`(,_term ,path ,anchor ,_lib)
     (racket--describe-path+anchor (racket-file-name-back-to-front path)
                                   anchor))))

(defun racket--describe-search-completing-read ()
  "A `completing-read' UX for user to pick doc index item.
Return nil or \(term path anchor lib\)."
  (let* ((affixator (racket--make-affix [16
                                         [16 racket-describe-search-kind]
                                         [32 racket-describe-search-from-libs]
                                         [0  racket-describe-search-lang-fams]]))
         (candidates nil)
         (collection
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (category              . ,racket--identifier-category)
                (display-sort-function . ,#'racket--describe-search-display-sort)
                (affixation-function   . ,affixator)))
             ((eq (car-safe action) 'boundaries) nil)
             (t
              (when (eq action t)
                (setq candidates
                      (racket--describe-search-make-strings
                       (racket--cmd/await nil `(doc-search ,string)))))
              (funcall (cond
                        ((null action) #'try-completion)
                        ((eq action t) #'all-completions)
                        (t             #'test-completion))
                       string
                       candidates
                       predicate)))))
         (predicate
          (lambda (v)
            (apply racket-doc-index-predicate-function
                   (get-text-property 0 'racket-affix v))))
         (prompt "Search Racket documentation: ")
         (require-match t)
         (initial-input (racket--thing-at-point 'symbol t))
         (history 'racket-identifier)
         (default initial-input)
         (history-add-new-input nil)) ;we'll add history below
    (when-let (str (completing-read prompt
                                    collection
                                    predicate
                                    require-match
                                    initial-input
                                    history
                                    default))
      (pcase (racket--describe-search-parse-result str)
        (`(,term ,path ,anchor ,lib)
         (add-to-history 'racket-identifier term) ;just term
         (list term path anchor lib))))))

(defun racket--describe-search-make-strings (items)
  "Make a list of candidate strings from back end ITEMS.

Each string has text properties needed by our affixation and
display-sort functions.

However `completing-read' returns a string stripped of text
properties. :( So we append the path and anchor, tab separated,
as invisible text. Use `racket--describe-search-parse-result' to
extract."
  (mapcar
   (pcase-lambda (`(,term ,sort ,what ,from ,fams ,pkg-sort
                          ,path ,anchor))
     (let* ((term (propertize term
                              'racket-affix (list what from fams)
                              'racket-sort (list (format "%09d" sort)
                                                 (format "%09d" pkg-sort))))
            (lib (substring from 0 (string-match (rx ?,) from)))
            (data (concat "\t" path "\t" anchor "\t" lib))
            (data (propertize data 'display "")))
       (concat term data)))
   items))

(defun racket--describe-search-parse-result (str)
  (when (string-match (rx bos
                          (group-n 1 (+? any)) ?\t ;term
                          (group-n 2 (+? any)) ?\t ;path
                          (group-n 3 (*? any)) ?\t ;anchor
                          (group-n 4 (*? any))     ;lib
                          eos)
                      str)
    (cl-loop for group from 1 to 4
             collect (match-string group str))))

(defun racket--describe-search-display-sort (strs)
  "A value for display-sort-function metadata."
  (cl-flet*
      ((term (s)
         (substring s 0 (string-match (rx ?\t) s)))
       (adjust-fams (fams)
         (pcase fams
           ("Racket" " Racket")
           (v v)))
       (key (v)
         (pcase-let
             ((`(,what ,from ,fams)
               (get-text-property 0 'racket-affix v))
              (`(,sort ,pkg-sort)
               (get-text-property 0 'racket-sort v)))
           (list (term v)
                 (adjust-fams fams)
                 pkg-sort
                 from
                 what
                 sort)))
       (key< (as bs)
         (cl-loop for a in as
                  for b in bs
                  unless (string= a b) return (string< a b)
                  finally return nil)))
    ;; `seq-sort-by' unavailable in Emacs 25, so instead of
    ;; (seq-sort-by #'key #'key< strs) spell it out:
    (seq-sort (lambda (a b)
                (key< (key a)
                      (key b)))
              strs)))

(provide 'racket-describe)

;; racket-describe.el ends here
