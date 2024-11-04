;;; racket-describe.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
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

(defun racket--do-describe (how repl-session-id str)
  "Get or create a `racket-describe-mode' buffer and display it.

HOW is somewhat complicated, due to this function being
overloaded to handle both showing documentation for an
already-known path and anchor (e.g. from `racket-xp-mode') as
well as seeing if STR is an identifier in a namespace for which
we can find documentation, or least return a description of its
signature and/or type. So:

- When HOW is (cons path anchor) we load/show that documentation,
  and ignore STR. We don't issue a back end command. (Earlier
  versions of Racket Mode used the back end to fetch the HTML or
  shr-dom, but these days we do it all in the front end.)
  REPL-SESSION-ID and STR are unused and may be nil.

- When HOW is \"namespace\" or a stringp pathname, we use that as
  the namespace in which to see if STR is an identifier, using
  the \"describe\" back end command. The command can return a few
  kinds of values; see the implementation below. When HOW is
  \"namespace\" then REPL-SESSION-ID should be
  `racket--repl-session-id'; else may be nil."
  (let ((buf-name (format "*Racket Describe <%s>*"
                          (racket-back-end-name))))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (eq major-mode 'racket-describe-mode)
        (racket-describe-mode))
      (racket--describe-maybe-push-here 'back) ;do before erasing buffer
      (setq racket--describe-stack-forward nil)
      (let ((buffer-read-only nil))
        (erase-buffer))
      ;; shr-insert-document seems to misbehave when buffer has no
      ;; window so do this early.
      (pop-to-buffer (current-buffer))
      (pcase how
        ;; If HOW is the doc path and anchor (the latter can be nil),
        ;; there's no need to issue a back end describe command.
        (`(,(and path (pred stringp)) . ,anchor)
         (racket--describe-insert-dom path
                                      anchor
                                      (racket--scribble-path->shr-dom path)))
        ;; If HOW is a string pathname or 'namspace, then we need to
        ;; use the back end describe command. It returns one of three
        ;; kinds of values.
        ((guard (or (stringp how) (eq how 'namespace)))
         (setq header-line-format
               (propertize (format "Getting information from back end about %s ..." str)
                           'face 'italic))
         (racket--cmd/async
          repl-session-id
          `(describe ,(racket-how-front-to-back how) ,str)
          (lambda (result)
            (pcase result
              ;; STR has documentation at path and anchor. Handle like
              ;; the case where we knew the path and anchor up-front.
              (`(,(and path (pred stringp)) . ,anchor)
               (let ((path (racket-file-name-back-to-front path)))
                 (racket--describe-insert-dom path
                                              anchor
                                              (racket--scribble-path->shr-dom path))))
              ;; STR doesn't have documentation, but it does have a
              ;; signature and/or type, and here is a dom about that
              ;; we can insert.
              (`(shr-dom ,dom)
               (racket--describe-insert-dom nil ;path
                                            str ;anchor
                                            dom))
              ;; STR doesn't seem to be an identifier we can describe.
              (`()
               (racket--describe-insert-dom nil ;path
                                            str ;anchor
                                            (racket--describe-not-found-dom str)))))))
        (_ (error "Bad value for `how`: %s" how))))))

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
  (imenu-add-to-menubar "On this page"))

;;; Search local docs

;; For people who don't want to use a web browser: Search local
;; documentation, and view in a `racket-describe-mode' buffer.

(defvar racket--doc-index (make-hash-table :test 'equal)
  "Hash-table from back end name to association list of doc index info.")

(defun racket--remove-doc-index ()
  "A `racket-stop-back-end-hook' to clean up `racket--doc-index'."
  (when-let (key (racket-back-end-name))
    (remhash key racket--doc-index)))

(add-hook 'racket-stop-back-end-hook #'racket--remove-doc-index)

(defun racket--doc-index-file-name (key)
  (make-directory racket-doc-index-directory t)
  (expand-file-name (concat (racket--file-name-slug
                             (concat "doc-index-" key))
                            ".eld")
                    racket-doc-index-directory))

(defun racket--doc-index-file-write (key data)
  (write-region (format ";; -*- no-byte-compile: t; lexical-binding: nil -*-\n%S"
                        data)
                nil
                (racket--doc-index-file-name key)
                nil
                'no-message))

(defun racket--doc-index-file-read (key)
  (with-temp-buffer
    (ignore-errors
      (insert-file-contents (racket--doc-index-file-name key))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun racket--doc-index ()
  "Get the doc index items.

For the item format, see `racket--doc-index-make-alist'.

Because the doc index is somewhat large and slow to get from the
back end, we use a couple levels of caching: 1. Memory, for the
duration of the Emacs session or back end lifetime, whichever is
shorter. 2. Local file system, between sessions. For this latter
we do want to make sure the file isn't outdated, e.g. user
installed new packages. The back end commands works like an HTTP
request with an If-None-Match header: We supply an \"etag\" value
it previously gave us. If that still matches the response is just
\"not modified\", otherwise a new etag and items."
  (let ((key (racket-back-end-name)))
    (or
     (gethash key racket--doc-index)
     (pcase-let*
         ((`(,old-etag . ,old-items) (racket--doc-index-file-read key))
          (items
           (pcase (with-temp-message "Checking back end doc index..."
                    (racket--cmd/await nil `(doc-index ,old-etag)))
             ('not-modified old-items)
             (`(,etag . ,items)
              (with-temp-message "Doc index changed; updating local file..."
                (racket--doc-index-file-write key (cons etag items)))
              items)))
          (items
           (with-temp-message "Processing doc index items for memory cache..."
             (racket--doc-index-make-alist items))))
       (puthash key items racket--doc-index)
       items))))

(defun racket--doc-index-make-alist (items)
  "Given back end ITEMS make an association list.

A list is a valid collection for completion, where the `car' is
the completion string. For this we use a string with invisible
text to influence sorting, appended to the visible search term.
This is vastly faster than using a :display-sort-function.

Our affixation function gets the propertized string, so we can
tuck the values it needs into a text property.

However the final result from `completing-read' is stripped of
text properties -- which is the only reason we need an
association list, to look up the path and anchor."
  (cl-flet ((wstr (width s)
              (let ((len (length s)))
                (if (<= len width)
                    (concat s
                            (make-string (- width len) 32))
                  (substring s 0 width)))))
    (mapcar
     (pcase-lambda (`(,uid ,term ,sort ,what ,from ,fams ,path ,anchor))
       (let* ((term (propertize term
                                'racket-affix (list what from fams)))
              (sort (let ((fams (wstr 32
                                      (pcase fams
                                        ("Racket" " Racket")
                                        (v v))))
                          (from (wstr 64
                                      (cond
                                       ((string-match-p "^racket/" from)
                                        (concat " 0_" from))
                                       ((string-match-p "^typed/racket/" from)
                                        (concat " 1_" from))
                                       ((string-match-p "^rhombus" from)
                                        (concat " 2_" from))
                                       ((string-equal "" from)
                                        (make-string 16 ?z))
                                       (t from))))
                          (what (wstr 32 what))
                          (sort (format "%09d" sort))
                          (uid  (format "%05d" uid)))
                      ;; We need uid only to guarantee uniqueness, not
                      ;; to influence sorting, so it goes last.
                      (concat fams from what sort uid)))
              (str (concat term (propertize sort 'display ""))))
         (list str path anchor)))
     items)))

(defun racket-describe-search ()
  "Search installed documentation; view using `racket-describe-mode'."
  (interactive)
  (let* ((affixator (racket--make-affix [16
                                         [16 racket-describe-search-kind]
                                         [32 racket-describe-search-from-libs]
                                         [0  racket-describe-search-lang-fams]]))
         (collection (racket--completion-table
                      (racket--doc-index)
                      `((category . ,racket--identifier-category)
                        (affixation-function . ,affixator))))
         (predicate (lambda (v)
                      (apply racket-doc-index-predicate-function
                             (get-text-property 0 'racket-affix (car v)))))
         (require-match t)
         (initial-input (racket--thing-at-point 'symbol t)))
    (when-let (str (completing-read "Describe: "
                                    collection
                                    predicate
                                    require-match
                                    initial-input))
      (pcase (assoc str (racket--doc-index))
        (`(,_str ,path ,anchor)
         (racket--do-describe (cons (racket-file-name-back-to-front path)
                                    anchor)
                              nil
                              (substring-no-properties str)))))))

(provide 'racket-describe)

;; racket-describe.el ends here
