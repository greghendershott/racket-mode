;;; racket-browse-url.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-custom)
(require 'racket-cmd)
(require 'racket-back-end)

(defun racket-browse-url (url &rest args)
  (when url
    (apply racket-browse-url-function url args)))

(defun racket-browse-file-url (path anchor)
  (when (or (file-remote-p path)
            (not (racket--back-end-local-p)))
    (user-error "Cannot use web browser to browse remote documentation; instead use `racket-describe'"))
  (racket-browse-url (concat "file://" path "#" anchor)))

(defun racket-browse-url-using-temporary-file (url &rest _args)
  "Browse a URL via a temporary HTML file using a meta redirect.

A suitable value for the variable `racket-browse-url-function'.

Racket documentation URLs depend on anchors -- the portion of the
URL after the # character -- to jump to a location within a page.
Unfortunately on some operating systems and/or versions of Emacs,
the default handling for browsing file URLs ignores anchors. This
function attempts to avoid the problem by using a temporary HTML
file with a meta redirect as a \"trampoline\".

Although the intent is to provide a default that \"just works\",
you do not need to use this. You can customize the variable
`racket-browse-url-function' instead to be `browse-url', or
`browse-url-browser-function' in case have have customized that,
or indeed whatever you want."
  (let* ((url  (if (string-match-p ".*://" url) url (concat "file://" url)))
         (file (make-temp-file "racket-browse-url-" nil ".html"))
         (file-uri (concat "file://" file))
         (html (format "<html><head><meta http-equiv=\"refresh\" content=\"0;url=%s\" /></head></html>" url)))
    (write-region html nil file nil 'no-wrote-file-message)
    (browse-url file-uri)))

(provide 'racket-browse-url)

;; racket-browse-url.el ends here
