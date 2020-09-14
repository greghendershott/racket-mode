;;; racket-browse-url.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020 by Greg Hendershott.
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

(require 'racket-custom)

(defun racket-browse-url (url &rest args)
  (when url
    (apply racket-browse-url-function url args)))

(defun racket-browse-url-using-temporary-file (url &rest _args)
  "Browse a URL via a temporary HTML file using a meta redirect.

A suitable value for the variable `racket-browse-url-function'.

On some operating systems, the default handling for file URLs
will ignore anchors -- the portion of the URL after the #
character. But Racket documentation URLs depend on these to jump
to a location within a page. This function attempts to work
around that problem by using a temporary HTML file with a meta
redirect as a \"trampoline\".

You might think that Emacs' `browse-url' would handle this
portably, but as of Emacs 26 it does not. Although a user may
customize the variable `browse-url-browser-function' to a
specific technique that works, the default doesn't necessarily
work for anchors on for instance macOS or Windows.

For Racket Mode, we do want Racket documentation to \"just work\"
-- and because it does not do so on 2/3 operating systems, we
reluctantly handle this. Note that a user can customize the
variable `racket-browse-url-function' to `browse-url' -- which
indeed is our default on *nix -- or to
`browse-url-browser-function' in case they have customized that,
or indeed to whatever they want. So this is an attempt to work
better by default, while still supporting users who want to
customize."
  (let* ((url  (if (string-match ".*://" url) url (concat "file://" url)))
         (file (make-temp-file "racket-browse-url-" nil ".html"))
         (html (format "<html><head><meta http-equiv=\"refresh\" content=\"0;url=%s\" /></head></html>" url)))
    (write-region html nil file nil 'no-wrote-file-message)
    (browse-url file)))

(provide 'racket-browse-url)

;; racket-browse-url.el ends here
