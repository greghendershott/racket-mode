;;; racket-ppss.el --- Major mode for Racket language.

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

;; Note: These doc strings are from the Parser State info topic, as of
;; Emacs 25.1.

(defun racket--ppss-paren-depth (xs)
  "The depth in parentheses, counting from 0.
*Warning:* this can be negative if there are more close parens
than open parens between the parser’s starting point and end
point."
  (elt xs 0))

(defun racket--ppss-containing-sexp (xs)
  "The character position of the start of the innermost parenthetical
grouping containing the stopping point; ‘nil’ if none."
  (elt xs 1))

(defun racket--ppss-last-sexp (xs)
  "The character position of the start of the last complete
subexpression terminated; ‘nil’ if none.
Valid only for `parse-partial-sexp' -- NOT `syntax-ppss'."
  (elt xs 2))

(defun racket--ppss-string-p (xs)
  "Non-‘nil’ if inside a string.
More precisely, this is the character that will terminate the
string, or ‘t’ if a generic string delimiter character should
terminate it."
  (elt xs 3))

(defun racket--ppss-comment-p (xs)
  "‘t’ if inside a non-nestable comment (of any comment style;
*note Syntax Flags::); or the comment nesting level if inside a
comment that can be nested."
  (elt xs 4))

(defun racket--ppss-quote-p (xs)
  "‘t’ if the end point is just after a quote character."
  (elt xs 5))

(defun racket--ppss-min-paren-depth (xs)
  "The minimum parenthesis depth encountered during this scan.
Valid only for `parse-partial-sexp' -- NOT `syntax-ppss'."
  (elt xs 6))

(defun racket--ppss-comment-type (xs)
  "What kind of comment is active: ‘nil’ if not in a comment or
in a comment of style ‘a’; 1 for a comment of style ‘b’; 2 for a
comment of style ‘c’; and ‘syntax-table’ for a comment that
should be ended by a generic comment delimiter character."
  (elt xs 7))

(defun racket--ppss-string/comment-start (xs)
  "The string or comment start position.
While inside a comment, this is the position where the comment
began; while inside a string, this is the position where the
string began. When outside of strings and comments, this element
is ‘nil’."
  (elt xs 8))

(provide 'racket-ppss)

;; racket-ppss.el ends here
