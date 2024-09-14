;;; racket-unicode-input-method.el -*- lexical-binding: t; -*-

;; Copyright (c) 2015-2020 by Greg Hendershott
;; Portions Copyright (c) 2024 by Ben Carriel

;; Author: Ben Carriel
;; URL: https://github.com/bkc39/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Following the example of haskell-unicode-input-method.el

(require 'quail)
(require 'cl-lib)

;;;###autoload
(defun racket-custom-unicode-input-method-enable ()
  "Enable the Racket custom Unicode input method.

This function sets up and activates the `racket-custom-unicode` input method.
When called interactively, it prepares the custom Unicode bindings for Racket
programming and sets it as the active input method."
  (interactive)
  (racket-custom-unicode-setup)
  (set-input-method "racket-custom-unicode"))

(quail-define-package
 "racket-custom-unicode"                ;name
 "UTF-8"                                ;language
 "λ"                                    ;title (in mode line)
 t                                      ;guidance
 "Racket Unicode input method."         ;docstring
 nil                                    ;translation-keys
 nil                                    ;forget-last-selection
 nil                                    ;deterministic
 nil                                    ;kbd-translate
 nil                                    ;show-layout
 nil                                    ;create-decode-map
 nil                                    ;maximum-shortest
 nil                                    ;overlay-plist
 nil                                    ;update-translation-function
 nil                                    ;conversion-keys
 t)                                     ;simple

(defun quail-package->translations (qp)
  "Retrieve the translations for a given Quail package.

This function activates the input method specified by QP, builds its decode map,
and returns a list of translations.

Arguments:
  QP -- The name of the Quail package as a string.

Returns:
  A list of cons cells where each car is a key sequence (string) and each cdr is
  the corresponding Unicode character (string).

Example usage:
  (quail-package->translations \"racket-custom-unicode\")
  ;; => ((\"lambda\" . \"λ\") (\"pi\" . \"π\") ...)

Possible exceptions:
  This function raises an error if QP is not a valid Quail package."
  (with-temp-buffer
    (activate-input-method qp)
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defvar racket-custom-unicode-tweaks
  nil
  "A list of functions applied to keys in `racket-custom-unicode`.")

(defvar racket-custom-unicode-user-translations
  nil
  "A list of user-defined bindings the `racket-custom-unicode` input method.

Each entry in the list is a cons cell, where the car is the key
sequence (a string) and the cdr is the corresponding Unicode
character (a string).  These mappings are added to the default Racket
Unicode translations when the input method is activated.")

(defun racket-custom-unicode-bind-key-sequence (keys binding)
  "Binds KEYS to a Unicode character BINDING in `racket-custom-Unicode-input`.

This function updates the `racket-custom-unicode-user-translations` with
the provided key sequence to Unicode character mapping.  If the key
sequence already has a binding, the new binding is added to the existing
list of bindings for that key sequence.

Arguments:
  KEYS -- A string representing the key sequence to be bound.

  BINDING -- A string representing the Unicode character to be bound to
  the key sequence.

Example usage:
  (racket-custom-unicode-bind-key-sequence \"\\\" ⟜\")

  ;; After running the above code, whenever the key sequence \"\\\" is typed,
  ;; the Unicode character ⟜ will be inserted.

This function can be used interactively."
  (interactive
   (list
    (read-string "Enter the key sequence: ")
    (read-string "Enter corresponding unicode: ")))
  (let ((bindings
         (alist-get keys racket-custom-unicode-user-translations nil)))
    (setf (alist-get keys racket-custom-unicode-user-translations)
          (cons binding bindings))))

;;;###autoload
(defun racket-custom-unicode-add-tweaks! (&rest tweaks)
  "Add TWEAKS to `racket-custom-unicode-bindings`.

Each tweak is a function that can modify the key sequences used
in the Racket Unicode input method.

TWEAKS are functions that take a single string argument and return
a modified string.

Arguments:
  TWEAKS -- A list of functions that take a string and return a modified string.

Example usage:
  (racket-custom-unicode-add-tweaks!
   (prefix-with \"rkt-\")
   (remove-suffix \"-temp\"))

Possible exceptions:
  This function itself does not raise exceptions, but the TWEAKS functions
  provided should handle any errors internally."
  (setq racket-custom-unicode-tweaks
        (append racket-custom-unicode-tweaks tweaks)))

(defun prefix-with (prefix)
  "Return a function that takes a string and prepends PREFIX to it."
  (lambda (keys)
    (concat prefix keys)))

(defun suffix-with (suffix)
  "Return a function that takes a string and appends SUFFIX to it."
  (lambda (keys)
    (concat keys suffix)))

(defun remove-suffix (suffix)
  "Return a function that removes the SUFFIX from a STRING, if present.
If the SUFFIX is not found at the end of the STRING or if the
STRING is shorter than the SUFFIX, the function returns the STRING unchanged.

Arguments:
  SUFFIX -- The suffix to be removed from the end of a string.

Returns:
  A function that takes a single argument STRING and returns it
  without the SUFFIX if the SUFFIX is present at the end.

Example usage:
  (let ((strip-er (remove-suffix \"er\")))
    (strip-er \"player\"))
  ;; => \"play\"

  (let ((strip-er (remove-suffix \"ing\")))
    (strip-er \"playing\"))
  ;; => \"play\"

  (let ((strip-er (remove-suffix \"xyz\")))
    (strip-er \"abcdef\"))
  ;; => \"abcdef\""
  (lambda (string)
    (if (and (string-suffix-p suffix string)
             (>= (length string) (length suffix)))
        (substring string 0 (- (length string) (length suffix)))
      string)))

(defun sequence1 (&rest fns)
  (lambda (x)
    (let ((out x))
      (dolist (f fns)
        (setq out (funcall f out)))
      out)))

(defun racket-custom-unicode-apply-tweaks ()
  (let ((tweak
         (apply #'sequence1 racket-custom-unicode-tweaks)))
    (mapcar (lambda (mapping)
              (cons (funcall tweak (car mapping))
                    (cdr mapping)))
            (append (quail-package->translations "racket-unicode")
                    racket-custom-unicode-user-translations))))

(defun racket-custom-unicode-setup ()
  (with-temp-buffer
    (set-input-method "racket-custom-unicode")
    (dolist (tr (racket-custom-unicode-apply-tweaks))
      (quail-defrule (car tr) (cdr tr) "racket-custom-unicode" t))))

(provide 'racket-custom-unicode-input-method)

;;; racket-custom-unicode-input-method.el ends here
