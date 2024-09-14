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
  (with-temp-buffer
    (activate-input-method qp)
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defvar racket-custom-unicode-tweaks
  nil
  "A list of functions that tweak the keys in the `racket-custom-unicode-bindings`.")

(defvar racket-custom-unicode-user-translations
  nil)

(defun racket-custom-unicode-bind-key-sequence (keys binding)
  (interactive
   (list
    (read-string "Enter the key sequence: ")
    (read-string "Enter corresponding unicode: ")))
  (let ((bindings
         (alist-get keys racket-custom-unicode-user-translations nil)))
    (setf (alist-get keys racket-custom-unicode-user-translations)
          (cons binding bindings))))

(defun racket-custom-unicode-add-tweaks! (&rest tweaks)
  (append racket-custom-unicode-tweaks tweaks))

(defun prefix-with (suffix)
  "Return a function that takes a string and prepends PREFIX to it."
  (lambda (keys)
    (concat keys suffix)))

(defun suffix-with (suffix)
  "Return a function that takes a string and appends SUFFIX to it."
  (lambda (keys)
    (concat keys suffix)))

(defun remove-suffix (suffix)
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
    (racket-custom-unicode-input-method-enable)
    (dolist (tr (racket-custom-unicode-apply-tweaks))
      (quail-defrule (car tr) (cdr tr) "racket-custom-unicode" t))))

(provide 'racket-custom-unicode-input-method)

;;; racket-custom-unicode-input-method.el ends here
