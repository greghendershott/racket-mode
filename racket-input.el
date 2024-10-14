;;; racket-input.el -*- lexical-binding: t; -*-

;; Copyright (c) 2024 by Greg Hendershott
;; See PROVENANCE note below

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; PROVENANCE: This is a copy and modification of the MIT licensed
;; <https://github.com/emacsmirror/agda2-mode/agda-input.el>. Most of
;; the modifications just change "agda-" name prefixes to "racket-".
;;
;; On the one hand, most of this code is applicable to defining and
;; customizing any input method. Ideally, we'd like it to exist as a
;; distinct, "generic" package, for use by any input method.
;;
;; On the other hand, it doesn't exist in that form. Even if we wanted
;; to advise users to install the agda2-mode package (i.e. we use it
;; as a "library"), it ships via Stack/Cabal -- not via any Emacs Lisp
;; package repo. That's not a reasonable way to ask a typical Emacs or
;; Racket user to install it.
;;
;; Finally, I would be happy to copy-paste this exactly, using the
;; "agda-" prefixed names. But if an Agda user happens to have it
;; installed the official way, that would conflict.
;;
;; TL;DR: Although doing a copy and rename is a bad way to reuse this
;; code, it seems the least worst way. :(

;;; Original commentary:

;; A highly customisable input method which can inherit from other
;; Quail input methods. By default the input method is geared towards
;; the input of mathematical and other symbols in Agda programs.
;;
;; Use M-x customize-group agda-input to customise this input method.
;; Note that the functions defined under "Functions used to tweak
;; translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the
;; ones added specifically for this one.
;;
;; Use agda-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

(require 'quail)
(require 'cl-lib)
;; Quail is quite stateful, so be careful when editing this code.  Note
;; that with-temp-buffer is used below whenever buffer-local state is
;; modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun racket-input-concat-map (f xs)
  "Concat (map F XS)."
  (apply 'append (mapcar f xs)))

(defun racket-input-to-string-list (s)
  "Convert a string S to a list of one-character strings, after
removing all space and newline characters."
  (racket-input-concat-map
   (lambda (c) (if (member c (string-to-list " \n"))
              nil
            (list (string c))))
   (string-to-list s)))

(defun racket-input-character-range (from to)
  "A string consisting of the characters from FROM to TO."
  (let (seq)
    (dotimes (i (1+ (- to from)))
      (setq seq (cons (+ from i) seq)))
    (concat (nreverse seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs

(defun racket-input-compose (f g)
  "λ x -> concatMap F (G x)"
    (lambda (x) (racket-input-concat-map f (funcall g x))))

(defun racket-input-or (f g)
  "λ x -> F x ++ G x"
    (lambda (x) (append (funcall f x) (funcall g x))))

(defun racket-input-nonempty ()
  "Only keep pairs with a non-empty first component."
  (lambda (x) (if (> (length (car x)) 0) (list x))))

(defun racket-input-prepend (prefix)
  "Prepend PREFIX to all key sequences."
    (lambda (x) `((,(concat prefix (car x)) . ,(cdr x)))))

(defun racket-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
    (lambda (x)
      (if (equal (substring (car x) 0 (length prefix)) prefix)
          (list x))))

(defun racket-input-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX."
    (lambda (x)
      (if (equal (substring (car x)
                            (- (length (car x)) (length suffix)))
                 suffix)
          (list x))))

(defun racket-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
    (lambda (x) (unless (member (car x) ss) (list x))))

(defun racket-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
    (lambda (x) `((,(substring (car x) n) . ,(cdr x)))))

(defun racket-input-drop-end (n)
  "Drop N characters from the end of each key sequence."
    (lambda (x)
      `((,(substring (car x) 0 (- (length (car x)) n)) .
         ,(cdr x)))))

(defun racket-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (racket-input-compose
   (racket-input-drop-beginning (length prefix))
   (racket-input-prefix prefix)))

(defun racket-input-drop-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX.
This suffix is dropped."
    (racket-input-compose
     (racket-input-drop-end (length suffix))
     (racket-input-suffix suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is 'racket-input-incorporate-changed-setting so
;; that the input method gets updated immediately when users customize
;; it. However, the setup functions cannot be run before all variables
;; have been defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not
;; performed until racket-input-setup is called at the end of this
;; file.

(defgroup racket-input nil
  "The Racket input method.
After tweaking these settings you may want to inspect the resulting
translations using `racket-input-show-translations'."
  :group 'racket)

(defcustom racket-input-tweak-all
  '(racket-input-compose
    (racket-input-prepend "\\")
    (racket-input-nonempty))
  "An expression yielding a function which can be used to tweak
all translations before they are included in the input method.
The resulting function (if non-nil) is applied to every
\(KEY-SEQUENCE . TRANSLATION) pair and should return a list of such
pairs. (Note that the translations can be anything accepted by
`quail-defrule'.)

If you change this setting manually (without using the
customization buffer) you need to call `racket-input-setup' in
order for the change to take effect."
  :set 'racket-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type 'sexp)

(defcustom racket-input-inherit
  nil
  "A list of Quail input methods whose translations should be
inherited by the Racket input method (with the exception of
translations corresponding to ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same kind as
`racket-input-tweak-all' which is used to tweak the translation
pairs of the input method.

The inherited translation pairs are added last, after
`racket-input-user-translations' and `racket-input-translations'.

For example:

#+BEGIN_SRC elisp
    ((\"TeX\" . (racket-input-compose
                 (racket-input-drop \\='(\"geq\" \"leq\" \"bullet\" \"qed\" \"par\"))
                 (racket-input-or
                  (racket-input-drop-prefix \"\\\\\")
                  (racket-input-or
                   (racket-input-compose
                    (racket-input-drop \\='(\"^l\" \"^o\" \"^r\" \"^v\"))
                    (racket-input-prefix \"^\"))
                   (racket-input-prefix \"_\"))))))
#+END_SRC

If you change this setting manually (without using the
customization buffer) you need to call `racket-input-setup' in
order for the change to take effect."
  :set 'racket-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom racket-input-translations
  '(;; Typed Racket
    ("All"          . ("∀"))
    ("Union"        . ("U"))
    ("Intersection" . ("∩"))
    ;; Redex
    ("test-->>E"    . ("test-->>∃"))
    ;; Turnstile
    ("vdash"        . ("⊢"))
    ("gg"           . ("≫"))
    ("rightarrow"   . ("→"))
    ("Rightarrow"   . ("⇒"))
    ("Leftarrow"    . ("⇐"))
    ("succ"         . ("≻"))
    ;; Other type rule symbols
    ("times"              . ("×"))
    ("Uparrow"            . ("⇑"))
    ("Downarrow"          . ("⇓"))
    ("Leftrightarrow"     . ("⇔"))
    ("rightarrow"         . ("→"))
    ("leftarrow"          . ("←"))
    ("Rightarrow"         . ("⇒"))
    ("Leftarrow"          . ("⇐"))
    ("nwarrow"            . ("↖"))
    ("nearrow"            . ("↗"))
    ("uparrow"            . ("↑"))
    ("downarrow"          . ("↓"))
    ("searrow"            . ("↘"))
    ("swarrow"            . ("↙"))
    ("leftrightarrow"     . ("↔"))
    ("updownarrow"        . ("⇕"))
    ("aleph"              . ("א"))
    ("emptyset"           . ("∅"))
    ("nabla"              . ("∇"))
    ("surd"               . ("√"))
    ("negation"           . ("¬"))
    ("infinity"           . ("∞"))
    ("prod"               . ("∏"))
    ("coprod"              . ("∐"))
    ("integrate"          . ("∫"))
    ("Oint"               . ("∮"))
    ("vee"                . ("∨"))
    ("wedge"              . ("∧"))
    ("follows"            . ("∘"))
    ("setin"              . ("∈"))
    ("ni"                 . ("∋"))
    ("notin"              . ("∉"))
    ("sqsubset"           . ("⊏"))
    ("sqsupset"           . ("⊐"))
    ("sqsubseteq"         . ("⊑"))
    ("sqsupseteq"         . ("⊒"))
    ("subset"             . ("⊂"))
    ("superset"           . ("⊃"))
    ("subseteq"           . ("⊆"))
    ("supseteq"           . ("⊇"))
    ("approx"             . ("≈"))
    ("cong"               . ("≌"))
    ("neq"                . ("≠"))
    ("sqcap"              . ("⊓"))
    ("sqcup"              . ("⊔"))
    ("models"             . ("⊧"))
    ;; Greek letters
    ("alpha"        . ("α"))
    ("Alpha"        . ("Α"))
    ("beta"         . ("β"))
    ("Beta"         . ("Β"))
    ("gamma"        . ("γ"))
    ("Gamma"        . ("Γ"))
    ("delta"        . ("δ"))
    ("Delta"        . ("Δ"))
    ("epsilon"      . ("ε"))
    ("Epsilon"      . ("Ε"))
    ("zeta"         . ("ζ"))
    ("Zeta"         . ("Ζ"))
    ("eta"          . ("η"))
    ("Eta"          . ("Η"))
    ("theta"        . ("θ"))
    ("Theta"        . ("Θ"))
    ("iota"         . ("ι"))
    ("Iota"         . ("Ι"))
    ("kappa"        . ("κ"))
    ("Kappa"        . ("Κ"))
    ("lambda"       . ("λ"))
    ("Lambda"       . ("Λ"))
    ("lamda"        . ("λ"))
    ("Lamda"        . ("Λ"))
    ("mu"           . ("μ"))
    ("Mu"           . ("Μ"))
    ("nu"           . ("ν"))
    ("Nu"           . ("Ν"))
    ("xi"           . ("ξ"))
    ("Xi"           . ("Ξ"))
    ("omicron"      . ("ο"))
    ("Omicron"      . ("Ο"))
    ("pi"           . ("π"))
    ("Pi"           . ("Π"))
    ("rho"          . ("ρ"))
    ("Rho"          . ("Ρ"))
    ("sigma"        . ("σ"))
    ("Sigma"        . ("Σ"))
    ("tau"          . ("τ"))
    ("Tau"          . ("Τ"))
    ("upsilon"      . ("υ"))
    ("Upsilon"      . ("Υ"))
    ("phi"          . ("φ"))
    ("Phi"          . ("Φ"))
    ("chi"          . ("χ"))
    ("Chi"          . ("Χ"))
    ("psi"          . ("ψ"))
    ("Psi"          . ("Ψ"))
    ("omega"        . ("ω"))
    ("Omega"        . ("Ω"))
    ("digamma"      . ("ϝ"))
    ("Digamma"      . ("Ϝ"))
    ("san"          . ("ϻ"))
    ("San"          . ("Ϻ"))
    ("qoppa"        . ("ϙ"))
    ("Qoppa"        . ("Ϙ"))
    ("sampi"        . ("ϡ"))
    ("Sampi"        . ("Ϡ"))
    ("stigma"       . ("ϛ"))
    ("Stigma"       . ("Ϛ"))
    ("heta"         . ("ͱ"))
    ("Heta"         . ("Ͱ"))
    ("sho"          . ("ϸ"))
    ("Sho"          . ("Ϸ"))
    ;; Double-struck letters
    ("|A|"          . ("𝔸"))
    ("|B|"          . ("𝔹"))
    ("|C|"          . ("ℂ"))
    ("|D|"          . ("𝔻"))
    ("|E|"          . ("𝔼"))
    ("|F|"          . ("𝔽"))
    ("|G|"          . ("𝔾"))
    ("|H|"          . ("ℍ"))
    ("|I|"          . ("𝕀"))
    ("|J|"          . ("𝕁"))
    ("|K|"          . ("𝕂"))
    ("|L|"          . ("𝕃"))
    ("|M|"          . ("𝕄"))
    ("|N|"          . ("ℕ"))
    ("|O|"          . ("𝕆"))
    ("|P|"          . ("ℙ"))
    ("|Q|"          . ("ℚ"))
    ("|R|"          . ("ℝ"))
    ("|S|"          . ("𝕊"))
    ("|T|"          . ("𝕋"))
    ("|U|"          . ("𝕌"))
    ("|V|"          . ("𝕍"))
    ("|W|"          . ("𝕎"))
    ("|X|"          . ("𝕏"))
    ("|Y|"          . ("𝕐"))
    ("|Z|"          . ("ℤ"))
    ("|gamma|"      . ("ℽ"))
    ("|Gamma|"      . ("ℾ"))
    ("|pi|"         . ("ℼ"))
    ("|Pi|"         . ("ℿ"))
    ;; Quantifiers
    ("forall"       . ("∀"))
    ("exists"       . ("∃"))
    ;; Numeric subscripts
    ("_0"           . ("₀"))
    ("_1"           . ("₁"))
    ("_2"           . ("₂"))
    ("_3"           . ("₃"))
    ("_4"           . ("₄"))
    ("_5"           . ("₅"))
    ("_6"           . ("₆"))
    ("_7"           . ("₇"))
    ("_8"           . ("₈"))
    ("_9"           . ("₉"))
    ;; Numeric superscripts
    ("^0"           . ("⁰"))
    ("^1"           . ("¹"))
    ("^2"           . ("²"))
    ("^3"           . ("³"))
    ("^4"           . ("⁴"))
    ("^5"           . ("⁵"))
    ("^6"           . ("⁶"))
    ("^7"           . ("⁷"))
    ("^8"           . ("⁸"))
    ("^9"           . ("⁹")))
  "A list of translations specific to the Racket input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

Note that if you customize this setting you will not
automatically benefit (or suffer) from modifications to its
default value when the library is updated.  If you just want to
add some bindings it is probably a better idea to customize
`racket-input-user-translations'.

These translation pairs are included after those in
`racket-input-user-translations', but before the ones inherited
from other input methods (see `racket-input-inherit').

If you change this setting manually (without using the
customization buffer) you need to call `racket-input-setup' in
order for the change to take effect."
  :set 'racket-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

(defcustom racket-input-user-translations nil
  "Like `racket-input-translations', but more suitable for user
customizations since by default it is empty.

These translation pairs are included first, before those in
`racket-input-translations' and the ones inherited from other input
methods."
  :set 'racket-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun racket-input-get-translations (qp)
  "Return a list containing all translations from the Quail
package QP (except for those corresponding to ASCII).
Each pair in the list has the form (KEY-SEQUENCE . TRANSLATION)."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defun racket-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "Racket")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (racket-input-get-translations qp)))))))

(defun racket-input-add-translations (trans)
  "Add the given translations TRANS to the Racket input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION). The
translations are appended to the current translations."
  (with-temp-buffer
    (dolist (tr (racket-input-concat-map (eval racket-input-tweak-all) trans))
      (quail-defrule (car tr) (cdr tr) "Racket" t))))

(defun racket-input-inherit-package (qp &optional fun)
  "Let the Racket input method inherit the translations from the
Quail package QP (except for those corresponding to ASCII).

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (racket-input-get-translations qp)))
    (racket-input-add-translations
     (if fun (racket-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defun racket-input-setup ()
  "Set up the Racket input method based on the customisable
variables and underlying input methods."

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package
     "Racket"                           ;name
     "UTF-8"                            ;language
     "∏"                                ;title (in mode line)
     t                                  ;guidance
     "Racket input method."             ;docstring
     nil                                ;translation-keys
     nil                                ;forget-last-selection
     nil                                ;deterministic
     nil                                ;kbd-translate
     nil                                ;show-layout
     nil                                ;create-decode-map
     nil                                ;maximum-shortest
     nil                                ;overlay-plist
     nil                                ;update-translation-function
     nil                                ;conversion-keys
     t))                                ;simple

  (racket-input-add-translations
   (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
           (append racket-input-user-translations
                   racket-input-translations)))
  (dolist (def racket-input-inherit)
    (racket-input-inherit-package (car def)
                                  (eval (cdr def)))))

(defun racket-input-incorporate-changed-setting (sym val)
  "Update the Racket input method based on the customisable
variables and underlying input methods.
Suitable for use in the :set field of `defcustom'."
  (set-default sym val)
  (racket-input-setup))

;; Set up the input method.

(racket-input-setup)

;;; Convenience minor mode

(define-minor-mode racket-input-mode
  "A minor mode convenience to enable the racket input method.

The racket input method lets you easily type various Unicode
symbols that might be useful when writing Racket code.

To automatically enable the racket-unicode input method in
racket-mode and racket-repl-mode buffers, put the following code
in your Emacs init file:

#+BEGIN_SRC elisp
    (add-hook \\='racket-mode-hook #\\='racket-input-mode)
    (add-hook \\='racket-repl-mode-hook #\\='racket-input-mode)
#+END_SRC

You may use the standard Emacs key C-\\ to toggle the current
input method.

When the racket input method is active, you can for example type
\"\\All\" and it is immediately replaced with \"∀\". A few other
examples:

| \\omega     | ω                        |
| \\x_1       | x₁                       |
| \\x^1       | x¹                       |
| \\A         | 𝔸                        |
| \\test-->>E | test-->>∃ (racket/redex) |
| \\vdash     | ⊢                        |

Use \"M-x describe-input-method racket\" to see a table of
all key sequences.

Use \"M-x customize-group racket-input\" to customize the input
method.

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil."
  :lighter ""
  (if racket-input-mode
      (set-input-method "Racket")
    (when (equal current-input-method "Racket")
      (deactivate-input-method))))

(define-obsolete-function-alias
  'racket-unicode-input-method-enable
  #'racket-input-mode
  "2024-10-15")

(provide 'racket-input)

;;; racket-input.el ends here
