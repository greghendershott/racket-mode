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

(defmacro translation-mappings (&rest pairs)
  "Create a list of translation pairs.

PAIRS are cons cells where the car is a string representing a key and
the cdr is a vector of characters to which it should be mapped."
  `(list ,@(mapcar (lambda (pair) `(list ,@pair)) pairs)))

(defvar racket-custom-unicode-tweaks
  nil
  "A list of functions that tweak the keys in the `racket-custom-unicode-bindings`.")

(defvar racket-custom-unicode-bindings
  (translation-mappings
   ;; Typed Racket
   ("All"          ["∀"])
   ("Union"        ["U"])
   ("Intersection" ["∩"])

   ;; Redex
   ("test-->>E"    ["test-->>∃"])

   ;; Turnstile
   ("vdash"        ["⊢"])
   ("gg"           ["≫"])
   ("rightarrow"   ["→"])
   ("Rightarrow"   ["⇒"])
   ("Leftarrow"    ["⇐"])
   ("succ"         ["≻"])

   ;; Other type rule symbols
   ;; (added: common math chars currently unsupported)
   ("times"              ["×"])
   ("Uparrow"            ["⇑"])
   ("Downarrow"          ["⇓"])
   ("Leftrightarrow"     ["⇔"])
   ("rightarrow"         ["→"])
   ("leftarrow"          ["←"])
   ("Rightarrow"         ["⇒"])
   ("Leftarrow"          ["⇐"])
   ("nwarrow"            ["↖"])
   ("nearrow"            ["↗"])
   ("uparrow"            ["↑"])
   ("downarrow"          ["↓"])
   ("searrow"            ["↘"])
   ("swarrow"            ["↙"])
   ("leftrightarrow"     ["↔"])
   ("updownarrow"        ["⇕"])
   ("aleph"              ["א"])
   ("emptyset"           ["∅"])
   ("nabla"              ["∇"])
   ("surd"               ["√"])
   ("negation"           ["¬"])
   ("infinity"           ["∞"])
   ("prod"               ["∏"])
   ("coprod"             ["∐"])
   ("integrate"          ["∫"])
   ("oint"               ["∮"])
   ("vee"                ["∨"])
   ("wedge"              ["∧"])
   ("follows"            ["∘"])
   ("setin"              ["∈"])
   ("ni"                 ["∋"])
   ("notin"              ["∉"])
   ("sqsubset"           ["⊏"])
   ("sqsupset"           ["⊐"])
   ("sqsubseteq"         ["⊑"])
   ("sqsupseteq"         ["⊒"])
   ("subset"             ["⊂"])
   ("superset"           ["⊃"])
   ("subseteq"           ["⊆"])
   ("supseteq"           ["⊇"])
   ("approx"             ["≈"])
   ("cong"               ["≌"])
   ("neq"                ["≠"])
   ("sqcap"              ["⊓"])
   ("sqcup"              ["⊔"])
   ("models"             ["⊧"])

   ;; Greek letters
   ("alpha"        ["α"])
   ("Alpha"        ["Α"])
   ("beta"         ["β"])
   ("Beta"         ["Β"])
   ("gamma"        ["γ"])
   ("Gamma"        ["Γ"])
   ("delta"        ["δ"])
   ("Delta"        ["Δ"])
   ("epsilon"      ["ε"])
   ("Epsilon"      ["Ε"])
   ("zeta"         ["ζ"])
   ("Zeta"         ["Ζ"])
   ("eta"          ["η"])
   ("Eta"          ["Η"])
   ("theta"        ["θ"])
   ("Theta"        ["Θ"])
   ("iota"         ["ι"])
   ("Iota"         ["Ι"])
   ("kappa"        ["κ"])
   ("Kappa"        ["Κ"])
   ("lambda"       ["λ"])
   ("Lambda"       ["Λ"])
   ("lamda"        ["λ"])
   ("Lamda"        ["Λ"])
   ("mu"           ["μ"])
   ("Mu"           ["Μ"])
   ("nu"           ["ν"])
   ("Nu"           ["Ν"])
   ("xi"           ["ξ"])
   ("Xi"           ["Ξ"])
   ("omicron"      ["ο"])
   ("Omicron"      ["Ο"])
   ("pi"           ["π"])
   ("Pi"           ["Π"])
   ("rho"          ["ρ"])
   ("Rho"          ["Ρ"])
   ("sigma"        ["σ"])
   ("Sigma"        ["Σ"])
   ("tau"          ["τ"])
   ("Tau"          ["Τ"])
   ("upsilon"      ["υ"])
   ("Upsilon"      ["Υ"])
   ("phi"          ["φ"])
   ("Phi"          ["Φ"])
   ("chi"          ["χ"])
   ("Chi"          ["Χ"])
   ("psi"          ["ψ"])
   ("Psi"          ["Ψ"])
   ("omega"        ["ω"])
   ("Omega"        ["Ω"])
   ("digamma"      ["ϝ"])
   ("Digamma"      ["Ϝ"])
   ("san"          ["ϻ"])
   ("San"          ["Ϻ"])
   ("qoppa"        ["ϙ"])
   ("Qoppa"        ["Ϙ"])
   ("sampi"        ["ϡ"])
   ("Sampi"        ["Ϡ"])
   ("stigma"       ["ϛ"])
   ("Stigma"       ["Ϛ"])
   ("heta"         ["ͱ"])
   ("Heta"         ["Ͱ"])
   ("sho"          ["ϸ"])
   ("Sho"          ["Ϸ"])

   ;; Double-struck letters
   ("|A|"           ["𝔸"])
   ("|B|"           ["𝔹"])
   ("|C|"           ["ℂ"])
   ("|D|"           ["𝔻"])
   ("|E|"           ["𝔼"])
   ("|F|"           ["𝔽"])
   ("|G|"           ["𝔾"])
   ("|H|"           ["ℍ"])
   ("|I|"           ["𝕀"])
   ("|J|"           ["𝕁"])
   ("|K|"           ["𝕂"])
   ("|L|"           ["𝕃"])
   ("|M|"           ["𝕄"])
   ("|N|"           ["ℕ"])
   ("|O|"           ["𝕆"])
   ("|P|"           ["ℙ"])
   ("|Q|"           ["ℚ"])
   ("|R|"           ["ℝ"])
   ("|S|"           ["𝕊"])
   ("|T|"           ["𝕋"])
   ("|U|"           ["𝕌"])
   ("|V|"           ["𝕍"])
   ("|W|"           ["𝕎"])
   ("|X|"           ["𝕏"])
   ("|Y|"           ["𝕐"])
   ("|Z|"           ["ℤ"])
   ("|gamma|"       ["ℽ"])
   ("|Gamma|"       ["ℾ"])
   ("|pi|"          ["ℼ"])
   ("|Pi|"          ["ℿ"])

   ;; Quantifiers
   ("forall"       ["∀"])
   ("exists"       ["∃"])

   ;; Numeric subscripts
   ("_0"           ["₀"])
   ("_1"           ["₁"])
   ("_2"           ["₂"])
   ("_3"           ["₃"])
   ("_4"           ["₄"])
   ("_5"           ["₅"])
   ("_6"           ["₆"])
   ("_7"           ["₇"])
   ("_8"           ["₈"])
   ("_9"           ["₉"])

   ;; Numeric superscripts
   ("^0"           ["⁰"])
   ("^1"           ["¹"])
   ("^2"           ["²"])
   ("^3"           ["³"])
   ("^4"           ["⁴"])
   ("^5"           ["⁵"])
   ("^6"           ["⁶"])
   ("^7"           ["⁷"])
   ("^8"           ["⁸"])
   ("^9"           ["⁹"])))

(defun racket-custom-unicode-bind-key-sequence (keys binding)
  (interactive
   (list
    (read-string "Enter the key sequence: ")
    (read-string "Enter corresponding unicode: ")))
  (let ((bindings
         (alist-get keys racket-custom-unicode-bindings nil)))
    (setf (alist-get keys racket-custom-unicode-bindings)
          (cons binding bindings))))

(defun racket-custom-unicode-add-tweaks! (&rest tweaks)
  (append racket-custom-unicode-tweaks tweaks))

(defun prefix-with (prefix)
  "Return a function that takes a string and prepends PREFIX to it."
  (lambda (str)
    (concat prefix str)))

(defun suffix-with (suffix)
  "Return a function that takes a string and appends SUFFIX to it."
  (lambda (key)
    (concat key suffix)))

(defun racket-custom-unicode-apply-tweaks ()
  (let ((tweak
         (apply #'sequence1 racket-custom-unicode-tweaks)))
    (mapcar (lambda (mapping)
              (cons (funcall tweak (car mapping))
                    (cdr mapping)))
            racket-custom-unicode-bindings)))

(defun racket-custom-unicode-setup ()
  (with-temp-buffer
    (racket-custom-unicode-input-method-enable)
    (dolist (tr (racket-custom-unicode-apply-tweaks))
      (quail-defrule (car tr) (cdr tr) "racket-custom-unicode" t))))

(provide 'racket-custom-unicode-input-method)

;;; racket-custom-unicode-input-method.el ends here
