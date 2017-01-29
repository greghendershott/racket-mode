;;; racket-unicode-input-method.el --- Racket Unicode helper functions

;; Copyright (c) 2015-2016 by Greg Hendershott
;; Portions Copyright (c) 2010-2011 by Roel van Dijk

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

;; Following the example of haskell-unicode-input-method.el

(require 'quail)

;;;###autoload
(defun racket-unicode-input-method-enable ()
  "Set input method to `racket-unicode`.

The `racket-unicode` input method lets you easily type various
Unicode symbols that might be useful when writing Racket
code.

To automatically enable the `racket-unicode` input method in
`racket-mode` buffers use `M-x customize-variable <RET>
racket-mode-hook` or put the following code in your Emacs init
file:

    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

Likewise for `racket-repl-mode` buffers:

    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

To temporarily enable this input method for a single buffer you
can use `M-x racket-unicode-input-method-enable`.

Use `C-\\` to toggle the input method.

When the `racket-unicode` input method is active, you can for
example type `All` and it is immediately replaced with `∀`. A few
other examples:

    omega     ω
    x_1       x₁
    x^1       x¹
    |A|       𝔸
    test-->>E test-->>∃ (racket/redex)

To see a table of all key sequences use `M-x
describe-input-method <RET> racket-unicode`.

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil via
`M-x customize-variable`."
  (interactive)
  (set-input-method "racket-unicode"))

(quail-define-package
 "racket-unicode"                       ;name
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

(quail-define-rules
 ;; Typed Racket
 ("All"              ["∀"])
 ("Union"            ["U"])
 ("Intersection"     ["∩"])
 ;; These would be nice except no such aliases provided by racket/contract.
 ;; ("->"               ["→"])
 ;; ("case->"           ["case→"])

 ;; Redex
 ("test-->>E"        ["test-->>∃"])

 ;; Greek letters
 ("alpha "           ["α"])
 ("Alpha "           ["Α"])
 ("beta "            ["β"])
 ("Beta "            ["Β"])
 ("gamma "           ["γ"])
 ("Gamma "           ["Γ"])
 ("delta "           ["δ"])
 ("Delta "           ["Δ"])
 ("epsilon "         ["ε"])
 ("Epsilon "         ["Ε"])
 ("zeta "            ["ζ"])
 ("Zeta "            ["Ζ"])
 ("eta "             ["η"])
 ("Eta "             ["Η"])
 ("theta "           ["θ"])
 ("Theta "           ["Θ"])
 ("iota "            ["ι"])
 ("Iota "            ["Ι"])
 ("kappa "           ["κ"])
 ("Kappa "           ["Κ"])
 ("lambda "          ["λ"])
 ("Lambda "          ["Λ"])
 ("lamda "           ["λ"])
 ("Lamda "           ["Λ"])
 ("mu "              ["μ"])
 ("Mu "              ["Μ"])
 ("nu "              ["ν"])
 ("Nu "              ["Ν"])
 ("xi "              ["ξ"])
 ("Xi "              ["Ξ"])
 ("omicron "         ["ο"])
 ("Omicron "         ["Ο"])
 ("pi "              ["π"])
 ("Pi "              ["Π"])
 ("rho "             ["ρ"])
 ("Rho "             ["Ρ"])
 ("sigma "           ["σ"])
 ("Sigma "           ["Σ"])
 ("tau "             ["τ"])
 ("Tau "             ["Τ"])
 ("upsilon "         ["υ"])
 ("Upsilon "         ["Υ"])
 ("phi "             ["φ"])
 ("Phi "             ["Φ"])
 ("chi "             ["χ"])
 ("Chi "             ["Χ"])
 ("psi "             ["ψ"])
 ("Psi "             ["Ψ"])
 ("omega "           ["ω"])
 ("Omega "           ["Ω"])
 ("digamma "         ["ϝ"])
 ("Digamma "         ["Ϝ"])
 ("san "             ["ϻ"])
 ("San "             ["Ϻ"])
 ("qoppa "           ["ϙ"])
 ("Qoppa "           ["Ϙ"])
 ("sampi "           ["ϡ"])
 ("Sampi "           ["Ϡ"])
 ("stigma "          ["ϛ"])
 ("Stigma "          ["Ϛ"])
 ("heta "            ["ͱ"])
 ("Heta "            ["Ͱ"])
 ("sho "             ["ϸ"])
 ("Sho "             ["Ϸ"])

 ;; Double-struck letters
 ("|A|"              ["𝔸"])
 ("|B|"              ["𝔹"])
 ("|C|"              ["ℂ"])
 ("|D|"              ["𝔻"])
 ("|E|"              ["𝔼"])
 ("|F|"              ["𝔽"])
 ("|G|"              ["𝔾"])
 ("|H|"              ["ℍ"])
 ("|I|"              ["𝕀"])
 ("|J|"              ["𝕁"])
 ("|K|"              ["𝕂"])
 ("|L|"              ["𝕃"])
 ("|M|"              ["𝕄"])
 ("|N|"              ["ℕ"])
 ("|O|"              ["𝕆"])
 ("|P|"              ["ℙ"])
 ("|Q|"              ["ℚ"])
 ("|R|"              ["ℝ"])
 ("|S|"              ["𝕊"])
 ("|T|"              ["𝕋"])
 ("|U|"              ["𝕌"])
 ("|V|"              ["𝕍"])
 ("|W|"              ["𝕎"])
 ("|X|"              ["𝕏"])
 ("|Y|"              ["𝕐"])
 ("|Z|"              ["ℤ"])
 ("|gamma|"          ["ℽ"])
 ("|Gamma|"          ["ℾ"])
 ("|pi|"             ["ℼ"])
 ("|Pi|"             ["ℿ"])

 ;; Quantifiers
 ("forall"           ["∀"])
 ("exists"           ["∃"])

 ;; Numeric subscripts
 ("_0 "              ["₀"])
 ("_1 "              ["₁"])
 ("_2 "              ["₂"])
 ("_3 "              ["₃"])
 ("_4 "              ["₄"])
 ("_5 "              ["₅"])
 ("_6 "              ["₆"])
 ("_7 "              ["₇"])
 ("_8 "              ["₈"])
 ("_9 "              ["₉"])

 ;; Numeric superscripts
 ("^0 "              ["⁰"])
 ("^1 "              ["¹"])
 ("^2 "              ["²"])
 ("^3 "              ["³"])
 ("^4 "              ["⁴"])
 ("^5 "              ["⁵"])
 ("^6 "              ["⁶"])
 ("^7 "              ["⁷"])
 ("^8 "              ["⁸"])
 ("^9 "              ["⁹"]))

(provide 'racket-unicode-input-method)

;;; racket-unicode-input-method.el ends here
