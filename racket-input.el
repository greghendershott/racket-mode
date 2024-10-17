;;; racket-input.el -*- lexical-binding: t; -*-

;; Copyright (c) 2024 by Greg Hendershott

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'quail)
(require 'racket-complete)
(require 'racket-util)

(defgroup racket-input nil
  "The Racket input method."
  :group 'racket)

(defcustom racket-input-prefix "\\"
  "A prefix for all `racket-input-translations', when used by the
Racket input method.

If you change this setting manually with `setq' (instead of using
the customization buffer or `setopt') you need to call
`racket-input-setup' in order for the change to take effect."
  :type '(choice (string :tag "Prefix")
                 (const :tag "None" nil)))

(defcustom racket-input-translations
  '(;; Typed Racket
    ("All"          "∀")
    ("Union"        "U")
    ("Intersection" "∩")
    ;; Redex
    ("test-->>E" "test-->>∃")
    ;; Turnstile
    ("vdash" "⊢")
    ("gg"    "≫")
    ("succ"  "≻")
    ;; Other type rule symbols
    ("times"          "×")
    ("Uparrow"        "⇑")
    ("Downarrow"      "⇓")
    ("Leftarrow"      "⇐")
    ("Rightarrow"     "⇒")
    ("leftarrow"      "←")
    ("rightarrow"     "→")
    ("nwarrow"        "↖")
    ("nearrow"        "↗")
    ("uparrow"        "↑")
    ("downarrow"      "↓")
    ("searrow"        "↘")
    ("swarrow"        "↙")
    ("leftrightarrow" "↔")
    ("updownarrow"    "⇕")
    ("aleph"          "א")
    ("emptyset"       "∅")
    ("nabla"          "∇")
    ("surd"           "√")
    ("negation"       "¬")
    ("infinity"       "∞")
    ("prod"           "∏")
    ("coprod"         "∐")
    ("integrate"      "∫")
    ("Oint"           "∮")
    ("vee"            "∨")
    ("wedge"          "∧")
    ("follows"        "∘")
    ("setin"          "∈")
    ("ni"             "∋")
    ("notin"          "∉")
    ("sqsubset"       "⊏")
    ("sqsupset"       "⊐")
    ("sqsubseteq"     "⊑")
    ("sqsupseteq"     "⊒")
    ("subset"         "⊂")
    ("superset"       "⊃")
    ("subseteq"       "⊆")
    ("supseteq"       "⊇")
    ("approx"         "≈")
    ("cong"           "≌")
    ("neq"            "≠")
    ("sqcap"          "⊓")
    ("sqcup"          "⊔")
    ("models"         "⊧")
    ;; Greek letters
    ("alpha"   "α")
    ("Alpha"   "Α")
    ("beta"    "β")
    ("Beta"    "Β")
    ("gamma"   "γ")
    ("Gamma"   "Γ")
    ("delta"   "δ")
    ("Delta"   "Δ")
    ("epsilon" "ε")
    ("Epsilon" "Ε")
    ("zeta"    "ζ")
    ("Zeta"    "Ζ")
    ("eta"     "η")
    ("Eta"     "Η")
    ("theta"   "θ")
    ("Theta"   "Θ")
    ("iota"    "ι")
    ("Iota"    "Ι")
    ("kappa"   "κ")
    ("Kappa"   "Κ")
    ("lambda"  "λ")
    ("Lambda"  "Λ")
    ("lamda"   "λ")
    ("Lamda"   "Λ")
    ("mu"      "μ")
    ("Mu"      "Μ")
    ("nu"      "ν")
    ("Nu"      "Ν")
    ("xi"      "ξ")
    ("Xi"      "Ξ")
    ("omicron" "ο")
    ("Omicron" "Ο")
    ("pi"      "π")
    ("Pi"      "Π")
    ("rho"     "ρ")
    ("Rho"     "Ρ")
    ("sigma"   "σ")
    ("Sigma"   "Σ")
    ("tau"     "τ")
    ("Tau"     "Τ")
    ("upsilon" "υ")
    ("Upsilon" "Υ")
    ("phi"     "φ")
    ("Phi"     "Φ")
    ("chi"     "χ")
    ("Chi"     "Χ")
    ("psi"     "ψ")
    ("Psi"     "Ψ")
    ("omega"   "ω")
    ("Omega"   "Ω")
    ("digamma" "ϝ")
    ("Digamma" "Ϝ")
    ("san"     "ϻ")
    ("San"     "Ϻ")
    ("qoppa"   "ϙ")
    ("Qoppa"   "Ϙ")
    ("sampi"   "ϡ")
    ("Sampi"   "Ϡ")
    ("stigma"  "ϛ")
    ("Stigma"  "Ϛ")
    ("heta"    "ͱ")
    ("Heta"    "Ͱ")
    ("sho"     "ϸ")
    ("Sho"     "Ϸ")
    ;; Double-struck letters
    ("|A|"     "𝔸")
    ("|B|"     "𝔹")
    ("|C|"     "ℂ")
    ("|D|"     "𝔻")
    ("|E|"     "𝔼")
    ("|F|"     "𝔽")
    ("|G|"     "𝔾")
    ("|H|"     "ℍ")
    ("|I|"     "𝕀")
    ("|J|"     "𝕁")
    ("|K|"     "𝕂")
    ("|L|"     "𝕃")
    ("|M|"     "𝕄")
    ("|N|"     "ℕ")
    ("|O|"     "𝕆")
    ("|P|"     "ℙ")
    ("|Q|"     "ℚ")
    ("|R|"     "ℝ")
    ("|S|"     "𝕊")
    ("|T|"     "𝕋")
    ("|U|"     "𝕌")
    ("|V|"     "𝕍")
    ("|W|"     "𝕎")
    ("|X|"     "𝕏")
    ("|Y|"     "𝕐")
    ("|Z|"     "ℤ")
    ("|gamma|" "ℽ")
    ("|Gamma|" "ℾ")
    ("|pi|"    "ℼ")
    ("|Pi|"    "ℿ")
    ;; Quantifiers
    ("forall" "∀")
    ("exists" "∃")
    ;; Numeric subscripts
    ("_0" "₀")
    ("_1" "₁")
    ("_2" "₂")
    ("_3" "₃")
    ("_4" "₄")
    ("_5" "₅")
    ("_6" "₆")
    ("_7" "₇")
    ("_8" "₈")
    ("_9" "₉")
    ;; Numeric superscripts
    ("^0" "⁰")
    ("^1" "¹")
    ("^2" "²")
    ("^3" "³")
    ("^4" "⁴")
    ("^5" "⁵")
    ("^6" "⁶")
    ("^7" "⁷")
    ("^8" "⁸")
    ("^9" "⁹"))
  "A list of translations.

Each element is (KEY-SEQUENCE-STRING TRANSLATION-STRING).

Used by the \"Racket\" input method as well as the
`racket-insert-symbol' command.

If you change this setting manually with `setq' (instead of using
the customization buffer or `setopt') you need to call
`racket-input-setup' in order for the change to take effect."
  :set 'racket-custom-set
  :initialize 'custom-initialize-default
  :type '(repeat (list (string :tag "Key sequence")
                       (string :tag "Translation"))))

(defun racket-input-setup ()
  "Set up the Racket input method based on the customization
variables."
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
  (dolist (tr racket-input-translations)
    (pcase-let* ((`(,key ,translation) tr)
                 (key (concat racket-input-prefix key)))
     (quail-defrule key translation "Racket" t))))

(defun racket-custom-set (sym val)
  (set-default sym val)
  (racket-input-setup))

;; Set up the input method.
(racket-input-setup)

;;; Minor mode

;; This is a convenience for user configuration, as well as a good
;; documentation location.

(define-minor-mode racket-input-mode
  "A minor mode to enable the \"Racket\" input method.

The Racket input method lets you easily use various Unicode
symbols that might be useful when writing Racket code.

To automatically enable the Racket input method in racket-mode
and racket-repl-mode buffers, put the following code in your
Emacs init file:

#+BEGIN_SRC elisp
    (add-hook \\='racket-mode-hook #\\='racket-input-mode)
    (add-hook \\='racket-repl-mode-hook #\\='racket-input-mode)
#+END_SRC

You may use the standard Emacs key C-\\ to toggle the current
input method.

When the Racket input method is active, and `racket-input-prefix'
is the default \"\\\", you can for example type \"\\All\" and it
is immediately replaced with \"∀\".

See `racket-input-translations' for the full list of translations,
which is also used by the `racket-insert-symbol' command.

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

;;; Command flavor, using completing-read with annotations

(defun racket-insert-symbol ()
  "A command alternative to the \"Racket\" input method.

Presents a `completing-read' UI to choose and insert from
`racket-input-translations'. The symbols that would be inserted
are shown as annotations -- serves as a preview unlike what is
currently provided by the Emacs UI for input method."
  (interactive)
  (let* ((translations racket-input-translations)
         (affixator
          (lambda (strs)
            (let ((max-len 16))
              (dolist (str strs)
                (setq max-len (max max-len (1+ (length str)))))
              (seq-map (lambda (str)
                         (let ((v (cadr (assoc str translations))))
                           (list str
                                 ""
                                 (concat
                                  (make-string (- max-len (length str)) 32)
                                  (propertize v 'face 'bold)))))
                       strs))))
         (collection
          (racket--completion-table
           translations
           `((category . racket-symbol-name)
             (affixation-function . ,affixator))))
         (predicate nil)
         (require-match t))
    (when-let (str (completing-read "Symbol: "
                                    collection
                                    predicate
                                    require-match))
      (insert (cadr (assoc str racket-input-translations))))))

(provide 'racket-input)

;;; racket-input.el ends here
