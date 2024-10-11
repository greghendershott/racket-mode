;;; racket-input.el -*- lexical-binding: t; -*-

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
  `(("TeX" . (racket-input-compose
              (racket-input-drop '("geq" "leq" "bullet" "qed" "par"))
              (racket-input-or
               (racket-input-drop-prefix "\\")
               (racket-input-or
                (racket-input-compose
                 (racket-input-drop '("^l" "^o" "^r" "^v"))
                 (racket-input-prefix "^"))
                (racket-input-prefix "_")))))
    )
  "A list of Quail input methods whose translations should be
inherited by the Racket input method (with the exception of
translations corresponding to ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same kind as
`racket-input-tweak-all' which is used to tweak the translation
pairs of the input method.

The inherited translation pairs are added last, after
`racket-input-user-translations' and `racket-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `racket-input-setup' in
order for the change to take effect."
  :set 'racket-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom racket-input-translations
  (let ((max-lisp-eval-depth 2800))
    `(
      ;; Equality and similar symbols.

      ("eq"  . ,(racket-input-to-string-list "=∼∽≈≋∻∾∿≀≃⋍≂≅ ≌≊≡≣≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≍≎≏≬⋕＝"))
      ("eqn" . ,(racket-input-to-string-list "≠≁ ≉     ≄  ≇≆  ≢                 ≭    "))

      ("=n"  . ("≠"))
      ("~"    . ,(racket-input-to-string-list "∼～"))
      ("~n"  . ("≁"))
      ("~~"   . ("≈"))  ("~~n" . ("≉"))
      ("~~~"  . ("≋"))
      (":~"   . ("∻"))
      ("~-"   . ("≃"))  ("~-n" . ("≄"))
      ("-~"   . ("≂"))
      ("~="   . ("≅"))  ("~=n" . ("≇"))
      ("~~-"  . ("≊"))
      ("=="   . ("≡"))  ("==n" . ("≢"))
      ("==="  . ("≣"))
      ("="    . ("＝"))
      (".="   . ("≐"))  (".=." . ("≑"))
      (":="   . ("≔"))  ("=:"  . ("≕"))
      ("=o"   . ("≗"))
      ("(="   . ("≘"))
      ("and=" . ("≙"))  ("or=" . ("≚"))
      ("*="   . ("≛"))
      ("t="   . ("≜"))
      ("def=" . ("≝"))
      ("m="   . ("≞"))
      ("?="   . ("≟"))

      ;; Inequality and similar symbols.

      ("leq"  . ,(racket-input-to-string-list "<≪⋘≤≦≲ ≶≺≼≾⊂⊆ ⋐⊏⊑ ⊰⊲⊴⋖⋚⋜⋞＜"))
      ("leqn" . ,(racket-input-to-string-list "≮  ≰≨≴⋦≸⊀ ⋨⊄⊈⊊  ⋢⋤ ⋪⋬   ⋠"))
      ("geq"  . ,(racket-input-to-string-list ">≫⋙≥≧≳ ≷≻≽≿⊃⊇ ⋑⊐⊒ ⊱⊳⊵⋗⋛⋝⋟＞"))
      ("geqn" . ,(racket-input-to-string-list "≯  ≱≩≵⋧≹⊁ ⋩⊅⊉⊋  ⋣⋥ ⋫⋭   ⋡"))

      ("<="   . ("≤"))  (">="   . ("≥"))
      ("<=n"  . ("≰"))  (">=n"  . ("≱"))
      ("len"  . ("≰"))  ("gen"  . ("≱"))
      ("<n"   . ("≮"))  (">n"   . ("≯"))
      ("<~"   . ("≲"))  (">~"   . ("≳"))
      ("<~n"  . ("⋦"))  (">~n"  . ("⋧"))
      ("<~nn" . ("≴"))  (">~nn" . ("≵"))

      ("sub"   . ("⊂"))  ("sup"   . ("⊃"))
      ("subn"  . ("⊄"))  ("supn"  . ("⊅"))
      ("sub="  . ("⊆"))  ("sup="  . ("⊇"))
      ("sub=n" . ("⊈"))  ("sup=n" . ("⊉"))

      ("squb"   . ("⊏"))  ("squp"   . ("⊐"))
      ("squb="  . ("⊑"))  ("squp="  . ("⊒"))
      ("squb=n" . ("⋢"))  ("squp=n" . ("⋣"))

      ;; Set membership etc.

      ("member" . ,(racket-input-to-string-list "∈∉∊∋∌∍⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿"))

      ("inn" . ("∉"))
      ("nin" . ("∌"))

      ;; Intersections, unions etc.

      ("intersection" . ,(racket-input-to-string-list "∩⋂∧⋀⋏⨇⊓⨅⋒∏ ⊼      ⨉"))
      ("union"        . ,(racket-input-to-string-list "∪⋃∨⋁⋎⨈⊔⨆⋓∐⨿⊽⊻⊍⨃⊎⨄⊌∑⅀"))

      ("and" . ("∧"))  ("or"  . ("∨"))
      ("And" . ("⋀"))  ("Or"  . ("⋁"))
      ("i"   . ("∩"))  ("un"  . ("∪"))  ("u+" . ("⊎"))  ("u." . ("⊍"))
      ("I"   . ("⋂"))  ("Un"  . ("⋃"))  ("U+" . ("⨄"))  ("U." . ("⨃"))
      ("glb" . ("⊓"))  ("lub" . ("⊔"))
      ("Glb" . ("⨅"))  ("Lub" . ("⨆"))

      ;; Entailment etc.

      ("entails" . ,(racket-input-to-string-list "⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯"))

      ("|-"   . ("⊢"))  ("|-n"  . ("⊬"))
      ("-|"   . ("⊣"))
      ("|="   . ("⊨"))  ("|=n"  . ("⊭"))
      ("||-"  . ("⊩"))  ("||-n" . ("⊮"))
      ("||="  . ("⊫"))  ("||=n" . ("⊯"))
      ("|||-" . ("⊪"))

      ;; Divisibility, parallelity.

      ("|"  . ("∣"))  ("|n"  . ("∤"))
      ("||" . ("∥"))  ("||n" . ("∦"))

      ;; Some symbols from logic and set theory.

      ("all" . ("∀"))
      ("ex"  . ("∃"))
      ("exn" . ("∄"))
      ("0"   . ("∅"))
      ("C"   . ("∁"))

      ;; Corners, ceilings and floors.

      ("c"  . ,(racket-input-to-string-list "⌜⌝⌞⌟⌈⌉⌊⌋"))
      ("cu" . ,(racket-input-to-string-list "⌜⌝  ⌈⌉  "))
      ("cl" . ,(racket-input-to-string-list "  ⌞⌟  ⌊⌋"))

      ("cul" . ("⌜"))  ("cuL" . ("⌈"))
      ("cur" . ("⌝"))  ("cuR" . ("⌉"))
      ("cll" . ("⌞"))  ("clL" . ("⌊"))
      ("clr" . ("⌟"))  ("clR" . ("⌋"))

      ;; Various operators/symbols.

      ("qed"       . ("∎"))
      ("x"         . ("×"))
      ("o"         . ("∘"))
      ("comp"      . ("∘"))
      ("."         . ,(racket-input-to-string-list "∙．"))
      ("*"         . ("⋆"))
      (".+"        . ("∔"))
      (".-"        . ("∸"))
      (":"         . ,(racket-input-to-string-list "∶⦂ː꞉˸፥፦：﹕︓"))
      (","         . ,(racket-input-to-string-list "ʻ،⸲⸴⹁⹉、︐︑﹐﹑，､"))
      (";"         . ,(racket-input-to-string-list "⨾⨟⁏፤꛶；︔﹔⍮⸵;"))
      ("::"        . ("∷"))
      ("::-"       . ("∺"))
      ("-:"        . ("∹"))
      ("+ "        . ("⊹"))
      ("+"         . ("＋"))
      ("sqrt"      . ("√"))
      ("surd3"     . ("∛"))
      ("surd4"     . ("∜"))
      ("increment" . ("∆"))
      ("inf"       . ("∞"))
      ("&"         . ("⅋"))
      ("z;"        . ,(racket-input-to-string-list "⨟⨾"))
      ("z:"        . ("⦂"))

      ;; Circled operators.

      ("o+"  . ("⊕"))
      ("o--" . ("⊖"))
      ("ox"  . ("⊗"))
      ("o/"  . ("⊘"))
      ("o."  . ("⊙"))
      ("oo"  . ("⊚"))
      ("o*"  . ("⊛"))
      ("o="  . ("⊜"))
      ("o-"  . ("⊝"))

      ("O+"  . ("⨁"))
      ("Ox"  . ("⨂"))
      ("O."  . ("⨀"))
      ("O*"  . ("⍟"))

      ;; Boxed operators.

      ("b+" . ("⊞"))
      ("b-" . ("⊟"))
      ("bx" . ("⊠"))
      ("b." . ("⊡"))

      ;; APL boxed operators

      ("box="       . ("⌸"))
      ("box?"       . ("⍰"))
      ("box'"       . ("⍞"))
      ("box:"       . ("⍠"))
      ("box/"       . ("⍁"))
      ("box\\"      . ("⍂"))
      ("box<"       . ("⍃"))
      ("box>"       . ("⍄"))
      ("boxo"       . ("⌻"))
      ("boxO"       . ("⌼"))

      ("boxcomp"    . ("⌻"))
      ("boxcircle"  . ("⌼"))
      ("boxeq"      . ("⌸"))
      ("boxneq"     . ("⍯"))
      ("boxeqn"     . ("⍯"))

      ("boxl"       . ("⍇"))
      ("boxr"       . ("⍈"))
      ("boxu"       . ("⍐"))
      ("boxd"       . ("⍗"))

      ("boxdi"      . ("⌺"))
      ("boxdiv"     . ("⌹"))
      ("boxwedge"   . ("⍓"))
      ("boxvee"     . ("⍌"))
      ("boxdelta"   . ("⍍"))
      ("boxnabla"   . ("⍔"))

      ;; Various symbols.

      ("integral" . ,(racket-input-to-string-list "∫∬∭∮∯∰∱∲∳"))
      ("angle"    . ,(racket-input-to-string-list "∟∡∢⊾⊿"))
      ("join"     . ,(racket-input-to-string-list "⋈⋉⋊⋋⋌⨝⟕⟖⟗"))

      ;; Arrows.

      ("l"  . ,(racket-input-to-string-list "←⇐⇚⭅⇇⇆↤⇦↞↼↽⇠⇺↜⇽⟵⟸↚⇍⇷ ↹     ↢↩↫⇋⇜⇤⟻⟽⤆↶↺⟲                                     "))
      ("r"  . ,(racket-input-to-string-list "→⇒⇛⭆⇉⇄↦⇨↠⇀⇁⇢⇻↝⇾⟶⟹↛⇏⇸⇶ ↴    ↣↪↬⇌⇝⇥⟼⟾⤇↷↻⟳⇰⇴⟴⟿ ➵➸➙➔➛➜➝➞➟➠➡➢➣➤➧➨➩➪➫➬➭➮➯➱➲➳➺➻➼➽➾⊸"))
      ("u"  . ,(racket-input-to-string-list "↑⇑⤊⟰⇈⇅↥⇧↟↿↾⇡⇞          ↰↱➦ ⇪⇫⇬⇭⇮⇯                                           "))
      ("d"  . ,(racket-input-to-string-list "↓⇓⤋⟱⇊⇵↧⇩↡⇃⇂⇣⇟         ↵↲↳➥ ↯                                                "))
      ("ud" . ,(racket-input-to-string-list "↕⇕   ↨⇳                                                                    "))
      ("lr" . ,(racket-input-to-string-list "↔⇔         ⇼↭⇿⟷⟺↮⇎⇹                                                        "))
      ("ul" . ,(racket-input-to-string-list "↖⇖                        ⇱↸                                               "))
      ("ur" . ,(racket-input-to-string-list "↗⇗                                         ➶➹➚                             "))
      ("dr" . ,(racket-input-to-string-list "↘⇘                        ⇲                ➴➷➘                             "))
      ("dl" . ,(racket-input-to-string-list "↙⇙                                                                         "))

      ("l-"  . ("←"))  ("<-"  . ("←"))  ("l="  . ("⇐"))  ("<="  . ("⇐"))
      ("r-"  . ("→"))  ("->"  . ("→"))  ("r="  . ("⇒"))  ("=>"  . ("⇒"))
      ("u-"  . ("↑"))                   ("u="  . ("⇑"))
      ("d-"  . ("↓"))                   ("d="  . ("⇓"))
      ("ud-" . ("↕"))                   ("ud=" . ("⇕"))
      ("lr-" . ("↔"))  ("<->" . ("↔"))  ("lr=" . ("⇔"))  ("<=>" . ("⇔"))
      ("ul-" . ("↖"))                   ("ul=" . ("⇖"))
      ("ur-" . ("↗"))                   ("ur=" . ("⇗"))
      ("dr-" . ("↘"))                   ("dr=" . ("⇘"))
      ("dl-" . ("↙"))                   ("dl=" . ("⇙"))

      ("l==" . ("⇚"))  ("l-2" . ("⇇"))                   ("l-r-" . ("⇆"))
      ("r==" . ("⇛"))  ("r-2" . ("⇉"))  ("r-3" . ("⇶"))  ("r-l-" . ("⇄"))
      ("u==" . ("⟰"))  ("u-2" . ("⇈"))                   ("u-d-" . ("⇅"))
      ("d==" . ("⟱"))  ("d-2" . ("⇊"))                   ("d-u-" . ("⇵"))

      ("l--"  . ("⟵"))  ("<--"  . ("⟵"))  ("l~"  . ("↜" "⇜"))
      ("r--"  . ("⟶"))  ("-->"  . ("⟶"))  ("r~"  . ("↝" "⇝" "⟿"))
      ("lr--" . ("⟷"))  ("<-->" . ("⟷"))  ("lr~" . ("↭"))

      ("l-n"  . ("↚"))  ("<-n"  . ("↚"))  ("l=n"  . ("⇍"))
      ("r-n"  . ("↛"))  ("->n"  . ("↛"))  ("r=n"  . ("⇏"))  ("=>n"  . ("⇏"))
      ("lr-n" . ("↮"))  ("<->n" . ("↮"))  ("lr=n" . ("⇎"))  ("<=>n" . ("⇎"))

      ("l-|"  . ("↤"))  ("ll-" . ("↞"))
      ("r-|"  . ("↦"))  ("rr-" . ("↠"))
      ("u-|"  . ("↥"))  ("uu-" . ("↟"))
      ("d-|"  . ("↧"))  ("dd-" . ("↡"))
      ("ud-|" . ("↨"))

      ("l->" . ("↢"))
      ("r->" . ("↣"))

      ("r-o" . ("⊸"))  ("-o"  . ("⊸"))

      ("dz" . ("↯"))

      ;; Ellipsis.

      ("..." . ,(racket-input-to-string-list "⋯⋮⋰⋱"))

      ;; Box-drawing characters.

      ("---" . ,(racket-input-to-string-list "─│┌┐└┘├┤┬┼┴╴╵╶╷╭╮╯╰╱╲╳"))
      ("--=" . ,(racket-input-to-string-list "═║╔╗╚╝╠╣╦╬╩     ╒╕╘╛╞╡╤╪╧ ╓╖╙╜╟╢╥╫╨"))
      ("--_" . ,(racket-input-to-string-list "━┃┏┓┗┛┣┫┳╋┻╸╹╺╻
                                        ┍┯┑┕┷┙┝┿┥┎┰┒┖┸┚┠╂┨┞╀┦┟╁┧┢╈┪┡╇┩
                                        ┮┭┶┵┾┽┲┱┺┹╊╉╆╅╄╃ ╿╽╼╾"))
      ("--." . ,(racket-input-to-string-list "╌╎┄┆┈┊
                                        ╍╏┅┇┉┋"))

      ;; Triangles.

      ;; Big/small, black/white.

      ("t" . ,(racket-input-to-string-list "◂◃◄◅▸▹►▻▴▵▾▿◢◿◣◺◤◸◥◹"))
      ("T" . ,(racket-input-to-string-list "◀◁▶▷▲△▼▽◬◭◮"))

      ("tb" . ,(racket-input-to-string-list "◂▸▴▾◄►◢◣◤◥"))
      ("tw" . ,(racket-input-to-string-list "◃▹▵▿◅▻◿◺◸◹"))

      ("Tb" . ,(racket-input-to-string-list "◀▶▲▼"))
      ("Tw" . ,(racket-input-to-string-list "◁▷△▽"))

      ;; Squares.

      ("sq"  . ,(racket-input-to-string-list "■□◼◻◾◽▣▢▤▥▦▧▨▩◧◨◩◪◫◰◱◲◳"))
      ("sqb" . ,(racket-input-to-string-list "■◼◾"))
      ("sqw" . ,(racket-input-to-string-list "□◻◽"))
      ("sq." . ("▣"))
      ("sqo" . ("▢"))

      ;; Rectangles.

      ("re"  . ,(racket-input-to-string-list "▬▭▮▯"))
      ("reb" . ,(racket-input-to-string-list "▬▮"))
      ("rew" . ,(racket-input-to-string-list "▭▯"))

      ;; Parallelograms.

      ("pa"  . ,(racket-input-to-string-list "▰▱"))
      ("pab" . ("▰"))
      ("paw" . ("▱"))

      ;; Diamonds.

      ("di"  . ,(racket-input-to-string-list "◆◇◈"))
      ("dib" . ("◆"))
      ("diw" . ("◇"))
      ("di." . ("◈"))

      ;; Circles.

      ("ci"   . ,(racket-input-to-string-list "●○◎◌◯◍◐◑◒◓◔◕◖◗◠◡◴◵◶◷⚆⚇⚈⚉"))
      ("cib"  . ("●"))
      ("ciw"  . ("○"))
      ("ci."  . ("◎"))
      ("ci.." . ("◌"))
      ("ciO"  . ("◯"))

      ;; Stars.

      ("st"   . ,(racket-input-to-string-list "⋆✦✧✶✴✹ ★☆✪✫✯✰✵✷✸"))
      ("st4"  . ,(racket-input-to-string-list "✦✧"))
      ("st6"  . ("✶"))
      ("st8"  . ("✴"))
      ("st12" . ("✹"))

      ;; Blackboard bold letters.

      ("bA"   . ("𝔸"))
      ("bB"   . ("𝔹"))
      ("bC"   . ("ℂ"))
      ("bD"   . ("𝔻"))
      ("bE"   . ("𝔼"))
      ("bF"   . ("𝔽"))
      ("bG"   . ("𝔾"))
      ("bH"   . ("ℍ"))
      ("bI"   . ("𝕀"))
      ("bJ"   . ("𝕁"))
      ("bK"   . ("𝕂"))
      ("bL"   . ("𝕃"))
      ("bM"   . ("𝕄"))
      ("bN"   . ("ℕ"))
      ("bO"   . ("𝕆"))
      ("bP"   . ("ℙ"))
      ("bQ"   . ("ℚ"))
      ("bR"   . ("ℝ"))
      ("bS"   . ("𝕊"))
      ("bT"   . ("𝕋"))
      ("bU"   . ("𝕌"))
      ("bV"   . ("𝕍"))
      ("bW"   . ("𝕎"))
      ("bX"   . ("𝕏"))
      ("bY"   . ("𝕐"))
      ("bZ"   . ("ℤ"))
      ("bGG"  . ("ℾ"))
      ("bGP"  . ("ℿ"))
      ("bGS"  . ("⅀"))
      ("ba"   . ("𝕒"))
      ("bb"   . ("𝕓"))
      ("bc"   . ("𝕔"))
      ("bd"   . ("𝕕"))
      ("be"   . ("𝕖"))
      ("bf"   . ("𝕗"))
      ("bg"   . ("𝕘"))
      ("bh"   . ("𝕙"))
      ("bi"   . ("𝕚"))
      ("bj"   . ("𝕛"))
      ("bk"   . ("𝕜"))
      ("bl"   . ("𝕝"))
      ("bm"   . ("𝕞"))
      ("bn"   . ("𝕟"))
      ("bo"   . ("𝕠"))
      ("bp"   . ("𝕡"))
      ("bq"   . ("𝕢"))
      ("br"   . ("𝕣"))
      ("bs"   . ("𝕤"))
      ("bt"   . ("𝕥"))
      ("bu"   . ("𝕦"))
      ("bv"   . ("𝕧"))
      ("bw"   . ("𝕨"))
      ("bx"   . ("𝕩"))
      ("by"   . ("𝕪"))
      ("bz"   . ("𝕫"))
      ("bGg"  . ("ℽ"))
      ("bGp"  . ("ℼ"))

      ;; Blackboard bold numbers.

      ("b0"   . ("𝟘"))
      ("b1"   . ("𝟙"))
      ("b2"   . ("𝟚"))
      ("b3"   . ("𝟛"))
      ("b4"   . ("𝟜"))
      ("b5"   . ("𝟝"))
      ("b6"   . ("𝟞"))
      ("b7"   . ("𝟟"))
      ("b8"   . ("𝟠"))
      ("b9"   . ("𝟡"))

      ;; Mathematical bold letters.

      ("BA"   . ("𝐀"))
      ("BB"   . ("𝐁"))
      ("BC"   . ("𝐂"))
      ("BD"   . ("𝐃"))
      ("BE"   . ("𝐄"))
      ("BF"   . ("𝐅"))
      ("BG"   . ("𝐆"))
      ("BH"   . ("𝐇"))
      ("BI"   . ("𝐈"))
      ("BJ"   . ("𝐉"))
      ("BK"   . ("𝐊"))
      ("BL"   . ("𝐋"))
      ("BM"   . ("𝐌"))
      ("BN"   . ("𝐍"))
      ("BO"   . ("𝐎"))
      ("BP"   . ("𝐏"))
      ("BQ"   . ("𝐐"))
      ("BR"   . ("𝐑"))
      ("BS"   . ("𝐒"))
      ("BT"   . ("𝐓"))
      ("BU"   . ("𝐔"))
      ("BV"   . ("𝐕"))
      ("BW"   . ("𝐖"))
      ("BX"   . ("𝐗"))
      ("BY"   . ("𝐘"))
      ("BZ"   . ("𝐙"))
      ("Ba"   . ("𝐚"))
      ("Bb"   . ("𝐛"))
      ("Bc"   . ("𝐜"))
      ("Bd"   . ("𝐝"))
      ("Be"   . ("𝐞"))
      ("Bf"   . ("𝐟"))
      ("Bg"   . ("𝐠"))
      ("Bh"   . ("𝐡"))
      ("Bi"   . ("𝐢"))
      ("Bj"   . ("𝐣"))
      ("Bk"   . ("𝐤"))
      ("Bl"   . ("𝐥"))
      ("Bm"   . ("𝐦"))
      ("Bn"   . ("𝐧"))
      ("Bo"   . ("𝐨"))
      ("Bp"   . ("𝐩"))
      ("Bq"   . ("𝐪"))
      ("Br"   . ("𝐫"))
      ("Bs"   . ("𝐬"))
      ("Bt"   . ("𝐭"))
      ("Bu"   . ("𝐮"))
      ("Bv"   . ("𝐯"))
      ("Bw"   . ("𝐰"))
      ("Bx"   . ("𝐱"))
      ("By"   . ("𝐲"))
      ("Bz"   . ("𝐳"))

      ;; Mathematical bold Greek letters.

      ("BGA"  . ("𝚨"))
      ("BGB"  . ("𝚩"))
      ("BGC"  . ("𝚾"))
      ("BGD"  . ("𝚫"))
      ("BGE"  . ("𝚬"))
      ("BGG"  . ("𝚪"))
      ("BGH"  . ("𝚮"))
      ("BGI"  . ("𝚰"))
      ("BGK"  . ("𝚱"))
      ("BGL"  . ("𝚲"))
      ("BGM"  . ("𝚳"))
      ("BGN"  . ("𝚴"))
      ("BGO"  . ("𝛀"))
      ("BOmicron" . ("𝚶"))
      ("BGF"  . ("𝚽"))
      ("BPi"  . ("𝚷"))
      ("BGP"  . ("𝚿"))
      ("BGR"  . ("𝚸"))
      ("BGS"  . ("𝚺"))
      ("BGT"  . ("𝚻"))
      ("BGTH" . ("𝚯"))
      ("BGU"  . ("𝚼"))
      ("BGX"  . ("𝚵"))
      ("BGZ"  . ("𝚭"))
      ("BGa"  . ("𝛂"))
      ("BGb"  . ("𝛃"))
      ("BGc"  . ("𝛘"))
      ("BGd"  . ("𝛅"))
      ("BGe"  . ("𝛆"))
      ("BGg"  . ("𝛄"))
      ("BGh"  . ("𝛈"))
      ("BGi"  . ("𝛊"))
      ("BGk"  . ("𝛋"))
      ("BGl"  . ("𝛌"))
      ("BGm"  . ("𝛍"))
      ("BGn"  . ("𝛎"))
      ("BGo"  . ("𝛚"))
      ("Bomicron" . ("𝛐"))
      ("BGf"  . ("𝛗"))
      ("Bpi"  . ("𝛑"))
      ("BGp"  . ("𝛙"))
      ("BGr"  . ("𝛒"))
      ("BGs"  . ("𝛔"))
      ("BGt"  . ("𝛕"))
      ("BGth" . ("𝛉"))
      ("BGu"  . ("𝛖"))
      ("BGx"  . ("𝛏"))
      ("BGz"  . ("𝛇"))

      ;; Mathematical bold digits.

      ("B0"   . ("𝟎"))
      ("B1"   . ("𝟏"))
      ("B2"   . ("𝟐"))
      ("B3"   . ("𝟑"))
      ("B4"   . ("𝟒"))
      ("B5"   . ("𝟓"))
      ("B6"   . ("𝟔"))
      ("B7"   . ("𝟕"))
      ("B8"   . ("𝟖"))
      ("B9"   . ("𝟗"))

      ;; Fullwidth letters

      ("FA"   . ("Ａ"))
      ("FB"   . ("Ｂ"))
      ("FC"   . ("Ｃ"))
      ("FD"   . ("Ｄ"))
      ("FE"   . ("Ｅ"))
      ("FF"   . ("Ｆ"))
      ("FG"   . ("Ｇ"))
      ("FH"   . ("Ｈ"))
      ("FI"   . ("Ｉ"))
      ("FJ"   . ("Ｊ"))
      ("FK"   . ("Ｋ"))
      ("FL"   . ("Ｌ"))
      ("FM"   . ("Ｍ"))
      ("FN"   . ("Ｎ"))
      ("FO"   . ("Ｏ"))
      ("FP"   . ("Ｐ"))
      ("FQ"   . ("Ｑ"))
      ("FR"   . ("Ｒ"))
      ("FS"   . ("Ｓ"))
      ("FT"   . ("Ｔ"))
      ("FU"   . ("Ｕ"))
      ("FV"   . ("Ｖ"))
      ("FW"   . ("Ｗ"))
      ("FX"   . ("Ｘ"))
      ("FY"   . ("Ｙ"))
      ("FZ"   . ("Ｚ"))
      ("Fa"   . ("ａ"))
      ("Fb"   . ("ｂ"))
      ("Fc"   . ("ｃ"))
      ("Fd"   . ("ｄ"))
      ("Fe"   . ("ｅ"))
      ("Ff"   . ("ｆ"))
      ("Fg"   . ("ｇ"))
      ("Fh"   . ("ｈ"))
      ("Fi"   . ("ｉ"))
      ("Fj"   . ("ｊ"))
      ("Fk"   . ("ｋ"))
      ("Fl"   . ("ｌ"))
      ("Fm"   . ("ｍ"))
      ("Fn"   . ("ｎ"))
      ("Fo"   . ("ｏ"))
      ("Fp"   . ("ｐ"))
      ("Fq"   . ("ｑ"))
      ("Fr"   . ("ｒ"))
      ("Fs"   . ("ｓ"))
      ("Ft"   . ("ｔ"))
      ("Fu"   . ("ｕ"))
      ("Fv"   . ("ｖ"))
      ("Fw"   . ("ｗ"))
      ("Fx"   . ("ｘ"))
      ("Fy"   . ("ｙ"))
      ("Fz"   . ("ｚ"))

      ;; Fullwidth digits

      ("F0"   . ("０"))
      ("F1"   . ("１"))
      ("F2"   . ("２"))
      ("F3"   . ("３"))
      ("F4"   . ("４"))
      ("F5"   . ("５"))
      ("F6"   . ("６"))
      ("F7"   . ("７"))
      ("F8"   . ("８"))
      ("F9"   . ("９"))

      ;; Parentheses.

      ("(" . ,(racket-input-to-string-list "([{⁅⁽₍〈⎴⟅⟦⟨⟪⦃〈《「『【〔〖〚︵︷︹︻︽︿﹁﹃﹙﹛﹝（［｛｢❪❬❰❲❴⟮⦅⦗⧼⸨❮⦇⦉"))
      (")" . ,(racket-input-to-string-list ")]}⁆⁾₎〉⎵⟆⟧⟩⟫⦄〉》」』】〕〗〛︶︸︺︼︾﹀﹂﹄﹚﹜﹞）］｝｣❫❭❱❳❵⟯⦆⦘⧽⸩❯⦈⦊"))

      ("[[" . ("⟦"))
      ("]]" . ("⟧"))
      ("<"  . ,(racket-input-to-string-list "⟨<≪⋘≺⊂⋐⊏⊰⊲⋖＜"))
      (">"  . ,(racket-input-to-string-list "⟩>≫⋙≻⊃⋑⊐⊱⊳⋗＞"))
      ("<<" . ("⟪"))
      (">>" . ("⟫"))
      ("{{" . ("⦃"))
      ("}}" . ("⦄"))

      ("(b" . ("⟅"))
      (")b" . ("⟆"))

      ("lbag" . ("⟅"))
      ("rbag" . ("⟆"))

      ("<|" . ("⦉")) ;; Angle bar brackets
      ("|>" . ("⦊"))

      ("(|" . ("⦇")) ;; Idiom brackets
      ("|)" . ("⦈"))

      ("((" . ,(racket-input-to-string-list "⦅｟")) ;; Banana brackets
      ("))" . ,(racket-input-to-string-list "⦆｠"))

      ;; Primes.

      ("'" . ,(racket-input-to-string-list "′″‴⁗＇"))
      ("`" . ,(racket-input-to-string-list "‵‶‷｀"))

      ;; Fractions.

      ("frac" . ,(racket-input-to-string-list "¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞⅟"))

      ;; Bullets.

      ("bu"  . ,(racket-input-to-string-list "•◦‣⁌⁍"))
      ("bub" . ("•"))
      ("buw" . ("◦"))
      ("but" . ("‣"))

      ;; Musical symbols.

      ("note" . ,(racket-input-to-string-list "♩♪♫♬"))
      ("b"    . ("♭"))
      ("#"    . ("♯"))

      ;; Other punctuation and symbols.

      ("\\"         . ("\\"))
      ("en"         . ("–"))
      ("em"         . ("—"))
      ("!"          . ("！"))
      ("!!"         . ("‼"))
      ("?"          . ("？"))
      ("??"         . ("⁇"))
      ("?!"         . ("‽" "⁈"))
      ("!?"         . ("⁉"))
      ("die"        . ,(racket-input-to-string-list "⚀⚁⚂⚃⚄⚅"))
      ("asterisk"   . ,(racket-input-to-string-list "⁎⁑⁂✢✣✤✥✱✲✳✺✻✼✽❃❉❊❋＊"))
      ("8<"         . ("✂" "✄"))
      ("tie"        . ("⁀"))
      ("undertie"   . ("‿"))
      ("apl"        . ,(racket-input-to-string-list "⌶⌷⌸⌹⌺⌻⌼⌽⌾⌿⍀⍁⍂⍃⍄⍅⍆⍇⍈
                                               ⍉⍊⍋⍌⍍⍎⍏⍐⍑⍒⍓⍔⍕⍖⍗⍘⍙⍚⍛
                                               ⍜⍝⍞⍟⍠⍡⍢⍣⍤⍥⍦⍧⍨⍩⍪⍫⍬⍭⍮
                                               ⍯⍰⍱⍲⍳⍴⍵⍶⍷⍸⍹⍺⎕"))
      ("#"          . ("＃"))
      ("%"          . ("％"))
      ("&"          . ("＆"))
      ("*"          . ("＊"))
      ("/"          . ,(racket-input-to-string-list "／＼"))
      ("@"          . ("＠"))
      ("__"         . ("＿"))
      ("\""         . ("＂"))

      ;; Some combining characters.
      ;;
      ;; The following combining characters also have (other)
      ;; translations:
      ;; ̀ ́ ̂ ̃ ̄ ̆ ̇ ̈ ̋ ̌ ̣ ̧ ̱

      ("^--" . ,(racket-input-to-string-list"̅̿"))
      ("_--" . ,(racket-input-to-string-list"̲̳"))
      ("^~"  . ,(racket-input-to-string-list"̃͌"))
      ("_~"  .  (                         "̰"))
      ("^."  . ,(racket-input-to-string-list"̇̈⃛⃜"))
      ("_."  . ,(racket-input-to-string-list"̣̤"))
      ("^l"  . ,(racket-input-to-string-list"⃖⃐⃔"))
      ("^l-" .  (                         "⃖"))
      ("^r"  . ,(racket-input-to-string-list"⃗⃑⃕"))
      ("^r-" .  (                         "⃗"))
      ("^lr" .  (                         "⃡"))
      ("_lr" .  (                         "͍"))
      ("^^"  . ,(racket-input-to-string-list"̂̑͆"))
      ("_^"  . ,(racket-input-to-string-list"̭̯̪"))
      ("^v"  . ,(racket-input-to-string-list"̌̆"))
      ("_v"  . ,(racket-input-to-string-list"̬̮̺"))

      ;; Shorter forms of many greek letters plus ƛ.

      ("Ga"  . ("α"))  ("GA"  . ("Α"))
      ("Gb"  . ("β"))  ("GB"  . ("Β"))
      ("Gg"  . ("γ"))  ("GG"  . ("Γ"))
      ("Gd"  . ("δ"))  ("GD"  . ("Δ"))
      ("Ge"  . ("ε"))  ("GE"  . ("Ε"))
      ("Gz"  . ("ζ"))  ("GZ"  . ("Ζ"))
      ("Gh"  . ("η"))  ("GH"  . ("Η"))
      ("Gth" . ("θ"))  ("GTH" . ("Θ"))
      ("Gi"  . ("ι"))  ("GI"  . ("Ι"))
      ("Gk"  . ("κ"))  ("GK"  . ("Κ"))
      ("Gl"  . ("λ"))  ("GL"  . ("Λ"))  ("Gl-" . ("ƛ"))
      ("Gm"  . ("μ"))  ("GM"  . ("Μ"))
      ("Gn"  . ("ν"))  ("GN"  . ("Ν"))
      ("Gx"  . ("ξ"))  ("GX"  . ("Ξ"))
      ;; \omicron \Omicron
      ;; \pi \Pi
      ("Gr"  . ("ρ"))  ("GR"  . ("Ρ"))
      ("Gs"  . ("σ"))  ("GS"  . ("Σ"))
      ("Gt"  . ("τ"))  ("GT"  . ("Τ"))
      ("Gu"  . ("υ"))  ("GU"  . ("Υ"))
      ("Gf"  . ("φ"))  ("GF"  . ("Φ"))
      ("Gc"  . ("χ"))  ("GC"  . ("Χ"))
      ("Gp"  . ("ψ"))  ("GP"  . ("Ψ"))
      ("Go"  . ("ω"))  ("GO"  . ("Ω"))

      ;; Mathematical characters

      ("MiA" . ("𝐴"))
      ("MiB" . ("𝐵"))
      ("MiC" . ("𝐶"))
      ("MiD" . ("𝐷"))
      ("MiE" . ("𝐸"))
      ("MiF" . ("𝐹"))
      ("MiG" . ("𝐺"))
      ("MiH" . ("𝐻"))
      ("MiI" . ("𝐼"))
      ("MiJ" . ("𝐽"))
      ("MiK" . ("𝐾"))
      ("MiL" . ("𝐿"))
      ("MiM" . ("𝑀"))
      ("MiN" . ("𝑁"))
      ("MiO" . ("𝑂"))
      ("MiP" . ("𝑃"))
      ("MiQ" . ("𝑄"))
      ("MiR" . ("𝑅"))
      ("MiS" . ("𝑆"))
      ("MiT" . ("𝑇"))
      ("MiU" . ("𝑈"))
      ("MiV" . ("𝑉"))
      ("MiW" . ("𝑊"))
      ("MiX" . ("𝑋"))
      ("MiY" . ("𝑌"))
      ("MiZ" . ("𝑍"))
      ("Mia" . ("𝑎"))
      ("Mib" . ("𝑏"))
      ("Mic" . ("𝑐"))
      ("Mid" . ("𝑑"))
      ("Mie" . ("𝑒"))
      ("Mif" . ("𝑓"))
      ("Mig" . ("𝑔"))
      ("Mih" . ("ℎ"))
      ("Mii" . ("𝑖"))
      ("Mij" . ("𝑗"))
      ("Mik" . ("𝑘"))
      ("Mil" . ("𝑙"))
      ("Mim" . ("𝑚"))
      ("Min" . ("𝑛"))
      ("Mio" . ("𝑜"))
      ("Mip" . ("𝑝"))
      ("Miq" . ("𝑞"))
      ("Mir" . ("𝑟"))
      ("Mis" . ("𝑠"))
      ("Mit" . ("𝑡"))
      ("Miu" . ("𝑢"))
      ("Miv" . ("𝑣"))
      ("Miw" . ("𝑤"))
      ("Mix" . ("𝑥"))
      ("Miy" . ("𝑦"))
      ("Miz" . ("𝑧"))
      ("MIA" . ("𝑨"))
      ("MIB" . ("𝑩"))
      ("MIC" . ("𝑪"))
      ("MID" . ("𝑫"))
      ("MIE" . ("𝑬"))
      ("MIF" . ("𝑭"))
      ("MIG" . ("𝑮"))
      ("MIH" . ("𝑯"))
      ("MII" . ("𝑰"))
      ("MIJ" . ("𝑱"))
      ("MIK" . ("𝑲"))
      ("MIL" . ("𝑳"))
      ("MIM" . ("𝑴"))
      ("MIN" . ("𝑵"))
      ("MIO" . ("𝑶"))
      ("MIP" . ("𝑷"))
      ("MIQ" . ("𝑸"))
      ("MIR" . ("𝑹"))
      ("MIS" . ("𝑺"))
      ("MIT" . ("𝑻"))
      ("MIU" . ("𝑼"))
      ("MIV" . ("𝑽"))
      ("MIW" . ("𝑾"))
      ("MIX" . ("𝑿"))
      ("MIY" . ("𝒀"))
      ("MIZ" . ("𝒁"))
      ("MIa" . ("𝒂"))
      ("MIb" . ("𝒃"))
      ("MIc" . ("𝒄"))
      ("MId" . ("𝒅"))
      ("MIe" . ("𝒆"))
      ("MIf" . ("𝒇"))
      ("MIg" . ("𝒈"))
      ("MIh" . ("𝒉"))
      ("MIi" . ("𝒊"))
      ("MIj" . ("𝒋"))
      ("MIk" . ("𝒌"))
      ("MIl" . ("𝒍"))
      ("MIm" . ("𝒎"))
      ("MIn" . ("𝒏"))
      ("MIo" . ("𝒐"))
      ("MIp" . ("𝒑"))
      ("MIq" . ("𝒒"))
      ("MIr" . ("𝒓"))
      ("MIs" . ("𝒔"))
      ("MIt" . ("𝒕"))
      ("MIu" . ("𝒖"))
      ("MIv" . ("𝒗"))
      ("MIw" . ("𝒘"))
      ("MIx" . ("𝒙"))
      ("MIy" . ("𝒚"))
      ("MIz" . ("𝒛"))
      ("McA" . ("𝒜"))
      ("McB" . ("ℬ"))
      ("McC" . ("𝒞"))
      ("McD" . ("𝒟"))
      ("McE" . ("ℰ"))
      ("McF" . ("ℱ"))
      ("McG" . ("𝒢"))
      ("McH" . ("ℋ"))
      ("McI" . ("ℐ"))
      ("McJ" . ("𝒥"))
      ("McK" . ("𝒦"))
      ("McL" . ("ℒ"))
      ("McM" . ("ℳ"))
      ("McN" . ("𝒩"))
      ("McO" . ("𝒪"))
      ("McP" . ("𝒫"))
      ("McQ" . ("𝒬"))
      ("McR" . ("ℛ"))
      ("McS" . ("𝒮"))
      ("McT" . ("𝒯"))
      ("McU" . ("𝒰"))
      ("McV" . ("𝒱"))
      ("McW" . ("𝒲"))
      ("McX" . ("𝒳"))
      ("McY" . ("𝒴"))
      ("McZ" . ("𝒵"))
      ("Mca" . ("𝒶"))
      ("Mcb" . ("𝒷"))
      ("Mcc" . ("𝒸"))
      ("Mcd" . ("𝒹"))
      ("Mce" . ("ℯ"))
      ("Mcf" . ("𝒻"))
      ("Mcg" . ("ℊ"))
      ("Mch" . ("𝒽"))
      ("Mci" . ("𝒾"))
      ("Mcj" . ("𝒿"))
      ("Mck" . ("𝓀"))
      ("Mcl" . ("𝓁"))
      ("Mcm" . ("𝓂"))
      ("Mcn" . ("𝓃"))
      ("Mco" . ("ℴ"))
      ("Mcp" . ("𝓅"))
      ("Mcq" . ("𝓆"))
      ("Mcr" . ("𝓇"))
      ("Mcs" . ("𝓈"))
      ("Mct" . ("𝓉"))
      ("Mcu" . ("𝓊"))
      ("Mcv" . ("𝓋"))
      ("Mcw" . ("𝓌"))
      ("Mcx" . ("𝓍"))
      ("Mcy" . ("𝓎"))
      ("Mcz" . ("𝓏"))
      ("MCA" . ("𝓐"))
      ("MCB" . ("𝓑"))
      ("MCC" . ("𝓒"))
      ("MCD" . ("𝓓"))
      ("MCE" . ("𝓔"))
      ("MCF" . ("𝓕"))
      ("MCG" . ("𝓖"))
      ("MCH" . ("𝓗"))
      ("MCI" . ("𝓘"))
      ("MCJ" . ("𝓙"))
      ("MCK" . ("𝓚"))
      ("MCL" . ("𝓛"))
      ("MCM" . ("𝓜"))
      ("MCN" . ("𝓝"))
      ("MCO" . ("𝓞"))
      ("MCP" . ("𝓟"))
      ("MCQ" . ("𝓠"))
      ("MCR" . ("𝓡"))
      ("MCS" . ("𝓢"))
      ("MCT" . ("𝓣"))
      ("MCU" . ("𝓤"))
      ("MCV" . ("𝓥"))
      ("MCW" . ("𝓦"))
      ("MCX" . ("𝓧"))
      ("MCY" . ("𝓨"))
      ("MCZ" . ("𝓩"))
      ("MCa" . ("𝓪"))
      ("MCb" . ("𝓫"))
      ("MCc" . ("𝓬"))
      ("MCd" . ("𝓭"))
      ("MCe" . ("𝓮"))
      ("MCf" . ("𝓯"))
      ("MCg" . ("𝓰"))
      ("MCh" . ("𝓱"))
      ("MCi" . ("𝓲"))
      ("MCj" . ("𝓳"))
      ("MCk" . ("𝓴"))
      ("MCl" . ("𝓵"))
      ("MCm" . ("𝓶"))
      ("MCn" . ("𝓷"))
      ("MCo" . ("𝓸"))
      ("MCp" . ("𝓹"))
      ("MCq" . ("𝓺"))
      ("MCr" . ("𝓻"))
      ("MCs" . ("𝓼"))
      ("MCt" . ("𝓽"))
      ("MCu" . ("𝓾"))
      ("MCv" . ("𝓿"))
      ("MCw" . ("𝔀"))
      ("MCx" . ("𝔁"))
      ("MCy" . ("𝔂"))
      ("MCz" . ("𝔃"))
      ("MfA" . ("𝔄"))
      ("MfB" . ("𝔅"))
      ("MfC" . ("ℭ"))
      ("MfD" . ("𝔇"))
      ("MfE" . ("𝔈"))
      ("MfF" . ("𝔉"))
      ("MfG" . ("𝔊"))
      ("MfH" . ("ℌ"))
      ("MfI" . ("ℑ"))
      ("MfJ" . ("𝔍"))
      ("MfK" . ("𝔎"))
      ("MfL" . ("𝔏"))
      ("MfM" . ("𝔐"))
      ("MfN" . ("𝔑"))
      ("MfO" . ("𝔒"))
      ("MfP" . ("𝔓"))
      ("MfQ" . ("𝔔"))
      ("MfR" . ("ℜ"))
      ("MfS" . ("𝔖"))
      ("MfT" . ("𝔗"))
      ("MfU" . ("𝔘"))
      ("MfV" . ("𝔙"))
      ("MfW" . ("𝔚"))
      ("MfX" . ("𝔛"))
      ("MfY" . ("𝔜"))
      ("MfZ" . ("ℨ"))
      ("Mfa" . ("𝔞"))
      ("Mfb" . ("𝔟"))
      ("Mfc" . ("𝔠"))
      ("Mfd" . ("𝔡"))
      ("Mfe" . ("𝔢"))
      ("Mff" . ("𝔣"))
      ("Mfg" . ("𝔤"))
      ("Mfh" . ("𝔥"))
      ("Mfi" . ("𝔦"))
      ("Mfj" . ("𝔧"))
      ("Mfk" . ("𝔨"))
      ("Mfl" . ("𝔩"))
      ("Mfm" . ("𝔪"))
      ("Mfn" . ("𝔫"))
      ("Mfo" . ("𝔬"))
      ("Mfp" . ("𝔭"))
      ("Mfq" . ("𝔮"))
      ("Mfr" . ("𝔯"))
      ("Mfs" . ("𝔰"))
      ("Mft" . ("𝔱"))
      ("Mfu" . ("𝔲"))
      ("Mfv" . ("𝔳"))
      ("Mfw" . ("𝔴"))
      ("Mfx" . ("𝔵"))
      ("Mfy" . ("𝔶"))
      ("Mfz" . ("𝔷"))

      ;; (Sub / Super) scripts
      ;;
      ;; Unicode 12.1 omits several latin characters from sub/superscript.
      ;; https://www.quora.com/Why-is-there-no-character-for-superscript-q-in-Unicode
      ;;
      ;; Perhaps they will be added in future versions, however there are no
      ;; proposals for it currently in the pipeline:
      ;; https://www.unicode.org/alloc/Pipeline.html

      ("_a" . ("ₐ"))
      ;; ("_b" . ("b"))
      ;; ("_c" . ("c"))
      ;; ("_d" . ("d"))
      ("_e" . ("ₑ"))
      ;; ("_f" . ("f"))
      ;; ("_g" . ("g"))
      ("_h" . ("ₕ"))
      ("_i" . ("ᵢ"))
      ("_j" . ("ⱼ"))
      ("_k" . ("ₖ"))
      ("_l" . ("ₗ"))
      ("_m" . ("ₘ"))
      ("_n" . ("ₙ"))
      ("_o" . ("ₒ"))
      ("_p" . ("ₚ"))
      ;; ("_q" . ("q"))
      ("_r" . ("ᵣ"))
      ("_s" . ("ₛ"))
      ("_t" . ("ₜ"))
      ("_u" . ("ᵤ"))
      ("_v" . ("ᵥ"))
      ;; ("_w" . ("w"))
      ("_x" . ("ₓ"))
      ;; ("_y" . ("y"))
      ;; ("_z" . ("z"))

      ("_Gb" . ("ᵦ"))
      ("_Gg" . ("ᵧ"))
      ("_Gr" . ("ᵨ"))
      ("_Gf" . ("ᵩ"))
      ("_Gc" . ("ᵪ"))

      ("^a" . ("ᵃ"))
      ("^b" . ("ᵇ"))
      ("^c" . ("ᶜ"))
      ("^d" . ("ᵈ"))
      ("^e" . ("ᵉ"))
      ("^f" . ("ᶠ"))
      ("^g" . ("ᵍ"))
      ("^h" . ("ʰ"))
      ("^i" . ("ⁱ"))
      ("^j" . ("ʲ"))
      ("^k" . ("ᵏ"))
      ("^l" . ("ˡ"))
      ("^m" . ("ᵐ"))
      ("^n" . ("ⁿ"))
      ("^o" . ("ᵒ"))
      ("^p" . ("ᵖ"))
      ;; ("^q" . ("q"))
      ("^r" . ("ʳ"))
      ("^s" . ("ˢ"))
      ("^t" . ("ᵗ"))
      ("^u" . ("ᵘ"))
      ("^v" . ("ᵛ"))
      ("^w" . ("ʷ"))
      ("^x" . ("ˣ"))
      ("^y" . ("ʸ"))
      ("^z" . ("ᶻ"))

      ("^A" . ("ᴬ"))
      ("^B" . ("ᴮ"))
      ;; ("^C" . ("C"))
      ("^D" . ("ᴰ"))
      ("^E" . ("ᴱ"))
      ;; ("^F" . ("F"))
      ("^G" . ("ᴳ"))
      ("^H" . ("ᴴ"))
      ("^I" . ("ᴵ"))
      ("^J" . ("ᴶ"))
      ("^K" . ("ᴷ"))
      ("^L" . ("ᴸ"))
      ("^M" . ("ᴹ"))
      ("^N" . ("ᴺ"))
      ("^O" . ("ᴼ"))
      ("^P" . ("ᴾ"))
      ;; ("^Q" . ("Q"))
      ("^R" . ("ᴿ"))
      ;; ("^S" . ("S"))
      ("^T" . ("ᵀ"))
      ("^U" . ("ᵁ"))
      ("^V" . ("ⱽ"))
      ("^W" . ("ᵂ"))
      ;; ("^X" . ("X"))
      ;; ("^Y" . ("Y"))
      ;; ("^Z" . ("Z"))

      ("^Gb" . ("ᵝ"))
      ("^Gg" . ("ᵞ"))
      ("^Gd" . ("ᵟ"))
      ("^Ge" . ("ᵋ"))
      ("^Gth" . ("ᶿ"))
      ("^Gf" . ("ᵠ"))
      ("^Gc" . ("ᵡ"))

      ;; Some ISO8859-1 characters.

      (" "         . (" "))
      ("!"         . ("¡"))
      ("cent"      . ("¢"))
      ("brokenbar" . ("¦"))
      ("degree"    . ("°"))
      ("?"         . ("¿"))
      ("^a_"       . ("ª"))
      ("^o_"       . ("º"))

      ;; Circled, parenthesised etc. numbers and letters.

      ( "(0)" . ,(racket-input-to-string-list " ⓪🄀⓿🄋🄌"))
      ( "(1)" . ,(racket-input-to-string-list "⑴①⒈❶➀➊"))
      ( "(2)" . ,(racket-input-to-string-list "⑵②⒉❷➁➋"))
      ( "(3)" . ,(racket-input-to-string-list "⑶③⒊❸➂➌"))
      ( "(4)" . ,(racket-input-to-string-list "⑷④⒋❹➃➍"))
      ( "(5)" . ,(racket-input-to-string-list "⑸⑤⒌❺➄➎"))
      ( "(6)" . ,(racket-input-to-string-list "⑹⑥⒍❻➅➏"))
      ( "(7)" . ,(racket-input-to-string-list "⑺⑦⒎❼➆➐"))
      ( "(8)" . ,(racket-input-to-string-list "⑻⑧⒏❽➇➑"))
      ( "(9)" . ,(racket-input-to-string-list "⑼⑨⒐❾➈➒"))
      ("(10)" . ,(racket-input-to-string-list "⑽⑩⒑❿➉➓"))
      ("(11)" . ,(racket-input-to-string-list "⑾⑪⒒⓫"))
      ("(12)" . ,(racket-input-to-string-list "⑿⑫⒓⓬"))
      ("(13)" . ,(racket-input-to-string-list "⒀⑬⒔⓭"))
      ("(14)" . ,(racket-input-to-string-list "⒁⑭⒕⓮"))
      ("(15)" . ,(racket-input-to-string-list "⒂⑮⒖⓯"))
      ("(16)" . ,(racket-input-to-string-list "⒃⑯⒗⓰"))
      ("(17)" . ,(racket-input-to-string-list "⒄⑰⒘⓱"))
      ("(18)" . ,(racket-input-to-string-list "⒅⑱⒙⓲"))
      ("(19)" . ,(racket-input-to-string-list "⒆⑲⒚⓳"))
      ("(20)" . ,(racket-input-to-string-list "⒇⑳⒛⓴"))

      ("(a)"  . ,(racket-input-to-string-list "⒜Ⓐⓐ🅐🄰🅰"))
      ("(b)"  . ,(racket-input-to-string-list "⒝Ⓑⓑ🅑🄱🅱"))
      ("(c)"  . ,(racket-input-to-string-list "⒞Ⓒⓒ🅒🄲🅲"))
      ("(d)"  . ,(racket-input-to-string-list "⒟Ⓓⓓ🅓🄳🅳"))
      ("(e)"  . ,(racket-input-to-string-list "⒠Ⓔⓔ🅔🄴🅴"))
      ("(f)"  . ,(racket-input-to-string-list "⒡Ⓕⓕ🅕🄵🅵"))
      ("(g)"  . ,(racket-input-to-string-list "⒢Ⓖⓖ🅖🄶🅶"))
      ("(h)"  . ,(racket-input-to-string-list "⒣Ⓗⓗ🅗🄷🅷"))
      ("(i)"  . ,(racket-input-to-string-list "⒤Ⓘⓘ🅘🄸🅸"))
      ("(j)"  . ,(racket-input-to-string-list "⒥Ⓙⓙ🅙🄹🅹"))
      ("(k)"  . ,(racket-input-to-string-list "⒦Ⓚⓚ🅚🄺🅺"))
      ("(l)"  . ,(racket-input-to-string-list "⒧Ⓛⓛ🅛🄻🅻"))
      ("(m)"  . ,(racket-input-to-string-list "⒨Ⓜⓜ🅜🄼🅼"))
      ("(n)"  . ,(racket-input-to-string-list "⒩Ⓝⓝ🅝🄽🅽"))
      ("(o)"  . ,(racket-input-to-string-list "⒪Ⓞⓞ🅞🄾🅾"))
      ("(p)"  . ,(racket-input-to-string-list "⒫Ⓟⓟ🅟🄿🅿"))
      ("(q)"  . ,(racket-input-to-string-list "⒬Ⓠⓠ🅠🅀🆀"))
      ("(r)"  . ,(racket-input-to-string-list "⒭Ⓡⓡ🅡🅁🆁"))
      ("(s)"  . ,(racket-input-to-string-list "⒮Ⓢⓢ🅢🅂🆂"))
      ("(t)"  . ,(racket-input-to-string-list "⒯Ⓣⓣ🅣🅃🆃"))
      ("(u)"  . ,(racket-input-to-string-list "⒰Ⓤⓤ🅤🅄🆄"))
      ("(v)"  . ,(racket-input-to-string-list "⒱Ⓥⓥ🅥🅅🆅"))
      ("(w)"  . ,(racket-input-to-string-list "⒲Ⓦⓦ🅦🅆🆆"))
      ("(x)"  . ,(racket-input-to-string-list "⒳Ⓧⓧ🅧🅇🆇"))
      ("(y)"  . ,(racket-input-to-string-list "⒴Ⓨⓨ🅨🅈🆈"))
      ("(z)"  . ,(racket-input-to-string-list "⒵Ⓩⓩ🅩🅉🆉"))

      ))
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
    (quail-define-package "Racket" "UTF-8" "∏" t ; guidance
     "Racket input method.
The purpose of this input method is to edit Racket programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

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

(provide 'racket-input)

;;; racket-input.el ends here
