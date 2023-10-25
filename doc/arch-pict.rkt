;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require pict
         pict/color
         (only-in racket/draw make-color))

(define pipe-color "blue")
(define ssh-color "purple")

(define host-color      (make-color 0 0 0 0.0))
(define front-end-color (make-color #xF0 #xF7 #xF0 1.0))
(define back-end-color  (make-color #xF7 #xFF #xF7 1.0))

(define (background #:color color p)
  (cc-superimpose (filled-rectangle #:color color
                                    (pict-width p)
                                    (pict-height p))
                  p))

;; Simplify usage of raw `frame` and `inset`. Nicer to supply as
;; keyword arg prefixes rather than suffixes. Also handle common case
;; of (inset (frame (inset __))). Also add background color.
(define (box #:inset [in #f]
             #:outset [out #f]
             #:color [color #f]
             #:background [bg #f]
             #:segment [segment #f]
             #:width [width #f]
             p)
  (let* ([p (if in (inset p in) p)]
         [p (if bg (background #:color bg p) p)]
         [p (frame #:color color
                   #:segment segment
                   #:line-width width
                   p)]
         [p (if out (inset p out) p)])
    p))

(define (front-end)
  (box
   #:inset 5
   #:outset 5
   #:background front-end-color
   (vl-append
    (text "Emacs front end" '(bold))
    (hc-append
     (text "Command requests/responses and notifications via ")
     (colorize (text "pipe" '(bold)) pipe-color)
     (text " or ")
     (colorize (text "ssh" '(bold)) ssh-color)
     (text ".")))))

(define (backend path)
  (define i/o-color (if (regexp-match? #rx"^/[^:]+:" path) ssh-color pipe-color))
  (box
   #:inset 5
   #:color (light "black")
   #:background back-end-color
   (vc-append
    5
    (text "Racket back end process" '(bold))
    (box #:color (light "black") #:background (light "black")
         #:inset 2 #:outset 6
         (colorize (text path '(bold . modern)) "white"))
    (ht-append
     10
     (colorize
      (box #:inset 5 (text "Commands"))
      i/o-color)
     (vl-append
      4
      (colorize (box #:inset 2 (text "REPL 1"))
                i/o-color)
      (colorize (box #:inset 2 (text "REPL 2"))
                i/o-color)
      (colorize (box #:inset 2
                     #:segment 2
                     (text "REPL n" '(italic)))
                i/o-color))))))

(define (back-end-source-files)
  (box
   #:outset 2
   #:inset 2
   #:color (light "gray")
   #:background (light "gray")
   (text "/tmp/racket-mode-back-end/*.rkt" 'modern 10)))

(define (host name . paths)
  (box
   #:inset 5
   #:color "gray"
   #:width 2
   #:background host-color
   (vc-append
    5
    (box #:inset 2
         #:background "black"
         (colorize (text name '(bold . modern) 14) "white"))
    (if (equal? name "localhost")
        (front-end)
        (back-end-source-files))
    (inset (apply hc-append 10 (map backend paths))
           5))))

;; (host "localhost" "/")
;; (host "localhost" "/" "/path/to/project")

(define (scenario local . remotes)
  (inset
   (ht-append
    10
    (apply host local)
    (apply vl-append
           10
           (for/list ([remote remotes])
             (apply host remote))))
   10))

(define images
  (list
   (scenario '("localhost" "/"))
   (scenario '("localhost" "/" "/path/to/project/"))
   (scenario '("localhost" "/" "/path/to/project/")
             '("remote" "/user@remote:/"))
   (scenario '("localhost" "/" "/path/to/project/")
             '("remote" "/user@remote:/" "/user@remote:/path/"))
   (scenario '("localhost" "/" "/path/to/project/")
             '("alpha" "/user@alpha:/" "/user@alpha:/path/")
             '("bravo" "/user@bravo:/" "/user@bravo:/path/"))))

(module+ interactive
  images)

(module+ main
  (require file/convertible)
  (for ([(image n) (in-indexed images)])
    (with-output-to-file
      (format "scenario-~a.svg" n)
      #:exists 'replace
      #:mode 'binary      (λ ()  (display (convert image 'svg-bytes))))))
