;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;; Provenance:
;;
;;  https://gist.githubusercontent.com/mflatt/6ab71f8214c5fd98dae98c8531056fa2/raw/b407e8ff41d37b5a992fc7a516d6dae0459b694a/text-lines.rkt
;;
;;  https://github.com/racket/expeditor/issues/10#issuecomment-974146291.

(provide text-lines?
         (rename-out [empty empty-text-lines])

         ;; 0 is the position before everything, and the position
         ;; after a newline is on the subsequent line

         text-length         ; t -> position at end
         insert              ; t position str -> t, detecting "\n"
         delete              ; t start-position end-position -> t
         get-text            ; t start-position end-position -> string
         open-input-text     ; t start-position -> input-port

         position->start     ; t position -> position of line start
         position->line      ; t position -> line number
         line->start         ; t line number -> position

         text-line-count)    ; t -> one more than line number at end

;; ----------------------------------------

(define (insert t pos str)
  (check-in-range 'insert t pos)
  (let loop ([t t] [i 0] [pos pos] [accum 0])
    (cond
      [(= i (string-length str))
       (if (zero? accum)
           t
           (adjust-within-line t pos (substring str (- i accum) i)))]
      [(char=? #\newline (string-ref str i))
       (define len (add1 accum))
       (loop (insert-newline t pos (substring str (- i accum) i))
             (add1 i)
             (+ pos len)
             0)]
      [else (loop t (add1 i) pos (add1 accum))])))

(define (delete t pos end)
  (check-in-range 'delete t pos)
  (check-in-range 'delete t end)
  (let delete ([t t] [pos pos] [end end])
    (cond
      [(= pos end) t]
      [else
       (define-values (left-len left-count sub-t) (find-line t pos))
       (define line-start (+ left-len (node-left-len sub-t)))
       (define line-len (node-len sub-t))
       (define line-end (+ line-start line-len))
       (cond
         [(end . < . line-end)
          (adjust-within-line t pos (- pos end))]
         [(= pos line-start)
          (define new-t (delete-line t line-start))
          (delete new-t line-start (- end line-len))]
         [else
          (define keep (substring (node-content sub-t) 0 (- pos line-start)))
          (define new-t (delete t line-start end))
          (insert new-t line-start keep)])])))

(define (get-text t pos [end (text-length t)])
  (check-in-range 'get-text t pos)
  (check-in-range 'get-text t end)
  (define str
    (let loop ([pos pos] [end end])
      (define-values (left-len left-count sub-t) (find-line t pos))
      (define line-start (+ left-len (node-left-len sub-t)))
      (define line-len (node-len sub-t))
      (define line-end (+ line-start line-len))
      (cond
        [(<= end line-end)
         (define rel-pos (- pos line-start))
         (cond
           [(= end line-end)
            (string-append (substring (node-content sub-t) rel-pos) "\n")]
           [else
            (define rel-end (- end line-start))
            (substring (node-content sub-t) rel-pos rel-end)])]
        [else
         (define pre-str (loop pos line-end))
         (define post-strs (loop line-end end))
         (cons pre-str
               (if (pair? post-strs)
                   post-strs
                   (list post-strs)))])))
  (if (string? str)
      str
      (apply string-append str)))

;; ----------------------------------------

;; a node represents one text line that ends with a newline
(struct node (content      ; characters in this line, excluding ending newline
              left-len     ; characters in left subtree
              total-len    ; total in both subtrees
              left-count   ; number of lines in left subtree
              total-count  ; total number of lines
              height       ; head of tree (for balancing)
              left         ; left subtree
              right)       ; right subtree
  #:transparent
  #:authentic
  #:reflection-name 'lines-of-text)

(define (text-lines? v) (node? v))

(define (node-len n)
  (content-len (node-content n)))

(define (content-len content)
  (add1 (string-length content)))

(define (text-length n)
  (sub1 (node-total-len n)))

(define (text-line-count n)
  (node-total-count n))

;; represent an editor with a sentinel newline, but hide
;; its existence to the outside
(define empty (node "" 0 1 0 1 0 #f #f))

(define (check-in-range* who t pos limit what)
  (unless (node? t)
    (raise-argument-error who "text-lines?" t))
  (unless (exact-nonnegative-integer? pos)
    (raise-argument-error who "exact-nonnegative-integer?" pos))
  (unless (pos . < . limit)
    (raise-arguments-error who
                           (format "~a is out of bounds" what)
                           what pos
                           "upper limit" (sub1 limit))))

(define (check-in-range who t pos)
  (check-in-range* who t pos (node-total-len t) "position"))

(define (check-in-range-line who t line)
  (check-in-range* who t line (add1 (node-total-count t)) "line"))

;; ----------------------------------------

(define (tree-height t)
  (cond
    [(not t) 0]
    [else (node-height t)]))

(define (tree-total-len t)
  (cond
    [(not t) 0]
    [else (node-total-len t)]))

(define (tree-total-count t)
  (cond
    [(not t) 0]
    [else (node-total-count t)]))

;; ----------------------------------------

(define (combine content left right)
  (node content
        (tree-total-len left)
        (+ (content-len content) (tree-total-len left) (tree-total-len right))
        (tree-total-count left)
        (+ 1 (tree-total-count left) (tree-total-count right))
        (+ 1 (max (tree-height left) (tree-height right)))
        left
        right))

(define (reverse-combine content right left)
  (combine content left right))

;; ----------------------------------------

(define (position->start t pos)
  (check-in-range 'position->start t pos)
  (define-values (left-len left-count sub-t) (find-line t pos))
  (+ left-len (node-left-len sub-t)))

;; ----------------------------------------

(define (position->line t pos)
  (check-in-range 'position->line t pos)
  (define-values (left-len left-count sub-t) (find-line t pos))
  (+ left-count (node-left-count sub-t)))

;; ----------------------------------------

(define (find-line t pos)
  (cond
    [(< pos (node-left-len t))
     (find-line (node-left t) pos)]
    [else
     (define right-left-len (+ (node-len t) (node-left-len t)))
     (define new-pos (- pos right-left-len))
     (cond
       [(new-pos . < . 0) (values 0 0 t)]
       [else
        (define right-left-count (+ 1 (node-left-count t)))
        (define-values (left-len left-count sub-t) (find-line (node-right t) new-pos))
        (values (+ left-len right-left-len)
                (+ left-count right-left-count)
                sub-t)])]))

;; ----------------------------------------

(define (line->start t line)
  (check-in-range-line 'line->start t line)
  (find-start t line))

(define (find-start t line)
  (define here (node-left-count t))
  (cond
    [(line . < . here)
     (find-start (node-left t) line)]
    [(line . > . here)
     (define pre (+ (node-left-len t) (node-len t)))
     (+ (find-start (node-right t) (- line here 1))
        pre)]
    [else
     (node-left-len t)]))

;; ----------------------------------------

(define (adjust-within-line t pos amt) ; amt is string or negative number
  (check-in-range 'adjust-within-line t pos)
  (unless (or (string? amt)
              (and (exact-integer? amt) (negative? amt)))
    (raise-argument-error 'adjust-within-line "(or string? (and/c exact-integer? negative?))" amt))
  (when (exact-integer? amt)
    (define-values (left-len left-count line-t) (find-line t pos))
    (when ((+ left-len (node-left-len line-t) (node-len line-t))
           . <= .
           (+ pos amt))
      (raise-arguments-error 'adjust-within-line
                             "subtracting too much"
                             "amount" amt)))
  (adjust t pos amt))

(define (adjust t pos amt)
  (define rel-pos (- pos (node-left-len t)))
  (cond
    [(rel-pos . < . 0)
     (combine (node-content t)
              (adjust (node-left t) pos amt)
              (node-right t))]
    [(rel-pos . >= . (node-len t))
     (define new-pos (- rel-pos (node-len t)))
     (combine (node-content t)
              (node-left t)
              (adjust (node-right t) new-pos amt))]
    [else
     (combine (cond
                [(string? amt) (string-append (substring (node-content t) 0 rel-pos)
                                              amt
                                              (substring (node-content t) rel-pos))]
                [else (string-append (substring (node-content t) 0 rel-pos)
                                     (substring (node-content t) (- rel-pos amt)))])
              (node-left t)
              (node-right t))]))

;; ----------------------------------------

;; inserts `len` characters that end with a newline
(define (insert-newline t pos content)
  (check-in-range 'insert-newline t pos)
  (unless (string? content)
    (raise-argument-error 'insert-newline "string?" content))
  (define-values (left-len left-count sub-t) (find-line t pos))
  (define start (+ left-len (node-left-len sub-t)))
  (define delta (- pos start))
  (cond
    [(zero? delta)
     ;; insert new line before existing one
     (insert-line t pos content)]
    [else
     ;; split node by first shrinking, then insert
     (define pre (substring (node-content sub-t) 0 delta))
     (insert-line (adjust-within-line t start (- delta))
                  start
                  (string-append pre content))]))

;; ----------------------------------------

(define (delete-line t pos)
  (check-in-range 'delete-line t pos)
  ;; sanity check:
  (define start (position->start t pos))
  (unless (= start pos)
    (error 'delete-line "line does not start at position"))
  (delete-node t pos))

;; ----------------------------------------

(define (insert-line t pos str)
  (cond
    [(not t) (combine str #f #f)]
    [(<= pos (node-left-len t))
     (insert-to t pos str
                node-left
                node-right
                combine
                rotate-right)]
    [else
     (define right-left-len (+ (node-len t) (node-left-len t)))
     (when (pos . < . right-left-len)
       (error "insert-line cannot insert into the middle"))
     (insert-to t (- pos right-left-len) str
                node-right
                node-left
                reverse-combine
                rotate-left)]))

;; Like insert, but inserts to a child, where `node-to'
;; determines the side where the child is added,`node-other'
;; is the other side, and `comb' builds the new tree gven the
;; two new children.
(define-syntax-rule (insert-to t new-pos new-content node-to node-other comb rotate)
  (begin
    ;; Insert into the `node-to' child:
    (define new-to (insert-line (node-to t) new-pos new-content))
    (define new-other (node-other t))

    (define new-t (comb (node-content t) new-to new-other))

    ;; Check for rotation:
    (define to-height (tree-height new-to))
    (define other-height (tree-height new-other))
    (if ((- to-height other-height) . = . 2)
        (rotate new-t)
        new-t)))

(define (delete-node t pos)
  (define key (node-left-len t))
  (cond
    [(pos . < . key)
     (delete-from t pos
                  node-left
                  node-right
                  combine
                  rotate-left)]
    [(not (= pos key))
     (delete-from t (- pos key (node-len t))
                  node-right
                  node-left
                  reverse-combine
                  rotate-right)]
    [else
     (define l (node-left t))
     (define r (node-right t))
     (cond
       [(not l) r]
       [(not r) l]
       [else
        (delete-here t)])]))

(define-syntax-rule (delete-from t pos node-to node-other comb rotate)
  (begin
    ;; Delete from the `node-to' child:
    (define new-to (delete-node (node-to t) pos))
    (define new-other (node-other t))

    (define new-t (comb (node-content t) new-to new-other))

    ;; Check for rotation:
    (define to-height (tree-height new-to))
    (define other-height (tree-height new-other))
    (if ((- to-height other-height) . = . -2)
        (rotate new-t)
        new-t)))

(define-syntax-rule (delete-here t)
  (begin
    ;; Delete by moving from `from` to `other`
    (define from (node-left t))
    (define new-t
      (let loop ([end from] [left-len 0])
        (cond
          [(node-right end)
           => (lambda (e) (loop e (+ left-len (node-left-len end) (node-len end))))]
          [else
           (define pos (node-left-len end))
           (define new-from (delete-node from (+ pos left-len)))
           (combine (node-content end) new-from (node-right t))])))

    ;; Check for rotation:
    (define from-height (tree-height (node-left new-t)))
    (define other-height (tree-height (node-right new-t)))
    (if ((- from-height other-height) . = . -2)
        (rotate-left new-t)
        new-t)))

(define-syntax-rule (define-rotate rotate node-to node-other comb)
  (begin
    ;; Helper rotate function:
    (define (rotate t)
      (define to (node-to t))
      (define to-balance (- (tree-height (node-to to))
                            (tree-height (node-other to))))
      (cond
        [(to-balance . < . 0)
         (double-rotate t)]
        [else
         (single-rotate t)]))

    ;; Helper double-rotate function:
    (define (double-rotate t)
      (define orange (node-to t))
      (define yellow (node-other orange))
      (define A (node-to orange))
      (define B (node-to yellow))
      (define C (node-other yellow))
      (define D (node-other t))
      (single-rotate (comb (node-content t)
                           (comb (node-content yellow)
                                 (comb (node-content orange)
                                       A
                                       B)
                                 C)
                           D)))

    ;; Helper single-rotate function:
    (define (single-rotate t)
      (define yellow (node-to t))
      (comb (node-content yellow)
            (node-to yellow)
            (comb (node-content t)
                  (node-other yellow)
                  (node-other t))))))

(define-rotate rotate-right node-left node-right combine)
(define-rotate rotate-left node-right node-left reverse-combine)

;; ----------------------------------------

(module+ main
  (define (do-check av bv form)
    (unless (equal? av bv)
      (error 'fail "~s: ~v vs. ~v" form av bv)))
  (define-syntax-rule (check a b)
    (do-check a b '(check a b)))
  (define (at desc) (printf "~a\n" desc))

  (at "empty")
  (check (position->start empty 0) 0)
  (check (position->line empty 0) 0)
  (check (text-length empty) 0)
  (check (text-line-count empty) 1)

  (at "insert within only line")
  (let* ([t (adjust-within-line empty 0 "xxx")])
    (check (get-text t 0 3) "xxx")
    (check (text-length t) 3)
    (check (text-line-count t) 1)
    (check (position->start t 0) 0)
    (check (position->line t 0) 0)
    (check (line->start t 0) 0)
    (check (position->start t 2) 0)
    (check (position->line t 2) 0)
    (check (position->start t 3) 0)
    (check (position->line t 3) 0))

  (at "insert line 1")
  (let* ([t (insert-newline empty 0 "")])
    ;; "|", where "|" means newline
    (check (get-text t 0 1) "\n")
    (check (text-length t) 1)
    (check (text-line-count t) 2)
    (check (position->start t 0) 0)
    (check (position->line t 0) 0)
    (check (position->start t 1) 1)
    (check (position->line t 1) 1)
    (at "insert 3 within line 0")
    (let* ([t (adjust-within-line t 0 "xxx")])
      ;; "xxx|"
      (check (get-text t 0 4) "xxx\n")
      (check (position->start t 0) 0)
      (check (position->line t 0) 0)
      (check (position->start t 1) 0)
      (check (position->start t 3) 0)
      (check (position->line t 3) 0)
      (check (position->start t 4) 4)
      (check (position->line t 4) 1)
      (at "insert 1 within line 1")
      (let* ([t (adjust-within-line t 4 "x")])
        ;; "xxx|x"
        (check (get-text t 0 5) "xxx\nx")
        (check (position->start t 4) 4)
        (check (position->line t 4) 1)
        (check (position->start t 5) 4)
        (check (position->line t 5) 1)
        (at "delete 1 within line 0")
        (let* ([t (adjust-within-line t 1 -1)])
          ;; "xx|x"
          (check (get-text t 0 4) "xx\nx")
          (check (position->start t 0) 0)
          (check (position->line t 0) 0)
          (check (position->start t 1) 0)
          (check (position->start t 2) 0)
          (check (position->line t 2) 0)
          (check (position->start t 3) 3)
          (check (position->line t 3) 1)
          (check (position->start t 4) 3)
          (check (position->line t 4) 1)
          (at "insert before line 1")
          (let* ([t (insert-newline t 3 "yyyy")])
            ;; "xx|yyyy|x"
            (check (get-text t 0 9) "xx\nyyyy\nx")
            (check (position->start t 0) 0)
            (check (position->line t 0) 0)
            (check (position->start t 2) 0)
            (check (position->line t 2) 0)
            (check (position->start t 3) 3)
            (check (position->line t 3) 1)
            (check (position->start t 7) 3)
            (check (position->line t 7) 1)
            (check (position->start t 8) 8)
            (check (position->line t 8) 2)
            (check (position->start t 9) 8)
            (check (position->line t 9) 2)
            (void))
          (at "insert newline into line 1")
          (let* ([t (insert-newline t 1 "yyyy")])
            ;; "xyyyy|x|x"
            (check (get-text t 0 9) "xyyyy\nx\nx")
            (check (position->start t 0) 0)
            (check (position->line t 0) 0)
            (check (position->start t 1) 0)
            (check (position->line t 1) 0)
            (check (position->start t 6) 6)
            (check (position->line t 6) 1)
            (at "delete line 0")
            (let* ([t (delete-line t 0)])
              ;; "x|x"
              (check (position->start t 0) 0)
              (check (position->line t 0) 0)
              (check (position->start t 1) 0)
              (check (position->line t 1) 0)
              (check (position->start t 2) 2)
              (check (position->line t 2) 1)
              (check (position->start t 3) 2)
              (check (position->line t 3) 1)
              (void)))))))

  (at "three lines")
  (let* ([t (insert empty 0 "abc\ndef\nghi")])
    (check (get-text t 0 11) "abc\ndef\nghi")
    (check (get-text (insert t 0 "xy") 0 13) "xyabc\ndef\nghi")
    (check (get-text (insert t 1 "xy") 0 13) "axybc\ndef\nghi")
    (check (get-text (insert t 4 "xy") 0 13) "abc\nxydef\nghi")
    (check (get-text (insert t 5 "xy") 0 13) "abc\ndxyef\nghi")
    (check (get-text (insert t 11 "xy") 0 13) "abc\ndef\nghixy")
    (check (get-text (insert t 0 "x\ny") 0 14) "x\nyabc\ndef\nghi")
    (check (get-text (insert t 4 "x\ny") 0 14) "abc\nx\nydef\nghi")
    (check (get-text (insert t 7 "x\ny") 0 14) "abc\ndefx\ny\nghi")
    (check (get-text (delete t 0 1) 0 10) "bc\ndef\nghi")
    (check (get-text (delete t 1 2) 0 10) "ac\ndef\nghi")
    (check (get-text (delete t 2 3) 0 10) "ab\ndef\nghi")
    (check (get-text (delete t 3 4) 0 10) "abcdef\nghi")
    (check (get-text (delete t 4 5) 0 10) "abc\nef\nghi")
    (check (get-text (delete t 0 4) 0 7) "def\nghi")
    (check (get-text (delete t 2 5) 0 8) "abef\nghi")
    (check (get-text (delete t 1 10) 0 2) "ai"))

  (at "random modify")
  (define (random-modify-test W)
    (define N 32)
    (define M 8)
    (define (make-str W) (make-string (sub1 W) #\-))
    ;; insert lines of length W in a random order
    (define t
      (for/fold ([t empty]) ([i (in-range N)])
        (insert-newline t (* (random (add1 i)) W) (make-str W))))
    (define (check-N*W t N str)
      (define W (add1 (string-length str)))
      (define content (apply string-append
                             (for/list ([i N])
                               (string-append str "\n"))))
      (for* ([i (in-range N)]
             [j (in-range W)])
        (check (position->line t (+ (* i W) j)) i))
      (check (get-text t 0 (* N W)) content)
      (for ([i (in-range (* N W))]
            [k (in-range 5)])
        (define j (+ i (random (add1 (- (* N W) i)))))
        (check (get-text t i j) (substring content i j))))
    (check-N*W t N (make-str W))
    ;; try inserting then deleting at each point within the line
    (for ([k (in-range W)])
      (define new-str (string-append (substring (make-str W) 0 k)
                                     "!"
                                     (substring (make-str W) k)))
      (define t+
        (for/fold ([t t]) ([i (in-range (sub1 N) -1 -1)])
          (adjust-within-line t (+ (* i W) k) "!")))
      (check-N*W t+ N new-str)
      (define t-
        (for/fold ([t t+]) ([i (in-range 0 N)])
          (adjust-within-line t (+ (* i W) k) -1)))
      (check-N*W t- N (make-str W)))
    ;; delete a few random lines
    (define t-
      (for/fold ([t t]) ([i (in-range (- N M 1) -1 -1)])
        (delete-line t (* (random (add1 i)) W))))
    (check-N*W t- M (make-str W)))
  (for ([i (in-range 100)])
    (for ([W (in-range 2 6)])
      (random-modify-test W)))

  (at "random")
  (define (random-create-test)
    (define n (add1 (random 4096)))
    (define str (make-string n))
    (for ([i (in-range n)])
      (define ch (random 27))
      (string-set! str i (if (zero? ch)
                             #\newline
                             (integer->char (+ (sub1 (char->integer #\a)) ch)))))
    (define t (insert empty 0 str))
    (check (get-text t 0 n) str)
    (for ([i 1])
      (define start (random n))
      (define len (random (- n start)))
      (check (get-text (delete t start (+ start len)) 0 (- n len))
             (string-append (substring str 0 start)
                            (substring str (+ start len))))))
  (for ([i (in-range 1000)])
    (random-create-test))

  (void))

;; Provenance: framework/mred/private/snipfile.rkt
(require (only-in racket/port make-input-port/read-to-peek)
         (only-in racket/match match))
(define (open-input-text t [start 0])
  (unless (text-lines? t)
    (raise-argument-error 'open-input-text "text-lines?" t))
  (unless (exact-nonnegative-integer? start)
    (raise-argument-error 'open-input-text "exact-nonnegative-integer?" start))
  (define-values (pipe-r pipe-w) (make-pipe))
  (define in (make-input-port/read-to-peek
              t
              ;; read-in
              (let ([beg start])
                (λ (s)
                  (match (read-bytes-avail!* s pipe-r)
                    [0
                     (match (min (- (text-length t) beg) 4096)
                       [0
                        (close-output-port pipe-w)
                        eof]
                       [amt
                        (define end (+ beg amt))
                        (write-string (get-text t beg end) pipe-w)
                        (set! beg end)
                        (read-bytes-avail!* s pipe-r)])]
                    [v v])))
              ;; fast-peek
              (λ (s skip general-peek)
                (match (peek-bytes-avail!* s skip #f pipe-r)
                  [0 (general-peek s skip)]
                  [v v]))
              ;; close
              void))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (set-port-next-location! in 1 0 (add1 start)) ;port pos is 1-based
  in)

(module+ test
  (require rackunit
           racket/port)
  (define len 240000)
  (define (random-char _ix) (integer->char (+ 32 (random 26))))
  (define str (list->string (build-list len random-char)))
  (define text (insert empty 0 str))
  (let loop ([pos 0])
    (check-equal? (substring str pos) (get-text text pos))
    (check-equal? (substring str pos) (port->string (open-input-text text pos)))
    (define next-pos (+ pos 10000))
    (when (<  next-pos len)
      (loop next-pos))))
