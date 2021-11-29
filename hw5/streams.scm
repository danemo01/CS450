(define the-empty-stream '())

;(define (cons-stream a b)
;  (cons a (delay b)))

(define (stream-car obj) (car obj))
(define (stream-cdr obj) (force (cdr obj)))
(define stream-null? null?)

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-accumulate proc init stream)
  (if (stream-null? stream)
      init
      (proc (stream-car stream)
            (stream-accumulate proc init (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define x (cons-stream '1 (cons-stream 'a '())))
(define y (cons-stream '100 (cons-stream 'f '())))

(define z (stream-append x y))

(define (list-square n)
  (stream-accumulate cons '()
                     (stream-map (lambda (n) (* n n))
                                 (stream-enumerate-interval 1 n))))



(define (debug-stream str) (begin (display "Debug:") (newline) (display str) (newline)  (display "done") (newline)) )

;Part 1 - Q1)

(define (display-n stream n)
      ; if statement checks if the stream is null or if n == (stream-car stream)
     (if (and (> n 0 ) (not (stream-null? stream)))
        (begin ; we begin
          (display (stream-car stream)) ; display the value
          (newline)  ; new line
          (display-n (stream-cdr stream) (- n 1)))))

;; defining the integer infinite stream
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
;; moving it into integers
(define integers (integers-starting-from 1))
;; runs it
(begin (display-n integers 10) (newline))


; Part 1 - Q2)
; commented for now
(define (stream-map proc . argstreams)

  (begin 
  ;(debug-stream argstreams) 
      (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))))))


; Part 2 - Q3)

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(begin (display-n integers 5) (newline))


;; now create something similarto sieve
(define (divisible? x y) (= (remainder x y) 0))

(define (isnotdiv-235? val)
  (if (not (or (divisible? val 2) (divisible? val 3) (divisible? val 5) ))
    #t
    #f
   ))

(define (notdiv-235 stream)
    (cons-stream 
      (stream-car stream)
      (notdiv-235 (stream-filter isnotdiv-235?  (stream-cdr stream)))))

(define q2 (notdiv-235 (integers-starting-from 7)))

(display-n q2 10)
