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

; # Homework 1
(define (display-n stream n)
  (stream-map (lambda (n) (display n)(newline)) stream))

(display-n (stream-enumerate-interval 1 10) 10)



;; New version

(define (display-n-new stream n)
      ; if statement checks if the stream is null or if n == (stream-car stream)
     (if (or (stream-null? stream) (eq? n (stream-car stream)))
        the-empty-stream ; if so returns an empty stream
        (begin ; we begin
          (display (stream-car stream)) 
          (display-n-new (stream-cdr stream) n)
          )))


; Part 1 - Q2)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;(define integers (integers-starting-from 1))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;(integers-starting-from-no 1)

;(define integers (integers-starting-from 1))
;(define display-new-n integers 10)
