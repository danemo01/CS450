; Let's define the square before doing the exam
(define (square x)
  (* x x))

(define (f x y)
  (let ((a (+ x y))
	(b (* x y)))
    (+ (square (+ a b))
       (square (- a b))) ))

(f 2 2)

(f 3 3)

(f 2 3)
