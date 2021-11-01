(define (stream-car obj) (car obj))
(define (stream-cdr obj) (force (cdr obj)))
(define stream-null? null?)

(define (display-n stream n)
      ; if statement checks if the stream is null
      ; or if n == (stream-car stream)
     (if (and (> n 0 ) (not (stream-null? stream)))
        (begin ; we begin
          (display (stream-car stream)) ; display the value
          (newline)  ; new line
          (display-n (stream-cdr stream) (- n 1)))))

(define (list->strm lst)
    (if (null? lst)
        '()
        (cons-stream (car lst) (list->strm (cdr lst)))
     )
 )

(define (number->list-of-digits val) 
  (define (convert->list42 lst)
    (if (null? lst)
        '()
        (cons (- (char->integer (car lst)) 48)   
              (convert->list42 (cdr lst)))))
              
  (convert->list42 (string->list (number->string val))))


; actions:
;  if ()
;


; We need to create a function that gives us an out
(define (mult-stream m strm)
  (define (action a a-list pow inptstrm)
        ; if (m + (a%pow) >= pow  )
    (if (>= (+ m (modulo a pow)) pow) 
          (consume a a-list pow inptstrm)
          (produce a a-list pow inptstrm)
    ))

  (define (consume a a-list pow inptstrm)
    (if (null? inptstrm)
        a-list         ; new-a = m*(car inptstrm) + a*10
        (let ((new-a (+ (* (car inptstrm) m) (* a 10)))) ; 117
          (let ((new-a-list (number->list-of-digits new-a))) ; (1 1 7)
            (let ((new-pow (expt 10 (- (length new-a-list) 1)))) ; 100
              (if (and (= 0 (car a-list)) 
                       (= (length a-list) (length new-a-list)))
                    (action new-a (append (list (car a-list)) new-a-list)
                        new-pow (cdr inptstrm)) ; (7)
                    (action new-a new-a-list new-pow (cdr inptstrm)) 
            ))))))

  (define (produce a a-list pow inptstrm)
    (if (null? inptstrm) 
      a-list
      (let ((new-a (modulo a pow)) ; 38
            (new-a-list (cdr a-list))) ; (0 3 8)
        (let ((new-pow (expt 10 (- (length new-a-list) 1)))) ; 100
          (cons-stream (car a-list) (action new-a new-a-list new-pow inptstrm))   
    ))))

  (consume 0 '(0) 1 strm))



(define (m_11 m) (car m))
(define (m_12 m) (cadr m))
(define (m_21 m) (caddr m))
(define (m_22 m) (cadddr m))

(define (add-matrix m1 m2)
    (list
        (+ (m_11 m1) (m_11 m2))
        (+ (m_12 m1) (m_12 m2))
        (+ (m_21 m1) (m_21 m2))
        (+ (m_22 m1) (m_22 m2))
     ))

(define (compose A B)
    (list
        (+ (*(m_11 A) (m_11 B)) (* (m_12 A) (m_21 B)))
        (+ (*(m_11 A) (m_12 B)) (* (m_12 A) (m_22 B)))
        (+ (*(m_21 A) (m_11 B)) (* (m_22 A) (m_21 B)))
        (+ (*(m_21 A) (m_12 B)) (* (m_22 A) (m_22 B))))
)

(define (matrix->int m x)
    (quotient
        (+ (* (m_11 m) x) (m_12 m))
        (+ (* (m_21 m) x) (m_22 m))
     ))

(define (shiftmatrix n)
    (list 10 (* -10 n) 0 1)
) 

(define (matrixes lst)
    (cons-stream lst (matrixes (add-matrix lst '(1 4 0 2)))))



(define (pi)
    (define (action a strm)
        (if (equal? (matrix->int a 3) (matrix->int a 4))
            (produce a strm)
            (consume a strm)))

    (define (consume a strm)
        (let ((new-a (compose a (stream-car strm))))
            (action new-a (stream-cdr strm))))
    (define (produce a strm)
      (let ((new-val (matrix->int a 3)))
        (let ((new-a (compose (shiftmatrix new-val) a)))
          (cons-stream new-val (action new-a strm)))))
    (consume '(1 6 0 3) (stream-cdr (matrixes '(1 6 0 3)))))

(display-n (pi) 1000)


