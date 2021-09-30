;; read-file
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

(define source (with-input-from-file "units.dat" read-file))

(define (one-to-element pair)
  (define (get-unit key-value) (car (cadadr key-value)))
  (define (get-expo pair) (cdr pair))
  (if (assoc (car pair) source)
      (cons (get-unit (assoc (car pair) source))
            (get-expo pair))
      pair))

(define (track-elems lst counter type)
  (if (null? lst)
      counter
       (if (equal? (caar lst) type)
           (track-elems (cdr lst) (+ (cadar lst) counter) type)
           (track-elems (cdr lst) counter type))))

(define (triple-tracker lst sc mc kgc)
  (list (track-elems lst sc 'sec)
        (track-elems lst mc 'm)
        (track-elems lst kgc 'kg)))

(define (convertable? a b)
  (equal? (triple-tracker a 0 0 0)
          (triple-tracker b 0 0 0)))

(define (get-unit lst) (caar lst))
(define (get-expo lst) (cadar lst))

(define (get-factor u)
  (if (assoc u source)
      (caadr (assoc u source))
      1)) ; already elemental unit

(define (mult-unit-list l)
  (if (null? l)
      1
      (*
       (expt
        (get-factor (get-unit l))
        (get-expo l))
       (mult-unit-list (cdr l)))))

(define (convert-to-element lst)
  (map one-to-element lst))

(define (convert a b)
  (if (convertable? (convert-to-element (cdr a)) (convert-to-element b))
      (let ((front (* (car a) (mult-unit-list (cdr a))))
            (back (mult-unit-list b)))
        (cons (/ front back) b))
      #f))

(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1))) ; 0.010230654761904762
(convert '(27.5 (lbm 1)(furlong 1)(fortnight -2)) '((ton 1)(mi 1)(hr -2))) ; 1.5224188633786846e-008
(convert '(27.5 (km 1)(km 1)) '((yd 2))) ; 32889726.273279708
(convert '(1 (mg 1)) '((km 2))) ; false
(convert '(1 (in 1)) '((mg 1))) ; false
(convert '(1 (m 1)) '((in 1)))  ; 39.37007874015748
(convert '(1 (yd 2)) '((km 1)))  ; false

;Edge case:
;Positive
;      1) (km 2) ==> (km 1)(km 1) /// DONE
;      2) (acre 1) ==> (m 2) ==> (km 2) /// DONE
;Negative
;      3) (yd 2) => (km 1) == (km 2) // Edge case debugging????
;      4) (m 1) => (in 1) /// DONE
;      5) (mg 1) => (km 1) /// DONE






