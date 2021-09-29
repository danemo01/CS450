;; read-file
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
	'()
	(cons expr (read-file)))))

(define source (with-input-from-file "units.dat" read-file))


(assoc 'in source)

(define value (caadr (assoc 'in source)))
(define unit (car (assoc 'in source)))

(define (to-m x)
  (* x (/ 1 value)))

(define (convert quantity unit-list)
  	1)

(convert '(2 (km 1)) '((cm 1)))
; 
;
;
;
