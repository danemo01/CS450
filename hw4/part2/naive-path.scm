(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

(define data (with-input-from-file "dist.dat" read-file))

 (define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (get-table)
      local-table
      )
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'gettable-proc) get-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table))

;((t 'insert-proc) 'a 1 1)

(define (load-data data)
  (if (null? data)
      '()
      (let ((vertex (caar data))
            (edge (cadar data))
            (weight (caddar data)))
        (begin
          ((t 'insert-proc) vertex edge weight)
          (load-data (cdr data))))))

(load-data data)

; Test that everything was inserted

((t 'lookup-proc) 'start 'p1)
((t 'lookup-proc) 'start 'p2)
((t 'lookup-proc) 'p1 'p2)
((t 'lookup-proc) 'p2 'end)
((t 'lookup-proc) 'p1 'end)

((t 'gettable-proc))
(cdr ((t 'gettable-proc)))

(define stlist (assoc 'start (cdr ((t 'gettable-proc)))))
stlist

(define (naive-cost node) ;'p1
  (if (null? (cdr (assoc node (cdr ((t 'gettable-proc)))))) ; false - ((end . 22) (p2 . 1))
      "infinity"
      (let ((childlst (cdr (assoc node (cdr ((t 'gettable-proc))))))) ; childlst -  ((end . 22) (p2 . 1))
        (minim (for-each-lord node childlst))) ;'p1  ((end . 22) (p2 . 1))
  ))


(define (for-each-lord node childlst) ;  ((p2 . 1))
  (let ((child (caar childlst))) ; child - p2
  (if (null? child) ; no
      100000000
      (if (eq? child 'end) ; p2 == end? yes
          (cons ((t 'lookup-proc) node child) (for-each-lord node (cdr childlst))) ; 
          (cons (+ ((t 'lookup-proc) node child) (naive-cost child)) (for-each-lord node (cdr childlst)))))))
                                                             
(define (minim lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (minim (cdr lst))) (car lst))
          (else (minim (cdr lst)))) )

(naive-cost 'start)