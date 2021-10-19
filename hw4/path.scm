(define (lookup-1d key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert!-1d key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define memo-list (list '*table*))

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

(define (load-data data)
  (if (null? data)
      '()
      (let ((vertex (caar data))
            (edge (cadar data))
            (weight (caddar data)))
        (begin
          ((t 'insert-proc) vertex edge weight)
          (load-data (cdr data))))))


(define (cost node) ;'p1
  (if (eq? #f (assoc node (cdr ((t 'gettable-proc)))))
      9999999999999
      (let ((node-cost (lookup-1d node memo-list)))
        (if (eq? #f node-cost)
            (let ((childlst (cdr (assoc node (cdr ((t 'gettable-proc)))))))
              (let ((minstval (for-each-node node childlst 9999999999999)))
                (begin (insert!-1d node minstval memo-list) minstval)))
            node-cost
          ))))
;(insert!-1d 'a 1 1D-list)

(define (for-each-node node childlst minval) 
  (if (null? childlst) 
      minval
      (let ((child (caar childlst))
            (child-weight (cdar childlst)))
        (if (eq? child 'end) 
          (for-each-node node (cdr childlst) (min minval child-weight))
          (for-each-node node (cdr childlst)
                         (min minval (+ child-weight (cost child))))))))



(begin (define t (make-table)) (load-data data) (cost 'start))


