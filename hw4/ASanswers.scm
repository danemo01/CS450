; Danley Nemorin
; Local State Variables

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 15)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; this produces a make-withdraw balance section
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))


;; Example fro mclass
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT")
                m)))
  dispatch)

; Question - 1.1
(define make-account-lambda
  (lambda (balance)
    (define withdraw (lambda (amount)
		       (if (>= balance amount)
			   (begin (set! balance (- balance amount))
				  balance)
			   "Insufficient funds")))
    (define deposit (lambda (amount)
                      (set! balance (+ balance amount))
                      balance))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT")
		  m)))))

; Question - 1.2
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds")))
            ((eq? m 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
            (else (error "Unknown request -- MAKE-ACCOUNT")
                  m)))))

; Question - 1.3
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
	(cond ((eq? m 'withdraw)
	       (if (>= balance amount)
		   (begin (set! balance (- balance amount))
			  balance)
		   "Insufficient funds"))
	      ((eq? m 'deposit)
	       (set! balance (+ balance amount))
	       balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT")
		    m))))))


(define acc (make-account 100))
((acc 'withdraw) 10)

(define acc2 (make-account-lambda 100))
((acc2 'withdraw) 15)
((acc2 'deposit) 2500)

(define acc3 (make-account-inline 100))
((acc3 'withdraw) 50)
((acc3 'deposit) 4532)

(define acc6 (make-account-inline-factored 100))
((acc6 'withdraw) 32)
((acc6 'deposit) 41235)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; 3.
(define make-monitored
  (let ((calls-amount 0))
  (lambda (f)
    (lambda (mf)
      (cond ((eq? mf 'how-many-calls?)
             calls-amount)
            ((eq? mf 'reset-count)
             (set! calls-amount 0))
            (else (begin (set! calls-amount (+ 1 calls-amount))
                         (f mf))))))))
; 4.
(define make-pw-account
  (lambda (balance passwrd)
    (define acc4 (make-account-inline balance))
    (lambda (input f)
      (if (eq? input passwrd)
          (acc4 f)
          (lambda (dummyval)
            "Incorrect Password"))     
    )))

(define acc-final (make-pw-account 100 'secret-password))

((acc-final 'secret-password 'withdraw) 40)
((acc-final 'sefcret-password 'deposit) 50)


(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

