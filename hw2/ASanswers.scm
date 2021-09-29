;Question 1

(define (is-list? x)
  	(cond ((null? x) #t)
	      ((pair? x) #t)
	      (else #f)))

; Since the predicate states that there should only be two things,
; A list is either something that is empty, or something that is a pair
; So essentially I check if it's n


; Question 2

; In order to answer this question we need to traverse through
; The list, and append unto a new list while we're doing that.
; I used the append producure that was on the book.
(define (append list1 list2)
  	(if (null? list1)
	    list2
	    (cons (car list1) (append (cdr list1) list2))))

; Here we'll need to traverse the list
(define (my-reverse items)
  	(if (null? items) '()
	    (append (my-reverse (cdr items)) (list (car items)))))

; On this step we'll recursively step through each
; We essentially will call my-reverse before we append,
; When we eventually get to the emptylist of items
; Then it will return an empty list, and append a
; brand new list

; Question 4
(define (square) (* x x))

(define (map proc items)
  	(if (null? items)
	    '()
	    (cons (proc (car items))
		  (map proc (cdr items)))))

(define (square-list items)
  	(if (null? items)
	    '()
	    (cons (square(car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (square) (items)))


; The 1st way we square and apply recurison into (cdr items)
; the 2nd way we simply use map that will do everything for us.


; I apologize, I didn't have much time. 
