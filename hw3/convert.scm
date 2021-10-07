;; read-file
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

(define source (with-input-from-file "units.dat" read-file))


; So this is incorrect
; Essentially what this does is that it returns a given
; the one-to-element pair
; This is bad because if there more than 1 element
; This won't work in other cases.
; But this will most likely work 
(define (one-to-element pair)
  (define (get-base-unit key-value) (car (cadadr key-value)))
  (define (get-base-exp key-value) (cadr (cadadr key-value)))
  (define (get-expo pair) (cdr pair))
  (let ((base-unitlist (assoc (car pair) source))) 
  (if base-unitlist
      (let ((base-exp (cadr (cadadr base-unitlist ))))
      (cons (get-base-unit base-unitlist)
            (list (* (car (get-expo pair)) base-exp))))
      pair)))

; Will take the list
; It checks if the element is equvilent to the type
; If so, add it into the counter and continue
; If not, then go to the next element
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

; Triple-tracker will pass in list1 and list2
(define (convertable? unit-list1 unit-list2 )
  (equal? (triple-tracker unit-list1 0 0 0)
          (triple-tracker unit-list2 0 0 0)))

(define (get-unit lst) (caar lst))
(define (get-expo lst) (cadar lst))

(define (get-factor u)
  (if (assoc u source)
      (caadr (assoc u source))
      1)) ; already elemental unit

; 
(define (mult-unit-list l)
  (if (null? l)
      1
      (*
       (expt
        (get-factor (get-unit l))
        (get-expo l))
       (mult-unit-list (cdr l)))))

  
; work on progress
(define (convert-to-element lst)
  (define (getbase lst)
    (if (equal? (assoc (caar lst) source) #f)
        (list '())
        (cdadr (assoc (caar lst) source))))
  (if (null? lst)
      '()
      (let ((multi-unit (getbase lst)))
        (if (equal? (length (cdr multi-unit)) 0)
            (cons (one-to-element (car lst)) (convert-to-element (cdr lst)))
            (multi-to-element multi-unit lst)
            ))))
;It will past the innerlist
; Then we will check if the innerlist is null
; if it is, call convert-to-element again
; if we pass ((m 1)(sec 1
(define (multi-to-element innerlst lst)
  (if (null? (cdr innerlst)) ; if 
      (cons (one-to-element (car innerlst)) (convert-to-element (cdr lst)))
      (cons (list (caar innerlst) (* (cadar innerlst) (cadar lst)))
            (multi-to-element (cdr innerlst) lst)) 
  ))



(define (convert quantity unit-list) ; We pass quantity and unit-list
  ;we verify if both of unitlist being passed, have the similar
  ;amount of base
  (if (convertable? (convert-to-element (cdr quantity))
                    (convert-to-element unit-list))
      (let ((qBaseUnit (* (car quantity) (mult-unit-list (cdr quantity))))
            (toBaseUnit (mult-unit-list unit-list)))
        (cons (/ qBaseUnit toBaseUnit) unit-list))
      #f))

(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
; 0.010230654761904762
(convert '(27.5 (lbm 1)(furlong 1)(fortnight -2)) '((ton 1)(mi 1)(hr -2)))
; 1.5224188633786846e-008
(convert '(27.5 (km 1)(km 1)) '((yd 2))) ; 32889726.273279708
(convert '(1 (mg 1)) '((km 2))) ; false
(convert '(1 (in 1)) '((mg 1))) ; false
(convert '(1 (m 1)) '((in 1)))  ; 39.37007874015748
(convert '(1 (yd 2)) '((km 1)))  ; false

(convert '(1 (hp 1)) '((ft 1)(lbf 1)(min -1)))


; My notes on the conversion part
; I believe checking for
; On regarding the verificiation if the base units are the same for 
; Originally I designed it by creating a list that I would parse using map
; I would be able to get the base unit, and then I would be able to see how
; many base units that are similar would be in there.
; For example if I had something like '(27.5 (furlong 1)(fortnight -1))
; to '((mi 1)(hr -1)) 
; I would have two lists where list1 represented my quantity and list2
; represented the unit list.
; So I would have this: ((m 1)(sec -1)) for ((furlong 1)(fortnight-1))
; and ((m 1)(sec -1)) for ((mi 1)(hr -1))
; Then after that, I could compare both of them to see if they're accurate
; Originally I thought equals, but then after rereading the write up of the HW
; scenerios like '((fortnight -1)(furlong 1)) were correct and would give me
; ((sec -1)(m 1)) which was fine, but It led to some issues. 
; Then if we were converting something like (27.5 (m 1)(m 1)) to (m 2)
; this wouldn't work, and so I spent a day running around figuring out
; how I would recursvely analyze if both of the quantity and unitlist
; had the same base! I didn't know what to do, nor how to approach it
; Then after looking and understanding the Conversion function

; I took a new approach.
; I understood that there were three base units, and that
; They needed to represent each other in their base units
; I also understood that the conversion is acceptable if
; both sides have the equalvent base units then that would
; then they would be equivelent. So in order to do that, I thought of
; a 3-element list such as: (m sec kg), and if parsed a list that represented
; (base-unit exponent) then we could run a counter that counts
; the exponenets that appear within the unit-list and quantity
; then all I would have to do is just use equal? and it would be fine
; For example:
; (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
; Once I call convert-to-element on both of the unitlist I get:
; ((furlong 1)(fortnight -1)): ((m 1) (sec -1))
; '((mi 1)(hr -1))) : ((m 1) (sec -1))
; Then I check if it's an convertible and essentialy will run a checker on all
; and it will run triple-checker on both of the converted list
; and return this:
; ((furlong 1)(fortnight -1)): ((m 1) (sec -1)) -> (-1 1 0)
; '((mi 1)(hr -1))) : ((m 1) (sec -1)) -> (-1 1 0)
; then run equal? and confirm if it's true or not. and if it's true then
; It's true that both quantiy and unit-list are the same
; If it's not, then it (convert) will return false.
; I implented it, and it was able to work but I believe I came
; into some issues with Joules as parsing joule from unit.dat does
; provide you with a list and not a pair, so parsing it was really hard
; and was unable to check for it.


; As for the conversion, this one took a lot of practice
; I took time trying to figure out how well the conversion could be
; I understood that there was a way of simply getting the conversion
; for everything and then multplying by the scalar would one.
; For example: (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
; I knew that I could simply just get the base conversion and
; (furlong 1)(fortnight -1) would represent
; furlong/furlong -> (201.16800/1209600) meters/second
; I tested by posting this on google and it worked perfectly fine.
; I understood that if I simply multiplied the base-unit conversion
; with the scalar value (27.5) I would be able to translate it fine
; Then I realized that I could easily get the conversion to
; what the unitlist needed to be by dividing by that.
; So if I needed to convert 27.5 (furlong per furnight) to miles per hr
; Then all I would really need to do is find the base conversion for
; miles per hr, and then I divide the conversion
; I have for (furlong 1)(furnight -1)
; by the conversion of miles per hr.
; And shockingly enough it worked with no issue what so ever.

; After HW3 Extension:
; I'm keeping my notes down as the same as
; I didn't have enough time to change it
; But I was able to solve the issue the running this input:
; (convert '(1 (hp 1)) '((ft 1)(lbf 1)(min -1)))
; As mention above my original issue was that, when I was able to retrieve
; The base unitlist from unit.dats, I didn't write my code to be able to
; add the other baseunit. So the baseunit for 'hp is (kg 1)(m 2)(sec -3)
; And It only took (kg 1), and it's because I really didn't know how to
; recurisvely do it. However after trial and error rereading some of the notes.
; I realized that I just needed an extra helper function that also iterated
; through the recursion of convert-to-element called multi-to-elemen
; Essentially I check if there's an extra nested list within the base unitlist
; for a particular unit on units.dat. 
; 
; I also broke down the convert-to-element procedure where it called map,
; and a prodecure, but broke it down where it's similar to map. But
; It also checks to see if there's more.

