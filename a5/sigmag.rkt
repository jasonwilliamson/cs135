;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sigmag) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 05, Problem 3 (sigmag.rkt)  
;; ****************************************  


(define-struct sigmag (sig mag))
;; A SigMag is a (make-sigmag Sym Num)
;; requires: sig is one of 'positive, 'negative, 'zero
;;           mag > 0
;;           if sig is 'zero, then mag = 1


;; my-sigmag-fn: SigMag -> Any
(define (my-sigmag-fn s)
  (... (sigmag-sig s) ...
       (sigmag-mag s)...))


;; A (listof SigMag) is one of:
;; * empty
;; * (cons SigMag (listof SigMag))


;; (num->sigmag num) consumes a Num and produces its equivalent
;; SigMag value.
;; num->sigmag: Num -> SigMag
;; Examples:
(check-expect (num->sigmag 6) (make-sigmag 'positive 6))
(check-expect (num->sigmag -6) (make-sigmag 'negative 6))
 
(define (num->sigmag num)
  (cond [(> 0 num) (make-sigmag 'negative (abs num))] 
        [(< 0 num) (make-sigmag 'positive num)]
        [else (make-sigmag 'zero 1)]))

;; Tests
(check-expect (num->sigmag 0) (make-sigmag 'zero 1)) 


;; (sigmag->num s) consumes a SigMan and produces its equivalent number
;; sigmag->num SigMag -> Num
;; Examples:
(check-expect (sigmag->num (make-sigmag 'positive 6)) 6) 
(check-expect (sigmag->num (make-sigmag 'negative 6)) -6) 

(define (sigmag->num s) 
  (cond [(symbol=? 'negative (sigmag-sig s))(* -1 (sigmag-mag s))]
        [else (sigmag-mag s)]))
             
  
;; Tests
(check-expect (sigmag->num (make-sigmag 'zero 1)) 1)  


;; (convert-sigmag-list loss) consumes a (listof Sigmag) and produces the 
;; corresponding list of numbers. The order of the vaues from the input list
;; must be maintained in the output list.
;; convert-sigmag-list: (listof Sigmag) -> (listof Num)
;; Examples:
(check-expect (convert-sigmag-list (list (make-sigmag 'positive 2)
                                         (make-sigmag 'negative 5)))
              (list 2 -5))
(check-expect (convert-sigmag-list (list (make-sigmag 'zero 1)
                                         (make-sigmag 'postive 5)))
              (list 1 5))

(define (convert-sigmag-list loss)
  (cond [(empty? loss) empty]
        [else (cons (sigmag->num (first loss))
                    (convert-sigmag-list (rest loss)))]))

;; Tests
(check-expect (convert-sigmag-list empty) empty)
(check-expect (convert-sigmag-list (list (make-sigmag 'negative 2)
                                         (make-sigmag 'negative 5)))
              (list -2 -5))


;; (count-sigmag a b) consumes two Int a and b and produces a (listof SigMag)
;; representing the integers, a, a+1, ....,b in that order exactly.
;; If a > b then function produces an empty list.
;; count-sigmag: Int Int -> (listof Sigmag)
;; Examples:
(check-expect (count-sigmag 0 1)
              (list (make-sigmag 'zero 1)
                    (make-sigmag 'positive 1))) 
(check-expect (count-sigmag -1 1)
              (list (make-sigmag 'negative 1)
                    (make-sigmag 'zero 1)
                    (make-sigmag 'positive 1)))  

(define (count-sigmag a b)
  (cond [(> a b) empty]
        [else (cons (num->sigmag a)
                    (count-sigmag (+ 1 a) b))]))

;; Tests
(check-expect (count-sigmag 2 1) empty) 
(check-expect (count-sigmag -3 -1)
              (list (make-sigmag 'negative 3)
                    (make-sigmag 'negative 2)
                    (make-sigmag 'negative 1))) 
(check-expect (count-sigmag 0 0)
              (list (make-sigmag 'zero 1)))
