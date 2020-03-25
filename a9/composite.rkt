;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname composite) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 09, Problem 3 (composite.rkt)  
;; **************************************** 


;; (composite f g) consumes f and g, both functions, and produces
;; the composite function ( f ◦ g). That is the funciton computes
;; f ( g ( x ) ) for any given x. Both f and g consume one parameter each
;; and the result of g can be consumed by f
;; composite: ( Num -> Num ) (Num -> Num) -> (Num -> Num)
;; requires: the result of g must be consumable by f
;; Examples:
(check-expect ((composite abs sub1) -5) 6)
(check-expect ((composite add1 cos) 0) 2)

(define (composite f g)
  (lambda (x) (f (g x))))

(check-expect ((composite sqr sub1) -2) 9)


;; (inverse-of-square-list lon) consumes lon, a list of positive numbers
;; and produces a new list of positive numbers with each element being 
;; the inverse of the square of the corresponding element in the original
;; list
;; inverse-of-square-list: (listof Num) -> (listof Num)
;; requires: lon must! be a list of positive numbers
;; Examples:
(check-expect (inverse-of-square-list '(1 2 5)) '(1 1/4 1/25))
(check-expect (inverse-of-square-list '(2/3 2/5 5)) '(9/4 25/4 1/25))
 
(define (inverse-of-square-list lon)
  (local [(define (inverse x )(expt x -1))
          (define (inverse-of-square x) ((composite inverse sqr) x))]
    (map inverse-of-square lon)))

;; Tests
(check-expect (inverse-of-square-list empty) empty)
(check-expect (inverse-of-square-list '(2/3 2/5 1 2 5)) '(9/4 25/4 1 1/4 1/25))


;; (composite-list lof) consumes lof, a list of functions (f1,..., fn) (where 
;; each consumes and produces a number), and produces the composite function
;; ( f1 ◦ . . . ◦ fn). If the list is empty, composite-list produces the
;; identity function f(x) = x
;; composite-list: (listof (Num -> Num)) -> (Num -> Num)
;; Examples:
(check-expect ((composite-list (list add1 sqr sub1)) 3) 5)
(check-expect ((composite-list empty) 3) 3)

(define (composite-list lof)
  (foldr (lambda (x y) (composite x y)) identity lof))

;; Tests
(check-expect ((composite-list (list add1 sqr cos)) 0) 2)

