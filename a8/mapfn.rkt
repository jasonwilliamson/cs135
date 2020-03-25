;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 08, Problem 4 (mapfn.rkt)  
;; ****************************************  

;; (mapfn lof lon) consumes lof, a (listof Functions) and lof, (listof Num),
;; where lon is a list of two numbers. Producing a list of the results of 
;; applying each function in lof to the given two numbers in lon.
;; mapfn: (listof Function) (listof Num) -> (listof Any)
;; requires: lon is a (listof Num) that must contain two numbers
;; Examples:
(check-expect (mapfn (list + - * / list) '(3 2)) '(5 1 6 1.5 (3 2)))
(check-expect (mapfn (list max min) '(3 2)) '(3 2))
(check-expect (mapfn empty '(3 2)) empty)

(define (mapfn lof lon)
  (cond [(empty? lof) empty]
        [else 
         (local [(define f (first lof))]
           (cons (f (first lon)(second lon))
                    (mapfn (rest lof) lon)))]))

;; Tests
(check-expect (mapfn (list > <) '(3 2)) (list true false))
(check-expect (mapfn (list quotient gcd expt) '(3 2)) '(1 1 9))
