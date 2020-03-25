;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname nest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 07, Problem 2 (nest.rkt)  
;; **************************************** 

;; A nested list of number (Nest-List-Num) is one of the following:
;; * empty
;; * (cons Num Nest-List-Num)
;; * (cons Nest-List-Num Nest-List-Num)


;; (max-nest nln) consumes nln, a nested list of numbers and produces a 
;; positive integer representing the maximum nesting that occurs in a 
;; consumed nested list of numbers.
;; max-nest: (Nest-List-Num) -> Nat
;; Examples:
(check-expect (max-nest '((1) (2) ((7)) 4 5)) 3)
(check-expect (max-nest '()) 1)
 
(define (max-nest nln)
  (cond [(empty? nln) 1]
        [(number? (first nln)) (+ 0 (max-nest (rest nln)))]
        [else
         (max (+ 1 (max-nest (first nln)))
              (max-nest (rest nln)))]))

;; Tests
(check-expect (max-nest '((1) (2) (((7))) 4 5)) 4)
(check-expect (max-nest '(2 (3) ((4)) (((5))) )) 4)
(check-expect (max-nest '(2 (3) ((4)) (((2 (3) ((4)) (((5))) ))) )) 7)
  


