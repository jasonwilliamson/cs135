;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname siblings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 07, Problem 3 (siblings.rkt)  
;; **************************************** 


;; A nested list of number (Nest-List-Num) is one of the following:
;; * empty
;; * (cons Num Nest-List-Num)
;; * (cons Nest-List-Num Nest-List-Num)


;; (count-siblings nln n) consumes nln, a nested list of numbers, and n, an 
;; intenger, and produced the number of siblings n has within the root list
;; provided
;; count-siblings: (Nest-List-Num) Num -> Nat
;; requires: n must exist within the list and not be nested 
;; Examples:
(check-expect (count-siblings '((1 2 3) 7 8 ((4 5 6))) 4) 2)
(check-expect (count-siblings '((1 2 3) 8 ((4 5 6))) 8) 0)
 
(define (count-siblings nln n) 
  (cond [(empty? nln) 0] 
        [(and (number? (first nln))
              (= n (first nln))) (+ 0 (count-siblings (rest nln) n))]
        [(number? (first nln)) (+ 1 (count-siblings (rest nln) n))]
        [else (count-siblings (rest nln) n)]))


;; (total-siblings t n) consumes t, a nested list of numbers, and n, an integer
;; and produces the number of siblings of a leaf
;; total-siblings: (Nest-List-Num) Int -> Nat
;; requires: the leaf, n, must exist within the nested list
;; Examples:
(check-expect (total-siblings '((1 2 3) ((4 5 6))) 5) 2)
(check-expect (total-siblings (list (list 1 2) 3 (list 4)) 3) 0)

(define (total-siblings t n)
  (cond [(empty? t) 0]
        [(and (cons? t) (member? n t)) (count-siblings t n)]
        [(number? (first t)) (total-siblings (rest t) n)]
        [else (+  (total-siblings (first t) n)
                  (total-siblings (rest t) n))]))


;; (leaf? t n) consumes t, a nested list of numbers, and n, an integer
;; and produces true if the leaf exists within the nested list
;; otherwise producing false
;; leaf?: (Nest-List-Num) Int -> Bool
;; Examples:
(check-expect (leaf? (list (list 1 2) 3 (list 4)) 3) true)
(check-expect (leaf? (list (list 1 2 3) (list (list 4 5 6))) 9) false)

(define (leaf? t n)
  (cond [(empty? t) false]
        [(and (cons? t) (member? n t)) true]
        [(number? (first t)) (leaf? (rest t) n)]
        [else (or (leaf? (first t) n) 
                  (leaf? (rest t) n))]))

;; Tests
(check-expect (leaf? (list (list 1 2) (list 3) (list 4)) 3) true)


;; (num-siblings t n) consumes t, a nested list of numbers, and n, an integer
;; and produces the number of leaves that are siblings of n in t
;; (if n is a leaf in t), otherwise producing false (if n is not a leaf in t).
;; num-siblings: (Nest-List-Num) Int -> (anyof Nat false)
;; requires: all numbers within t must be distinct (non repeating)
;; Examples
(check-expect (num-siblings '((1 2 3) ((4 5 6))) 5) 2)
(check-expect (num-siblings (list (list 1 2) 3 (list 4)) 3) 0)
(check-expect (num-siblings (list (list 1 2 3) (list (list 4 5 6))) 9) false)
 
(define (num-siblings t n) 
  (cond [(leaf? t n) (total-siblings t n)]
        [else false]))

;; Tests
(check-expect (num-siblings (list (list 1 2 3) 
                                  (list (list 4 5 6) (list 3 9))) 9) 1)
(check-expect (num-siblings (list (list 1 2) (list 3) (list 4)) 3) 0)
   
   
      
   



         


              