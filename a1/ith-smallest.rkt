;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ith-smallest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 01, Problem 4 (ith-smallest.rkt)  
;; ****************************************  

(define k 1)
(define k (min a b c))

;; (ith-smallest i a b c) produces the i-th smallest number among
;; a, b, and c, given a value of i
;; ith-smallest Int Int Num Num Num -> Num
;; Requires: i must be in the range [1, 3] or (1 <= i <= 3)
;; Examples
(check-expect (ith-smallest 1 9 4 5) 4)
(check-expect (ith-smallest 2 7 8 1) 7)

(define (ith-smallest i a b c)
  
  (i a b c))