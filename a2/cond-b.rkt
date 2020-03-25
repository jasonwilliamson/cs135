;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 1 (cond-b.rkt)  
;; ****************************************  



;; (q1b x) produces a symbol based on basic predicate tests
;; q1b: Num -> Sym

(define (q1b x)    
    
  (cond
    [(and (p1? x)(< x 2015)) 'brown]
    [(and (and (p1? x)(p2? x)) (and (p3? x) (p1? x))) 'black]
    [(or (and (p1? x)(p2? x)) (p3? x)) 'white]
    [else 'purple]))
    
     