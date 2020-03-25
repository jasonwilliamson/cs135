;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 1 (cond-a.rkt)  
;; ****************************************  

 

;; (q1a x) produces a symbol based on basic predicate tests
;; q1a: Num -> Sym

(define (q1a x)
  
  (cond
    [(and (p1? x) (p2? x)) 'red]
    [(p1? x) 'blue]
    [(p2? x) 'green]
    [(p3? x) 'pink] 
    [else 'yellow]))


