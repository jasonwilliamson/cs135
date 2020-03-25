;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 01, Problem 3 (grades.rkt)  
;; ****************************************  

;; Constants
;; grade weighted values
(define midterm-weight (/ 25 100))
(define final-weight (/ 50 100))
(define exam-weight (+ midterm-weight final-weight))
(define particip-weight (/ 5 100))
(define assign-weight (/ 20 100))


;; (cs135-exam-grade midterm-grade final-grade) produces the weighted exam 
;; average in the course as a percentage in the range 0 to 100
;; cs135-exam-grade Num Num -> Num
;; Requires: midterm-grade and final-grade 
;;           must be in the range [0,100] inclusive
;; Examples:
(check-within (cs135-exam-grade 55 75) 68.3 .1)
(check-within (cs135-exam-grade 90 95) 93.3 .1)

(define (cs135-exam-grade midterm-grade final-grade)
  
  (/ (+ (* midterm-grade midterm-weight)
        (* final-grade final-weight))
     exam-weight))

;; Tests
(check-expect (cs135-exam-grade 0 0) 0)
(check-expect (cs135-exam-grade 100 100) 100) 
(check-expect (cs135-exam-grade 0 75) 50) 
 


;; (cs135-final-grade particip-grade w-assign-grade w-exam-grade) produces
;; the final grade in the course as a percentage in the range 0 to 100
;; cs135-final-grade Num Num Num -> Num
;; Requires: particip-grade, w-assign-grade, and w-exam-grade
;;           must be in the range [0,100] inclusive
;; Examples
(check-expect (cs135-final-grade 88 75 80) 79.4)
(check-expect (cs135-final-grade 77 77 66) 68.75)

(define (cs135-final-grade particip-grade w-assign-grade w-exam-grade)
  
  (+ (* particip-grade particip-weight)
     (* w-assign-grade assign-weight)
     (* w-exam-grade exam-weight)))

;; Tests
(check-expect (cs135-final-grade 0 0 0) 0)
(check-expect (cs135-final-grade 100 100 100) 100)
(check-expect (cs135-final-grade 50 50 50) 50)

 

;; (final-cs135-exam-grade-needed midterm-grade) produces a the minimum
;; mark needed on the final exam to obtain a 50% weighted exam average
;; as a percentage in the range 0 to 100, given a midterm grade.
;; final-cs135-exam-grade-needed Num -> Num
;; Requires: midterm-grade must be in the range [0,100] inclusive
;; Examples
(check-expect (final-cs135-exam-grade-needed 88) 31)
(check-expect (final-cs135-exam-grade-needed 50) 50)

(define (final-cs135-exam-grade-needed midterm-grade) 
  
  (/ (- (* 50 exam-weight)
        (* midterm-grade midterm-weight))
     final-weight))

;; Tests
(check-expect (final-cs135-exam-grade-needed 0) 75)
(check-expect (final-cs135-exam-grade-needed 100) 25)
(check-within (final-cs135-exam-grade-needed 70.4) 39.8 .1) 
         



  
  