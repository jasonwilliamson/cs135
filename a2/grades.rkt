;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 2 (grades.rkt)  
;; ****************************************  

;; Constants
;; grade weighted values
(define midterm-weight (/ 25 100))
(define final-weight (/ 50 100))
(define exam-weight (+ midterm-weight final-weight))
(define particip-weight (/ 5 100))
(define assign-weight (/ 20 100))

;; grade values 
(define passing-grade 50)
(define defualt-fail-grade 46)


;; (cs135-final-grade particip-grade w-assign-grade w-exam-grade) produces
;; the final grade in the course as a percentage in the range 0 to 100. If
;; the weighted assignment grade or the weighted exam average is below 50 
;; percent, a grade of 46 will be produces or the calculated final grade
;; which ever is smaller
;; cs135-final-grade: Num Num Num -> Num
;; Requires: particip-grade, w-assign-grade, and w-exam-grade
;;           must be in the range [0,100] inclusive
;; Examples
(check-expect (cs135-final-grade 80 80 80) 80)
(check-expect (cs135-final-grade 77 77 66) 68.75)

(define (cs135-final-grade particip-grade w-assign-grade w-exam-grade)
  
  (cond
    [(or (< w-assign-grade passing-grade) (< w-exam-grade passing-grade))
     (cond
       [(< defualt-fail-grade
        (+ (* particip-grade particip-weight)
           (* w-assign-grade assign-weight)
           (* w-exam-grade exam-weight)))
        defualt-fail-grade]
       [else 
        (+ (* particip-grade particip-weight)
           (* w-assign-grade assign-weight) 
           (* w-exam-grade exam-weight))])]
     [else  
      (+ (* particip-grade particip-weight) 
           (* w-assign-grade assign-weight)
           (* w-exam-grade exam-weight))]))

;; Tests
(check-expect (cs135-final-grade 0 0 0) 0)
(check-expect (cs135-final-grade 47 47 47) 46)
(check-expect (cs135-final-grade 45 45 46) 45.75)
(check-expect (cs135-final-grade 49 48 50) 46)