;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 01, Problem 1 (functions.rkt)  
;; ****************************************  
;; 
;; Test data provided by
;; http://www.mathopenref.com/spherevolume.html
;; http://en.wikipedia.org/wiki/Harmonic_mean
;; http://www.mathworks.com/help/symbolic/mupad_ref/stats-harmonicmean.html 


;; (sphere-volume r) calculates the volume of a sphere given the raduis r
;; sphere-volume Num -> Num
;; requires: r > 0 (r must be a postive number)
;; Examples:
(check-within (sphere-volume 2) 33.5 .1) 
(check-within (sphere-volume 10) 4188.8 .1)

(define (sphere-volume r ) 
  
  ( / (* 4 pi (expt r 3)) 3))

;; Tests
(check-expect (sphere-volume 0) 0) 
(check-within (sphere-volume 1) 4.1 .1) 
(check-within (sphere-volume 11) 5575.3 .1)



;; (HM x y z) calculates the harmanic mean of three numbers
;; HM Num Num Num -> Num
;; requires: x > 0, y > 0, z > 0 
;; Examples:
(check-expect (HM 1 2 4) (/ 12 7)) 
(check-expect (HM 2 2 2) 2)

(define (HM x y z) 
  
  (/ 3 (+ (/ 1 x) (/ 1 y) (/ 1 z))))
 
;; Tests
(check-expect (HM 2 3 5) (/ 90 31))
(check-expect (HM 1 1 1) 1)
(check-within (HM 10 7 11) 8.9 .1)



;; acceleration due to gravity
(define g 9.8) 

;; (height v t) calculates the height of an object thrown straight up
;; with velocity v after time t
;; height Num Num -> Num
;; requires: v > 0, t > 0
;; Examples
(check-within (height 15 .5) 6.2 .1)
(check-within (height 10 1) 5.1 .1)

(define (height v t)
  
  (- (* v t)(/ (* g (sqr t)) 2)))

;; Tests
(check-within (height 1 1) -3.9 0.1) 
(check-within (height 2 1) -2.9 0.1)
(check-within (height 4 1) -0.9 .1)



