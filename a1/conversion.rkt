;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 01, Problem 2 (conversion.rkt)  
;; ****************************************  
;; 
;; Test values provided by
;; Google ,and 
;; http://calculator-converter.com/l_100km_mpg_convert_mpg_to_l_per_100_km.php

;; Constants
;; Handy conversion values
(define meters-per-mile 1609.34) 
(define km-per-mile 1.60934)
(define seconds-per-hour 3600)
(define litres-per-gallon 3.78541)


;; (mph->m/s mph) consumes a speed in miles per hour (mph) and produces
;; a speed in units of meters per second (m/s)
;; mph->m/s Num -> Num
;; Examples:
(check-within (mph->m/s 1) 0.44 .01)
(check-within (mph->m/s 5) 2.23 .01) 

(define (mph->m/s mph)
  
  (/ (* mph meters-per-mile) seconds-per-hour)) 

;; Tests
(check-within (mph->m/s 10) 4.47 .01)
(check-within (mph->m/s 22) 9.83 .01)
(check-within (mph->m/s 33) 14.75 .01)        



;; (mpg->lp100km mpg) consumes a fuel effciency in miles-per-gallon (mpg)
;; and produces the same efficiency in units of litres-per-100km (lp100km)
;; mpg->lp100km Num -> Num
;; Requires: miles > 0
;; Examples:
(check-within (mpg->lp100km 18) 13.06 0.01)
(check-within (mpg->lp100km 10) 23.52 0.01) 

(define (mpg->lp100km miles)
  
  ( * (/ 100 (* km-per-mile miles)) litres-per-gallon))

;; Tests
(check-within (mpg->lp100km 1) 235.2 0.1) 
(check-within (mpg->lp100km 20) 11.7 0.1) 
(check-within (mpg->lp100km 40) 5.8 0.1) 



;; (lp100km->mpg litres) consumes a fuel effciency in litres-per-100km (lp100km)
;; and produces the same efficiency in units of miles-per-gallon (mpg)
;; lp100km->mpg Num -> Num
;; Requires: litres > 0
;; Examples  
(check-within (lp100km->mpg 10) 23.5 .1)
(check-within (lp100km->mpg 5) 47 .1)

(define (lp100km->mpg litres)
  
  (/ (/ 100 km-per-mile) (/ litres litres-per-gallon)))

;; Tests
(check-within (lp100km->mpg 1) 235.2 .1)
(check-within (lp100km->mpg 15) 15.6 .1)
(check-within (lp100km->mpg 100) 2.3 .1)


