;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood-cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 3 (blood-cond.rkt)  
;; **************************************** 


;; (can-donate-to/cond? donor-blood-type recipient-blood-type) produces
;; true if the donor's blood type is acceptable for the recipient's blood 
;; type, otherwise false
;; can-donate-to/cond?: Sym Sym -> Bool
;; Examples
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O- 'AB-) true)
 
(define (can-donate-to/cond? donor-bt recipient-bt)  
  
  (cond 
    [(equal? donor-bt 'O-) true]
    [(equal? recipient-bt 'AB+) true]
    [(equal? donor-bt 'O+) 
     (cond
       [(equal? recipient-bt 'O+) true]
       [(equal? recipient-bt 'A+) true]
       [(equal? recipient-bt 'B+) true]
       [else false])] 
    [(equal? donor-bt 'A-)
     (cond
       [(equal? recipient-bt 'A-) true]
       [(equal? recipient-bt 'A+) true]
       [(equal? recipient-bt 'AB-) true]
       [else false])]
    [(equal? donor-bt 'A+)
     (cond
       [(equal? recipient-bt 'A+) true]
       [else false])]
    [(equal? donor-bt 'B-)
     (cond
       [(equal? recipient-bt 'B-) true]
       [(equal? recipient-bt 'B+) true]
       [(equal? recipient-bt 'AB-) true]
       [else false])]
    [(equal? donor-bt 'B+)
     (cond
       [(equal? recipient-bt 'B+) true]
       [else false])]
    [(equal? donor-bt 'AB-)  
     (cond 
       [(equal? recipient-bt 'AB-) true]
       [else false])]))
     
;; Tests
(check-expect (can-donate-to/cond? 'O+ 'A-) false)
(check-expect (can-donate-to/cond? 'AB- 'AB+) true) 
(check-expect (can-donate-to/cond? 'AB- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'B+) false)
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'O+) false)



