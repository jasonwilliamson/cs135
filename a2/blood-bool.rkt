;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood-bool) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 3 (blood-bool.rkt)  
;; ****************************************


;; (can-donate-to/bool? donor-blood-type recipient-blood-type) produces
;; true if the donor's blood type is acceptable for the recipient's blood 
;; type, otherwise false
;; can-donate-to/bool?: Sym Sym -> Bool
;; Examples
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'O- 'AB-) true)

(define (can-donate-to/bool? donor-bt recipient-bt)
  
  (or (equal? donor-bt 'O-)
      (equal? recipient-bt 'AB+)
      (and (equal? donor-bt 'O+)
           (or (equal? recipient-bt 'O+)
               (equal? recipient-bt 'A+)
               (equal? recipient-bt 'B+)))
      (and (equal? donor-bt 'A-)
           (or (equal? recipient-bt 'A-)
               (equal? recipient-bt 'A+)
               (equal? recipient-bt 'AB-)))
      (and (equal? donor-bt 'A+)
           (equal? recipient-bt 'A+))
      (and (equal? donor-bt 'B-)
           (or (equal? recipient-bt 'B-)
               (equal? recipient-bt 'B+)
               (equal? recipient-bt 'AB-)))
      (and (equal? donor-bt 'B+)
           (equal? recipient-bt 'B+))
      (and (equal? donor-bt 'AB-)
           (equal? recipient-bt 'AB-))))

;; Tests
(check-expect (can-donate-to/bool? 'O+ 'A-) false)
(check-expect (can-donate-to/bool? 'AB- 'AB+) true) 
(check-expect (can-donate-to/bool? 'AB- 'AB-) true)
(check-expect (can-donate-to/bool? 'A- 'A+) true)
(check-expect (can-donate-to/bool? 'A+ 'B+) false)
(check-expect (can-donate-to/bool? 'A+ 'A+) true)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'B- 'O+) false)

  