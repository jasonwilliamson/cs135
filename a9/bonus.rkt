;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; c3 from Question 4a (bonus)
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))
(define c4 (lambda (f ) (lambda (x) (f (f (f (f x)))))))
(define c0 (lambda (f ) (lambda (x) x)))

;; Provided family of functions:
;; (define c0 (lambda (f ) (lambda (x) x)))
;; (define c1 (lambda (f ) (lambda (x) (f x))))
;; (define c2 (lambda (f ) (lambda (x) (f (f x))))) 
;; (define c3 (lambda (f ) (lambda (x) (f (f (f x)))))) 
;; (define c4 (lambda (f ) (lambda (x) (f (f (f (f x)))))))
;; ...
;; (define ck (lambda (f) (lambda (x) (f ... (f x) ... ))))


;; (c->nat cj) consumes a function cj in the format provided above
;; and produces the natural number j
;; c->nat (Any -> Any) -> Nat
;; Examples:
(check-expect (c->nat c0) 0)
(check-expect (c->nat c3) 3)

(define (c->nat cj)
  ((cj add1) 0))

;; Tests
(check-expect (c->nat c4) 4)


;; (nat->c j) consumes j, a natural number and produces the function
;; cj (predefined format)
;; nat->c: Nat -> (Any -> Any)
;; Examples:
(check-expect (((nat->c 2) list) 'hi) (list (list 'hi)))

(define (nat->c j) 
  (foldr (lambda (f y)(y f))identity  
         (make-list j (lambda (x) x))))
  