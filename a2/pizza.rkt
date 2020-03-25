;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 02, Problem 4 (pizza.rkt)  
;; **************************************** 

;; Constants
;; price values
(define small-price 6)
(define medium-price 8)
(define large-price 9.50)
(define standard-topping-price 1)
(define premium-topping-price 1.50)

;; Constants
;; coupon codes
(define half-off-deal .50)
(define big-eater-deal 18)
(define solo-deal 8)


;; (calculate-pizza-cost size stnd-top prem-top) produces the price of a pizza
;; specified by the three values consumed, size of pizza,
;; number of standard toppings, number of premium toppings
;; calculate-pizza-cost: Sym Nat Nat -> Num
;; Examples
(check-expect (calculate-pizza-cost 'large 1 2) 13.5)
(check-expect (calculate-pizza-cost 'small 1 1) 8.5)

(define (calculate-pizza-cost size stnd-top prem-top)
  
  (+ 
   (cond
     [(equal? size 'small) small-price]
     [(equal? size 'medium) medium-price]
     [(equal? size 'large) large-price])
   (* standard-topping-price stnd-top)
   (* premium-topping-price prem-top)))

;; Tests
(check-expect (calculate-pizza-cost 'medium 1 2) 12)
(check-expect (calculate-pizza-cost 'large 5 5) 22)
   
    


;; (pizza-price size stnd-top prem-top coupon) produces the price of a pizza
;; specified by the four values consumed, size of pizza,
;; number of standard toppings, number of premium toppings, and
;; coupon code
;; pizza-price: Sym Nat Nat Sym -> Num
;; Required: only one coupon code can be used and value 'none is
;;           used to represent no couplon code
;; Examples
(check-expect (pizza-price 'large 1 2 'supersize) 10)
(check-expect (pizza-price 'large 1 2 'none) 13.5)

(define (pizza-price size stnd-top prem-top coupon)
   
  (cond 
    
    [(equal? coupon 'none)
     (calculate-pizza-cost size stnd-top prem-top)]
    
    [(equal? coupon 'half-off)
     (* (calculate-pizza-cost size stnd-top prem-top) half-off-deal)]
    
    [(equal? coupon 'big-eater)
     big-eater-deal]
    
    [(and (equal? coupon 'supersize) 
          (or (equal? size 'medium)
              (equal? size 'large)))
     (calculate-pizza-cost 'small stnd-top prem-top)]
    
    [(and (equal? coupon 'solo)
          (equal? size 'small)
          (= stnd-top 0)
          (= prem-top 2))
     solo-deal]
    
    [else (calculate-pizza-cost size stnd-top prem-top)]))

;; Tests
(check-expect (pizza-price 'small 0 2 'solo) 8)
(check-expect (pizza-price 'small 1 2 'solo) 10) 
(check-expect (pizza-price 'large 10 2 'big-eater) 18)
(check-expect (pizza-price 'large 1 2 'half-off) 6.75) 

      
     
     
  
  