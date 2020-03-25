;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 05, Problem 1 (change.rkt)  
;; ****************************************  


(define-struct coin (name value))
;; A Coin is a (make-coin Sym Num)
;; requires: value is a Num > 0


;; my-coin-fn Coin -> Any
(define (my-coin-fn c)
  (... (coin-name c)...
       (coin-value c)...))
   
 
;; my-listof-coin-fn (listof Coin) -> Any
(define (my-listof-coin-fn loc)
  (cond [(empty? loc)...]
        [else (... (coin-name (first loc)) ...
                   (coin-value (first loc))...
                   (my-listof-coin-fn (rest loc)) ...)]))


;; Provided in Basic Tests
(define penny (make-coin 'penny 0.01))
(define nickel (make-coin 'nickel 0.05))
(define dime (make-coin 'dime 0.10))


;; (value-of-coin loc name) consumes a (listof Coin) and a symbol represening 
;; the name of a coin. Producing the value associated with the name of the 
;; given coin, or 0 if the coin name is not in the list of Coins
;; value-of-coin (listof Coin) Sym -> Num
;; requires: Coins in the (listof Coin) ,or loc, must all have distinct names
;; Examples:
(check-expect (value-of-coin (list penny nickel dime) 'nickel) 0.05)
(check-expect (value-of-coin (list penny nickel dime) 'looney) 0)

(define (value-of-coin loc name)
  (cond [(empty? loc) 0]
        [else (cond [(symbol=? name (coin-name (first loc)))
                     (coin-value (first loc))]
                    [else (value-of-coin (rest loc) name)])]))
                      
;; Tests
(check-expect (value-of-coin empty 'nickel) 0) 
(check-expect (value-of-coin (list penny nickel dime) 'dime) 0.10)
(check-expect (value-of-coin (list penny nickel dime) 'penny) 0.01)


;; (sum-coins loc los) comsumes two lists, first is a (listof Coin) 
;; and the second is a (listof Sym). Producing the total value of the 
;; currency from the symbols listed in the second list that correspond
;; to the Coins in the first
;; sum-coins: (listof Coin) (listof Sym) -> Num
;; requires: Coins in the (listof Coin) ,or loc, must all have distinct names
;; Examples:
(check-expect (sum-coins (list penny nickel dime)
                         (list 'penny 'dime 'dime)) 0.21)
(check-expect (sum-coins (list penny nickel dime)
                         empty) 0) 
 
(define (sum-coins loc los)
  (cond [(empty? los) 0]
        [else (+ (value-of-coin loc (first los))  
                 (sum-coins loc (rest los)))])) 

;; Tests
(check-expect (sum-coins (list penny nickel dime)
                         (list 'looney 'dime 'dime)) 0.20) 
(check-expect (sum-coins (list penny nickel dime)
                         (list 'dime 'dime 'dime)) 0.30)
                    

