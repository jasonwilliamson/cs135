;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname phone) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 06, Problem 3 (phone.rkt)  
;; ****************************************


;; An association list (Char AL) is one of:
;; * empty
;; * (cons (list Char Num) Char AL)

;; define: association-list
(define phone-letters '( (#\a 2)(#\b 2)(#\c 2)
                         (#\d 3)(#\e 3)(#\f 3)
                         (#\g 4)(#\h 4)(#\i 4)
                         (#\j 5)(#\k 5)(#\l 5)
                         (#\m 6)(#\n 6)(#\o 6)
                         (#\p 7)(#\q 7)(#\r 7)(#\s 7)
                         (#\t 8)(#\u 8)(#\v 8)
                         (#\w 9)(#\x 9)(#\y 9)(#\z 9)))


;; (contains-false? lst) consumes lst, (listof Any) and produces
;; the value true if the value false is found within the list. 
;; Otherwise function produces the value false.
;; contains-false?: (listof Any) -> Bool
;; Examples:
(check-expect (contains-false? (list 1 2 3 false)) true)
(check-expect (contains-false? (list 1 2 3)) false)

(define (contains-false? lst) 
  (cond [(empty? lst) false]
        [(false? (first lst)) true]
        [else (contains-false? (rest lst))]))

;; (look-up-al k alst) consumes k, a Char and alst a (Char AL)
;; producing the value corresponding to a key k found in alst or
;; false if k not present.
;; look-up-al: Char (Char AL) -> (anyof Nat Bool)
;; requires: all values k must be in lowercase.
;; Examples:
(check-expect (look-up-al #\a phone-letters) 2)
(check-expect (look-up-al #\0 phone-letters) false)

(define (look-up-al k alst)
  (cond [(empty? alst) false]
        [(char=? k (first (first alst))) (second (first alst))]
        [else (look-up-al k (rest alst))])) 


;; (look-up-list loc) consumes loc, a (listof Char) and produces a 
;; list of values determined by checking each element in the list against
;; a (Char AL).
;; look-up-list: (listof Char) -> (listof (anyof Nat Bool))
;; Example:
(check-expect (look-up-list '(#\a #\e #\i)) '(2 3 4))
(check-expect (look-up-list '(#\a #\b #\c)) '(2 2 2))

(define (look-up-list loc)
  (cond [(empty? loc) empty]
        [else (cons (look-up-al (char-downcase (first loc)) phone-letters)
                    (look-up-list (rest loc)))]))

;; Tests
(check-expect (look-up-list empty) empty)

;; (lon->nat lon acc) consumes lon, (listof Nat) and acc, Nat and
;; that represent the exponential value accumultor
;; Producing a positive integer from a lon, where the first element
;; is 10^0's the second is 10^1's and so on.
;; lon->nat: (listof Nat) Nat -> Nat
;; Examples:
(check-expect (lon->nat '(1 2 3) 0) 321)
(check-expect (lon->nat '(3 2 1) 1) 1230)

(define (lon->nat lon acc)
  (cond [(empty? lon) 0]
        [else  
         ( + (* (first lon)(expt 10 acc))
             (lon->nat (rest lon)(add1 acc)))]))

;; Tests
(check-expect (lon->nat empty 0) 0)

;; (reverse-list/acc lst acc) consumes lst, a (listof Any) and acc,
;; a accumulator list (listof Any), and produces a 
;; list with the values in reverse order.
;; reverse-list/acc: (listof Any) (listof Any) -> (listof Any)
;; Examples:
(check-expect (reverse-list/acc '(1 2 3) empty) '(3 2 1))
(check-expect (reverse-list/acc '(3 2 1) empty) '(1 2 3))

(define (reverse-list/acc lst acc)
  (cond [(empty? lst) acc]
        [else (reverse-list/acc (rest lst)
                                (cons (first lst) acc))]))

;; Tests
(check-expect (reverse-list/acc empty empty) empty)
(check-expect (reverse-list/acc '(3) empty) '(3))

;; (mnemonic->num s al) consumes s, a string and al, an association list
;; that maps character keys to digit values. Producing a natural number
;; by substituting each character with its associated value. This function
;; is not case sensitive and will take strings with both upper and lower case 
;; letters. However the association list must contain lowercase chars.
;; If any character does not appear in the given association list
;; the function will produce the value false. If the string is empty the 
;; function will produce the value 0.
;; mnemonic->num: Str (Char AL) -> (anyof Nat Bool)
;; Example:
(check-expect (mnemonic->num "aei" '((#\a 2) (#\e 3) (#\i 4))) 234)
(check-expect (mnemonic->num "ae9" '((#\a 2) (#\e 3) (#\i 4))) false)

(define (mnemonic->num s al) 
  (cond [(empty? s) 0]
        ;return false if the list has a false value in it
        [(contains-false? (look-up-list (string->list s))) false]
        [else (lon->nat (reverse-list/acc (look-up-list (string->list s)) 
                                          empty) 0)])) 
         
;; Tests 
(check-expect (mnemonic->num "" '((#\a 2) (#\e 3) (#\i 4))) 0)
(check-expect (mnemonic->num empty '((#\a 2) (#\e 3) (#\i 4))) 0)
(check-expect (mnemonic->num "&ab" '((#\a 2) (#\e 3) (#\i 4))) false)
