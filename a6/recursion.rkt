;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 06, Problem 1 (recursion.rkt)  
;; ****************************************  


;; (after-n n v los) consumes three parameters: the natural number n, the symbol
;; to search for v, and a list of symbols. Producing a list of symbols that
;; occur after the nth occurence of v in the consumed list.
;; after-n: Nat Sym (listof Sym) -> (listof Sym)
;; Examples:
(check-expect (after-n 2 'a '(a b a e n d)) '(e n d))
(check-expect (after-n 0 'z '(a b a e n d)) '(a b a e n d)) 
 
(define (after-n n v los)
  (cond [(empty? los) empty] ;v does not appear in the list at least n times
        [(zero? n) los]      ;produce the list that appear after nth occurences
        [else (cond [(symbol=? v (first los)) (after-n (sub1 n) v (rest los))]
                    [else (after-n n v (rest los))])]))

;; Tests
(check-expect (after-n 10 'a '(a b a e n d)) empty)
(check-expect (after-n 0 'a empty) empty)       
(check-expect (after-n 3 'a '(a b a e a d)) '(d))


;; (list-posn lloi psn) consumes a (listof (listof Int)) and a Posn. Producing 
;; an integer or false. The integer that is produced is the yth element of xth
;; list in the consumed list of lists. If there is no such element, the function
;; produces false
;; list-posn: (listof (listof Int)) Posn -> (anyof Int Bool)
;; requires: both lloi and psn must be positive integers
;; Examples:
(check-expect (list-posn '((1 2 3) (4 5) (6 7 8)) (make-posn 2 1)) 4)
(check-expect (list-posn '((1 2 3) (4 5) (6 7 8)) (make-posn 1 4)) false) 

(define (list-posn lloi psn)
  (cond [(empty? lloi) false] 
        [(= 1 (posn-x psn)) ; xth found
         (cond [(empty? (first lloi)) (list-posn empty psn)] ; yth D.N.E  
               [(= 1 (posn-y psn)) (first (first lloi))]     ; yth found
               ;else yth not yet found, recursive call preserving nested list
               [else (list-posn (cons (rest (first lloi)) (rest lloi)) 
                                (make-posn 1 (sub1 (posn-y psn))))])]
        [else (list-posn (rest lloi) 
                         (make-posn (sub1 (posn-x psn)) (posn-y psn)))])) 
 
;; Tests  
(check-expect (list-posn '((1 2 3) (4 5) (6 7 8)) (make-posn 1 11)) false)  
(check-expect (list-posn '((1 2 3) (4 5) (6 7 8)) (make-posn 2 2)) 5)    
(check-expect (list-posn '((1 2 3) (4 5) (6 7 8)) (make-posn 1 2)) 2) 


;; (every-mod m lst acc) consumes a positive integer representing 
;; a list (lst) ,a modulo value (m), an accumulator (acc), and produces
;; a list where each acc modulo m = 0
;; every-mod: Nat (listof any) Nat
;; requires: acc must be given a value of 1
;; Examples:
(check-expect (every-mod '(1 2 3 4 5 6 7 8 9) 3 1) '(3 6 9))
(check-expect (every-mod '(1 2 3 4) 1 1) '(1 2 3 4))
 
(define (every-mod lst m acc) 
  (cond [(empty? lst) empty]
        [(zero? (modulo acc m))  
         (cons (first lst)(every-mod (rest lst) m (add1 acc)))]
        [else (every-mod (rest lst) m (add1 acc))]))

;; Tests
(check-expect (every-mod empty 1 1) empty)
(check-expect (every-mod '(1 2 3 4 5 6 7 8 9) 11 1) empty)
(check-expect (every-mod '(1 2 3 4 5 6 7 8 9) 9 1) '(9))

;; (every-nth lst n) consumes a list and a positive integer n, and produces
;; a list containing every nth element from the original list, where the first
;; element in the list is at index 1
;; every-nth: (listof Any) Nat -> (listof Any)
;; Examples:
(check-expect (every-nth '(1 2 3 4 5 6 7 8 9) 3) '(3 6 9))
(check-expect (every-nth '(1 2 3 4) 1) '(1 2 3 4))

(define (every-nth lst n)
  (every-mod lst n 1)) 

;; Tests
(check-expect (every-nth empty 1) empty)
(check-expect (every-nth '(1 2 3 4 5 6 7 8 9) 11) empty)
(check-expect (every-nth '(1 2 3 4 5 6 7 8 9) 9) '(9))


;; (mult-score los1 los2) consumes two list of symbols each of the same length.
;; The first list holds the responses to multiple choice questions, the second
;; list has the correct answers to the corresponding questions. 
;; The score for each question is:
;; * 5 if the answer is correct;
;; * 2 if the answer is blank;
;; * 0 if the answer is incorrect;
;; Producing the total score, or 0 if both lists are empty
;; mult-score: (listof Sym) (listof Sym) -> Nat
;; requires: los1 and los2 must be the same length
;;           los1 values must be one of:
;;                'A, 'B, 'C, 'D, 'E, 'blank
;;           los2 values must be one of:
;;                'A, 'B, 'C, 'D, 'E
;; Examples:
(check-expect (mult-score '(A B blank D) '(B B B D)) 12)
(check-expect (mult-score empty empty) 0)

(define (mult-score los1 los2)
  (cond [(empty? los1) 0]
        [(symbol=? (first los1) 'blank) 
         (+ (mult-score (rest los1) (rest los2)) 2)]
        [(symbol=? (first los1) (first los2))
         (+ (mult-score (rest los1) (rest los2)) 5)]
        [else (mult-score (rest los1) (rest los2))]))

;; Tests
(check-expect (mult-score '(E E E E) '(B B B D)) 0)
(check-expect (mult-score '(B B B D) '(B B B D)) 20)
(check-expect (mult-score '(blank blank blank blank) '(B B B D)) 8) 


               