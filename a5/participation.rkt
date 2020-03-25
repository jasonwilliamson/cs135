;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 05, Problem 2 (participation.rkt)  
;; ****************************************  


;; (occurrences lon num) consumes a (listof Num) and a Num and
;; produces the amount of items the given number occurs in the list
;; occurrences: (listof Num) Num -> Nat
;; Examples:
(check-expect (occurrences (list 8 5 9 5 2 5) 5) 3)
(check-expect (occurrences (list 8 5 9 5 2 5) 7) 0)

(define (occurrences lon num)
  (cond [(empty? lon) 0]
        [(= (first lon) num)
         (+ 1 (occurrences (rest lon) num))]
        [else (occurrences (rest lon) num)]))

;; Tests
(check-expect (occurrences (list 8 8 8) 8) 3)
(check-expect (occurrences (list 8 5 8 5 2 8) 8) 3)
(check-expect (occurrences empty 7) 0) 


;; (questions-to-be-counted num) consumes a Nat that represents the total 
;; amount of questions through-out the term, producing a Nat that 
;; represents the number of questions that should be counted
;; (75% of these total number of questions)
;; Examples:
(check-expect (questions-to-be-counted 5) 4)
(check-expect (questions-to-be-counted 7) 6)

(define (questions-to-be-counted num)
  (ceiling (* num .75)))

;; Tests
(check-expect (questions-to-be-counted 0) 0)

;; (max-score num) consumes a Nat that represents 
;; the total number of questions asked, and produces the maximum
;; possible score based on the number of questions that should be counted
;; max-score Nat -> Nat
;; requires: num > 0
;; Examples:
(check-expect (max-score 5) 8)
(check-expect (max-score 7) 12)

(define (max-score num)
  (* (questions-to-be-counted num) 2))

;; Tests
(check-expect (max-score 0) 0)


;; (total-marks twos ones counted-questions) consumes a twos representing the
;; amount of value 2's achieved, and a ones representing the amount of value 1's
;; achieved, and counted-questions representing the amount on marks the total
;; will be out of. Producing the total marks used in calculating a 
;; participation mark. If there are not enough 2's and 1's the function will 
;; fill the remaining mark with zeros.
;; total-marks: Nat Nat Nat -> Nat
;; Examples:
(check-expect (total-marks 5 0 4) 8)
(check-expect (total-marks 3 0 4) 6) 

(define (total-marks twos ones counted-questions)
  (cond [(<= counted-questions twos) (* 2 counted-questions)]
        [(> counted-questions twos)
         (cond [(> (- counted-questions twos) ones) (+ (* 2 twos) ones)]
               [else (+ (- counted-questions twos) (* 2 twos))])])) 

;; Tests
(check-expect (total-marks 3 1 4) 7) 
(check-expect (total-marks 1 1 4) 3) 
(check-expect (total-marks 0 0 0) 0) 
(check-expect (total-marks 2 3 4) 6) 


;; (participation lon) consumes a (listof Num) that represent participation
;; grade scores and produces a number (in the range of 0 to 5 inclusive)
;; representing the total participation mark.
;; participation: (listof Num) -> Num
;; requires: lon must be non-empty
;;           participation grade is either 0, 1, 2.
;;           (where participation grade in the numbers in lon)
;; Examples:
(check-expect (participation (list 2 1 2 1 1)) 3.75)
(check-expect (participation (list 0 0 0 0 0)) 0)

(define (participation lon)
  (* 5 (/ (total-marks (occurrences lon 2)
                       (occurrences lon 1)
                       (questions-to-be-counted (length lon)))
          (max-score (length lon)))))

;; Tests
(check-expect (participation (list 1 1 1 1 1)) 2.5)
(check-expect (participation (list 2 2 2 2 2)) 5)
(check-expect (participation (list 0 2 0 1 0)) 1.875)
                
                     
                
                