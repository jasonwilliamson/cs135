;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 08, Problem 3 (participation.rkt)  
;; ****************************************  


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
  (local [
   
   ;; (questions-to-be-counted num) consumes a Nat that represents the total 
   ;; amount of questions through-out the term, producing a Nat that 
   ;; represents the number of questions that should be counted
   ;; (75% of these total number of questions)
   (define (questions-to-be-counted num)
     (ceiling (* num .75)))
   
   
   ;; (max-score num) consumes a Nat that represents 
   ;; the total number of questions asked, and produces the maximum
   ;; possible score based on the number of questions that should be counted
   ;; max-score Nat -> Nat
   ;; requires: num > 0
   (define (max-score num)
     (* (questions-to-be-counted num) 2))
   
   
   ;; (total-marks twos ones counted-questions) consumes a twos representing the
   ;; amount of value 2's achieved, and a ones representing the amount of value 1's
   ;; achieved, and counted-questions representing the amount on marks the total
   ;; will be out of. Producing the total marks used in calculating a 
   ;; participation mark. If there are not enough 2's and 1's the function will 
   ;; fill the remaining mark with zeros.
   ;; total-marks: Nat Nat Nat -> Nat
   (define (total-marks twos ones counted-questions)
     (cond [(<= counted-questions twos) (* 2 counted-questions)]
        [(> counted-questions twos)
         (cond [(> (- counted-questions twos) ones) (+ (* 2 twos) ones)]
               [else (+ (- counted-questions twos) (* 2 twos))])]))
   
   
   ;; (occurrences lon num) consumes a (listof Num) and a Num and
   ;; produces the amount of items the given number occurs in the list
   ;; occurrences: (listof Num) Num -> Nat
   ;; Examples:
   (define (occurrences lon num)
     (cond [(empty? lon) 0]
        [(= (first lon) num)
         (+ 1 (occurrences (rest lon) num))]
        [else (occurrences (rest lon) num)]))]
 
            
  (* 5 (/ (total-marks (occurrences lon 2)
                       (occurrences lon 1)
                       (questions-to-be-counted (length lon)))
          (max-score (length lon))))))

;; Tests
(check-expect (participation (list 1 1 1 1 1)) 2.5)
(check-expect (participation (list 2 2 2 2 2)) 5)
(check-expect (participation (list 0 2 0 1 0)) 1.875)