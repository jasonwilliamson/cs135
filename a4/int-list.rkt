;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 04, Problem 2 (int-lists.rkt)  
;; ****************************************  


;; (count-in-range loi low high) consumes a (listof Int), and two other integers
;; values, low and high; Producing the number of number that strictly within
;; the given range in the given list.
;; count-in-range: (listof Int) Int Int -> Int
;; required: low <= high
;; Examples
(check-expect (count-in-range (cons 1 (cons 2 (cons 3 (cons 4 empty)))) 2 4) 1)
(check-expect (count-in-range (cons -1 (cons -2 
                                        (cons -3 
                                         (cons -4 empty)))) -10 -6) 0)

(define (count-in-range loi low high)
  (cond
    [(empty? loi) 0]
    [else 
     (cond
       [(and (< low (first loi))
             (> high (first loi)))
        (+ 1 (count-in-range (rest loi) low high))]
       [else (count-in-range (rest loi) low high)])]))

;; Tests
(check-expect (count-in-range (cons 1 (cons 2 (cons 3 (cons 4 empty)))) 2 2) 0)
(check-expect (count-in-range (cons 1 (cons 2 (cons 3 (cons 4 empty)))) 0 5) 4)
(check-expect (count-in-range (cons 1 (cons 2 (cons 3 (cons 4 empty)))) -2 1) 0)
(check-expect (count-in-range (cons 1 (cons 2 (cons 3 (cons 4 empty)))) -2 2) 1)


;; (base-up loi base) consumes a (listof Int) and a non-zero integer base;
;; producing a new list whose ith entry is the base raised to the power of
;; the ith entry of the original list.
;; base-up: (listof Int) Int -> (listof Num)
;; requires: base must be non-zero
;; Examples
(check-expect (base-up (cons 1 (cons 2 (cons 3 empty))) 2)
              (cons 2 (cons 4 (cons 8 empty))))
(check-expect (base-up (cons -1 (cons -2 (cons -3 empty))) -2)
              (cons -0.5 (cons 0.25 (cons -0.125 empty))))

(define (base-up loi base)
  (cond
    [(empty? loi) empty]
    [else (cons (expt base (first loi)) (base-up (rest loi) base))]))

;; Tests
(check-expect (base-up (cons 1 (cons 2 (cons 3 empty))) 1)
              (cons 1 (cons 1 (cons 1 empty))))
(check-expect (base-up (cons 0 (cons 2 (cons -2 empty))) 2)
              (cons 1 (cons 4 (cons 0.25 empty))))
(check-expect (base-up (cons 1 (cons 2 (cons 3 empty))) 10)
              (cons 10 (cons 100 (cons 1000 empty))))


;; (descending? loi) consumes of (listof Int) and produces true if the entries
;; in the list appear in strictly descending order, otherwise producing false.
;; Note that a list with 0 or 1 entries is descending
;; descending?: (listof Int) -> Bool
;; Examples
(check-expect (descending? (cons 4 (cons 3 (cons 2 (cons 1 empty))))) true)
(check-expect (descending? (cons 3 (cons 2 (cons 1 (cons 1 empty))))) false)

(define (descending? loi)
  (cond 
    [(or (empty? loi)(empty? (rest loi))) true] 
    [else
     (cond
       [(> (first loi)(first (rest loi))) (descending? (rest loi))]
       [else false])]))

;; Tests
(check-expect (descending? (cons 0 empty)) true)
(check-expect (descending? empty) true)
(check-expect (descending? (cons 5 (cons -3 (cons -1 (cons 0 empty))))) false)


;; (abs-diff loi) consumes a nonempty (listof Int), producing the sum of all
;; the absolute differences between each pair of consecutive elements in the 
;; list. If the list contains one element, the function produces 0.
;; abs-diff: (listof Int) -> Int
;; requires: (listof Int) must be nonempty
;; Examples:
(check-expect (abs-diff (cons 1 (cons 2 (cons -2 (cons -1 empty))))) 6)
(check-expect (abs-diff (cons 1 (cons 2 (cons 3 (cons 4 empty))))) 3)

(define (abs-diff loi)
  (cond
    [(= (length loi) 1) 0]
    [else (+ (abs (- (first loi)(first (rest loi))))
             (abs-diff (rest loi)))]))

;; Tests
(check-expect (abs-diff (cons 4 empty)) 0)
(check-expect (abs-diff (cons 1 (cons 0 (cons -1 empty)))) 2)
(check-expect (abs-diff (cons 1 (cons 1 (cons 3 (cons 4 empty))))) 3) 
