;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 09, Problem 2 (alfs.rkt)  
;; **************************************** 

;; (x-coords-of-posns loa) consumes loa, a (listof Any) and produces the list
;; of all of the x-coordinates of the Posns in the list in the same order that
;; they appear in the consumed list
;; x-coords-of-posns: (listof Any) -> (listof Num)
;; Examples:
(check-expect (x-coords-of-posns (list (make-posn 1 2) 'sun (make-posn 3 4)))
              '(1 3))
(check-expect (x-coords-of-posns (list "hello" 1245 'apple)) empty)

(define (x-coords-of-posns loa)
  ;; (x-coord p) consumes p, a Posn and returns the x-coord value of p
  ;; x-coord: (Posn) -> Num
  (local [(define (x-coord p)
            (posn-x p))]    
    (map x-coord (filter posn? loa))))

;; Tests
(check-expect (x-coords-of-posns (list (make-posn 1 2) (make-posn 5 6) 
                                       (make-posn 3 4))) '(1 5 3))


;; (alternating-sum lon) consumes lon, a (listof Num) and produces the
;; alternating sum (a1-a2+a3-a4+...(-1)^n-1 an). if the list is empty
;; the funciton produces 0.
;; alternating-sum (listof Num) -> Num
;; Examples:
(check-expect (alternating-sum '(1 2 3 4)) -2)
(check-expect (alternating-sum empty) 0)

(define (alternating-sum lon)
  (foldr - 0 lon))

;; Tests
(check-expect (alternating-sum '(7 3 5 6)) 3)
(check-expect (alternating-sum '(5 -4 1/2 8 -1)) 1/2)


;; (remove-duplicates lon) consumes lon, a (listof Num) and produces the list
;; with all but the first occurrence of every number removed.
;; remove-duplicates: (listof Num) -> (listof Num)
;; Examples:
(check-expect (remove-duplicates '(1 4 2 1 5 4)) '(1 4 2 5))
(check-expect (remove-duplicates empty) empty)

(define (remove-duplicates lon)
  ;; (remove-occurances n lon) consumes n, a Num and lon, a (listof Num)
  ;; producing a (listof Num) were all occurence of n is removed.
  (local [(define (remove-occurances n lon)
            (local [(define (occur? x) (not (= n x)))]
              (filter occur? lon)))]
    (foldr (lambda (x y) (cons x (remove-occurances x y))) empty lon)))

;; Tests
(check-expect (remove-duplicates '(1 1 4 2 -1 3 45 4 5 4)) '(1 4 2 -1 3 45 5))


;; (first-col llon) consumes a list of non-empty list of number denoting a 
;; recangular matrix of numbers, and produces the first column of the matrix
;; as a list
;; first-col: (listof (listof Num)) -> (listof Num)
;; requires: llon must be non-empty
;; Examples:
(check-expect (first-col '((1 2 3 4)
                           (5 6 7 8)
                           (9 10 11 12))) '(1 5 9))
(check-expect (first-col '((2 3 4 5 6)
                           (7 8 9 10 11)
                           (12 13 14 15 16))) '(2 7 12))

(define (first-col llon)
  (foldr (lambda (x y) (cons (first x) y)) empty llon))
  
;; Tests
(check-expect (first-col '((2 3 4 5 6)
                           (7 8 9 10 11)
                           (12 13 14 15 16)
                           (17 18 19 20 21))) '(2 7 12 17))


;; (add1-mat llon) consumes a (listof (listof Num)) denoting a rectangular
;; matrix of number, and produces the matrix resulting from adding 1 to 
;; every entry of the matrix
;; add1-mat llon: (listof (listof Num)) -> (listof (listof Num))
;; Examples:
(check-expect (add1-mat '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
              '((2 3 4 5) (6 7 8 9) (10 11 12 13)))
(check-expect (add1-mat empty)
              empty)

(define (add1-mat llon)
  (local [(define (my-filter lon)
            (foldr (lambda (x y) (cons (add1 x) y)) empty lon))]
    (map my-filter llon)))
  
;; Tests
(check-expect (add1-mat '((2 3 4 5 6)
                           (7 8 9 10 11)
                           (12 13 14 15 16)
                           (17 18 19 20 21)))
              '((3 4 5 6 7)
                (8 9 10 11 12)
                (13 14 15 16 17)
                (18 19 20 21 22)))


;; (sum-at-zero lof) consumes lof, a (listof Functions)
;; (f1,..., fn) (where each consumes and produces a number) the function 
;; produces the value f1(0)+···+ fn(0), the sum of all functions in the
;; list taken at zero
;; sum-at-zero: (listof (Num -> Num)) -> (listof Num)
;; Examples:
(check-expect (sum-at-zero (list add1 sqr add1)) 2)
(check-expect (sum-at-zero empty) 0)

(define (sum-at-zero lof)
  (foldr (lambda (x y)(+ (x 0) y)) 0 lof))

;; Tests
(check-expect (sum-at-zero (list sin cos sin)) 1)



 