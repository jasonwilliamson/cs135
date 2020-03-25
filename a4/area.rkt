;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 04, Problem 4 (area.rkt)  
;; ****************************************  

;; (add-to-end-of-list list element) consumes a (listof Any) and an element,
;; adding the element to the end of the list.
;; add-to-end-of-list: (listof Any) -> (listof Any)
;; requires: element must be non empty
;; Examples
(check-expect (add-to-end-of-list (cons 3 (cons 6 (cons 2 empty))) 3)
              (cons 3 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (add-to-end-of-list (cons 2 empty) 2)
              (cons 2(cons 2 empty))) 

(define (add-to-end-of-list lst element)
  (cond
    [(empty? lst) empty]
    [(not (cons? (rest lst))) 
          (cons (first lst)
                (cons element (add-to-end-of-list (rest lst) element)))]
    [else (cons (first lst) (add-to-end-of-list (rest lst) element))]))

;; Tests
(check-expect (copy-first-to-end (cons 3 (cons 4 (cons 5 empty))))
              (cons 3 (cons 4 (cons 5 (cons 3 empty)))))


;; (copy-first-to-end list) consumes a non empty (listof Any) and produces the 
;; the same list with its first element to the end of the list.
;; copy-first-to-end: (listof Any) -> (listof Any)
;; requires: provided list must be non empty
;; Examples:
(check-expect (copy-first-to-end (cons 3 (cons 6 (cons 2 empty))))
              (cons 3 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (copy-first-to-end (cons 2 empty))
              (cons 2(cons 2 empty)))

(define (copy-first-to-end lst)
  (add-to-end-of-list lst (first lst)))

;; Tests
(check-expect (copy-first-to-end (cons 3 (cons 4 (cons 5 empty))))
              (cons 3 (cons 4 (cons 5 (cons 3 empty)))))

;; (shoelace-alg lop) consumes a list of Posn and produces a value using
;; the "shoelace" algorithm, [(x1 * y2) - (x2 * y1)] +
;;                           [(x2 * y3) - (x3 * y2)] + 
;;                           ...                     +
;;                           [(xn * y1) - (x1 * yn)]
;; shoelace-alg: (listof Posn) -> Num
;; required: non-empty (listof Posn)
(check-expect (shoelace-alg  
               (cons (make-posn 1 1) 
                     (cons (make-posn 0 0) 
                           (cons (make-posn 1 0) 
                                 (cons (make-posn 1 1) empty))))) 1)
(check-expect (shoelace-alg  
               (cons (make-posn 1 1) 
                     (cons (make-posn 3 2) 
                           (cons (make-posn 5 5) 
                                 (cons (make-posn 1 1) empty))))) 4)

(define (shoelace-alg lop)
  (cond
    [(or (empty? lop) (not (cons? (rest lop)))) 0]
    [else (+
     (- (* (posn-x (first lop))(posn-y (first (rest lop))))
        (* (posn-y (first lop))(posn-x (first (rest lop)))))
     (shoelace-alg (rest lop)))])) 

;; Tests
(check-expect (shoelace-alg  
               (cons (make-posn 0 0) 
                     (cons (make-posn 3 -1) 
                     (cons (make-posn 10 0) 
                     (cons (make-posn 8 7) 
                           (cons (make-posn 0 0) empty)))))) 80)

;; (area-of-polygon lop) consumes a non-empty list of Posns, and produces the
;; area of the polygon with those Posns as successive vertices.
;; area-of-polygon: (listof Posn) -> Num
;; requires: (listof Posn) must be non-empty
;; Examples:
(check-expect (area-of-polygon 
               (cons (make-posn 1 1) 
                     (cons (make-posn 0 0) 
                           (cons (make-posn 1 0) empty))))
              1/2)
(check-expect (area-of-polygon  
               (cons (make-posn 1 1) 
                     (cons (make-posn 3 2) 
                           (cons (make-posn 5 5) 
                                 (cons (make-posn 1 1) empty))))) 2)

(define (area-of-polygon lop)
  (/ (shoelace-alg (copy-first-to-end lop)) 2)) 

;; Tests
(check-expect (area-of-polygon  
               (cons (make-posn 0 0) 
                     (cons (make-posn 3 -1) 
                     (cons (make-posn 10 0) 
                     (cons (make-posn 8 7) 
                           (cons (make-posn 0 0) empty)))))) 40)



          