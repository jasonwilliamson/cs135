;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inside) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 03, Problem 5 (inside.rkt)  
;; ****************************************  

;; Supporting test information provided by
;; http://www.mathopenref.com/coordtrianglearea.html
;; http://www.mathopenref.com/coordpolygonarea.html


;; (tri-area p1 p2 p3) consumes three distinct Posn's and produces the area
;; of the triangle they form
;; tri-area: Posn Posn Posn -> Num
;; Examples
(check-expect (tri-area (make-posn 0 0)(make-posn 5 4)(make-posn 8 2)) 11)
(check-expect (tri-area (make-posn 0 0) (make-posn 0 2)(make-posn 2 2)) 2)

(define (tri-area p1 p2 p3)
  (abs (/ (+ (* (posn-x p1)(- (posn-y p2)(posn-y p3)))
             (* (posn-x p2)(- (posn-y p3)(posn-y p1)))
             (* (posn-x p3)(- (posn-y p1)(posn-y p2)))) 2)))

;; Tests
(check-expect (tri-area (make-posn 23 30) (make-posn 15 15)(make-posn 50 25))
              222.5)
(check-expect (tri-area (make-posn 9 15) (make-posn 20 13)(make-posn 20 5)) 44)


;; (quadril-area p1 p2 p3 p4) consumes four distinct Posn's and produces the
;; area of the quadrilateral that is formed from these coordinates
;; quadril-area: Posn Posn Posn Posn -> Num
;; Examples
(check-expect (quadril-area (make-posn 2 -2)(make-posn 2 6)
                            (make-posn 15 2)(make-posn 15 -4)) 91)
(check-expect (quadril-area (make-posn 0 0) (make-posn 0 2)
                            (make-posn 2 2) (make-posn 2 0)) 4)

(define (quadril-area p1 p2 p3 p4)
  (+ (tri-area p1 p2 p3)
     (tri-area p3 p4 p1))) 

;; Tests
(check-expect (quadril-area (make-posn 0 0) (make-posn 0 0)
                            (make-posn 0 0) (make-posn 0 0)) 0)
(check-expect (quadril-area (make-posn 2 2) (make-posn 4 10)
                            (make-posn 9 7) (make-posn 11 2)) 45.5)


;; (inside? p1 p2 p3 p4 p5) consumes five distinct Posn's, where
;; p1, p2, p3, p4 are vertices of the quadrilateral, p5 is another point.
;; the function produces true if and only if the given point lies of the 
;; interior of the given quadrilateral (the interior includes the boudary)
;; Examples
(check-expect (inside? (make-posn 0 0) (make-posn 0 2)
                       (make-posn 2 2) (make-posn 2 0)
                       (make-posn 1 1))
              true) 
(check-expect (inside? (make-posn 0 0) (make-posn 0 2)
                       (make-posn 2 2) (make-posn 2 0)
                       (make-posn 13 2))
              false)

(define (inside? p1 p2 p3 p4 p5)  
  (cond
    [(= (quadril-area p1 p2 p3 p4)
        (+ (tri-area p1 p2 p5)
           (tri-area p2 p3 p5)
           (tri-area p3 p4 p5)
           (tri-area p1 p4 p5)))  
     true] 
    [else false])) 

;; Tests
(check-expect (inside? (make-posn 2 -2)(make-posn 2 6)
                       (make-posn 15 2)(make-posn 15 -4)(make-posn 1 0)) 
              false)
(check-expect (inside? (make-posn 2 -2)(make-posn 2 6)
                       (make-posn 15 2)(make-posn 15 -4)(make-posn 4 1)) 
              true)