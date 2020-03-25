;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 03, Problem 3 (lines.rkt)  
;; ****************************************  


(define-struct line (slope intercept))
;; A Line is a (make-line (anyof 'undefined Num) Num)
;; requires: fields correspond to a standard equation
;;           of a line. i.e. y = mx + b
;;           where m = slope, and b = intercept


;; (calculate-slope p1 p2) consumes two distinct points and produces
;; the slope value
;; calculate-slope: Posn Posn -> Num
;; requires: the two points can not represent a vertical line
;; Examples:
(check-expect (calculate-slope (make-posn 0 0) (make-posn 1 1)) 1)
(check-expect (calculate-slope (make-posn -1 1)(make-posn 1 3)) 1)

(define (calculate-slope p1 p2) 
  (/ (- (posn-y p1)(posn-y p2)) (- (posn-x p1)(posn-x p2))))

;; Tests
(check-expect (calculate-slope (make-posn -2 4)(make-posn 4 1)) -1/2)
(check-expect (calculate-slope (make-posn -2 4)(make-posn 2 -2)) -3/2)
             
              
;; (calculate-intercept slope p1) consumes a slope value, and a point
;; and produces the y-intercept of the line
;; calculate-intercept: Num Posn -> Num
;; Examples:
(check-expect (calculate-intercept 1 (make-posn -1 1)) 2)
(check-expect (calculate-intercept -1/2 (make-posn -2 4)) 3)
 
(define (calculate-intercept slope p1)
  ( + (* slope (* -1 (posn-x p1))) (posn-y p1))) 

;; Tests
(check-expect (calculate-intercept -3/2 (make-posn -2 4)) 1) 
(check-expect (calculate-intercept 0 (make-posn 0 0)) 0)


;; (two-points->line point1 point2) consumes two distinct points and produces
;; the corresponding Line which runs through these two points. If the line
;; is vertical the slope will be set as 'undefined and intercept will be
;; set to the intercept of the x-axis
;; two-points->line: Posn Posn -> Line
;; Examples: 
(check-expect (two-points->line (make-posn 0 0) (make-posn 1 1))
              (make-line 1 0))
(check-expect (two-points->line (make-posn -1 1)(make-posn 1 3))
              (make-line 1 2)) 

(define (two-points->line point1 point2) 
  
  (cond
    [(= 0 (- (posn-x point1) (posn-x point2)))
     (make-line 'undefined (posn-x point1))]   
    [else (make-line  
           (calculate-slope point1 point2)
           (calculate-intercept (calculate-slope  point1 point2) point1))]))

;; Tests
(check-expect (two-points->line (make-posn 1 4)(make-posn 1 1))
              (make-line 'undefined 1)) 
(check-expect (two-points->line (make-posn -2 4)(make-posn 4 1))
              (make-line -1/2 3))
(check-expect (two-points->line (make-posn -2 4)(make-posn 2 -2))
              (make-line -3/2 1))


;; (x-intersection line1 line1) produces the x coordinate value of the 
;; the intersection between the two lines consumed
;; x-intersection: Line Line -> Num
;; Example:
(check-expect (x-intersection (make-line 1 0) (make-line -1 0)) 0)
(check-expect (x-intersection (make-line 3 2) (make-line 2 -1)) -3)

(define (x-intersection line1 line2)
  (/ (- (line-intercept line2)(line-intercept line1))
     (- (line-slope line1)(line-slope line2))))

;; Tests
(check-expect (x-intersection (make-line 1 1) (make-line -1 1)) 0)
(check-expect (x-intersection (make-line 5 1) (make-line 3 2)) 1/2)
  

;; (y-intersection x-value line1) produces the y coordinate value of the 
;; intersection between two lines by consuming a predetermined x-value
;; between two intersecting lines, as well as, as Line value to solve for.
;; y-intersection Num Line -> Num
;; Example:
(check-expect (y-intersection 0 (make-line 1 0)) 0)
(check-expect (y-intersection -3 (make-line 3 2)) -7)

(define (y-intersection x-value line1)
  (+ (* (line-slope line1) x-value) (line-intercept line1)))

;; Tests
(check-expect (y-intersection 0 (make-line 1 1)) 1) 
(check-expect (y-intersection 1/2 (make-line 5 1)) 3.5)

  
;; (intersection line1 line2) produces a Posn denoting the point of intersection
;; of the two consumed Lines in the case the Lines intersect exactly once,
;; otherwise produces 'undefined in the case where lines do not intersect or
;; are have infinitely many points of intersection.
;; intersection: Line Line -> (anyof 'undefined Posn)
;; Examples:
(check-expect (intersection (make-line 1 0) (make-line -1 0))
              (make-posn 0 0)) 
(check-expect (intersection (make-line 1 1) (make-line -1 1))
              (make-posn 0 1)) 

(define (intersection line1 line2)
   
  (cond 
    [(and (equal? 'undefined (line-slope line1))
          (equal? 'undefined (line-slope line2))) 'undefined]
  
    [(or (equal? 'undefined (line-slope line1))
         (equal? 'undefined (line-slope line2))) 
     (cond
       [(equal? 'undefined (line-slope line1)) 
        (make-posn
         (line-intercept line1)
         (y-intersection (line-intercept line1) line2))]
       [else  
        (make-posn
         (line-intercept line2)
         (y-intersection (line-intercept line2) line1))])]
    [else 
     (make-posn 
      (x-intersection line1 line2) 
      (y-intersection (x-intersection line1 line2) line1))])) 
  
;; Tests
(check-expect (intersection (make-line 3 -3) (make-line 'undefined 12))
              (make-posn 12 33)) 
(check-expect (intersection (make-line 'undefined 12)(make-line 3 -3) )
              (make-posn 12 33)) 
(check-expect (intersection (make-line 'undefined 12)(make-line 'undefined -3) )
              'undefined) 
(check-expect (intersection (make-line 3 2) (make-line 2 -1))
              (make-posn -3 -7)) 

                            
        
  
  