;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 03, Problem 2 (robot.rkt)  
;; ****************************************  


;; (update-position start-posn x-dist y-dist) consumes a start position
;; and adds the x-dist and y-dist values to that position to produce a new
;; position
;; update-position: Posn Num Num -> Posn
;; Example
(check-expect (update-position (make-posn 0 0) 0 2) (make-posn 0 2))
(check-expect (update-position (make-posn 0 0) 3 0) (make-posn 3 0))

(define (update-position start-posn x-dist y-dist)
  
  (make-posn (+ (posn-x start-posn) x-dist)
             (+ (posn-y start-posn) y-dist)))

;; Tests 
(check-expect (update-position (make-posn 0 0) 0 -2) (make-posn 0 -2))
(check-expect (update-position (make-posn 0 0) -3 0) (make-posn -3 0))


;; (robot current-position direction distance) consumes a current position, 
;; a direction of motion, and a distance. Producing a new Posn with the 
;; robot's new position after moving the given distance in the given direction.
;; robot: Posn Sym Num -> Posn
;; requires: direction must be one of 'north, 'east, 'south, or 'west.
;;           distance >= 0
;; Example
(check-expect (robot (make-posn 0 0) 'north 2) (make-posn 0 2))
(check-expect (robot (make-posn 0 0) 'west 2) (make-posn -2 0))

(define (robot current-position direction distance) 
  
  (cond 
    [(symbol=? 'north direction)
     (update-position current-position 0 distance)]
    [(symbol=? 'east direction)
     (update-position current-position distance 0)]
    [(symbol=? 'south direction)
     (update-position current-position 0 (* -1 distance))]
    [(symbol=? 'west direction)
     (update-position current-position (* -1 distance) 0)]))

;; Tests
(check-expect (robot (make-posn 0 0) 'east 2) (make-posn 2 0))
(check-expect (robot (make-posn 0 0) 'south 2) (make-posn 0 -2))  
(check-expect (robot (make-posn 0 0) 'south 0) (make-posn 0 0)) 
(check-expect (robot (make-posn 0 0) 'north 0) (make-posn 0 0)) 
(check-expect (robot (make-posn 0 0) 'east 0) (make-posn 0 0)) 
(check-expect (robot (make-posn 0 0) 'west 0) (make-posn 0 0)) 
 
