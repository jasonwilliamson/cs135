;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname line-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 04, Problem 3 (line-lists.rkt)  
;; ****************************************  


(define-struct line (slope intercept))
;; A Line is a (make-line (anyof 'undefined Num) Num)
;; requires: fields correspond to a standard equation
;;           of a line. i.e. y = mx + b
;;           where m = slope, and b = intercept


;; A (listof Line) is one of:
;; * empty
;; * (cons Line (listof Line))


;; my-lol-fn: (listof Line) -> Any
(define (my-lol-fn lol)
  (cond [(empty? lol) ...]
        [else (... (first lol) ... (rest lol))]))


;; Provided and additional definitions
(define line1 (make-line 1 0))
(define line2 (make-line 0 3))
(define line3 (make-line -1/2 -2))
(define line4 (make-line 'undefined -4))
(define line5 (make-line 'undefined 1))
(define line-list (cons line1 (cons line2 (cons line3 (cons line4 empty)))))
(define line-list-b (cons line1 empty))
(define line-list-c (cons line4 (cons line5 (cons line4 empty))))
(define line-list-d (cons line4 (cons line3 (cons line2 (cons line1 empty)))))
(define line-list-e (cons line1 (cons line5 empty)))
(define lineA (make-line 2 -3))
(define lineB (make-line 1 -2))
(define lineC (make-line 4 3))
(define line-list1 (cons lineA (cons lineB (cons lineC empty))))


;; (negate-slope lol) consumes a (listof Line) and produces the same list with
;; all the slopes negated (unless the slope was 'undefined, in which case,
;; it remains 'undefinded) produced list maintains same order as consumed list
;; negate-slope: (listof Line) -> (listof Line)
;; Examples:
(check-expect (negate-slope line-list)
              (cons (make-line -1 0)
                    (cons (make-line 0 3) 
                          (cons (make-line 0.5 -2) 
                                (cons (make-line 'undefined -4) empty)))))
(check-expect (negate-slope line-list-b)(cons (make-line -1 0) empty))
 
(define (negate-slope lol) 
  (cond
    [(empty? lol) empty]
    [else 
     (cond
       [(equal? (line-slope (first lol)) 'undefined)
        (cons (first lol) (negate-slope (rest lol)))]
       [else 
        (cons (make-line ( * -1 (line-slope (first lol)))
                         (line-intercept (first lol))) 
              (negate-slope (rest lol)))])])) 

;; Tests
(check-expect (negate-slope line-list-b)(cons (make-line -1 0) empty))
(check-expect (negate-slope line-list-c)
              (cons (make-line 'undefined -4)
                    (cons (make-line 'undefined 1)
                          (cons (make-line 'undefined -4) empty))))
(check-expect (negate-slope line-list-d)
              (cons (make-line 'undefined -4)  
                    (cons (make-line 0.5 -2) 
                          (cons (make-line 0 3) 
                                (cons (make-line -1 0) empty)))))


;; (positive-line lol) consumes a (listof Line) and produces the list of those
;; lines which have either a positive slope or a positive (x or y) intercept.
;; The new list maintains the same order as the list provided. 0 is not 
;; considered positive
;; positive-line: (listof Line) -> (listof Line)
;; Examples:
(check-expect (positive-line line-list) 
              (cons (make-line 1 0) (cons (make-line 0 3) empty)))
(check-expect (positive-line line-list-e) line-list-e)
 
(define (positive-line lol)
  (cond
    [(empty? lol) empty]
    [else 
     (cond
       [(equal? (line-slope (first lol)) 'undefined)
        (cond 
          [(< 0 (line-intercept (first lol)))
           (cons (first lol) (positive-line (rest lol)))]
          [else (positive-line (rest lol))])]
       [(or (< 0 (line-slope (first lol)))
            (< 0 (line-intercept (first lol))))
        (cons (first lol) (positive-line (rest lol)))]
       [else (positive-line (rest lol))])]))
           
;; Tests
(check-expect (positive-line line-list) 
              (cons (make-line 1 0) (cons (make-line 0 3) empty)))
(check-expect (positive-line 
               (cons (make-line -1 0) (cons (make-line 0 -3) empty))) empty)
(check-expect (positive-line 
               (cons (make-line 0 0) 
                     (cons (make-line 'undefined 0)
                           (cons (make-line 0 0) empty)))) empty)


;; (through-point lol point) consumes a (listof Line) and a point (represented 
;; as a Posn) and produces a list of lines which pass through the point. 
;; (in case of the slope values, 'undefinded is also valid)
;; through-point (listof Line) Posn -> (listof Line)
;; Examples:
(check-expect (through-point line-list (make-posn -4 3))
              (cons (make-line 0 3) (cons (make-line 'undefined -4) empty)))
(check-expect (through-point line-list (make-posn 5 2)) empty)
 
(define (through-point lol point)
  (cond
    [(empty? lol) empty]
    [(equal? 'undefined (line-slope (first lol)))
     (cond
       [(= (line-intercept (first lol)) (posn-x point))
        (cons (first lol) (through-point (rest lol) point))]
       [else (through-point (rest lol) point)])]
    [(= (posn-y point) ( + (* (line-slope (first lol))
                              (posn-x point))
                           (line-intercept (first lol))))
     (cons (first lol) (through-point (rest lol) point))]
    [else (through-point (rest lol) point)]))
                        

;; Tests
(check-expect (through-point line-list (make-posn -4 3))
              (cons (make-line 0 3) (cons (make-line 'undefined -4) empty)))
(check-expect (through-point line-list1 (make-posn 3 3))
              (cons (make-line 2 -3) empty)) 
(check-expect (through-point line-list1 (make-posn 4 2))
              (cons lineB empty)) 
          

;; (parallel-non-intersect-lines lol) consumes a (listof Line) with atleast
;; two elements, and produces a list of Boolean values, true indicating that a 
;; pair of lines are parallel and non-intersecting; false otherwise.
;; (parallel-non-intersect-lines (listof Line) -> (listof Bool)
;; requires: (listof Line) provided must contain atleast two elements
;; Examples:
(check-expect (parallel-non-intersect-lines 
               (cons line1 (cons line1 
                                 (cons (make-line 1 4) 
                                       (cons line2 empty)))))
              (cons false (cons true (cons false empty))))
(check-expect (parallel-non-intersect-lines
               (cons line1 (cons lineB 
                                 (cons line3
                                       (cons line4
                                             (cons line5 empty))))))
              (cons true (cons false (cons false (cons true empty)))))
                                             
(define (parallel-non-intersect-lines lol)
  (cond
    [(or (empty? lol)(not (cons? (rest lol)))) empty]
    [(or (equal? 'undefined (line-slope (first lol)))
         (equal? 'undefined (line-slope (first (rest lol)))))
     (cond
       [(and (and (equal? 'undefined (line-slope (first lol)))
                  (equal? 'undefined (line-slope (first (rest lol)))))
             (not (= (line-intercept (first lol))
                     (line-intercept (first (rest lol))))))
        (cons true (parallel-non-intersect-lines (rest lol)))]
       [else
        (cons false (parallel-non-intersect-lines (rest lol)))])]   
    [(and (= (line-slope (first lol)) (line-slope (first (rest lol))))
          (not (= (line-intercept (first lol)) 
                  (line-intercept(first (rest lol))))))
     (cons true (parallel-non-intersect-lines (rest lol)))]
    [else (cons false (parallel-non-intersect-lines (rest lol)))])) 

;; Tests
(check-expect (parallel-non-intersect-lines
               (cons line4 (cons line4 empty))) 
              (cons false empty))
(check-expect (parallel-non-intersect-lines line-list-c)
              (cons true (cons true empty))) 
(check-expect (parallel-non-intersect-lines line-list-e)
              (cons false empty))
