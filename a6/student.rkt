;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 06, Problem 2 (student.rkt)  
;; ****************************************


;; (sort lon) consumes lon, a (listof Num) and sorts the elements of
;; lon in descending order
;; sort: (listof Num) -> (listof Num)
;; Examples:
(check-expect (sort '(1.5 1.7 1.6)) '(1.7 1.6 1.5))
(check-expect (sort '(112 44 22 22)) '(112 44 22 22))

(define (sort lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (sort (rest lon)))]))

;; Tests
(check-expect (sort empty) empty)
(check-expect (sort '(1.9 1.5 1.3)) '(1.9 1.5 1.3))


;; (insert n slon) consumes n, a number and slon, a sorted list, and
;; produces a sorted list
;; insert: Num (listof Num)
;; requires: slon is sorted in non-increasing order
;; Examples:
(check-expect (insert 2.0 '(1.9 1.5)) '(2 1.9 1.5))
(check-expect (insert 2.0 empty) '(2))

(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(>= n (first slon))(cons n slon)]
        [else (cons (first slon)(insert n (rest slon)))]))

;; Tests
(check-expect (insert 1.8 '(1.9 1.5)) '(1.9 1.8 1.5))
(check-expect (insert 1.3 '(1.9 1.5)) '(1.9 1.5 1.3))

 
;; (tallest los lon) consumes los, a (listof Symbols) represening first names 
;; students, and a lon (listof Num) representing the corresponding height of
;; the students. Producing the name of the student who is the tallest; in the
;; case of a tie, however, the name of the first student in the list with the
;; maximal height will be produced.
;; tallest: (listof Sym) (listof Num) -> Sym
;; requires: los and lon must be the same length and both are non-empty.
;;           lon must contain all positive numbers, n > 0
;; Examples:
(check-expect (tallest '(Bernie Raj Amy) '(1.5 1.7 1.6)) 'Raj)
(check-expect (tallest '(Bob Bill Ben Barry) '(2 2.3 2.4 1.5)) 'Ben)

(define (tallest los lon)
  (cond [(= (first (sort lon)) (first lon)) (first los)]
        [else (tallest (rest los) (rest lon))]))  

;; Tests
(check-expect (tallest '(Bob Bill Ben Barry) '(2 2.3 2.3 2.3)) 'Bill)
(check-expect (tallest '(Bob Bill Ben Barry) '(2 2 1.9 1.9001)) 'Bob)
(check-expect (tallest '(Bob) '(2)) 'Bob)


;; (min-list/acc lon min-so-far) consumes lon, a (listof Num), and 
;; min-so-far, a Num. Producing the minimum element of the lon and min-so-far
;; min-list/acc: (listof Num) Num -> Num
;; Examples:
(check-expect (min-list/acc '(10 12 14) 9) 9)
(check-expect (min-list/acc '(10 12 14) 15) 10)

(define (min-list/acc lon min-so-far)
  (cond [(empty? lon) min-so-far]
        [(< (first lon) min-so-far)
         (min-list/acc (rest lon) (first lon))]
        [else (min-list/acc (rest lon) min-so-far)]))

;; Tests
(check-expect (min-list/acc empty 9) 9)
(check-expect (min-list/acc '(1.2 1.4 0.9) 3.2) 0.9)
(check-expect (min-list/acc empty 0) 0)
         

;; (shortest los lon) consumes los, a (listof Symbols) represening first names 
;; students, and a lon (listof Num) representing the corresponding height of
;; the students. Producing the name of the student who is the shortest; in the 
;; case of a tie, however, the name of the first student in the list with the
;; minimal height will be produced.
;; shortest: (listof Sym) (listof Num) -> Sym
;; requires: los and lon must be the same length and both are non-empty.
;;           lon must contain all positive numbers, n > 0
;; Examples:
(check-expect (shortest '(Bernie Raj Amy) '(1.5 1.7 1.6)) 'Bernie)
(check-expect (shortest '(Bernie Raj Amy) '(1.7 1.6 1.5)) 'Amy)

(define (shortest los lon) 
  (cond [(= (min-list/acc (rest lon)(first lon)) (first lon))
           (first los)]
        [else (shortest (rest los)(rest lon))])) 

;; Tests
(check-expect (shortest '(Bernie) '(1.7)) 'Bernie)
(check-expect (shortest '(Bernie Raj Amy) '(1.5 1.5 1.5)) 'Bernie)


;; An association list (Sym AL) is one of:
;; * empty
;; * (cons (list Sym Num) Sym AL)


;; (student-al los lon) consumes los, a (listof Symbols) represening first names
;; students, and a lon (listof Num) representing the corresponding height of
;; the students. Producing a association list with the keys being the student
;; names, and the values being the height of the corresponding student. With
;; the order of the keys being the same order as the list of names consumed.
;; student-al: (listof Sym) (listof Num) -> (Sym AL)
;; requires: los and lon must be the same length and both are non-empty.
;;           lon must contain all positive numbers, n > 0
;;           los must be a list of distinct symbols
(check-expect (student-al '(Bernie Raj Amy) '(1.5 1.7 1.6))
              '((Bernie 1.5) (Raj 1.7) (Amy 1.6)))
(check-expect (student-al empty empty) empty)

(define (student-al los lon) 
  (cond ([empty? los] empty)
        [else (cons (list (first los)(first lon))
                    (student-al (rest los)(rest lon)))]))

;; Tests
(check-expect (student-al '(Bob Bill Ben Barry) '(2 2.3 2.3 2.3))
              '((Bob 2)(Bill 2.3)(Ben 2.3)(Barry 2.3)))
(check-expect (student-al '(Bob Bill Ben Barry) '(2.1 3.1 2.3 2.7))
              '((Bob 2.1)(Bill 3.1)(Ben 2.3)(Barry 2.7)))


;; (basketball al h) cosumes al, an association list with its keys
;; being the names as symbols, and the values being the numerical
;; height of the students, and a positive height, h. 
;; Producing the list of names for those students that are at least
;; as tall as the givin height, in the same order that the names appear 
;; in the consumed list.
;; basketball: (Sym AL) -> (listof Sym)
;; requires: the numerical height of students in al, to be a positive
;;           height.
;; Examples:
(check-expect (basketball '((Bernie 1.5) (Raj 1.7) (Amy 1.6)) 1.6)
              '(Raj Amy))
(check-expect (basketball '((Bob 2.1)(Bill 3.1)(Ben 2.3)(Barry 2.7)) 3)
              '(Bill))
                          
(define (basketball al h)
  (cond [(empty? al) empty]
        [(<= h (second (first al))) 
           (cons (first (first al))
                 (basketball (rest al) h))]
        [else (basketball (rest al) h)])) 

;; Tests
(check-expect (basketball '((Bob 2.1)(Bill 3.1)(Ben 2.3)(Barry 2.7)) 2.11)
              '(Bill Ben Barry))
(check-expect (basketball empty 3)
              empty)
(check-expect (basketball '((Bob 2.1)(Bill 2.1)(Ben 2.3)(Barry 2.7)) 3)
              empty)

