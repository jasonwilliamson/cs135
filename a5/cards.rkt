;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 05, Problem 4 (cards.rkt)  
;; ****************************************  


(define-struct card (rank suit))
;; A Card is a (make-card Nat Sym)
;; requires: suit is one of 'clubs, 'diamonds, 'hearts, 'spades


;; A (listof Card) is one of:
;; * empty
;; * (cons Card (listof Card))


;; Provide defines
(define four-clubs (make-card 4 'clubs))
(define five-hearts (make-card 5 'hearts))
(define six-clubs (make-card 6 'clubs))
(define ten-clubs (make-card 10 'clubs))
(define nine-spades (make-card 9 'spades))


;; (is-flush? loc) consumes a loc (listof Card) and produces true if
;; all the cards in the consumed list have the same suit, and false
;; otherwise. If the list is empty or just contains one card, then
;; produces true.
;; is-flush? (listof Card) -> Bool
;; Examples:
(check-expect (is-flush? (list six-clubs four-clubs six-clubs)) true)
(check-expect (is-flush? (list six-clubs four-clubs five-hearts)) false)

(define (is-flush? loc)
  (cond [(or (empty? loc)
              (empty? (rest loc))) true]
        [(symbol=? (card-suit (first loc)) 
                   (card-suit (second loc))) (is-flush? (rest loc))]
        [else false]))  

;; Tests
(check-expect (is-flush? (list six-clubs)) true)
(check-expect (is-flush? empty) true)
(check-expect (is-flush? (list six-clubs five-hearts)) false)


;; (better-rank? c1 c2) consumes two Cards and produces a true if the first
;; given card has strictly higher rank than the second given card.
;; better-rank?: Card Card -> Bool
;; Examples:
(check-expect (better-rank? six-clubs five-hearts) true)
(check-expect (better-rank? four-clubs five-hearts) false)

(define (better-rank? c1 c2)
  (> (card-rank c1) (card-rank c2)))  

;; Tests
(check-expect (better-rank? four-clubs four-clubs) false)


;; (insert c sloc) inserts the card c into the sorted list of sloc
;; producing a sorted (listof Card)
;; insert: Card -> (listof Card)
;; Examples:
(check-expect (insert six-clubs empty) (list six-clubs))
(check-expect (insert six-clubs (list six-clubs ten-clubs))
              (list six-clubs six-clubs ten-clubs))

(define (insert c sloc)
  (cond [(empty? sloc) (cons c empty)]
        [(<= (card-rank c)(card-rank (first sloc))) (cons c sloc)]
        [else (cons (first sloc) (insert c (rest sloc)))]))

;; Tests
(check-expect (insert empty empty) (cons empty empty))   


;; (sort-hand-by-rank loc) consumes a loc, (listof Card), and produces a
;; a sorted in non-decreasing order by rank (listof Card) containing the 
;; same cards as the consumed list.
;; sort-hand-by-rank: (listof Card) -> (listof Card)
;; Examples:
(check-expect (sort-hand-by-rank (list six-clubs four-clubs five-hearts))
              (list four-clubs five-hearts six-clubs))
(check-expect (sort-hand-by-rank 
               (list six-clubs ten-clubs four-clubs five-hearts))
              (list four-clubs five-hearts six-clubs ten-clubs))

(define (sort-hand-by-rank loc)
  (cond [(empty? loc) empty]
        [else (insert (first loc) (sort-hand-by-rank (rest loc)))])) 

;; Tests
(check-expect (sort-hand-by-rank empty)empty)
(check-expect (sort-hand-by-rank (list nine-spades five-hearts five-hearts))
              (list five-hearts five-hearts nine-spades))
(check-expect (sort-hand-by-rank (list nine-spades five-hearts ten-clubs))
              (list five-hearts nine-spades ten-clubs))


;; (consecutive? a sloc) consumes a Nat and a sorted (listof Card) by rank in 
;; non-descending order. Producing true if the Card values are all consecutive
;; integers and false otherwise.
;; consecutive? Nat (listof Card) -> Bool
;; requires: sloc must be sorted in non-descending order according to rank
;; Example
(check-expect (consecutive? 3 (list four-clubs five-hearts)) true)
(check-expect (consecutive? 3 (list four-clubs ten-clubs)) false)

(define (consecutive? a sloc)
  (cond [(empty? sloc) true] 
        [(= (add1 a) (card-rank (first sloc)))
            (consecutive? (add1 a) (rest sloc))]
        [else false])) 
 
;; Tests 
(check-expect (consecutive? 3 empty) true)


;; (is-straight? loc) consumes a (listof Card) and produces true if the cards 
;; be arranged into a straight, and false otherwise. 
;; is-straight? (listof Card) -> Bool
;; Examples:
(check-expect (is-straight? (list six-clubs four-clubs five-hearts)) true)
(check-expect (is-straight? (list ten-clubs four-clubs five-hearts)) false)
 
(define (is-straight? loc) 
  (cond [(empty? loc) true]
        [else (consecutive? (card-rank (first(sort-hand-by-rank loc))) 
                            (rest (sort-hand-by-rank loc)))]))

;; Tests 
(check-expect (is-straight? empty) true)
(check-expect (is-straight? (list nine-spades five-hearts five-hearts)) false)
   

