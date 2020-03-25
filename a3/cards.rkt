;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 03, Problem 4 (cards.rkt)  
;; ****************************************  


(define-struct card (rank suit))
;; A Card is a (make-card Nat Sym)
;; required: rank of a Card is an integer in the range 1 to 13, inclusive.
;;           suit of a Card is one of 'clubs, 'diamonds, 'hearts, or 'spades.


(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;; Constants to help with examples and testing
;; including basic tests data
(define three-clubs (make-card 3 'clubs))
(define two-spades (make-card 2 'spades))
(define one-hearts (make-card 1 'hearts))
(define straight-hand (make-hand three-clubs two-spades one-hearts))
(define four-clubs (make-card 4 'clubs))
(define ace-hearts (make-card 1 'hearts))
(define ace-diamonds (make-card 1 'diamonds))
(define three-spades (make-card 3 'spades))
(define king-clubs (make-card 13 'clubs))
(define jack-spades (make-card 11 'spades))
(define queen-spades (make-card 12 'spades))
(define king-spades (make-card 13 'spades))
(define ace-spades (make-card 1 'spades))
(define three-hearts (make-card 3 'hearts))
(define straight-flush-hand (make-hand king-spades
                                       queen-spades
                                       jack-spades))
(define flush-hand (make-hand three-clubs
                              four-clubs
                              king-clubs))
(define three-of-a-kind-hand (make-hand three-clubs
                                        three-spades
                                        three-hearts))
(define pair-hand (make-hand king-spades
                             king-clubs
                             ace-hearts))
(define high-card-hand (make-hand three-clubs
                                  two-spades 
                                  king-spades)) 


;; (suit->value suit) consumes a Card's suit and produces a value 
;; in the range of 1 to 4 inclusive.
;; Increasing from 'clubs = 1, 'diamonds = 2, 'hearts = 3, 'spades = 4
;; suit-value: Sym -> Nat
;; required: suit must be one of the required Card suit values
;; Examples
(check-expect (suit->value 'clubs) 1)
(check-expect (suit->value 'hearts) 3)

(define (suit->value suit)
  (cond
    [(symbol=? 'clubs suit) 1]
    [(symbol=? 'diamonds suit) 2]
    [(symbol=? 'hearts suit) 3]
    [else 4]))

;; Tests
(check-expect (suit->value 'spades) 4)
(check-expect (suit->value 'diamonds) 2)


;; (better-card card1 card2) consumes two Card's and produces the Card
;; which is the better of the two.
;; The better card is either the card with the better suit, with suits
;; increasing from 'clubs (worst), 'diamonds (second-worst), 
;; hearts (second-best), or 'spades (best), or, if the cards have the 
;; same suit, then the best card is the card with the largest/highest rank.
;; Examples:
(check-expect (better-card three-clubs two-spades) two-spades)
(check-expect (better-card four-clubs three-clubs) four-clubs)

(define (better-card card1 card2)
  
  (cond
    [(symbol=? (card-suit card1)(card-suit card2))
     (cond
       [(>= (card-rank card1)(card-rank card2)) card1]
       [else card2])]
    [(>= (suit->value (card-suit card1))
         (suit->value (card-suit card2)))
         card1]
    [else card2])) 
 
;; Tests
(check-expect (better-card ace-hearts ace-diamonds) ace-hearts)
(check-expect (better-card two-spades three-spades) three-spades)
(check-expect (better-card king-clubs queen-spades) queen-spades)
(check-expect (better-card ace-spades queen-spades) queen-spades)


;; (consecutive-order? n1 n2 n3) consumes three natural numbers and 
;; produces true if the provided numbers are in consecutive order,
;; otherwise produces false
;; consecutive-order?: Nat Nat Nat -> Bool
;; Examples:
(check-expect (consecutive-order? 1 2 3) true)
(check-expect (consecutive-order? 2 8 9) false)

(define (consecutive-order? n1 n2 n3)
  
  (or (and (= (abs (- n1 1)) n2)
           (= (abs (- n2 1)) n3)) 
      (and (= (abs (- n1 1)) n3) 
           (= (abs (- n3 1)) n2))
      (and (= (abs (- n3 1)) n2)
           (= (abs (- n2 1)) n1))))
    

;; Tests
(check-expect (consecutive-order? 9 8 7) true)
(check-expect (consecutive-order? 9 7 8) true)
(check-expect (consecutive-order? 9 7 3) false) 
(check-expect (consecutive-order? 7 7 7) false)
(check-expect (consecutive-order? 7 8 7) false)
(check-expect (consecutive-order? 6 5 4) true)
(check-expect (consecutive-order? 6 5 1) false)
  


;; (hand-value hand) consumes a Hand and produces a symbol indicating
;; the best hand-value of the given Hand, the values produced are:
;; 'straight-flush (highest value) - all three cards are the same suit
;;                 and their ranks are three consecutive integers.
;; 'flush (second-highest value) - all three cards are the same suit
;; 'straight (third-highest value) - the ranks of the three cards are
;;                 three consecutive integers.
;; 'three-of-a-kind (third-lowest value) - the ranks of the three cards
;;                 are the same.
;; 'pair (second-lowest value) - the ranks of two of the cards are the same
;; 'high-card (lowest value) - none of the previous outcomes are statisfied
;; hand-value: Hand -> Sym
;; Examples:
(check-expect (hand-value straight-hand) 'straight)
(check-expect (hand-value straight-flush-hand) 'straight-flush)

(define (hand-value hand)
  
  (cond
    [(and (symbol=? (card-suit (hand-c1 hand))
                    (card-suit (hand-c2 hand)))
          (symbol=? (card-suit (hand-c2 hand))
                    (card-suit (hand-c3 hand))))
     (cond
       [(consecutive-order? (card-rank (hand-c1 hand))
                            (card-rank (hand-c2 hand))
                            (card-rank (hand-c3 hand))) 'straight-flush]
       [else 'flush])]
    [(consecutive-order? (card-rank (hand-c1 hand))
                         (card-rank (hand-c2 hand))
                         (card-rank (hand-c3 hand))) 'straight]
    [(and (= (card-rank (hand-c1 hand))
             (card-rank (hand-c2 hand)))
          (= (card-rank (hand-c1 hand))
             (card-rank (hand-c3 hand)))) 'three-of-a-kind]
    [(or (= (card-rank (hand-c1 hand))
            (card-rank (hand-c2 hand)))
         (= (card-rank (hand-c1 hand))
            (card-rank (hand-c3 hand)))
         (= (card-rank (hand-c2 hand))
            (card-rank (hand-c3 hand)))) 'pair]
    [else 'high-card]))
     
;; Tests
(check-expect (hand-value straight-hand) 'straight)
(check-expect (hand-value flush-hand) 'flush)
(check-expect (hand-value three-of-a-kind-hand) 'three-of-a-kind)
(check-expect (hand-value pair-hand) 'pair)
(check-expect (hand-value high-card-hand) 'high-card)

       
  
  


