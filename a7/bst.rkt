;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 07, Problem 1 (bst.rkt)  
;; **************************************** 


(define-struct node (key val left right)) 
;; A binary search tree (BST) is one of 
;; * empty
;; * (make-node Num Str BST BST),
;; where for each node (make-node k v l r), every node in the subtree
;; rooted at l has a key less than k, and every node in the subtree rooted 
;; at r has a key greater than k.


;; Sample Binary Search Tree
(define my-123-bst (make-node 2 "two"
                              (make-node 1 "one" empty empty)
                              (make-node 3 "three" empty empty)))
(define my-123cat-bst (make-node 2 "two"
                              (make-node 1 "one" empty empty)
                              (make-node 3 "cat" empty empty)))
(define my-34-bst (make-node 3 "three"
                              empty
                              (make-node 4 "four" empty empty)))
(define my-234-bst (make-node 3 "three"
                              (make-node 2 "two" empty empty)
                              (make-node 4 "four" empty empty)))
(define my-1234-bst (make-node 3 "three"
                              (make-node 2 "two" 
                                         (make-node 1 "one" empty empty)
                                                    empty)
                              (make-node 4 "four" empty empty)))
(define my-12345-bst (make-node 3 "three"
                              (make-node 2 "two" 
                                         (make-node 1 "one" empty empty)
                                                    empty)
                              (make-node 4 "four" empty 
                                         (make-node 5 "five" empty empty))))
(define my-2458913-bst (make-node 8 "three"
                              (make-node 4 "two" 
                                         (make-node 2 "two" empty empty)
                                         (make-node 5 "two" empty empty))
                              (make-node 12 "four" 
                                         (make-node 9 "two" empty empty)
                                         (make-node 13 "two" empty empty))))


;; (bst-add abst k v) consumes abst, a (BST), a k, a key, and v, a value. 
;; Producing a (BST) resulting from adding the key k with value v to abst.
;; If abst already contains the key k it should be updated so that it
;; associates the key k with value v, instead of whatever value used to be
;; associated with k.
;; bst-add: (BST) Num Str -> (BST)
;; Examples:
(check-expect (bst-add (make-node 2 "two"
                                  (make-node 1 "one" empty empty)
                                  empty)
                       3 "three")
              my-123-bst)
(check-expect (bst-add my-123-bst 3 "cat") my-123cat-bst)
 
(define (bst-add abst k v)
  (cond [(empty? abst) (make-node k v empty empty)]
        [(= k (node-key abst)) (make-node k v (node-left abst)
                                              (node-right abst))]
        [(< k (node-key abst)) 
         ;check left node
         (make-node (node-key abst)
                    (node-val abst)
                    (bst-add (node-left abst) k v)
                    (node-right abst))] 
        [else 
         ;check right node
          (make-node (node-key abst)
                    (node-val abst)
                    (node-left abst)
                    (bst-add (node-right abst) k v))]))

;; Tests
(check-expect (bst-add my-34-bst 2 "two") my-234-bst)
(check-expect (bst-add my-234-bst 1 "one") my-1234-bst)
(check-expect (bst-add my-1234-bst 5 "five") my-12345-bst)


;; (bst-full? bst) consumes bst, a binary search tree producing true if
;; every node possess either 0 or 2 children, otherwise function produces
;; false. 
;; Note that an empty tree is considered full.
;; bst-full?: (BST) -> BOOL
;; Examples:
(check-expect (bst-full? my-123-bst) true)
(check-expect (bst-full? my-1234-bst) false) 

(define (bst-full? bst)
  (cond [(empty? bst) true]
        [(and (empty? (node-left bst))
              (empty? (node-right bst))) true]
        [(and (node? (node-left bst))
              (node? (node-right bst)))
         (cond [(and (bst-full? (node-left bst))
                     (bst-full? (node-right bst))) true]
               [else false])]
        [else false])) 

;; Tests
(check-expect (bst-full? empty) true)
(check-expect (bst-full? my-12345-bst) false)          
(check-expect (bst-full? my-123cat-bst) true) 


;; (bst-height bst) consumes bst, a binary search tree and produces the height
;; of the tree. The height of the binary tree is the maximum distance from the 
;; root to a leaf, measured in nodes (including the root and leaf). 
;; i.e. the empty tree has a height of 0, and a single node has a height 1.
;; bst-height: (BST) -> Nat
;; Examples:
(check-expect (bst-height my-123-bst) 2)
(check-expect (bst-height empty) 0)

(define (bst-height bst)
  (cond [(empty? bst) 0]
        [else (+ (cond
                   [(> (bst-height (node-left bst))
                       (bst-height (node-right bst))) 
                    (bst-height (node-left bst))]
                   [else (bst-height (node-right bst))]) 1)]))
                 
;; Tests
(check-expect (bst-height my-1234-bst) 3)
(check-expect (bst-height my-12345-bst) 3)
(check-expect (bst-height (make-node 1 "one" empty empty)) 1)


;; (bst-pefect? bst) consumes bst, a binary search tree and produces true
;; if the given binary search tree is perfect, that is, if every leaf in
;; the tree has the same depth (i.e. distance from the root). Otherwise
;; function produces false. 
;; Note that the empty tree is perfect
;; bst-perfect?: (BST) -> Bool
;; Examples:
(check-expect (bst-perfect? my-123-bst) true)
(check-expect (bst-perfect? my-12345-bst) false)

(define (bst-perfect? bst)
  (cond [(empty? bst) true]
        [(= (bst-height (node-left bst))
            (bst-height (node-right bst)))
         (cond [(and (bst-perfect? (node-left bst))
                     (bst-perfect? (node-right bst))) true]
               [else false])]
        [else false]))
                                    
;; Tests
(check-expect (bst-perfect? empty) true)
(check-expect (bst-perfect? my-2458913-bst) true)
(check-expect (bst-perfect? my-34-bst) false)


;; (bst->sal bst) consumes bst, a binary search tree and produces an
;; association list (AL) that contains all the entries in the tree,
;; sorted by thier keys in ascending order.
;; bst->sal: (BST) -> (AL)
(check-expect (bst->sal my-123-bst)
              '((1 "one") (2 "two") (3 "three")))
(check-expect (bst->sal empty) empty)

(define (bst->sal bst)
  (cond [(empty? bst) empty]
        [else (append (bst->sal (node-left bst))
                      (cons (list (node-key bst) (node-val bst)) empty)
                      (bst->sal (node-right bst)))]))
  
;; Tests
(check-expect (bst->sal my-34-bst)
              '((3 "three")(4 "four")))
(check-expect (bst->sal my-1234-bst)
              '((1 "one") (2 "two") (3 "three")(4 "four")))
(check-expect (bst->sal my-2458913-bst)
              '((2 "two")(4 "two")(5 "two")(8 "three")
                         (9 "two")(12 "four")(13 "two")))
  


  
 
          