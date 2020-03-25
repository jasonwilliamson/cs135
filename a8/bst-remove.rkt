;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bst-remove) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 08, Problem 2 (bst-remove.rkt)  
;; **************************************** 


(define-struct node (key val left right)) 
;; A binary search tree (BST) is one of 
;; * empty
;; * (make-node Num Str BST BST),
;; where for each node (make-node k v l r), every node in the subtree
;; rooted at l has a key less than k, and every node in the subtree rooted 
;; at r has a key greater than k.


;; Example binary search tree from the assignment:
(define t 
  (make-node 5 "" (make-node 3 "" (make-node 2 "" (make-node 1 "" empty empty)
                                                     empty)
                                     (make-node 4 "" empty empty))
                     (make-node 7 "" (make-node 6 "" empty empty) empty)))

(define t2 
  (make-node 5 "" (make-node 3 "" (make-node 2 "" (make-node 1 "" empty empty)
                                                     empty)
                                     (make-node 4 "" empty empty))
                     (make-node 7 "" empty (make-node 8 "" empty empty))))
(define t2-7 
  (make-node 5 "" (make-node 3 "" (make-node 2 "" (make-node 1 "" empty empty)
                                                     empty)
                                     (make-node 4 "" empty empty))
                     (make-node 8 "" empty empty)))


 
 
;; (bst-remove bst k) consumes bst, a binary search tree (BST), 
;; and k, a key. Producing the (BST) resulting from removing the node
;; with key k from the given (BST) (if it exists). If there is no
;; node with key k, in the tree, then the function produces the original
;; (BST) unchanged.
(check-expect (bst-remove t 5)
    (make-node 6 "" (make-node 3 "" (make-node 2 "" (make-node 1 "" empty empty)
                                               empty)
                               (make-node 4 "" empty empty))
               (make-node 7 "" empty empty)))
(check-expect (bst-remove t 1)
    (make-node 5 "" (make-node 3 "" (make-node 2 "" empty empty)
                               (make-node 4 "" empty empty)) 
               (make-node 7 "" (make-node 6 "" empty empty) empty))) 
 
(define (bst-remove bst k)
  ;; (check-left n) consumes n, a Node and produces
  ;; the left most child node of n
  ;; if n has no left most child, function produces n itself
  ;; check-left: Node -> Node
  (local [(define (check-left n)
                         (cond [(empty? (node-left n)) n]
                               [else (check-left (node-left n))]))]
  (cond [(empty? bst) empty]
        [(= k (node-key bst))
         (cond
              ;no children, remove node
              [(and (empty? (node-left bst))
                    (empty? (node-right bst))) empty]
              ;exactly one child, replace with its only child
              [(and (empty? (node-left bst))
                    (not (empty? (node-right bst)))) 
               (make-node (node-key (node-right bst))
                          (node-val (node-right bst))
                          (node-left (node-right bst))
                          (node-right (node-right bst)))]
              ;exactly one child, replace with its only child
              [(and (not (empty? (node-left bst)))
                    (empty? (node-right bst)))
               (make-node (node-key (node-left bst))
                          (node-val (node-left bst))
                          (node-left (node-left bst))
                          (node-right (node-left bst)))]
              ;has two children, replace with successor
              [else  
               (local [(define successor (check-left (node-right bst)))]
                 (make-node (node-key successor)
                            (node-val successor)
                            (node-left bst)
                            (node-right 
                             (bst-remove bst (node-key successor)))))])] 
        ;check left node
        [(< k (node-key bst))
         (make-node (node-key bst)
                    (node-val bst)
                    (bst-remove (node-left bst) k)
                    (node-right bst))] 
        ;check right node
        [else
         (make-node (node-key bst)
                    (node-val bst)
                    (node-left bst)
                    (bst-remove (node-right bst) k))])))
              

;; Tests
(check-expect (bst-remove t 7)
   (make-node 5 "" (make-node 3 "" (make-node 2 "" (make-node 1 "" empty empty)
      empty) (make-node 4 "" empty empty))
     (make-node 6 "" empty empty))) 
(check-expect (bst-remove t 37) t)
(check-expect (bst-remove t2 7) t2-7)

                
    
  