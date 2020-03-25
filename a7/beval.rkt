;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname beval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;  
;; ****************************************  
;;      Jason Williamson (20552360)  
;;     CS 135 Winter 2015  
;;     Assignment 07, Problem 4 (beval.rkt)  
;; ****************************************  

;; A boolean expression (Bexp) is one of: 
;; * a Boolean value (true or false)
;; * a comparison expression (Cexp)
;; * a compound boolean expression (Cbexp)

(define-struct cexp (fn arg1 arg2))
;; A Cexp is a (make-cexp (anyof '> '< '=) Num Num)

(define-struct cbexp (op args))
;; A Cbexp is a (make-cbexp (anyof 'and 'or) Cbexplist)

;; A Cbexplist is one of:
;; * empty 
;; * (cons Bexp Cbexplist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates (part (a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my-cexp-fn: Cexp -> Any
(define (my-cexp-fn info)
  (...(cexp-fn info)...
      (cexp-arg1 info)...
      (cexp-arg2 info)...))

;; my-cbexp-fn: Cbexp -> Any
(define (my-cbexp-fn info)
  (...(cbexp-op info)...
      (cbexp-args info)...));;might want to do something to the list
      
;; my-bexp-fn: Bexp -> Any
(define (my-bexp-fn info)
  (cond [(cexp? info)
         (...(cexp-fn info)...
             (cexp-arg1 info)...
             (cexp-arg2 info)...)]
        [(cbexp? info)
         (...(cbexp-op info)...
             (cbexp-args info)...)]
        [(boolean? info)...]))
         

;; my-cbexplist-fn: (Cbexplist) -> Any
(define (my-cbexplist-fn lst)
  (cond [(empty? lst) ...]
        [else (... (first lst)...
                   (my-cbexplst-fn (rest lst))...)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bool-eval (part (b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (apply f cbexlst) consumes f, a Sym representing a
;; compound boolean expresssion (anyof 'and 'or), and cbexlst, 
;; a Cbexplist. Producing a boolean value derived from the consumed compound
;; boolean expression and all the elements in Cbexplist
;; Note apply is a helper function to bool-eval
;; apply: Sym Cbexplist -> Bool
;; requires: Sym must be (anyof 'and 'or)
;;           cbexlst must be non empty list
;; Examples:
(check-expect (apply 'and (list (make-cexp '> 5 2)  true true)) true)
(check-expect (apply 'and (list (make-cexp '> 5 2)  false)) false)
 
(define (apply f cbexlst)
  (cond[(and (empty? cbexlst)(symbol=? f 'and)) true]
       [(and (empty? cbexlst)(symbol=? f 'or)) false]
       [(symbol=? f 'and)
        (and (bool-eval (first cbexlst))(apply f (rest cbexlst)))]
       [(symbol=? f 'or)
        (or (bool-eval (first cbexlst))(apply f (rest cbexlst)))]))

;; Tests 
(check-expect (apply 'or (list (make-cexp '> 5 100)  false)) false)
(check-expect (apply 'or (list (make-cexp '> 5 100)  true)) true)
(check-expect (apply 'or empty) false) 
 

;; (bool-eval bexpr) consumes bexpr, a boolean expression and produces
;; the boolean value of the boolean expression
;; bool-eval: Bexp -> Bool
;; Examples:
(check-expect (bool-eval (make-cbexp
                          'and
                          (list (make-cexp '> 5 2)  true true)))
              true)
(check-expect (bool-eval (make-cexp '< 4 7)) true) 
(check-expect (bool-eval (make-cbexp 'or empty)) false)
 
(define (bool-eval bexpr) 
  (cond[(boolean? bexpr) bexpr]
       [(cexp? bexpr) 
        (cond [(symbol=? '> (cexp-fn bexpr))
               (> (cexp-arg1 bexpr)(cexp-arg2 bexpr))]
              [else (< (cexp-arg1 bexpr)(cexp-arg2 bexpr))])]
       [else (apply (cbexp-op bexpr)(cbexp-args bexpr))]))

;; Tests 
(check-expect (bool-eval (make-cbexp
                          'and
                          (list (make-cexp '> 5 2)  true false))) false) 
(check-expect (bool-eval (make-cbexp
                          'or 
                          (list (make-cexp '> 5 100)  false false))) false) 
              



