;; PL Project - Fall 2020
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var     (string)    #:transparent)    ;; a variable, e.g., (var "foo")
(struct num     (int)       #:transparent)    ;; a constant number, e.g., (num 17)
(struct bool    (bool)      #:transparent)    ;; a boolean variable, e.g, (#t or #f)

(struct minus   (e1 e2)     #:transparent)    ;; subtract two expressions
(struct mult    (e1 e2)     #:transparent)    ;; multiply two expressions
(struct div     (e1 e2)     #:transparent)    ;; divide two expressions
(struct plus    (e1 e2)     #:transparent)    ;; add two expressions

(struct neg     (e1)        #:transparent)    ;; negate an expression
(struct andalso (e1 e2)     #:transparent)    ;; logical and two expressions
(struct orelse  (e1 e2)     #:transparent)    ;; logical or two expressions
(struct cnd     (e1 e2 e3)  #:transparent)    ;; condition, if e1 then e2 else e3

(struct iseq    (e1 e2)     #:transparent)    ;; check equality
(struct ifnzero (e1 e2 e3)  #:transparent)    ;; if e1 is zero then e2 else e3
(struct ifleq   (e1 e2 e3 e4) #:transparent)  ;; if e1 < e2 then e3 else e4


(struct lam     (s1 s2 e)   #:transparent)    ;; a recursive(?) 1-argument function
(struct apply   (e1 e2)     #:transparent)    ;; function application
(struct with    (s e1 e2)   #:transparent)    ;; let e1 be s in e2
(struct apair   (e1 e2)     #:transparent)    ;; a pair of two expressions
(struct first   (e1)          #:transparent)  ;; first element of pair
(struct second  (e1)         #:transparent)   ;; second element of pair

(struct munit   ()          #:transparent)    ;; unit value -- good for ending a list
(struct ismunit (e)         #:transparent)    ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f)     #:transparent) 


(struct key     (s e)         #:transparent)  ;; key holds corresponding value of s which is e
(struct record  (k r)        #:transparent)   ;; record holds several keys
;; TODO: record with k and munit m
(struct value   (s r)         #:transparent)  ;; value returns corresponding value of s in r

(struct letrec  (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Problem 1

(define (racketlist->numexlist xs) (cond  [(equal? xs '())
                                                (munit)]
                                          [#t
                                                (apair (car xs) (racketlist->numexlist (cdr xs)))]
                                    )
)

(define (numexlist->racketlist xs) (cond  [(equal? xs munit) 
                                                '()]
                                          [#t
                                                (cons (first xs) (numexlist->racketlist (second xs)))  ]
                                    )
)

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		"CHANGE" 
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
