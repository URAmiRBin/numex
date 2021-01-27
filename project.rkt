;; PL Project - Fall 2020
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs
(struct var     (string)    #:transparent)    ;; a variable, e.g., (var "foo")
(struct num     (int)       #:transparent)    ;; a constant number, e.g., (num 17)
(struct bool    (boolean)   #:transparent)    ;; a boolean variable, e.g, (#t or #f)

(struct minus   (e1 e2)     #:transparent)    ;; subtract two numex num
(struct mult    (e1 e2)     #:transparent)    ;; multiply two numex num
(struct div     (e1 e2)     #:transparent)    ;; divide two numex num
(struct plus    (e1 e2)     #:transparent)    ;; add two numex num

(struct neg     (e1)        #:transparent)    ;; negate a numex num or bool
(struct andalso (e1 e2)     #:transparent)    ;; logical and two numex bool
(struct orelse  (e1 e2)     #:transparent)    ;; logical or two numex bool
(struct cnd     (e1 e2 e3)  #:transparent)    ;; condition, if e1 then e2 else e3

(struct iseq    (e1 e2)     #:transparent)    ;; check equality
(struct ifnzero (e1 e2 e3)  #:transparent)    ;; if e1 is zero then e2 else e3
(struct ifleq   (e1 e2 e3 e4) #:transparent)  ;; if e1 < e2 then e3 else e4


(struct lam     (s1 s2 e)   #:transparent)    ;; a recursive(?) 1-argument function
(struct apply   (e1 e2)     #:transparent)    ;; function application
(struct with    (s e1 e2)   #:transparent)    ;; let e1 be s in e2
(struct apair   (e1 e2)     #:transparent)    ;; a pair of two expressions
(struct first   (e1)        #:transparent)    ;; first element of pair
(struct second  (e1)        #:transparent)    ;; second element of pair

(struct munit   ()          #:transparent)    ;; unit value -- good for ending a list
(struct ismunit (e)         #:transparent)    ;; if e1 is unit then true else false

(struct key     (s e)       #:transparent)    ;; key holds corresponding value of s which is e
(struct record  (k r)       #:transparent)    ;; record/munit holds several keys
(struct value   (s r)       #:transparent)    ;; value returns corresponding value of s in r

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f)     #:transparent) 

(struct letrec  (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Problem 1

(define (racketlist->numexlist xs) (cond  [(equal? xs '()) (munit)]
                                          [#t (apair (car xs) (racketlist->numexlist (cdr xs)))]
                                    )
)

(define (numexlist->racketlist xs) (cond  [(equal? xs munit) '()]
                                          [#t (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
                                    )
)

;; Problem 2

;; lookup a variable in an environment
;; an environment is a racket list of racket pairs
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		  [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond 
        [(var? e)
          (if (string? (var-string e))
              (envlookup env (var-string e))
              (error "NUMEX var should be string")
          )
        ]
        [(num? e)
          (if (integer? (num-int e))
              (num (num-int e))
              (error "NUMEX num should be an integer")
          )
        ]
        [(bool? e)
          (if (boolean? (bool-boolean e))
              (bool (bool-boolean e))
              (error "NUMEX bool should be a boolean")
          )
        ]
        [(plus? e) 
          (let  (
                  [v1 (eval-under-env (plus-e1 e) env)]
                  [v2 (eval-under-env (plus-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (+ (num-int v1) (num-int v2)))
                    (error "NUMEX addition applied to non-number")
                )
          )
        ]
        [(minus? e)
          (let  (
                  [v1 (eval-under-env (minus-e1 e) env)]
                  [v2 (eval-under-env (minus-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (- (num-int v1) (num-int v2)))
                    (error "NUMEX subtraction applied to non-number")
                )
          )
        ]
        [(mult? e)
          (let  (
                  [v1 (eval-under-env (mult-e1 e) env)]
                  [v2 (eval-under-env (mult-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (* (num-int v1) (num-int v2)))
                    (error "NUMEX multiplication applied to non-number")
                )
          )
        ]
        [(div? e)
          (let  (
                  [v1 (eval-under-env (div-e1 e) env)]
                  [v2 (eval-under-env (div-e2 e) env)]
                )
                (if (and (num? v1) (num? v2) (ifnzero v2 #t #f))
                    (num (quotient (num-int v1) (num-int v2)))
                    (error "NUMEX division applied to non-number or e2 is zero")
                )
          )
        ]
        [(andalso? e)
          (let  (
                  [v1 (eval-under-env (andalso-e1 e) env)]
                    [v2 (eval-under-env (andalso-e2 e) env)]
                )
                (if (and (bool? v1) (bool? v2))
                    (bool (and (bool-boolean v1) (bool-boolean v2)))
                    (error "NUMEX and also applied to non-bool")
                )
          )
        ]
        [(orelse? e)
          (let  (
                  [v1 (eval-under-env (orelse-e1 e) env)]
                  [v2 (eval-under-env (orelse-e2 e) env)]
                )
                (if (and (bool? v1) (bool? v2))
                    (bool (or (bool-boolean v1) (bool-boolean v2)))
                    (error "NUMEX or else applied to non-bool")
                )
          )
        ]
        [(neg? e)
          (let  (
                  [v1 (eval-under-env (neg-e1 e) env)]
                )
                (cond
                  [(num? v1) (num (- (num-int v1)))]
                  [(bool? v1) (bool (not (bool-boolean v1)))]
                  [#t (error "NUMEX neg applied to non-num and non-bool")]
                )
          )
        ]
        [(cnd? e)
          (let  (
                  [v1 (eval-under-env (cnd-e1 e) env)]
                )
                (if (bool? v1)
                      (cond
                        [(bool-boolean v1) (eval-under-env (cnd-e2 e) env)]
                        [#t (eval-under-env (cnd-e3 e) env)]
                      )
                  (error "NUMEX cnd should have bool for first arg")
                )
          )
        ]
        [(iseq? e)
            (let  (
                    [v1 (eval-under-env (iseq-e1 e) env)]
                    [v2 (eval-under-env (iseq-e2 e) env)]
                  )
                  (cond [(and (num? v1) (num? v2))
                            (cond 
                              [(equal? (num-int v1) (num-int v2)) (bool #t)]
                              [#t (bool #f)]
                            )
                        ]
                        [(and (bool? v1) (bool? v2))
                            (cond 
                                [(equal? (bool-boolean v1) (bool-boolean v2)) (bool #t)]
                                [#t (bool #f)]
                            )
                        ]
                        [(or (and (bool? v1) (num? v2)) (and (num? v1) (bool? v2))) (bool #f)]
                        [#t (error "NUMEX iseq args should be num")]
                  )      
            )
        ]
        [(ifnzero? e)
            (let  (
                    [v1 (eval-under-env (ifnzero-e1 e) env)]
                  )
                  (if (num? v1)
                      (cond
                        [(eq? (num-int v1) 0) (eval-under-env (ifnzero-e3 e) env)]
                        [#t (eval-under-env (ifnzero-e2 e) env)]
                      )
                      (error "NUMEX ifnzero first arg should be num")
                  )
            )
        ]
        [(ifleq? e)
          (let  (
                  [v1 (eval-under-env (ifleq-e1 e) env)]
                  [v2 (eval-under-env (ifleq-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (cond
                      [ (or (eq? (num-int v1) (num-int v2)) (< (num-int v1) (num-int v2)))
                        (eval-under-env (ifleq-e3 e) env)
                      ]
                      [#t (eval-under-env (ifleq-e4 e) env)]
                    )
                    (error "NUMEX e1 and e2 should be numbers")
                )
          )
        ]
        [(with? e)
          (let  (
                  [v1 (eval-under-env (with-e1 e) env)]
                )
                (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))
          )
        ]
        [(lam? e) (closure env e)]
        [(closure? e)
          (closure (closure-env e) (closure-f e))
        ]
        [(apply? e)
          (let  (
                  [v1 (eval-under-env (apply-e1 e) env)]
                )
                (cond
                  [(closure? v1) (let (
                                        [v2 (closure-f v1)]
                                      )
                                      (let  (
                                              [v3 (eval-under-env (apply-e2 e) env)]
                                            )
                                            (eval-under-env (lam-e v2)
                                                (cons
                                                      (cons (lam-s2 v2) v3)
                                                      (cons (cons  (lam-s1 v2) v1) (closure-env v1))
                                                )
                                            )
                                        )
                                  )
                  ]
                  [#t (error "NUMEX aply first arg should be closure")]
                )
          )
        ]
        [(apair? e)
        (let  (
                [v1 (eval-under-env (apair-e1 e) env)]
                [v2 (eval-under-env (apair-e2 e) env)]
              )
              (apair v1 v2)
        )
        ]
        [(first? e)
        (let  (
                [v1 (eval-under-env (first-e1 e) env)]
              )
              (cond
                [(apair? v1) (eval-under-env (apair-e1 v1) env)]
                [#t (error "NUMEX first arg should be apair")]
              )
        )]
        [(second? e)
        (let  (
                [v1 (eval-under-env (second-e1 e) env)]
              )

              (cond
                [(apair? v1) (eval-under-env (apair-e2 v1) env)]
                [#t (error "NUMEX second arg should be apair")]
              )
        )]
        [(ismunit? e)
        (let  (
                [v1 (eval-under-env (ismunit-e e) env)]
              )
              (cond
                [(munit? v1) (bool #t)]
                [#t (bool #f)]
              )
        )]
        [(letrec? e)
        (let  (
                [v1 (eval-under-env (letrec-e1 e) env)]
                [v2 (eval-under-env (letrec-e2 e) env)]
                [v3 (eval-under-env (letrec-e3 e) env)]
                [v4 (eval-under-env (letrec-e4 e) env)]
              )
              (
                eval-under-env (letrec-e5 e)  (cons (cons (letrec-s4 3) v4)
                                                    (cons  (cons (letrec-s3 e) v3)
                                                    (cons  (cons (letrec-s2 e) v2)
                                                    (cons (cons (letrec-s1 e) v1) env))))
                                                  
              )
        )]
        [(key? e)
        (let  (
                [v1 (eval-under-env (key-e e) env)]
              )
              (if (string? (key-s e))
                  (key (key-s e) v1)
                  (error "NUMEX first arg should be a string")
              )
        )]
        [(record? e)
        (let  (
                [v1 (eval-under-env (record-k e) env)]
                [v2 (eval-under-env (record-r e) env)]
              )
              (cond
                [(and (key? v1) (munit? v2)) (record v1 v2)]
                [(and (key? v1) (record? v2)) (record v1 v2)]
                [#t (error "NUMEX first arg should be key second munit or record")]
              )
        )]
        [(value? e)
        (let  (
                [v1 (eval-under-env (value-r e) env)]
              )
              (if (and (string? (value-s e)) (record? v1))
                   
                  (cond 
                    [(equal? (value-s e) (key-s (record-k v1))) (eval-under-env (key-e (record-k v1)) env)]
                    [(munit? (record-r v1)) (munit)]
                    [#t (eval-under-env (value (value-s e) (record-r v1)) env)]
                  )
                  (error "NUMEX first arg should be string and second record")
              )
        )]
        [(munit? e) (munit)]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) 
  (cond 
    [(eq? (munit? e1) #t) e2]
    [#t e3]
  )
)

(define (with* bs e2)
  (cond
    [(null? bs) (with "s" (munit) e2)]
    [#t (with  (car (car bs))
              (cdr (car bs))
              (with* (cdr bs) e2)
        )
    ]
  )
)

(define (ifneq e1 e2 e3 e4)
  (let  (
          [v1 e1]
          [v2 e2]
        )
        (cond 
              [(and (num? v1) (num? v2)) (ifleq v1 v2 (ifleq v2 v1 e4 e3) e3)]
              [(and (bool? v1) (bool? v2)) (cnd (orelse (andalso v1 v2) (andalso (neg v1) (neg v2))) e4 e3)]
        )
  )
)

;; Problem 4

(define numex-filter
  (lam "f" "func"   (lam "map" "list" (cnd  (ismunit (var "list"))
                                              (munit)
                                              (cnd
                                                (iseq (apply (var "func") (first (var "list"))) (num 0) )
                                                (apply (var "map") (second (var "list")))
                                                (apair (first (var "list")) (apply (var "map") (second (var "list"))))
                                              )                                              
                                      )
                    )
  )
)

(define numex-all-gt
  (lam "f" "i"
    (lam "allgt" "list"
      (apply
        (apply numex-filter (lam "getgt" "x" 
                                (cnd (iseq (var "x") (var "i"))
                                    (num 0)
                                    (ifleq (var "i") (var "x") (num 1) (num 0))
                                )
                            )
        )
        (var "list")
      )
    )
  )
)


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (car (compute-free-vars-rec e))
)

(define (compute-free-vars-rec e)
  (cond
    [(var? e) (cons e (set (var-string e)))]
    [(bool? e) (cons e (set))]
    [(num? e) (cons e (set))]
    [(munit? e) (cons e (set))]
    [(closure? e) (cons e (set))]
    [(munit? e) (cons e (set))]

    [(neg? e) (let ([v1 (compute-free-vars-rec (neg-e1 e))])
              (cons (neg (car v1)) (cdr v1)))
    ]

    [(first? e) (let ([v1 (compute-free-vars-rec (first-e1 e))])
              (cons (first (car v1)) (cdr v1)))
    ]

    [(second? e) (let ([v1 (compute-free-vars-rec (second-e1 e))])
              (cons (second (car v1)) (cdr v1)))
    ]

    [(ismunit? e) (let ([v1 (compute-free-vars-rec (ismunit-e e))])
              (cons (ismunit (car v1)) (cdr v1)))
    ]

    [(apair? e) (let  ([v1 (compute-free-vars-rec (apair-e1 e))]
                      [v2 (compute-free-vars-rec (apair-e2 e))]
                )
                (cons (apair (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ] 

    [(plus? e)  (let  ([v1 (compute-free-vars-rec (plus-e1 e))]
                      [v2 (compute-free-vars-rec (plus-e2 e))]
                )
                (cons (plus (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(minus? e)  (let   ([v1 (compute-free-vars-rec (minus-e1 e))]
                        [v2 (compute-free-vars-rec (minus-e2 e))]
                )
                (cons (minus (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(mult? e)  (let  ([v1 (compute-free-vars-rec (mult-e1 e))]
                      [v2 (compute-free-vars-rec (mult-e2 e))]
                )
                (cons (mult (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(div? e)  (let  ([v1  (compute-free-vars-rec (div-e1 e))]
                      [v2 (compute-free-vars-rec (div-e2 e))]
                )
                (cons (div (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(andalso? e)  (let  ([v1 (compute-free-vars-rec (andalso-e1 e))]
                      [v2 (compute-free-vars-rec (andalso-e2 e))]
                )
                (cons (andalso (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(orelse? e)  (let  ([v1 (compute-free-vars-rec (orelse-e1 e))]
                      [v2 (compute-free-vars-rec (orelse-e2 e))]
                )
                (cons (orelse (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(iseq? e)  (let  ([v1 (compute-free-vars-rec (iseq-e1 e))]
                      [v2 (compute-free-vars-rec (iseq-e2 e))]
                )
                (cons (iseq (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(apply? e)  (let  ([v1 (compute-free-vars-rec (apply-e1 e))]
                      [v2 (compute-free-vars-rec (apply-e2 e))]
                )
                (cons (apply (car v1) (car v2))
                      (set-union (cdr v1) (cdr v2))
                ))
    ]

    [(cnd? e) (let
                ([v1 (compute-free-vars-rec (cnd-e1 e))]
                [v2 (compute-free-vars-rec (cnd-e2 e))]
                [v3 (compute-free-vars-rec (cnd-e3 e))]
              )
              (cons (cnd (car v1) (car v2) (car v3))
                    (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))
              ))
    
    ]

    [(ifnzero? e) (let
                ([v1 (compute-free-vars-rec (ifnzero-e1 e))]
                [v2 (compute-free-vars-rec (ifnzero-e2 e))]
                [v3 (compute-free-vars-rec (ifnzero-e3 e))]
              )
              (cons (ifnzero (car v1) (car v2) (car v3))
                    (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))
              ))
    
    ]

    [(ifleq? e) (let
                ([v1 (compute-free-vars-rec (ifleq-e1 e))]
                [v2 (compute-free-vars-rec (ifleq-e2 e))]
                [v3 (compute-free-vars-rec (ifleq-e3 e))]
                [v4 (compute-free-vars-rec (ifleq-e4 e))]
              )
              (cons (ifleq (car v1) (car v2) (car v3) (car v4))
                    (set-union (cdr v1) (set-union (cdr v2) (set-union (cdr v3) (cdr v4))))
              ))
    ]

    [(lam? e) (let
                ([v1 (compute-free-vars-rec (lam-e e))])
                (let
                  ([v2 (set-remove (set-remove (cdr v1) (lam-s2 e)) (lam-s1 e))])
                  (cons (fun-challenge (lam-s1 e) (lam-s2 e) (lam-e e) v2) v2)
                )
              )
    ]
    [(with? e)
      (let  (
              [v1 (compute-free-vars-rec (with-e1 e))]
              [v2 (compute-free-vars-rec (with-e2 e))]
            )
            (cons (with (with-s e) (car v1) (car v2))
                  (set-remove (set-union (cdr v1) (cdr v2)) (with-s e))
            )
      )
    ]

    [#t error("something happened")]
  )
)


(define (generateNewEnv env freevars)
  (cond [(null? env) '()]
        [#t (cond
                [(set-member? freevars (car (car env))) (cons (car env) (generateNewEnv (cdr env) freevars))]
                [#t (generateNewEnv (cdr env) freevars)]
            )
        ]
  )
)


;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond 
        [(var? e)
          (if (string? (var-string e))
              (envlookup env (var-string e))
              (error "NUMEX var should be string")
          )
        ]
        [(num? e)
          (if (integer? (num-int e))
              (num (num-int e))
              (error "NUMEX num should be an integer")
          )
        ]
        [(bool? e)
          (if (boolean? (bool-boolean e))
              (bool (bool-boolean e))
              (error "NUMEX bool should be a boolean")
          )
        ]
        [(plus? e) 
          (let  (
                  [v1 (eval-under-env-c (plus-e1 e) env)]
                  [v2 (eval-under-env-c (plus-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (+ (num-int v1) (num-int v2)))
                    (error "NUMEX addition applied to non-number")
                )
          )
        ]
        [(minus? e)
          (let  (
                  [v1 (eval-under-env-c (minus-e1 e) env)]
                  [v2 (eval-under-env-c (minus-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (- (num-int v1) (num-int v2)))
                    (error "NUMEX subtraction applied to non-number")
                )
          )
        ]
        [(mult? e)
          (let  (
                  [v1 (eval-under-env-c (mult-e1 e) env)]
                  [v2 (eval-under-env-c (mult-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (num (* (num-int v1) (num-int v2)))
                    (error "NUMEX multiplication applied to non-number")
                )
          )
        ]
        [(div? e)
          (let  (
                  [v1 (eval-under-env-c (div-e1 e) env)]
                  [v2 (eval-under-env-c (div-e2 e) env)]
                )
                (if (and (num? v1) (num? v2) (ifnzero v2 #t #f))
                    (num (quotient (num-int v1) (num-int v2)))
                    (error "NUMEX division applied to non-number or e2 is zero")
                )
          )
        ]
        [(andalso? e)
          (let  (
                  [v1 (eval-under-env-c (andalso-e1 e) env)]
                    [v2 (eval-under-env-c (andalso-e2 e) env)]
                )
                (if (and (bool? v1) (bool? v2))
                    (bool (and (bool-boolean v1) (bool-boolean v2)))
                    (error "NUMEX and also applied to non-bool")
                )
          )
        ]
        [(orelse? e)
          (let  (
                  [v1 (eval-under-env-c (orelse-e1 e) env)]
                  [v2 (eval-under-env-c (orelse-e2 e) env)]
                )
                (if (and (bool? v1) (bool? v2))
                    (bool (or (bool-boolean v1) (bool-boolean v2)))
                    (error "NUMEX or else applied to non-bool")
                )
          )
        ]
        [(neg? e)
          (let  (
                  [v1 (eval-under-env-c (neg-e1 e) env)]
                )
                (cond
                  [(num? v1) (num (- (num-int v1)))]
                  [(bool? v1) (bool (not (bool-boolean v1)))]
                  [#t (error "NUMEX neg applied to non-num and non-bool")]
                )
          )
        ]
        [(cnd? e)
          (let  (
                  [v1 (eval-under-env-c (cnd-e1 e) env)]
                )
                (if (bool? v1)
                      (cond
                        [(bool-boolean v1) (eval-under-env-c (cnd-e2 e) env)]
                        [#t (eval-under-env-c (cnd-e3 e) env)]
                      )
                  (error "NUMEX cnd should have bool for first arg")
                )
          )
        ]
        [(iseq? e)
            (let  (
                    [v1 (eval-under-env-c (iseq-e1 e) env)]
                    [v2 (eval-under-env-c (iseq-e2 e) env)]
                  )
                  (cond [(and (num? v1) (num? v2))
                            (cond 
                              [(equal? (num-int v1) (num-int v2)) (bool #t)]
                              [#t (bool #f)]
                            )
                        ]
                        [(and (bool? v1) (bool? v2))
                            (cond 
                                [(equal? (bool-boolean v1) (bool-boolean v2)) (bool #t)]
                                [#t (bool #f)]
                            )
                        ]
                        [(or (and (bool? v1) (num? v2)) (and (num? v1) (bool? v2))) (bool #f)]
                        [#t (error "NUMEX iseq args should be num")]
                  )      
            )
        ]
        [(ifnzero? e)
            (let  (
                    [v1 (eval-under-env-c (ifnzero-e1 e) env)]
                  )
                  (if (num? v1)
                      (cond
                        [(eq? (num-int v1) 0) (eval-under-env-c (ifnzero-e3 e) env)]
                        [#t (eval-under-env-c (ifnzero-e2 e) env)]
                      )
                      (error "NUMEX ifnzero first arg should be num")
                  )
            )
        ]
        [(ifleq? e)
          (let  (
                  [v1 (eval-under-env-c (ifleq-e1 e) env)]
                  [v2 (eval-under-env-c (ifleq-e2 e) env)]
                )
                (if (and (num? v1) (num? v2))
                    (cond
                      [ (or (eq? (num-int v1) (num-int v2)) (< (num-int v1) (num-int v2)))
                        (eval-under-env-c (ifleq-e3 e) env)
                      ]
                      [#t (eval-under-env-c (ifleq-e4 e) env)]
                    )
                    (error "NUMEX e1 and e2 should be numbers")
                )
          )
        ]
        [(with? e)
          (let  (
                  [v1 (eval-under-env-c (with-e1 e) env)]
                )
                (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v1) env))
          )
        ]
        [(fun-challenge? e) 
          (closure (generateNewEnv env (set-remove (fun-challenge-freevars e) (fun-challenge-nameopt e))) e)
        ]
        [(closure? e)
          (closure (closure-env e) (closure-f e))
        ]
        [(apply? e)
          (let  (
                  [v1 (eval-under-env-c (apply-e1 e) env)]
                )
                (cond
                  [(closure? v1) (let (
                                        [v2 (closure-f v1)]
                                      )
                                      (let  (
                                              [v3 (eval-under-env-c (apply-e2 e) env)]
                                            )
                                            (eval-under-env-c (lam-e v2)
                                                (cons
                                                      (cons (lam-s2 v2) v3)
                                                      (cons (cons  (lam-s1 v2) v1) (closure-env v1))
                                                )
                                            )
                                        )
                                  )
                  ]
                  [#t (error "NUMEX aply first arg should be closure")]
                )
          )
        ]
        [(apair? e)
        (let  (
                [v1 (eval-under-env-c (apair-e1 e) env)]
                [v2 (eval-under-env-c (apair-e2 e) env)]
              )
              (apair v1 v2)
        )
        ]
        [(first? e)
        (let  (
                [v1 (eval-under-env-c (first-e1 e) env)]
              )
              (cond
                [(apair? v1) (eval-under-env-c (apair-e1 v1) env)]
                [#t (error "NUMEX first arg should be apair")]
              )
        )]
        [(second? e)
        (let  (
                [v1 (eval-under-env-c (second-e1 e) env)]
              )

              (cond
                [(apair? v1) (eval-under-env-c (apair-e2 v1) env)]
                [#t (error "NUMEX second arg should be apair")]
              )
        )]
        [(ismunit? e)
        (let  (
                [v1 (eval-under-env-c (ismunit-e e) env)]
              )
              (cond
                [(munit? v1) (bool #t)]
                [#t (bool #f)]
              )
        )]
        [(letrec? e)
        (let  (
                [v1 (eval-under-env-c (letrec-e1 e) env)]
                [v2 (eval-under-env-c (letrec-e2 e) env)]
                [v3 (eval-under-env-c (letrec-e3 e) env)]
                [v4 (eval-under-env-c (letrec-e4 e) env)]
              )
              (
                eval-under-env-c (letrec-e5 e)  (cons (cons (letrec-s4 3) v4)
                                                    (cons  (cons (letrec-s3 e) v3)
                                                    (cons  (cons (letrec-s2 e) v2)
                                                    (cons (cons (letrec-s1 e) v1) env))))
                                                  
              )
        )]
        [(key? e)
        (let  (
                [v1 (eval-under-env-c (key-e e) env)]
              )
              (if (string? (key-s e))
                  (key (key-s e) v1)
                  (error "NUMEX first arg should be a string")
              )
        )]
        [(record? e)
        (let  (
                [v1 (eval-under-env-c (record-k e) env)]
                [v2 (eval-under-env-c (record-r e) env)]
              )
              (cond
                [(and (key? v1) (munit? v2)) (record v1 v2)]
                [(and (key? v1) (record? v2)) (record v1 v2)]
                [#t (error "NUMEX first arg should be key second munit or record")]
              )
        )]
        [(value? e)
        (let  (
                [v1 (eval-under-env-c (value-r e) env)]
              )
              (if (and (string? (value-s e)) (record? v1))
                   
                  (cond 
                    [(equal? (value-s e) (key-s (record-k v1))) (eval-under-env-c (key-e (record-k v1)) env)]
                    [(munit? (record-r v1)) (munit)]
                    [#t (eval-under-env-c (value (value-s e) (record-r v1)) env)]
                  )
                  (error "NUMEX first arg should be string and second record")
              )
        )]
        [(munit? e) (munit)]
        [#t (error (format "bad NUMEX expression: ~v" e))])
)

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
