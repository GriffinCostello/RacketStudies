#lang racket



;; ===================================== Preamble ======================================

(provide (all-defined-out)) ;; so we can put tests in a second file

;; Definition of structures for MF/PL programs - Do NOT change
(struct var (string) #:transparent) ;; a variable, e.g., (var "foo")
(struct int (num) #:transparent) ;; a constant number, e.g., (int 17)
(struct add (e1 e2) #:transparent) ;; add two expressions
(struct if> (e1 e2 e3 e4) #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual) #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2) #:transparent) ;; make a new pair
(struct fst (e) #:transparent) ;; get first part of a pair
(struct snd (e) #:transparent) ;; get second part of a pair
(struct aunit () #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)



;; ====================================== Answers ======================================

; 2.1: TODO
; If we reach aunit we reached end, return empty list
; If its apair, and first element is a list, then recurse through that nested list
; If not then just use that value
; Then conver tthe rest of the pairs
(define (mfpl-list->rkt-list lst)
  (cond
    [(aunit? lst)
     '()]
    [(apair? lst)
     (cons
      (let ([first (apair-e1 lst)])
        ;If the first value is aunit or pair, then we recurse throught this pair
        (if (or (aunit? first) (apair? first))
            (mfpl-list->rkt-list first)
            first))
      (mfpl-list->rkt-list (apair-e2 lst)))]
    [else
     (error "not an mfpl list")]))

; 2.2: TODO
; If the list is empty, return aunit
; If not take the first element, if its a list recurse throught that
; If not then just use that value
; Build everything using apair
(define (rkt-list->mfpl-list lst)
   (if (empty? lst)
      (aunit)
      (apair
       (let ([first (car lst)])
         (if (list? first)
             (rkt-list->mfpl-list first)
             first))
       (rkt-list->mfpl-list (cdr lst)))))

; 2.3
;; Lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond
    [(null? env) (error "unbound variable during evaluation" str)]
    [(equal? (car (car env)) str) (cdr (car env))]
    [#t (envlookup (cdr env) str)]))
; TODO:
;; DO NOT change the two cases given to you.
;; DO add more cases for other kinds of MF/PL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond
    [(var? e) (envlookup env (var-string e))]
    [(add? e)
     (let ([v1 (eval-under-env (add-e1 e) env)] [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (int (+ (int-num v1) (int-num v2)))
           (error "MF/PL addition applied to non-number")))]
    ; If it's an integer then it evaluates to itself
    [(int? e) e]
    ;Evaluate the two numbers being compared
    ;If v1 > v2, then e3, otherwise e4
    [(if>? e)
     (let ([v1 (eval-under-env (if>-e1 e) env)] [v2 (eval-under-env (if>-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (if>-e3 e) env)
               (eval-under-env (if>-e4 e) env))
           (error "MF/PL if> applied to non-number")))]
    ; Return the first element in a apair if the expression is apair
    [(fst? e)
     (let ([v (eval-under-env (fst-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "MF/PL fst applied to non-pair")))]
    ; Return the second element in a apair if the expression is apair
    [(snd? e)
     (let ([v (eval-under-env (snd-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "MF/PL snd applied to non-pair")))]
    [(isaunit? e)
     (let ([v (eval-under-env (isaunit-e e) env)])
       (if (aunit? v)
           (int 1)
           (int 0)))]
    ; Evaluate the expression, then exrtend the environment with variable and value
    [(mlet? e)
     (let ([v (eval-under-env (mlet-e e) env)])
       (eval-under-env
        (mlet-body e)
        (cons (cons (mlet-var e) v) env)))]
    ;Ensures the function expression is a closure
    ;Use the closure's environment
    ;If it's recursive add it's own name to the enviornment
    [(call? e)
     (let ([funval (eval-under-env (call-funexp e) env)] [actval (eval-under-env (call-actual e) env)])
       (if (closure? funval)
           ;gets the function defined and it's environment
           (let* ([funexpr (closure-fun funval)] [baseenv (closure-env funval)]
                  [namedenv
                   ;if its recursive then function binds itself to it's environment
                   (if (fun-nameopt funexpr)
                       (cons (cons (fun-nameopt funexpr) funval)
                             baseenv)
                       baseenv)]
                  ;bind the parameter to it's environment
                  [final-env
                   (cons (cons (fun-formal funexpr) actval)
                         namedenv)])
             ;call the function
             (eval-under-env (fun-body funexpr) final-env))
           (error "MF/PL: call applied to non-function")))]
    ; Don't evaluate body right away
    ; Create a closure with the environment
    [(fun? e)
     (closure env e)]
    [(apair? e)
     (let ([v1 (eval-under-env (apair-e1 e) env)] [v2 (eval-under-env (apair-e2 e) env)])
       (apair v1 v2))]
    [(aunit? e) e]
    [#t (error (format "bad MF/PL expression: ~v" e))]))

;; Do NOT change (modify eval-under-env instead!)
;; Note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

; 2.4: TODO
; Check if the first element is aunit, if it is
; then 1 > 0, so we reutnr e2, otherwise e3
(define (ifaunit e1 e2 e3)
  (if> (isaunit e1) (int 0) e2 e3))

; 2.5: TODO
; If list is empty, evaluate to e2
; If not, take  the pair (variable expression)
; Then create an mlet binding that recurisvely wraps the rest of the bindings
(define (mlet* lstlst e2)
  (if (empty? lstlst)
      e2
      (let* ([pair (car lstlst)]
             [variablename (car pair)]
             [variableexpression (cdr pair)]
             [rest (cdr lstlst)])
        (mlet variablename variableexpression (mlet* rest e2)))))
                                 

; 2.6: TODO
; Checks if e1 and e2 are equal by checking if
; e1 !> e2 and that e2!>e1
; If it's neither of these then they are equal
(define (if= e1 e2 e3 e4)
  (if> e1 e2
       e4
       (if> e2 e1
            e4
            e3)))

; 2.7: TODO
(define mfpl-map #f)
;; This binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mfpl-map" "f"    ;; it is  function "mfpl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;;

; 2.8: TODO
(define mfpl-map-add-N
  (mlet "map" mfpl-map #f ; (notice map is now in MF/PL scope)
        ))



;; ==================================== Test suite =====================================
; See prob2_test.rkt
