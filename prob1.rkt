#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file


;; ====================================== Answers ======================================

; 1.1: TODO
; Checks if n=0, if it is return empty list
; If not then add number to front of list and decrement n
(define (stream-for-n-steps s n)
  (cond
    [(= n 0) '()]
    [else
     (let ([p (s)])
       (cons (car p)
             (stream-for-n-steps (cdr p) (- n 1))))]))
            
  
; 1.2: TODO
; Get pair of numbers (initially 0 and 1)
; Add the first number (x) to head of list
; Then pass the second number (y) as first value in pair
; Then pass x+y as second value in pair
(define fibo-stream
  (letrec ([f (λ (x y)
                (cons x
                      (λ () (f y (+ x y)))))])
    (λ () (f 0 1))))

; 1.3: TODO
; Call the stream and check if (car p) satisfies f
; if it does, add it to the list
; Then call the function again with rest of stream
(define (filter-stream f s)
  (λ ()
    (let ([p (s)])
      (if (f (car p))
          (cons (car p)
                (filter-stream f (cdr p)))
          ((filter-stream f (cdr p)))))))

; 1.4: TODO
; g takes a current index, computes (f i) and cons that value with a λ that calls g on (i + delta)
; gets initiatilized with i0
(define-syntax create-stream
  (syntax-rules (using starting at with increment)
    [(create-stream name using f starting at i0 with increment delta)
     (define name
       (letrec ([g (λ (i)
                     (cons (f i)
                           (λ () (g (+ i delta)))))])
         (λ () (g i0))))]))



;; ==================================== Test suite =====================================

(require rackunit)

;; Sample stream for testing stream-for-n-steps
(define nat-num-stream (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]) (lambda () (f 0))))

;; Test create-stream macro
(create-stream squares using (lambda (x) (* x x)) starting at 5 with increment 2)

(define tests
  (test-suite "Sample tests for A3 P1"
   (check-equal? (stream-for-n-steps nat-num-stream 10)
                 '(0 1 2 3 4 5 6 7 8 9)
                 "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps fibo-stream 10) 
                 '(0 1 1 2 3 5 8 13 21 34) 
                 "fibo-stream test")
   (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (> i 5)) nat-num-stream) 5)
                 '(6 7 8 9 10)
                 "filter stream test")
   (check-equal? (stream-for-n-steps squares 5)
                 '(25 49 81 121 169)
                 "stream defined using a macro. only tests is return value")))

;; Run the tests
(require rackunit/text-ui)
(run-tests tests)
