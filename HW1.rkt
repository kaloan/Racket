#lang racket
(require rackunit)
(require rackunit/text-ui)

;Задача 1 и тестове 1

(define (S i j f)
  (define (Sumator a b g sum)
    (if(> a b) sum
       (Sumator (+ a 1) b g (+ sum (g a)))))
  (Sumator i j f 0))

(define tests
  (test-suite "Sum tests"
    
    (test-case "" (check-equal? (S 1 10 (lambda (x) x)) 55))
    (test-case "" (check-equal? (S 1 20 (lambda (x) (* x x))) 2870))
    (test-case "" (check-equal? (S 25 40 (lambda (x) (* 0 x))) 0))
    (test-case "" (check-equal? (S 53 8 (lambda (x) (+ (* x x) (- x 1)))) 0))
  )
)
 (run-tests tests 'verbose)


;Задача 2 и тестове 2

(define (getMax L)
  (define (findMax List max)
    (cond ((null? List) max)
          ((> (car List) max) (findMax (cdr List) (car List)))
          (else (findMax (cdr List) max))))
  (if (null? L) '()
      (findMax L (car L))))


(define tests2
  (test-suite "Max tests"
    
    (test-case "" (check-equal? (getMax '(5 3 15 8 7 23 1 9 8)) 23))
    (test-case "" (check-equal? (getMax '(-20 -13 -2 1 2 3 4 5 6 7 8 9 10)) 10))
    (test-case "" (check-equal? (getMax '(28 8 16 8 7 82 71 9 87287 82 61 6 8 1 10 83)) 87287))
    (test-case "" (check-equal? (getMax '()) '()))
  )
)
 (run-tests tests2 'verbose)