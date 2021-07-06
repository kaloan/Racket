#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме да намерим сумата от цифрите на дадено число. Ама итеративно.
(define (sum-digits x)
  (if (< x 10)
  x
  (+ (modulo x 10) (sum-digits(floor (/ x 10)))))) 
;(define (sum-digits-iter x i))
  
 (define tests (test-suite
  "Iterative sum digits"
  (test-case "" (check-equal? (sum-digits 0) 0))
  (test-case "" (check-equal? (sum-digits 1) 1))
  (test-case "" (check-equal? (sum-digits 9) 9))
  (test-case "" (check-equal? (sum-digits 10) 1))
  (test-case "" (check-equal? (sum-digits 14) 5))
  (test-case "" (check-equal? (sum-digits 145) 10))
  ; (test-case "" (check-equal? (sum-digits-iter 0) 0))
 ; (test-case "" (check-equal? (sum-digits-iter 1) 1))
 ; (test-case "" (check-equal? (sum-digits-iter 9) 9))
 ; (test-case "" (check-equal? (sum-digits-iter 10) 1))
 ; (test-case "" (check-equal? (sum-digits-iter 145) 10))
))
 (run-tests tests 'verbose)