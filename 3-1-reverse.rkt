#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме да обърнем цифрите на дадено число.
(define (count-digits x)
  (if(< x 10)
     1
     (+ 1 (count-digits (/ x 10)))))
(define (my-reverse x)
  (define (my-rev x i)
  (if(< x 10)
     x
     (+ (* (modulo x 10)(expt 10 i))(my-rev (floor (/ x 10)) (- i 1)))))
  (my-rev x (- (count-digits  x) 1)))
(define (palindrome? x)
  (if (= x (my-reverse x))
      #t
      #f))
 (define tests (test-suite
  "Reverse tests"
   (test-case "" (check-equal? (my-reverse 0) 0))
  (test-case "" (check-equal? (my-reverse 5) 5))
  (test-case "" (check-equal? (my-reverse 10) 1))
  (test-case "" (check-equal? (my-reverse 21) 12))
  (test-case "" (check-equal? (my-reverse 10234) 43201))
  (test-case "" (check-equal? (my-reverse 10234003) 30043201))
))
(define tests1 (test-suite
  "Palindrome tests"
   (check-true (palindrome? 1))
  (check-false (palindrome? 10))
  (check-true (palindrome? 101))
  (check-true (palindrome? 121))
  (check-false (palindrome? 122))
  (check-false (palindrome? 221))
  (check-false (palindrome? 1220))
  (check-true (palindrome? 1221))
  (check-false (palindrome? 12210))
  (check-true (palindrome? 1234321))
  (check-true (palindrome? 12344321))
  (check-false (palindrome? 123421))
  (check-false (palindrome? 124321))
))
 (run-tests tests 'verbose)
 (run-tests tests1 'verbose)
