#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме да проверим дали даден низ съдържа в себе си друг даден низ.
(define (substr? x y)
  (if (= x 0)
      #f
      (if (and (< y 10) (= (modulo x 10) y))
          #t
          (if (= (modulo x 10) (modulo y 10))
              (substr? (floor (/ x 10)) (floor (/ y 10)))
              (substr? (floor (/ x 10)) y))))
  )
 (define tests (test-suite
  "Substring tests"
   (check-true (substr? 1234 23))
  (check-true (substr? 1234 12))
  (check-true (substr? 1234 1))
  (check-true (substr? 1234 2))
  (check-true (substr? 1234 4))
  (check-false (substr? 761235971249 18))
))
 (run-tests tests 'verbose)
