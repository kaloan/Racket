#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме да обърнем число от двоична в десетична бройна система
(define (bin-to-dec x)
  (define (bin-to-dec-i x i)
  (if(< x 10)
     (* x (expt 2 i))
     (+ (* (expt 2 i) (modulo x 10)) (bin-to-dec-i (floor (/ x 10)) (+ i 1)))))
  (bin-to-dec-i x 0))
 (define tests (test-suite
  "binary to decimal tests"
   (test-case "zero" (check-equal? (bin-to-dec 0) 0))
  (test-case "one" (check-equal? (bin-to-dec 1) 1))
  (test-case "rip" (check-equal? (bin-to-dec 1010010110000) 5296))
  (test-case "power of 2" (check-equal? (bin-to-dec 10000) 16))
))
 (run-tests tests 'verbose)