#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме да напишем процедурата accumulate
(define (accumulate op null_value a b)
  (define (iter i s)
    (if (> i b) s
        (iter (+ i 1) (op s i))))
  (iter a null_value))

 (define tests1
  (test-suite "Accumulate tests"
     (check-equal? (accumulate + 0 1 100 ) 5050)
    (check-equal? (accumulate + 0 9 9 ) 9)
    (check-equal? (accumulate * 1 1 5 ) 120)
  )
)



; Искаме да дефинираме процедурата, която намира n!, като използваме accumulate
(define (fact x)
  (accumulate * 1 1 x))
 (define tests2 (test-suite
  "Factorial tests"
   (test-case "" (check-equal? (fact 5) 120))
  (test-case "" (check-equal? (fact 3) 6))
))



; Искаме да повдигнем число на степен, използвайки процедурата accumulate
(define (pow x y)
  (accumulate * 1 x y))
 (define tests3 (test-suite
  "Pow tests"
   (test-case "" (check-equal? (pow 3 3) 27))
  (test-case "" (check-equal? (pow 2 10) 1024))
))

 (run-tests tests1 'verbose)
 (run-tests tests2 'verbose)
 (run-tests tests3 'verbose)