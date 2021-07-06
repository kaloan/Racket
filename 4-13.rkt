#lang racket
(require rackunit)
(require rackunit/text-ui)



 ; Искаме функция, която приема списък и две числа и връща
; списък, състоящ се от елементите на списъка, които се намират на индекси от първото число до второто.

(define (slice l first second)
  (define (searcher L f)
    (if (null? L) null
    (if (= f 0) L
        (searcher (cdr L) (- f 1)))))
  (define (true-slice List F S)
    (if (null? List) null
    (if (> F S) null
        (cons (car List) (true-slice (cdr List) (+ F 1) S)))))
  (true-slice (searcher l first) first second))

 (define tests
 (test-suite "Slice tests"
     (check-equal? (slice '(1 9 8 2) 1 2) '(9 8))
     (check-equal? (slice '(9 7 2 3) 0 2) '(9 7 2))
     (check-equal? (slice '(9 7 2 5 6 2 3) 0 4) '(9 7 2 5 6)) 
  )
)
 (run-tests tests 'verbose)