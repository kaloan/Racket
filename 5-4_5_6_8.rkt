#lang racket
(require rackunit)
(require rackunit/text-ui)
; Искаме да натрупаме резултат, обхождайки списък.
(define (reduce op l nvalue)
  (if (null? l) nvalue
      (op (car l) (reduce op (cdr l) nvalue))))
 (define tests
  (test-suite "Reduce tests"
    
    (test-case "" (check-equal? (reduce + '(1 2 3 4 5) 0) 15))
    (test-case "" (check-equal? (reduce * '(1 2 4 6 8 10) 1) 3840))
  )
)
 (run-tests tests 'verbose)




; Искаме да проверим дали някой елемент в даден списък удовлетворява даден предикат
(define (any? l pred)
  (cond ((null? l) #f)
    ((pred (car l)) #t)
    (else (any? (cdr l) pred))))
 (define tests2
  (test-suite "Any tests"
    
    (test-case "" (check-true (any? '(1 2 3 4 5 6 7) even?)))
    (test-case "" (check-false (any? '(2 4 6 8) odd?)))
  )
)
 (run-tests tests2 'verbose)

; Искаме да проверим дали всички елементи на списък удовлетворяват даден предикат
(define (all? l pred)
  (cond ((null? l) #t)
        ((pred (car l)) (all? (cdr l) pred))
        (else #f)))
 (define tests3
  (test-suite "All tests"
    
    (test-case "" (check-true (all? '(2 4 6) even?)))
    (test-case "" (check-false (all? '(2 5 6 8) odd?)))
  )
)
 (run-tests tests3 'verbose)

;Композираме функцията f(x) n пъти - f ( f ( ... ( f (x)) .. ))
(define (compose func x n)
  (if (= n 0) x
      (func (compose func x (- n 1)))))
 (define tests4
  (test-suite "compose tests"
     (test-case "" (check-equal? (compose (lambda (x) (/ x 2)) 1024 3) 128))
    (test-case "" (check-equal? (compose (lambda (x) (* 10 x)) 5 3) 5000))
  )
)
 (run-tests tests4 'verbose)