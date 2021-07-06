#lang racket
(require rackunit)
(require rackunit/text-ui)
 ; Искаме функция, която връща най-малкия елемент на списък.
(define (my-min l)
  
  (define (my-minim L cm)
    (if(null? L) cm
       (if (< (car L) cm) (my-minim (cdr L) (- cm (- cm (car L))))
           (my-minim (cdr L) cm))))
  
  (my-minim l (car l)))
(define (my-max l)
  
  (define (my-maxim L cm)
    (if(null? L) cm
       (if (> (car L) cm) (my-maxim (cdr L) (- cm (- cm (car L))))
           (my-maxim (cdr L) cm))))
  
  (my-maxim l (car l)))        
; Искаме и такава, която връща най-големия.
 (define tests
  (test-suite "min max tests"
    
    (test-case "min" (check-equal? (my-min '(1 2 3 4 5 6 7)) 1))
    (test-case "max" (check-equal? (my-max '(1 2 3 4 5 6 7)) 7))
  )
)
 (run-tests tests 'verbose)



; Искаме да приложим функция върху всеки от елементите на списък.
(define (my-map l op)
  (if (null? l) '()
      (cons (op (car l)) (my-map (cdr l) op))))
 (define tests2
  (test-suite "Map tests"
    
    (test-case "base" (check-equal? (my-map '() sqr) '()))
    (test-case "!base" (check-equal? (my-map '(1 2 3 4 5)  sqr) '(1 4 9 16 25)))
  )
)
 (run-tests tests2 'verbose)

; Искаме да вземем само елементите от даден списък, които ни интересуват.
(define (my-filter l op)
  (cond ((null? l) '())
        ((op (car l)) (cons (car l) (my-filter (cdr l) op)))
        (else (my-filter (cdr l) op))))
 (define tests3
  (test-suite "Filter tests"
    
    (test-case "base" (check-equal? (my-filter '() even?) '()))
    (test-case "!base" (check-equal? (my-filter (range 1 9) even?) '(2 4 6 8)))
  )
)
 (run-tests tests3 'verbose)