#lang racket
(require rackunit)
(require rackunit/text-ui)



; Искаме с подходящи извиквания на car и cdr да вземем всяко число.
 (define my-list '(1 2 3 (4 5) (6 (7 8))))
(define (reader list)
  (define (checker l)
        (if(pair? (car l))
         (reader (car l))
         null)
        )
  (if (null? list)
      null
      (if(pair? (car list))
         (begin (reader (car list)) (reader (cdr list)))
         (begin (print (car list))
                (reader (cdr list)))
         )
      )
  )
(reader my-list)


; Искаме да намерим дължината на даден списък
(define (my-len list)
  (if (null? list)
      0
      (+ 1 (my-len (cdr list)))))
 (define my-list-1 '(1 2 3 4 5 6 '("asd" 10 9 8) "b" "q" "Java" "sucks" "JS" "rocks"))
(define my-list-2 '(()))
 (define tests1
  (test-suite "Length tests"
  
    (test-case "base" (check-equal? (my-len '()) 0))
    (test-case "many elements" (check-equal? (my-len my-list-1) 13))
    (test-case "wow" (check-equal? (my-len my-list-2) 1))
  )
)
(run-tests tests1 'verbose)


; Да се конструира списъкът с числата от start до end.
(define (range from to)
  (list from (range (+ from 1) to)))
 (define tests2
  (test-suite "Range tests"
     (test-case "" (check-equal? (range 1 1) '(1)))
    (test-case "" (check-equal? (range 1 4) '(1 2 3 4)))
  )
)
 (run-tests tests2 'verbose)
 
