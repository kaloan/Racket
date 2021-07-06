#lang racket
(require rackunit)
(require rackunit/text-ui)



; Искаме да направим списък от всички стойности в даден такъв.
; Искаме нивата на влагане да изчезнат.
(define (flatten list)
  (define (checker l)
        (if(pair? (car l))
         (flatten (car l))
         null)
        )
  (if (null? list)
      null
      (if(pair? (car list))
         (cons (flatten (car list)) (flatten (cdr list)))
         (cons (car list)
                (flatten (cdr list)))
         )
      )
  )
 (define tests1
  (test-suite "Map tests"
    
    (test-case "" (flatten '(1 2 (3 (4 5) 6) 7 8)) '(1 2 3 4 5 6 7 8))
  )
)
 (run-tests tests1 'verbose)



; Сечение на двата списъка (приемаме, че няма повтарящи се елементи).
(define (intersection l1 l2)
  (if (null? l1) null
  (if (equal? (member (car l1) l2) #f) (intersection (cdr l1) l2)
      (cons (car l1)(intersection (cdr l1) l2)))))

      
 (define tests2
  (test-suite "Intersection tests"
     (test-case "" (check-equal? (intersection '(2 3 5) '(4 1 3 2)) '(2 3)))
    (test-case "" (check-equal? (intersection '(1 2 3) '(1 2 3)) '(1 2 3)))
    (test-case "" (check-equal? (intersection '(1 2 3) '(4 5 6)) '()))
    (test-case "" (check-equal? (intersection '(1) '(1)) '(1)))
  )
)
 (run-tests tests2 'verbose)



; Обединение на двата списъка (приемаме, че няма повтарящи се елементи).
(define (union l1 l2)
  (if (null? l1) l2
  (if (equal? (member (car l1) l2) #f) (union (cdr l1) (cons (car l1) l2))
      (union (cdr l1) l2))))
 (define tests3
  (test-suite "Union tests"
     (test-case "" (check-equal? (union '(2 3 5) '(4 1 3 2)) '(5 4 1 3 2)))
    (test-case "" (check-equal? (union '(1 2 3) '(1 2 3)) '(1 2 3)))
    (test-case "" (check-equal? (union '(1) '()) '(1)))
    (test-case "" (check-equal? (union '() '()) '()))
  )
)
 (run-tests tests3 'verbose)
