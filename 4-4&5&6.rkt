#lang racket
(require rackunit)
(require rackunit/text-ui)
; Искаме да проверим дали даден елемент се среща в даден списък
(define (member? x list)
  (if (null? list)
      #f
      (if(= x (car list))
           #t
           (member? x (cdr list)))))
 (define my-list '(1 2 3 4 5 6))
 (define tests1
  (test-suite "member tests"
  
    (test-case "base" (check-false (member? 2 '())))
    (test-case "!base" (check-true (member? 2 my-list)))
  )
)
 (run-tests tests1 'verbose)



; Искаме да залепим два списъка
(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

 (define tests2
  (test-suite "Append tests"
     (test-case "" (check-equal? (my-append '(1 2 3) '()) '(1 2 3)))
    (test-case "" (check-equal? (my-append '() '(1 2 3)) '(1 2 3)))
    (test-case "" (check-equal? (my-append '(4 5 6) '(1 2 3)) '(4 5 6 1 2 3)))
  )
)
 (run-tests tests2 'verbose)



; Търсим функция, която обръща даден списък
(define (reverse L)
  (if (null? L) L
      (my-append (reverse (cdr L)) (list (car L))))) 
 ; И нейн итеративен вариант
(define (reverse-iter L)
  (define (rev x y)
    (if (null? x) y
        (rev (cdr x) (cons (car x) y)))) 
  (rev L null))
 (define tests3
  (test-suite "Reverse tests"
      (check-equal? (reverse-iter '(1 2 3)) (reverse '(1 2 3)))
      (check-equal? (reverse '()) '())
      (check-equal? (reverse '(1)) '(1))
      (check-equal? (reverse '(1 5)) '(5 1))
  )
)
 (run-tests tests3 'verbose)