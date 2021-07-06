#lang racket
(require rackunit)
(require rackunit/text-ui)



 ; Искаме да преброим колко пъти се среща даден елемент в даден списък
 ; След това искаме да преброим колко пъти се срещат всички елементи от списъка l2 (подредени
; по същия начин) в списъка l1

(define (count-occurences-el list el)
  (if (null? list) 0
      (if (= (car list) el)
          (+ 1 (count-occurences-el(cdr list) el))
          (count-occurences-el(cdr list) el))))
 (define occurences-el-tests
  (test-suite "element occurences tests"
     (test-case "Count occurences of element 1"
	       (check-equal? (count-occurences-el '(1 2 3 4 1 5 1 2 7) 2) 2))
    (test-case "Count occurences of element 7"
	       (check-equal? (count-occurences-el '(1 2 3 4 1 5 1 2 7) 7) 1))
    (test-case "Count occurences of element 1"
	       (check-equal? (count-occurences-el '(1 2 3 4 1 5 1 2 7) 1) 3))
  )
)
(define (count-occurences l1 l2)
  (define (check-next ll1 ll2)
    (if (null? ll1) 0
        (if (null? ll2) 1
            (if (= (car ll1) (car ll2)) (check-next (cdr ll1) (cdr ll2))
                0))))
  (if (null? l1) 0
      (if (null? l2) 1
          (if (= (car l1) (car l2)) (+ (check-next (cdr l1) (cdr l2)) (count-occurences (cdr l1) l2))
          (count-occurences (cdr l1) l2)))))
 (define occurences-tests
  (test-suite "Occurences tests"
     (test-case "Count occurences of (1 2) in l1"
	       (check-equal? (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 2)) 2))
    (test-case "Count occurences of (1 2 3) in l2"
	       (check-equal? (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 2 3)) 1))
    (test-case "Count occurences of (1 3) in l2"
	       (check-equal? (count-occurences '(1 2 3 4 1 5 1 2 7) '(1 3)) 0))
  )
)
 (run-tests occurences-el-tests 'verbose)
(run-tests occurences-tests 'verbose)




; Търсим функция, която връща списък от първите n елемента на даден такъв.
(define (take n L)
  (if (= n 0) null
  (cons (car L) (take (- n 1) (cdr L)))))
 (define tests1
  (test-suite "Take tests"
     (check-equal? (take 2 '(1 2 3)) '(1 2))
     (check-equal? (take 0 '(2 9 2)) '())
  )
)
 (run-tests tests1 'verbose)



; Искаме да пропуснем първите n елемента от даден списък
(define (drop L n)
  (if (= n 0) L
      (drop (cdr L)(- n 1))))
 (define tests2
  (test-suite "Drop tests"
     (test-case "" (check-equal? (drop '(1 2 3 4 5 6) 3) '(4 5 6)))
    (test-case "" (check-equal? (drop '(1 2 3 4 5 6) 0) '(1 2 3 4 5 6)))
    (test-case "" (check-equal? (drop '(1 2 3 4 5 6) 5) '(6)))
  )
)
 (run-tests tests2 'verbose)