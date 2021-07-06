#lang racket
(require rackunit)
(require rackunit/text-ui)

;Задача 1 и тестове 1

(define (find-prime x)
  (define (divideList? t List)
    (if (null? List) #f
        (if (integer? (/ t (car List))) #t
            (divideList? t (cdr List)))))
  (define (iter-find k i n List)
    (if (= i n) k
        (if (not(divideList? k List)) (iter-find k (+ i 1) n (cons k List))
        (iter-find (+ k 1) i n List))))
  (iter-find 2 1 x (cons 2 '())))


(define tests1
  (test-suite "Reduce tests"
    
    (test-case "" (check-equal? (find-prime 1) 2))
    (test-case "" (check-equal? (find-prime 3) 5))
    (test-case "" (check-equal? (find-prime 15) 47))
    (test-case "" (check-equal? (find-prime 120) 659))
  )
)
 (run-tests tests1 'verbose)


; Задача 2 и тестове 2

(define (choose indexList matrix)
  (define (choose-i index nowList i)
    (cond 
      ((= i index) (car nowList))
      (else (choose-i index (cdr nowList) (+ i 1)))))
  (define (finalChoose iL mat j)
    (if(null? iL) '()
       (cons (choose-i j (choose-i (car iL) mat 0) 0) (finalChoose (cdr iL) mat (+ j 1)))))
  (finalChoose indexList matrix 0))


(define choices1 '((0 1 2 3) (10 11 12 13) (20 21 22 23) (30 31 32 33)))
(define choices2 '((1 2 3 4 5 6) (7 8 9 10 11 12) (13 14 15 16 17 18)(19 20 21 22 23 24)(25 26 27 28 29 30) (31 32 33 34 35 36)))
(define choices3 '((5 7 9) (3 0 14) (17 20 33)))
(define choices4 '((15 5 1 2) (10 11 12 13) (256 71 8 3) (24 78 82 91)))
(define tests2
  (test-suite "Reduce tests"
    
    (test-case "" (check-equal? (choose '(2 3 1 0) choices1) '(20 31 12 3)))
    (test-case "" (check-equal? (choose '(0 1 2 3 4 5) choices2) '(1 8 15 22 29 36)))
    (test-case "" (check-equal? (choose '(2 0 1) choices3) '(17 7 14)))
    (test-case "" (check-equal? (choose '(2 2 2 2) choices4) '(256 71 8 3)))
  )
)
 (run-tests tests2 'verbose)


;Задача 3 и тестове 3

(define (sortList L)
  (define (findMin List min)
       (cond ((null? List) min)
             ((< (car List) min) (findMin (cdr List) (car List)))
             (else (findMin (cdr List) min))))
  (define (ListDel List toDel notDeled)
    (cond ((null? List) '())
          ((and notDeled (= (car List) toDel)) (ListDel (cdr List) toDel #f))
          (else (cons (car List) (ListDel (cdr List) toDel notDeled)))))
  (define (trueSort List)
    (if (null? List) '()
        (cons (findMin List (car List)) (trueSort (ListDel List (findMin List (car List)) #t)))))
  (trueSort L)
)


(define tobesorted1 '(5 15 8 7 23 9 20))
(define tobesorted2 '(10 9 8 7 6 5 4 3 2 1 -20 -13 -2))
(define tobesorted3 '(2746 62 129 82 923 81 -9 67 -12 -5 27 100))
(define tobesorted4 '(1))
(define tests3
  (test-suite "Reduce tests"
    
    (test-case "" (check-equal? (sortList tobesorted1) '(5 7 8 9 15 20 23)))
    (test-case "" (check-equal? (sortList tobesorted2) '(-20 -13 -2 1 2 3 4 5 6 7 8 9 10)))
    (test-case "" (check-equal? (sortList tobesorted3) '(-12 -9 -5 27 62 67 81 82 100 129 923 2746)))
    (test-case "" (check-equal? (sortList tobesorted4) '(1)))
  )
)
 (run-tests tests3 'verbose)