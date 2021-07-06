#lang racket
(require rackunit)
(require rackunit/text-ui)
; Искаме да вземем н-тата колона на дадена матрица
(define (nth-col L n)
  (cond ((null? L) '())
        ((= n -1) '())
        (else (cons (car (reverse (car L)))(nth-col (cdr L) (- n 1))))))
 (define tests1
  (test-suite "nth col tests"
     (test-case "" (check-equal? (nth-col '((1 2 3) (4 5 6) (7 8 9)) 2) '(3 6 9)))
  )
)
 (run-tests tests1 'verbose)




; Искаме да транспонираме дадена матрица
;(define (transpose L)(apply map list L))
(define (transpose L)
  (if (null? (car L))
      `()
      (cons (map (lambda(x)(car x)) L) (transpose (map (lambda(x)(cdr x)) L)))
  )
)
 (define tests2
  (test-suite "transpose tests"
     (test-case "" (check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9))))
  )
)
 (run-tests tests2 'verbose)




; Искаме да вземем главния диагонал на матрица
(define (diagonal l)
  (define (helper L i)
    (cond
      ((null? L) '())
      ((= i 0) (car L))
      (else(helper (cdr L) (- i 1)))))
  (define (diag Li j)
    (if (null? Li) '()
        (cons (helper (car Li) j) (diag (cdr Li) (+ j 1)))))
  (diag l 0))
  
 (define tests3
  (test-suite "Diagonal tests"
     (test-case "" (check-equal? (diagonal '()) '()))
    (test-case "" (check-equal? (diagonal '((1 2 3) (4 5 6) (7 8 9))) '(1 5 9)))
  )
)
 (run-tests tests3 'verbose)