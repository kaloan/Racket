#lang racket
(require rackunit)
(require rackunit/text-ui)
; Искаме в дадена маррица да останат само елементи, които удовлетворяват даден предикат
(define (filter-matrix pred matrix)
  (if (null? matrix) '()
      (cons (filter pred (car matrix)) (filter-matrix pred (cdr matrix)))))
 (define tests
  (test-suite "Filter matrix tests"
     (test-case "" (check-equal? (filter-matrix even? '((1 2 3) (4 5 6) (7 8 9))) '((2) (4 6) (8))))
  )
)
 (run-tests tests 'verbose)



; Искаме да приложим функция на всеки елемент на дадена матрица
(define (map-matrix func matrix)
  (if (null? matrix) '()
      (cons (map func (car matrix)) (map-matrix func (cdr matrix)))))
 (define tests2
  (test-suite "Mat matrix tests"
     (test-case "" (check-equal? (map-matrix (lambda (x) (* x x)) '((1 2 3) (4 5 6) (7 8 9)))
				'((1 4 9) (16 25 36) (49 64 81))))
  )
)
 (run-tests tests2 'verbose)


; Искаме да пропуснем ред на матрица на даден индеь
(define (skip matrix row)
  (cond( (null? matrix) null)
      ((= row 0) (skip (cdr matrix) (- row 1)))
      (else (cons (car matrix) (skip (cdr matrix) (- row 1)))))) 
 (define tests3
  (test-suite "skip row tests"
     (test-case "" (check-equal? (skip '((1 2 3) (4 5 6) (7 8 9)) 1) '((1 2 3) (7 8 9))))
  )
)
 (run-tests tests3 'verbose)