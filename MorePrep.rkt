#lang racket
(require rackunit)
(require rackunit/text-ui)
(require rnrs/mutable-pairs-6)
(require srfi/41)

(define (cont_fraction x n)
  (if(> x n) 1
  (+ 1 (/ (expt 2 x) (cont_fraction (+ x 1) n))))
)
(define (cont n) (/ 1 (cont_fraction 1 n)))

;(begin (define L '(1 2 3 4 5 6 7 8))
;       (set-car! L (+ 3 (car L)))
;       (set-car! (cddr L) (cadr (cdddr L)))
;       (set-car! (cddddr L) (cadddr L))
;       L)

(define (f x y z) (x y z))

(define (deep-found? s L)
  (cond ((null? L) #f)
        ((not (list? (car L))) (if(eqv? s (car L)) #t
                                  (deep-found? s (cdr L))))
        (else (or (deep-found? s (car L)) (deep-found? s (cdr L))))))

(define (flatten L)
  (cond ((null? L) '())
        ((list? (car L)) (append (flatten (car L)) (flatten (cdr L))))
        (else (cons (car L) (flatten (cdr L))))))


(define (unzip L)
  (define (unzipHelper l l1 l2)
  (if (null? l) (cons l1 l2)
      (begin (cons l1 (caar L)) (cons l2 (cdar L)) (unzipHelper (cdr l) l1 l2))))
  (unzipHelper L '() '()))

(define (tasker m func gunc)
  (define (taskerHelper matrix i f g)
    (cond ((null? matrix) '())
          ((even? i) (cons (map f (car matrix)) (taskerHelper (cdr matrix) (+ i 1) f g)))
          ((odd? i) (cons (map g (car matrix)) (taskerHelper (cdr matrix) (+ i 1) f g)))
          ))
  (taskerHelper m 0 func gunc))
