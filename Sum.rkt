#lang racket
(define (sum a min max expr)
  (if (> min max)
      0
      (+ (let ([a min]) expr) (sum a (+ min 1) max expr))))

(define (sum2 i max expr)
  (for ([__a__ (in-range max)])
    (let ([i __a__]) expr)))

(let ([m 5] [acc 0])
(for ([i (in-range 4)])
  (set! acc (+ acc (+ i i))))
acc
)

(let ([m (+ 1 5)])
(for/sum ([i (in-range m)])
  (+ i i)))
