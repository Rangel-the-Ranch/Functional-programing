#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (size-of-number n)
  (define (helper a count)
    (if (= 0 a)
      count
      (helper (quotient a 10) (+ count 1))))
  (helper n 0))


(define (pow x n)
  (define (helper result steps)
    (if (= steps n)
        result
        (helper (* result x) (+ steps 1))))
  (helper 1 0))



(define (substr? a b)
  (define (helper subB size)
    (cond  [(< subB a) #f]
           [(= a (remainder subB size)) #t]
           [else (helper (quotient subB 10) size)]))
  
  (helper b (pow 10 (size-of-number a))))



(substr? 123 888123888)

    