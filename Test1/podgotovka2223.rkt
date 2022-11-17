#lang racket

(define (sum-numbers a b)
  (define (deserting-number? number last)
    (cond [(< number 10) (>= number last)]
          [(< (remainder number 10) last) #f]
          
          [else (deserting-number? (quotient number 10) (remainder number 10)) ]))
  (define (helper c sum)
    (cond [(> c b) sum]
          [(deserting-number? c 0) (helper (+ c 1) (+ sum c))]
          [else (helper (+ c 1) sum)]))
  (helper a 0))


;(sum-numbers 219 225)

(define (num-bigger-elements lst)
  (define (bigger-count n xs count)
    (cond [(equal? xs null) count]
          [(> (car xs) n) (bigger-count n (cdr xs) (+ count 1))]
          [else (bigger-count n (cdr xs) count)]))
  
  (define (helper xs)
    (cond [(equal? xs null) '()]
          [else (cons (list (car xs) (bigger-count (car xs) lst 0)) (helper (rest xs)))]))
  
  (helper lst))

;(num-bigger-elements '(5 6 3 4)); → '((5 1) (6 0) (3 3) (4 2))
;(num-bigger-elements '(1 1 1)) ; '((1 0) (1 0) (1 0))


(define (switchsum f g n)
  (define (result x)
    (define (helper count sum prev)
      (cond [(= count n) sum]
            [(even? count) (helper (+ count 1) (+ sum (f prev)) (f prev))]
            [else          (helper (+ count 1) (+ sum (g prev)) (g prev))]))
    (helper 0 0 x))
  result)



;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2) ;3
;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 2) 2) ;9
;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 3) 2) ;16
;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 4) 2) ;30

;(define (repeater str)
 ; ( λ (count glue)
  ;   (define (helper c)
   ;    (cond [(= c count) " "]
    ;         [else (string-append str glue (helper (+ 1 c)))]))))

(define (repeater str)
  (λ(count glue)
    (if (= 0 couut)
        " "
        (λ (- count 1) glue))))

((repeater "I love Racket") 3 " ")

















