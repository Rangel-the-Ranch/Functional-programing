#lang racket

(define (f x y)
  (* x y))
(define (g x y)
  (+ x y))
;( (f 1) . (g 2) 5)





(define (woodcutters xs)
  ;xs
  (define (evaluatePath goLeft? value remXS lastOcup)
    (cond [(empty? remXS) value]
          [goLeft? (define curr (car remXS))
                     (if (> (- (car curr) (cdr curr)) lastOcup)
                         (max (evaluatePath #t (+ value 1) (cdr remXS) (car curr)) (evaluatePath #f (+ value 1) (cdr remXS) (car curr)))
                         (max (evaluatePath #t value (cdr remXS) lastOcup)         (evaluatePath #f value (cdr remXS) lastOcup)))]
          [(not goLeft?) (define curr (car remXS))
                           (define contestPos (+ (car curr) (cdr curr)))
                     (if (and (> (car curr) lastOcup) (or (empty? (cdr remXS))  (< contestPos (car (cadr remXS)))))
                         (max (evaluatePath #t (+ value 1) (cdr remXS) contestPos) (evaluatePath #f (+ value 1) (cdr remXS) contestPos))
                         (max (evaluatePath #t value (cdr remXS) (car curr))         (evaluatePath #f value (cdr remXS) (car curr))))]))
  
  (max (evaluatePath #t 0 xs -99999999999999999) (evaluatePath #f 0 xs -99999999999999999))
)

;(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (19 . 1)))
;(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1))) 
;(woodcutters '((10 . 4) (15 . 1) (19 . 3) (20 . 1))) 