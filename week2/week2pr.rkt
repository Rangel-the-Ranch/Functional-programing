#lang racket

(define (mygcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

;(mygcd -2 10)

(define(maxdevisor n)
  (define (helper a b)
    (if (= 0 (remainder a b))
        b
        (helper a (- b 1))))
    (helper n (- n 1)))

;(maxdevisor 21)

(define (sum-odds a b)
  (define (helper x y sum)
    (if (> x y)
        sum
        (helper (+ x 1) y (+ sum (* x (remainder x 2))))))
  (helper a b 0))

;(sum-odds 5 10)

(define (prime? n)
  (define (helper a)   
    (if (= 0 (remainder n a))
        (if ( = 1 a)
            #t
            #f)
        (helper (- a 1))))
  (helper (- n 1)))

;(prime? 19)

(define (count-deviders n)
  (define (helper num count)
    (cond[ ( = 0 num)             count]
         [(= 0 (remainder n num))  (helper (- num 1) (+ count 1)) ]
         [else (helper (- num 1) 0)]))
  (helper (- n 1) 0))

(count-deviders 7)
    
        


