(define (prime? n)
  (define (helper a)   
    (if (= 0 (remainder n (- a 1)))
        (if ( = 1 (- a 1))
            1
            0)
        (helper (- a 1))))
  (helper n))
(prime? 19)