#lang racket

;##################### ЗАДАЧА 1 ###########################################

;Помощна Функция която събира цифрите на естествено число
(define (sum-of-digits n)
  (define (helper curr sum)
  (if (< curr 10)
      (+ sum  (remainder curr 10))
      (helper (quotient curr 10) (+ sum (remainder curr 10)))))
   (helper n 0))

;Функция която намира броя на k-специалните числа в затворения интервал [a, b]
(define (count-specials k a b)
  
    (define (is-k-special? n k)
    (if (and (= 0 (remainder n k)) (= 0 (remainder (sum-of-digits n) k)))
        #t
        #f))
  
  (define (helper x count)
    (if (> x b)
        count
        (if (is-k-special? x k)
            (helper (+ x 1) (+ count 1))
            (helper (+ x 1) count))))

  
  (helper a 0));рекурсивно???????????????????, коментари

;(count-specials 3 3 9) 
;(count-specials 5 10 100)
;(count-specials 8 100 200)
;(count-specials 15 1000 2000) 
 
        
;##################### ЗАДАЧА 2 ###########################################

(define (size-of-number n)
  (define (helper a count)
    (if (= 0 a)
      count
      (helper (quotient a 10) (+ count 1))))
  (helper n 0))

;(define (left-rotate n keep)
  
  
  
    
    
        
