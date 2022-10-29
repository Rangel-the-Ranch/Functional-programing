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
  
    (define (is-k-special? n k);Проверява дали число е k-специално
    (if (and (= 0 (remainder n k)) (= 0 (remainder (sum-of-digits n) k)))
        #t
        #f))
  
  (define (helper x count)
    (if (> x b)
        count
        (if (is-k-special? x k)
            (helper (+ x 1) (+ count 1))
            (helper (+ x 1) count))))

  
  (helper a 0))

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

(define (pow x n)
  (define (helper result steps)
    (if (>= steps n)
        result
        (helper (* result x) (+ steps 1))))
  (helper 1 0))


(define (max-rot n)
  ;Дефинираме константа, предотвартява извикването на (size-of-number n) няколко пъти
  (define size-of-N (size-of-number n))
  
  ;Функция която завърта наляво число с "keep" на брой цифри непроменени от ляво 
  (define (left-rotate n keep)
    ;Помощна функция връщаща непроменение цифри
    (define (kept-digits)
      (quotient (* n (pow 10 keep)) (pow 10 size-of-N)))
    ;помощна функция връщаща цифрата завъртяна към последна позиция на новото N
    (define (moving-back-digit)
      (remainder  (quotient (* n 10) (pow 10 (- size-of-N keep) )) 10))
    ;Помощна функция връщаща цифрите които се изместват с една позция на ляво
    (define (moving-foward-digits)
      (remainder n (pow 10 ( - (- size-of-N keep) 1))))
    ( + (* (kept-digits) (pow 10 (- size-of-N keep))) (* (moving-foward-digits) 10) (moving-back-digit)))

  ;helper - прави последователни завъртания на ляво и запазва най-голямото по изискване на задачата
  (define (helper biggest current count)
    (cond [(> count size-of-N) biggest]
          [(> current biggest) (helper current (left-rotate current (+ 0 count)) (+ 1 count))]
          [(<= current biggest)(helper biggest (left-rotate current (+ 0 count))(+ 1 count))]))
  
   (helper 0 n 0))

;(max-rot 56789) ➝ 68957
;(max-rot 12490) ➝ 29140
;(max-rot 38458215) ➝ 85821534
;(max-rot 195881031) ➝ 988103115
;(max-rot 896219342) ➝ 962193428
;(max-rot 69418307) ➝ 94183076
;(max-rot 257117280) ➝ 571172802











    
    
        
