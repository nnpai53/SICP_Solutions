(define (div-by-three num)
  (if (= (remainder num 3) 0)
      (div-by-three (/ num 3))
      num))

(define (div-by-two num)
  (if (= (remainder num 2) 0)
      (div-by-two (/ num 2))
      num))

(define (cons-alt x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-alt z)
  (round (/ (log (div-by-three z)) (log 2))))

(define (cdr-alt z)
  (round (/ (log (div-by-two z)) (log 3))))