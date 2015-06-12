(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))
 
(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(define (square x)
  (* x x))

(define (sqr-iter guess x old-guess)
  (if (good-enough? guess old-guess)
          guess
          (sqr-iter (improve guess x) x guess)))


(define (sqroot x)
  (sqr-iter 1.0 x 0.0))
    