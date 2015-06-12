(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqr-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqr-iter (improve guess x)
                x)))
 
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqr-iter (improve guess x) x)))

(define (new-sqrt x) (new-sqrt-iter 1.0 x))


(define (sqroot x)
  (sqr-iter 1.0 x))
    