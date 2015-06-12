(define (cont-frac n d k)
  (define (cont-frac-rec x)
    (if (> x k) 
        0
        (/ (n x) (+ (d x) (cont-frac-rec (+ x 1))))))
  (cont-frac-rec 1))

(define (cont-frac-iter n d k)
  (define (iter-func x result)
    (if (< x 1)
        result
        (iter-func (- x 1) (/ (n x) (+ (d x) result)))))
  (iter-func k 0))

(define (tan-cf x k)
  (define (numerator-tan n)
  (if (= n 1)
      x
      (- 0 (square x))))
  (cont-frac-iter numerator-tan denominator-tan k))

(define (denominator-tan n)
  (- (* 2 n) 1))

(define (square number)
  (* number number))