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

(define (denominator-e x)
  (if (not (= (remainder x 3) 2))
      1
      (fast-expt 2 (+ (/ (- x 2) 3) 1))))

(define (fast-expt base exp)
  (cond ((= exp 0) 1)
        ((iseven exp) (square (fast-expt base (/ exp 2))))
        (else (* base (fast-expt base (- exp 1))))))

(define (iseven number)
  (= (remainder number 2) 0))
                 
(define (square x)
  (* x x))

(define (euler-constant k)
  (+ (cont-frac-iter (lambda (i) 1.0) denominator-e k)
     2))