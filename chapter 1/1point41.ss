(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ 1 x))

(define (square x)
  (* x x))