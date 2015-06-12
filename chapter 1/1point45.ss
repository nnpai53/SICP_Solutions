(define tolerance 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (total-damps n)
  (floor (/ (log n) (log 2))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (isclose-enough v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (if (isclose-enough guess (f guess))
        (f guess)
        (try (f guess))))
  (try first-guess))

(define (nth-root-of x n)
  (fixed-point-of-transform
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp (total-damps n)) 1.0))