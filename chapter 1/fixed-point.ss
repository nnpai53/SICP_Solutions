(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (isclose-enough v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (isclose-enough guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt-new x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average x y)
  (/ (+ x y) 2.0))