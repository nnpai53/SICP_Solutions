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

(define (golden-ratio x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) x))

(golden-ratio 1.0)