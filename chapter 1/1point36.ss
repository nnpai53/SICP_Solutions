(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (isclose-enough v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (isclose-enough guess next)
          next
          (try next))))
  (try first-guess))

(define (x-raisedto-x x)
  (fixed-point (lambda (y) (/ (log 1000) (log y))) x))

(define (x-raisedto-x-new x)
  (fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) x))

(define (average a b)
  (/ (+ a b) 2.0))