(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-new x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-new-two x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

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

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (average x y)
  (/ (+ x y) 2.0))

(define dx 0.00001)