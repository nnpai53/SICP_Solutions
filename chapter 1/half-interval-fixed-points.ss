(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (isclose-enough neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((ispositive test-value)
                 (search f neg-point mid-point))
                ((isnegative test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(define (isclose-enough x y)
  (< (abs (- x y)) 0.001))

(define (average a b)
  (/ (+ a b) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (isnegative a-value) (ispositive b-value))
           (search f a b))
          ((and (isnegative b-value) (ispositive a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (isnegative x)
  (< x 0))

(define (ispositive x)
  (> x 0))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(half-interval-method sin 2.0 4.0)