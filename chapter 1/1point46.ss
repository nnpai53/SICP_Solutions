(define (iterative-improve isgood-enough improve-guess)
  (lambda (x)
    (if (isgood-enough x)
        x
        ((iterative-improve isgood-enough improve-guess) (improve-guess x)))))

(define tolerance 0.00001)

(define (average x y)
   (/ (+ x y) 2.0))

(define (square x)
  (* x x))

(define (sqrt-new x)
  (define (isgood-enough value)
    (< (abs (- (square value) x)) tolerance))
  (define (improve value)
    (average value (/ x value)))
   ((iterative-improve isgood-enough improve) 1.0))

(define (fixed-point f guess)
  (define (isclose-enough v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define isgood-enough 
    (lambda (x) (isclose-enough x (f x))))
  ((iterative-improve isgood-enough f) guess))