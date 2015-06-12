(define dx 0.00001)

(define (average a b c)
  (/ (+ a b c) 3.0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))))

(define (smooth-n f n)
  (repeated (smooth f) n))

(define (square x)
  (* x x))