(define (cube x)
  (* x x x))

(define (iseven x)
  (= (remainder x 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
  
(define (simpsons f a b n)
  (let ((h (/ (- b a) n)))
  (define (simpson-term x)
    (cond ((= x a) (f a))
          ((= x b) (f b))
          ((iseven (/ (- x a) h)) (* 2 (f x)))
          (else (* 4 (f x)))))
  (define (simpson-next x)
    (+ x h))
  (* (/ h 3) (sum simpson-term a simpson-next b))))
