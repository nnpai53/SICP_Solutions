(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (plus1 a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b)))) 

(define (plus a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))