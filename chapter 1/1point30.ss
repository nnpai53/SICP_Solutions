(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n)
  (+ n 1))

(define (sum-cubes-new a b)
  (sum cube a inc b))

(define (identity x)
  x)
(define (sum-integers-new a b)
  (sum identity a inc b))

(define (cube x)
  (* x x x))