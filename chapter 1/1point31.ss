(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (identity x)
  x)

(define (square x)
  (* x x))

(define (plus-two x)
  (+ x 2))

(define (factorial n)
  (product identity 1 inc n))

(define (iter-product term a next b)
  (define (iter-result x result)
    (if (> x b)
        result
        (iter-result (next x) (* (term x) result))))
  (iter-result a 1))

(define (pi-product b)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2)))) 
  (* (product pi-term 1 inc b) 4.0))

    
      