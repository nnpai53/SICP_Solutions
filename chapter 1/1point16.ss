(define (square x)
  (* x x))

(define (iter-fast-expt b n)
  (iter-fast-helper 1 b n))

(define (iter-fast-helper a b n)
  (cond ((= n 0) a)
        ((iseven n) (iter-fast-helper a (square b) (/ n 2)))
        (else (iter-fast-helper (* a  b) b (- n 1)))) )
  
(define (iseven n)
  (= (remainder n 2) 0))