(define (exptrec b n)
  (if (= n 0)
      1
      (* b (exptrec b (- n 1)))))
(define (square x)
  (* x x))

(define (exptiter b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* product b))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((iseven n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (iseven n)
  (= (remainder n 2) 0))