(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double x )
  (* 2 x))

(define (half x)
  (/ x 2))

(define (multrec a b)
  (cond ((= b 0) 0)
        ((iseven b) (double (multrec a (half b))))
        (else (+ a (multrec a (- b 1))))))
              
(define (iseven n)
  (= (remainder n 2) 0))