(define (double x)
  (* 2 x))

(define (half x)
  (/ x 2))

(define (multiter a b)
  (multiter-helper 0 a b))

(define (multiter-helper c a b)
  (cond ((= b 0) c)
        ((iseven b) (multiter-helper c (double a) (half b)))
        (else (multiter-helper (+ c a) a (- b 1)))))

(define (iseven n)
  (= (remainder n 2) 0))