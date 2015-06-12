(define (frec n)
  (cond ((< n 3) n)
        (else  (+ (+ (frec (- n 1)) (* 2 (frec (- n 2)))) (* 3 (frec (- n 3))) ) ) ) )

(define (fiter n)
  (fiter-helper n 2 1 0))

(define (fiter-helper n a b c)
  (cond ((= n 0) c)
        ((= n 1) b)
        ((= n 2) a)
        (else (fiter-helper (- n 1) (cal-value a b c) a b)) ) )

(define (cal-value v1 v2 v3)
  (+ (+ v1 (* 2 v2)) (* 3 v3)))


