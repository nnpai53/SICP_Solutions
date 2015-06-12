(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((iseven exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (try-it a n)
    (= (expmod a n n) a))

(define (carmichael-proof n)
  (fermat-test n 1))

(define (square x)
  (* x x))

(define (fermat-test n number)
  (cond ((= number n) #t)
        ((try-it number n) (fermat-test n (+ 1 number)))
        (else #f)))

(define (iseven n)
  (divides 2 n))

(define (divides divisor number)
  (= (remainder number divisor) 0))