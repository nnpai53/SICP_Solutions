(#%require (only racket/base random))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((iseven exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (iseven n)
  (= (remainder n 2) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-primes n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-primes n (- times 1)))
        (else #f)))