(#%require racket/base)
(#%require profile)


(define (next number)
  (if (= number 2)
      3
      (+ 2 number)
      ))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square x)
  (* x x))

(define (divides divisor number)
  (= (remainder number divisor) 0))

(define (isprime n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((isprime n)
      (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (iseven start)
       (search-for-primes (+ start 1) end)
       (cond ((< start end) (timed-prime-test start)
                            (search-for-primes (+ start 2) end)))))
(define (iseven n)
   (= (remainder n 2) 0))