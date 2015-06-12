(#%require racket/base)
(#%require profile)

(define (iseven n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (check-nontrivial-sqrt x value m) 
       (if (and (= value 1) 
                (not (= x 1)) 
                (not (= x (- m 1)))) 
           0 
           value))

(define (square-root-unity x m)
  (check-nontrivial-sqrt x (remainder (square x) m) m)) 
     

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((iseven exp)
        (square-root-unity (expmod base (/ exp 2) m) m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (check-it x)
       (and (not (= x 0)) (= x 1))) 

(define (try-it a n)
    (check-it (expmod a (- n 1) n)))

(define (miller-rabin n)
  (try-it (+ 1 (random (- n 1))) n))

 (define (fast-prime n times) 
   (cond ((= times 0) #t) 
         ((miller-rabin n) (fast-prime n (- times 1))) 
         (else #f)))
 
 (define (isprime n)
   (cond ((iseven n) (fast-prime n (/ n 2)))
         (else (fast-prime n (/ (+ 1 n) 2)))))