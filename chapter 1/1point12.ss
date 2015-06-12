(define (pasc n k)
  (cond ((< n k) 0)
        ((= k 1) n)
        ((= n k) 1)
        ((= k 0) 1)
        (else (+ (pasc (- n 1) k) (pasc (- n 1) (- k 1)))) ) )

