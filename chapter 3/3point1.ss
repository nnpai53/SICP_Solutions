(define (make-accumulator initial-value)
    (define (add-value sum-value)
           (begin (set! initial-value (+ initial-value sum-value))
               initial-value))
    add-value)

;;used for testing
(define A (make-accumulator 5))
(A 10)
(A 10)
