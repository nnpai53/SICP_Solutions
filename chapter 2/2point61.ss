;; non-iterative version

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; iterative version
;; though I doubt this is O(n) because of the many append involved
#|
(define (adjoin-set x set)
  (define (adjoin-iter x left-part right-part)
    (cond ((null? right-part) (append left-part (list x)))
          ((= x (car right-part)) (append left-part right-part))
          ((< x (car right-part)) (append left-part (list x) right-part))
          (else (adjoin-iter x (append left-part (list (car right-part))) (cdr right-part)))))
  (adjoin-iter x '() set))
|#



              