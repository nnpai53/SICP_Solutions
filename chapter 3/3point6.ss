(#%require racket/base)
(#%require racket/list)

;; A very elegant approach taken from here
;; https://wqzhang.wordpress.com/2009/07/11/sicp-exercise-3-6/
(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))
(define random-init 137)
(define rand 
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset) 
             (lambda (new-x)
               (set! x new-x)))
            (else (error "unknown request"))))
    dispatch))

;; used for testing purposes
(rand 'generate)
(rand 'generate) 
((rand 'reset) 3062)
(rand 'generate)

