(#%require racket/base)
(#%require racket/list)

(define (make-account balance)
    (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                   balance)
                "Insufficient Funds"))
    (define (deposit amount)
        (begin (set! balance (+ balance amount))
               balance))
    (define (dispatch m)
        (cond ((eq? m 'withdraw ) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
    dispatch)

;; creating a make-account object and testing its behaviour
(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

;; another call to make-account will produce a completely separate account object, which maintains its
;; own local balance .
(define acc2 (make-account 100))