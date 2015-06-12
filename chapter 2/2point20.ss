(define (same-parity x . z)
  (define (has-same-parity number)
    (= (remainder number 2) (remainder x 2)))
  (define (iter-same-parity result some-list)
    (cond ((null? some-list) result)
          ((has-same-parity (car some-list)) (iter-same-parity (append result (list (car some-list))) (cdr some-list)))
          (else (iter-same-parity result (cdr some-list)))))
  (iter-same-parity (list x) z))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)