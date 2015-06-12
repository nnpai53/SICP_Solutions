(define (last-pair list1)
  (cond ((null? list1) (display "error: empty list not allowed"))
        ((null? (cdr list1)) list1)
        (else (last-pair (cdr list1)))))

(define test-list (list 23 72 149 34))

(last-pair test-list)