(define (equal-one? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
        ((and (pair? list1) (pair? list2)) (and (equal-one? (car list1) (car list2))
                                                (equal-one? (cdr list1) (cdr list2))))
        (else #f)))

;used for testing
(equal-one? '(a b c) '(a b c))
         