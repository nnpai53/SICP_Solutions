(define (reverse-new list1)
  (cond ((null? list1) '())
        ((not (pair? list1)) list1)
        (else (append (reverse-new (cdr list1)) (list (car list1)) ))))

(define (deep-reverse some-list)
  (cond ((null? some-list) '())
        ((not (pair? some-list)) some-list)
        (else (append (deep-reverse (cdr some-list)) (list (deep-reverse (car some-list)))))))
 
(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
(deep-reverse (list 1 2 (list 4 5 6)))