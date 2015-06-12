(define (reverse-new list1)
  (if (null? list1)
      list1
      (append (reverse-new (cdr list1)) (list (car list1)))))

(define (reverse-new-iter list1)
  (define (iter-new-reverse some-list result)
    (if (null? some-list)
        result
        (iter-new-reverse (cdr some-list) (append (list (car some-list)) result))))
  (iter-new-reverse list1 '()))

(define some-list (list 34 56 24 21 98 48 35))

(define some-list1 (list 1 4 9 16 25))

(reverse-new some-list)

(reverse-new some-list1)