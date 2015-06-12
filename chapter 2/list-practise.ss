(define one-through-four (list 1 2 3 4))

(car one-through-four)

(cdr one-through-four)

(cadr one-through-four)

(car (cdr one-through-four))

(caddr one-through-four)

(cons 10 one-through-four)

(cons 5 one-through-four)

(define (list-ref-new items n)
  (if (= n 0)
      (car items)
      (list-ref-new (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref-new squares 3)

(define (length-new items)
  (if (null? items)
      0
      (+ 1 (length-new (cdr items)))))
(define odds (list 1 3 5 7))

(length-new odds)

(define (length-iter items)
  (define (iter-length some-list count)
    (if (null? some-list)
        count
        (iter-length (cdr some-list) (+ 1 count))))
  (iter-length items 0))

(length-iter odds)

(append squares odds)

(append odds squares)

(define (append-new list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append-new squares odds)

(append-new odds squares)

    