(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list-modified items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))
                    ))))
  (iter items '()))

(define (square-list-correct items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))
                    ))))
  (iter items '()))

(define (square x)
  (* x x))

(square-list (list 1 2 3 4))

(square-list-modified (list 1 2 3 4))

(square-list-correct (list 1 2 3 4))