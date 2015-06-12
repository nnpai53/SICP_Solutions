(define (fringe some-tree)
  (cond ((null? some-tree) '())
        ((not (pair? some-tree)) (list some-tree))
        (else (append (fringe (car some-tree)) (fringe (cdr some-tree))))))

(define x (list (list 1 2) (list 3 4)))

(define y (list 1 2 (list 3 4 5 (list 6 7 8)) 9))

(fringe x)
(fringe (list x x))
(fringe y)