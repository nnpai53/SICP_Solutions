(define (square x)
  (* x x))

(define (square-tree some-tree)
  (cond ((null? some-tree) '())
        ((not (pair? some-tree)) (square some-tree))
        (else (cons (square-tree (car some-tree))
              (square-tree (cdr some-tree))))))

(define (square-tree-map some-tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       some-tree))

(square-tree (list 1 (list 2 (list 3 4) 5)
                   (list 6 7)))

(square-tree-map (list 1 (list 2 (list 3 4) 5)
                   (list 6 7)))