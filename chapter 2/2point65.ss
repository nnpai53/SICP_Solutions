;; pre-requisite functions used in the functions below
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(define (union-set tree1 tree2)
  (let ((list1 (tree->list-2 tree1))
        (list2 (tree->list-2 tree2)))
    (let ((unified-list (union-of-lists list1 list2)))
      (list-tree unified-list))))

(define (union-of-lists list1 list2)
  (cond ((null? list2) list1)
        ((null? list1) list2)
        (else (let ((x1 (car list1)) (x2 (car list2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr list1) (cdr list2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr list1) list2)))
                      ((> x1 x2)
                       (cons x2 (union-set list1 (cdr list2)))))))))
;; example testing
(define example-tree1 (make-tree 5 
                                 (make-tree 3 
                                            (make-tree 1 '() '())
                                            '())
                                 (make-tree 9
                                            (make-tree 7 '() '())
                                            (make-tree 11 '() '()))))

(define example-tree2 (make-tree 4 
                                 (make-tree 2 
                                            (make-tree 1 '() '())
                                            '())
                                 (make-tree 9
                                            (make-tree 8 '() '())
                                            (make-tree 11 '() '()))))

(union-set example-tree1 example-tree2)