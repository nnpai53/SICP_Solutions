;; pre-requisite functions used in the functions below
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; actual functions defined in 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; some examples to test it on
;; tree in figure 2.17
(define example-tree1 (make-tree 7 
                                (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                                (make-tree 9 '() (make-tree 11 '() '()))))
;; trees in figure 2.16
(define example-tree2 (make-tree 1 '() 
                                 (make-tree 3 '()
                                            (make-tree 5 '()
                                                       (make-tree 7 '()
                                                                  (make-tree 9 '()
                                                                             (make-tree 11 '() '())))))))
(define example-tree3 (make-tree 3 
                                 (make-tree 1 '() '())
                                 (make-tree 7 
                                            (make-tree 5 '() '())
                                            (make-tree 9 '() (make-tree 11 '() '())))))
(define example-tree4 (make-tree 5 
                                 (make-tree 3 
                                            (make-tree 1 '() '())
                                            '())
                                 (make-tree 9
                                            (make-tree 7 '() '())
                                            (make-tree 11 '() '()))))
                                 
                                            
                                                                  
(tree->list-1 example-tree1)
(tree->list-2 example-tree1)

(tree->list-1 example-tree2)
(tree->list-2 example-tree2)

(tree->list-1 example-tree3)
(tree->list-2 example-tree3)

(tree->list-1 example-tree4)
(tree->list-2 example-tree4)

;; Order of growth is O(n logn) for tree->list-1 since it uses append and
;; append has O(n) time complexity
;; whereas order of growth is O(n) for tree-list-2 since it just uses cons 
;; and first flattens the right branch and then flattens the left branch