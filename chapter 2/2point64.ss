;; pre-requisite functions used in the functions below
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; Actual functions defined in 2.64
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

;; 2.64 part a
;; Working of partial-tree
;; The partial-tree takes in a list and splits it in the middle.
;; the middle element is selected to be the root of the tree
;; then it recursively does this to both the left half of the list and the right half
;; of the list till the input list is '() at which point the function returns a null list concatenated to
;; the list of remaining elements (which itself is null) so the end result returned is (() ()) to which the
;; root element is concatenated i.e. (root-elem () ()) (this is the case for the leaf nodes).
;; the return value of the partial-tree function is a tree of the left part of the list concatenated to a 
;; list consisting of the list of remaining elements, thus aptly-named as partial-tree. The function eventually
;; computes partially the tree of first the left half of the tree and then the right part of the tree.

;; The tree converted from the list (1 3 5 7 9 11)

(list-tree (list 1 3 5 7 9 11))

;; 2.64 part b
;; The order of growth of the partial-tree function can be found out by the following recurrence relation
;; T(n) = 2T(n/2) + c. (since there are 2 calls to partial-tree functions of size n/2, left half and right half)
;; and the time involved in a cons operation is a constant.
;; solving which we have the time complexity to be given as O(n).