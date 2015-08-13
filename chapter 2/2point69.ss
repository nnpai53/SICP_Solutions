(#%require racket/base)
(#%require racket/list)

;; pre-requisite procedures used in the functions in the exercise

;; defining leaf nodes
(define (make-leaf symbol weight) 
  (list 'leaf symbol weight))

(define (leaf? object) 
  (eq? (car object) 'leaf))

(define (symbol-leaf x) 
  (cadr x))

(define (weight-leaf x) 
  (caddr x))

;; defining code-tree

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; defining the tree structure and selectors
(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;; sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; make-leaf-set function

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ;symbol
                               (cadr pair))  ;frequency
                    (make-leaf-set (cdr pairs))))))

;; actual exercise 2.69
;; the generate-huffman-tree function defined
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; successive-merge function to be defined
;; this was my solution which does not give the optimal solution and is also lengthier and messier
#|
(define (successive-merge initial-pair-leaves)
  (cond ((= 1 (length initial-pair-leaves)) (car initial-pair-leaves))
        ((= 2 (length initial-pair-leaves)) (make-code-tree (car initial-pair-leaves)
                                                            (cadr initial-pair-leaves)))
        (else (let ((sm-remaining (successive-merge (cddr initial-pair-leaves))))
                (if (and (> (weight sm-remaining) (weight (car initial-pair-leaves)))
                         (> (weight sm-remaining) (weight (cadr initial-pair-leaves))))
                    (make-code-tree (make-code-tree (car initial-pair-leaves)
                                                    (cadr initial-pair-leaves))
                                    sm-remaining)
                    (make-code-tree (make-code-tree sm-remaining
                                                    (cadr initial-pair-leaves))
                                    (car initial-pair-leaves)))))))
|#

;; this solution is taken from here 
;; http://community.schemewiki.org/?sicp-ex-2.69
;; by chris is awesome, more intuitive and elegant
;; btw I found the same solution here as well
;; https://wqzhang.wordpress.com/2009/06/29/sicp-exercise-2-69/
;; So this settles the problem :)

(define (successive-merge leaf-set)
   (if (= (length leaf-set) 1) 
       (car leaf-set) 
       (let ((first (car leaf-set)) 
             (second (cadr leaf-set)) 
             (rest (cddr leaf-set))) 
         (successive-merge (adjoin-set (make-code-tree first second) 
                                       rest))))) 
;; used for testing of the functions written
;(define pairs-for-trees
;  (list '(A 8) '(B 3) '(C 1) '(D 1) '(E 1) '(F 1) '(G 1) '(H 1)))

;(define pairs-for-trees
;  (list '(F 1) '(G 1) '(H 1)))
