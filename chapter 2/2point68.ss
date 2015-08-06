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

;; actual exercise 2.68
;; the encode function defined

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; encode-symbol function to be defined
(define (encode-symbol some-symbol tree)
  (define (encode-1 some-symbol current-branch code-till-now)
    (cond ((leaf? current-branch) 
           (if (equal? (symbol-leaf current-branch) some-symbol)
               code-till-now
               '()))
          ((not (exists-symbol? some-symbol (symbols current-branch))) '())
          (else (merge-codes (encode-1 some-symbol (left-branch current-branch) (append code-till-now (list 0)))
                             (encode-1 some-symbol (right-branch current-branch) (append code-till-now (list 1)))))))
  (encode-1 some-symbol tree '()))

(define (merge-codes code1 code2)
  (cond ((and (null? code1) (null? code2))
         (error "bad symbol: MERGE-CODES" (append code1 code2)))
        ((null? code1) code2)
        ((null? code2) code1)
        (else (error "Something went wrong terribly" (append code1 code2)))))

(define (exists-symbol? some-symbol symbols-list)
  (cond ((null? symbols-list) #f)
        ((equal? some-symbol (car symbols-list)) #t)
        (else (exists-symbol? some-symbol (cdr symbols-list)))))

;; testing encode with result of 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define message-to-be-encoded '(a d a b b c a))

(encode message-to-be-encoded sample-tree)
;; returns (0 1 1 0 0 1 0 1 0 1 1 1 0) which is what the 
;; code in 2.67 was (yayyy!!!!)