;;The encode function designed in exercise 2.68 is given below
#|
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
|#
;; The most expensive step here is to search the symbol in the list at every node
;; in order to check if it exists and if it does continue in the subtree.
;; The cost for each such search in an unordered list is O(n).
;; For the special case as mentioned in 2.71, time to encode the most frequent symbol is O(1)
;; since it is easily found. 
;; For the least frequent symbol we need to perform roughly 
;; 1 + 2 + 3 + ...+ n i.e. ~ O(n^2) operations.