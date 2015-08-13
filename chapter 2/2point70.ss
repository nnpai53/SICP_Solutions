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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
   (if (= (length leaf-set) 1) 
       (car leaf-set) 
       (let ((first (car leaf-set)) 
             (second (cadr leaf-set)) 
             (rest (cddr leaf-set))) 
         (successive-merge (adjoin-set (make-code-tree first second) 
                                       rest)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

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

;; actual exercise 2.70
;; generating a huffman tree for the song symbols
(define song-symbols
  (list '(NA 16) '(YIP 9) '(SHA 3) '(A 2) '(GET 2) '(JOB 2) '(WAH 1) '(BOOM 1)))

(define huffman-tree-song (generate-huffman-tree song-symbols))

(define song-to-be-encoded
  '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na 
        Wah yip yip yip yip yip yip yip yip yip Sha boom))

;; We need 84 bits to encode the song using the huffman tree coding
;; If we had used fixed length coding we would have needed 36*3 = 108 bits.
(length (encode song-to-be-encoded huffman-tree-song))