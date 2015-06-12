(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch some-mobile)
  (car some-mobile))

(define (right-branch some-mobile)
  (car (cdr some-mobile)))

(define (branch-length some-branch)
  (car some-branch))

(define (branch-structure some-branch)
  (car (cdr some-branch)))
 
;; Alternate Definitions
;(define (make-mobile left right)
;  (cons left right))

;(define (make-branch length structure)
;  (cons length structure))

;(define (left-branch some-mobile)
;  (car some-mobile))

;(define (right-branch some-mobile)
;  (cdr some-mobile))

;(define (branch-length some-branch)
;  (car some-branch))

;(define (branch-structure some-branch)
;  (cdr some-branch))



(define (total-weight some-mobile)
  (define (total-branch-weight some-branch)
    (cond ((null? some-branch) 0)
          ((not (pair? some-branch)) some-branch)
          (else (total-weight (branch-structure some-branch)))))
  (cond ((null? some-mobile) 0)
        ((not (pair? some-mobile)) some-mobile)
        (else (+ (total-branch-weight (left-branch some-mobile)) (total-branch-weight (right-branch some-mobile))))))

(define (isbalanced some-mobile)
  (cond ((null? some-mobile) #t)
        ((not (pair? some-mobile)) #t)
        (else (let ((twl (total-weight (branch-structure (left-branch some-mobile))))
                    (twr (total-weight (branch-structure (right-branch some-mobile)))))
                (and (= (* (branch-length (left-branch some-mobile)) twl)
                        (* (branch-length (right-branch some-mobile)) twr))
                     (isbalanced (branch-structure (left-branch some-mobile)))
                     (isbalanced (branch-structure (right-branch some-mobile))))))))

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2)))

(total-weight level-1-mobile)
(total-weight level-2-mobile)
(total-weight level-3-mobile)

(isbalanced level-1-mobile)
(isbalanced level-2-mobile)
(isbalanced level-3-mobile)

(isbalanced (make-mobile (make-branch 10 1000) 
                         (make-branch 1 level-3-mobile)))