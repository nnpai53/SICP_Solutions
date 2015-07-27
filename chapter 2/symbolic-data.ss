(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(cdr '(a b c))

(define (memq-one item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; used for testing
(memq-one 'apple '(pear banana prune))

(memq-one 'apple '(x (apple sauce) y apple pear))