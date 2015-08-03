(#%require racket/base)
(#%require racket/list)

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

;; 2.58 part a
;; Commented for part b

#|(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))
|#

;; 2.58 b

(define (non-num-members as)
  (filter (lambda (x) (not (number? x))) as))

(define (num-members as)
  (filter number? as))

(define (more-than-one-number? as)
  (let ((nums (num-members as)))
    (if (or (null? nums) (null? (cdr nums)))
        #f
        #t)))

(define (zero-is-the-only-number? as)
  (let ((nums (num-members as)))
    (if (null? nums)
        #f
        (and (= (car nums) 0) (null? (cdr nums))))))

(define (one-is-the-only-number? as)
  (let ((nums (num-members as)))
    (if (null? nums)
        #t
        (and (= (car nums) 1) (null? (cdr nums))))))

(define (insert-signs result items sign) 
  (cond ((null? items) result)
        ((null? result) 
         (insert-signs (list (car items)) (cdr items) sign))
        (else
         (insert-signs (append result (list sign (car items)))
                       (cdr items) sign))))

(define (make-sum . as)
  (cond ((null? as) 0)
        ((null? (cdr as)) (car as))
        ((null? (non-num-members as)) (apply + as))
        ((more-than-one-number? as) 
         (apply make-sum 
                (append (non-num-members as) 
                        (list (apply + (num-members as))))))
        ((zero-is-the-only-number? as)
         (apply make-sum (non-num-members as)))
        (else (insert-signs '() as '+))))

; test make-sum
;(make-sum 0 0)
;Value: 0

;(make-sum 8 0 3 0 4 0 5 0)
;Value: 20

;(make-sum 'x 'y 'z)
;Value: (x + y + z)

;(make-sum 'x 0 'y)
;Value: (x + y)

;(make-sum 'x 0 1 0 3 4 0 'y)
;Value: (x + y + 8)

(define (make-product . ms)
  (cond ((null? ms) 1)
        ((null? (cdr ms)) (car ms))
        ((null? (non-num-members ms)) (apply * ms))
        ((more-than-one-number? ms)
         (apply make-product
                (append (non-num-members ms)
                        (list (apply * (num-members ms))))))
        ((zero-is-the-only-number? ms) 0)
        ((one-is-the-only-number? ms)
         (apply make-product (non-num-members ms)))
        (else (insert-signs '() ms '*))))

; test make-product
;(make-product 1 1)
;Value: 1

;(make-product 10 9 0 1)
;Value: 0

;(make-product 'x 0 'y 4)
;Value: 0

;(make-product 'x 'y 'z)
;Value: (x * y * z)

;(make-product 'x 'y 'z 0)
;Value: 0

;(make-product 1 'x 1 'y 'z)
;Value 24: (x * y * z)

;(make-product 'x 2 3 (make-sum 'y 2) 4)
;Value 25: (x * (y + 2) * 24)

(define (sum? x)
  (cond ((not (pair? x)) #f)
        ((member '+ x) #t)
        (else #f)))

(define (product? x)
  (cond ((not (pair? x)) #f)
        ((and (not (sum? x)) (member '* x)) #t)
        (else #f)))

;test sum? and product?
;(sum? '(x + 3 * (x + y + 2)))
;Value: #t

;(product? '(x + 3 * (x + y + 2)))
;Value: #f

;(sum? '(x * y * (z + 2)))
;Value: #f

;(product? '(x * y * (z + 2)))
;Value: #t

(define (list-index f some-list)
  (define (list-index-iter f value-list indx-val)
    (cond ((null? value-list) indx-val)
          ((f (cadr value-list)) indx-val)
          (else (list-index-iter f (cdr value-list) (+ 1 indx-val)))))
  (list-index-iter f some-list 1))

(define (addend s) 
  (let* ((index (list-index (lambda (x) (eq? x '+)) s))
         (a (take s index)))
    (if (null? (cdr a))
        (car a)
        a)))

(define (augend s) 
  (let* ((index (list-index (lambda (x) (eq? x '+)) s))
         (b (drop s (+ index 1))))
    (if (null? (cdr b))
        (car b)
        b)))

; test addend and augend
(addend '(x + 3 * (x + y + 2)))
;Value: x

(addend '(x * y + z))
;Value: (x * y)

(augend '(x + 3 * (x + y + 2)))
;Value: (3 * (x + y + 2))

(augend '(x * y + z))
;Value: z

(define (multiplier p) 
  (let* ((index (list-index (lambda (x) (eq? x '*)) p))
         (a (take p index)))
    (if (null? (cdr a))
        (car a)
        a)))

(define (multiplicand p) 
  (let* ((index (list-index (lambda (x) (eq? x '*)) p))
         (b (drop p (+ index 1))))
    (if (null? (cdr b))
        (car b)
        b)))

; test multiplier and multiplicand
(multiplier '(x * y * (z + 2)))
;Value: x

(multiplicand '(x * y * (z + 2)))
;Value: (y * (z + 2))

(multiplier '((z + 2) * x * y))
;Value: (z + 2)

(multiplicand '((z + 2) * x * y))
;Value: (x * y)
 
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                              (deriv (augend expr) var)))
        ((product? expr) (make-sum
                         (make-product (multiplier expr)
                                       (deriv (multiplicand expr) var))
                         (make-product (deriv (multiplier expr) var)
                                       (multiplicand expr))))
        ((exponentiation? expr) (make-product 
                                 (make-product (exponent expr)
                                               (make-exponentiation (base expr) (make-sum (exponent expr) -1)))
                                 (deriv (base expr) var)))
        (else #f)))

; test deriv
(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4
;test deriv
(deriv '(x + 3 * (x + y + 2)) 'y)
;Value: 3