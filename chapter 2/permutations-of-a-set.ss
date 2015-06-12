(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(define (filter predicate? sequence)
  (cond ((null? sequence) '())
        ((predicate? (car sequence)) (cons (car sequence) 
                                           (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (permutations s)
  (if (null? s)       ;empty set?
      (list '())      ;sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))