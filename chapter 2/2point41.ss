(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (filter predicate? sequence)
  (cond ((null? sequence) '())
        ((predicate? (car sequence)) (cons (car sequence)
                                     (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (generate-pairs n)
  (flatmap (lambda (y)
             (map (lambda (z) (list y z))
                  (enumerate-interval 1 n)))
                (enumerate-interval 1 n)))

(define (generate-triples n)
  (flatmap (lambda (x) 
         (map (lambda (pair) (list x (car pair) (cadr pair)))
              (generate-pairs n)))
       (enumerate-interval 1 n)))

(define (is-triple-sum-s? n s)
  (define (isequal-s x)
    (= x s))
  (define (is-sum-s pair)
    (isequal-s (+ (car pair) (cadr pair) (caddr pair))))
  (filter is-sum-s (generate-triples n)))
