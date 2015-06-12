(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse-fr (list 1 2 3 4 5))

(reverse-fl (list 1 2 3 4 5))