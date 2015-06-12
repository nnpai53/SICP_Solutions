(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(define (square x)
  (* x x))

(square-list (list 1 2 3 4))

(square-list-map (list 1 2 3 4))