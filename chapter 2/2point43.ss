(define (filter predicate? sequence)
  (cond ((null? sequence) '())
        ((predicate? (car sequence)) (cons (car sequence)
                                           (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (cons k new-row))))

(define (get-pair-k position k)
  (define (iter-pair-result stripped-position x)
    (if (= x k)
        (car stripped-position)
        (iter-pair-result (cdr stripped-position) (+ x 1))))
  (iter-pair-result position 1))

(define (safe? k position)
  (define (row-diagonal-check position value)
    (let ((row-to-check (get-pair-k position k))
          (one-of-row (get-pair-k position value)))
      (not (or (= (cdr row-to-check) (cdr one-of-row)) (= (abs (- (cdr row-to-check) (cdr one-of-row)))
                                                        (abs (- (car row-to-check) (car one-of-row))))))))
  (accumulate (lambda (x y) (and x y)) #t (map (lambda (value)
                                                 (row-diagonal-check position value))
                                               (enumerate-interval 1 (- k 1)))))