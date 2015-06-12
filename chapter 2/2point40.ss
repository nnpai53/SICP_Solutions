(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides a b)
  (= (remainder b a ) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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

(define (filter predicate? sequence)
  (cond ((null? sequence) '())
        ((predicate? (car sequence)) (cons (car sequence)
                                           (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

                    