(define (sum x y)
    (+ x y))

(define (diff x y)
    (- x y))

(define (square x)
    (* x x))

(define (greater x y)
    (if (> x y) x y) )

(define (smaller x y)
    (if (< x y) x y))

(define (largestsquaresum a b c)
    (sum (square (greater (greater a b) c)) 
         (square (diff (diff (sum (sum a b) c) (greater (greater a b) c)) (smaller (smaller a b) c) ))))