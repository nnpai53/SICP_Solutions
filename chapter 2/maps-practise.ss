(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map-new proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map-new proc (cdr items)))))

(map-new abs (list -10 2.5 -11.6 17))

(map-new (lambda (x) (* x x))
     (list 1 2 3 4))

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))

(define (scale-list-map items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list-map (list 1 2 3 4 5) 10)