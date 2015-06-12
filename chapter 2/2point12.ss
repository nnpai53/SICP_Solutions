(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- 0 (upper-bound y))
                               (- 0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
        
(define (div-interval x y)
  (cond ((and (or (< (lower-bound y) 0) (= (lower-bound y) 0)) (or (> (upper-bound y) 0) (= (upper-bound y) 0))) 
         ((newline)
          (display "Interval spans 0. Cannot divide")))
        (else
         (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))))

(define (make-center-percent center percentage)
  (let ((width (* (/ percentage 100.0) center)))
    (make-interval (- center width) (+ center width))))

(define (percent interval)
  (let ((width (/ (- (upper-bound interval) (lower-bound interval)) 2.0))
        (center (/ (+ (upper-bound interval) (lower-bound interval)) 2.0)))
    (* (/ width center) 100.0)))
