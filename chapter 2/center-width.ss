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
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
  (cond ((and (and (<= lbx 0) (>= ubx 0)) (and (<= lby 0) (>= uby 0)))
         (make-interval (min (* lbx uby) (* ubx lby)) 
                        (max (* lbx lby) (* ubx uby))))
        ((and (and (<=lbx 0) (>= ubx 0)) (and (>= lby 0) (>= uby 0)))
         (make-interval (* lbx uby) (* ubx uby)))
        ((and (and (<=lbx 0) (>= ubx 0)) (and (<= lby 0) (<= uby 0)))
         (make-interval (* ubx lby) (* lbx lby)))
        ((and (and (<=lbx 0) (<= ubx 0)) (and (>= lby 0) (>= uby 0)))
         (make-interval (* lbx uby) (* ubx lby)))
        ((and (and (<=lbx 0) (<= ubx 0)) (and (<= lby 0) (<= uby 0)))
         (make-interval (* ubx uby) (* lbx lby)))
        ((and (and (<=lbx 0) (<= ubx 0)) (and (<= lby 0) (>= uby 0)))
         (make-interval (* lbx uby) (* lbx lby)))
        ((and (and (>=lbx 0) (>= ubx 0)) (and (>= lby 0) (>= uby 0)))
         (make-interval (* lbx lby) (* ubx uby)))
        ((and (and (>=lbx 0) (>= ubx 0)) (and (<= lby 0) (>= uby 0)))
         (make-interval (* ubx lby) (* ubx uby)))
        ((and (and (>=lbx 0) (>= ubx 0)) (and (<= lby 0) (<= uby 0)))
         (make-interval (* ubx lby) (* lbx uby))))))

(define (div-interval x y)
  (cond ((and (or (< (lower-bound y) 0) (= (lower-bound y) 0)) (or (> (upper-bound y) 0) (= (upper-bound y) 0))) 
         ((newline)
          (display "Interval spans 0. Cannot divide")))
        (else
         (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))))
