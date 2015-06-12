;(define (gcd-new a b)
;  (if (= b 0)
;      a
;      (gcd-new b (remainder a b))))

(define (make-rat n d)
  (cond ((and (> n 0) (> d 0))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g))))
        ((and (< n 0) (> d 0))
         (let ((g (gcd (- 0 n) d)))
          (cons (/ n g) (/ d g))))
        ((and (> n 0) (< d 0))
         (let ((g (gcd n (- 0 d))))
          (cons (/ (- 0 n) g) (/ (- 0 d) g))))
        ((and (< n 0) (< d 0))
         (let ((g (gcd (- 0 n) (- 0 d))))
          (cons (/ (- 0 n) g) (/ (- 0 d) g))))))

; alternate definition
;(define (make-rat n d) 
;   (let ((g ((if (< d 0) - +) (gcd n d)))) 
;     (cons (/ n g) (/ d g)))) 
        
(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-(* (numer x) (denom y))
              (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (isequal-rat x y)
  (= (* (numer x) (denom y)
        (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat -1 -3))
(define minus-one-third (make-rat -1 3))

(print-rat one-third)
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (make-rat 6 -9)) 

(print-rat (add-rat minus-one-third minus-one-third))

(print-rat (mul-rat one-third minus-one-third))