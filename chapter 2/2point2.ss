(define (make-point x-cord y-cord)
  (cons x-cord y-cord))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (let ((sp (start-segment segment))
        (ep (end-segment segment)))
    (make-point (/ (+ (x-point sp) (x-point ep)) 2.0)
                (/ (+ (y-point sp) (y-point ep)) 2.0))))
               
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; testing
(define fp (make-point 1 2))
(define secp (make-point 3 4))
(define fseg (make-segment fp secp))
(print-point (mid-point fseg))