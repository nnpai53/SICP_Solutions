(define (square x)
  (* x x))

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

(define (lenCal line-segment)
  (let ((sp (start-segment line-segment))
        (ep (end-segment line-segment)))
    (sqrt (+ (square (abs (- (x-point sp) (x-point ep)))) (square (abs (- (y-point sp) (y-point ep))))))))
  
(define (make-rectangle left-bottom right-top)
  (cons left-bottom right-top))

(define (bottom-left rectangle)
  (car rectangle))

(define (top-right rectangle)
  (cdr rectangle))

;(define (bottom-right rectangle)
;  (let* ((bl (bottom-left rectangle))
;        (tr (top-right rectangle))
;        (mid-pt (mid-point (make-segment bl tr)))
;        (other-pt (make-point (/ (- (x-point bl) (x-point tr)) 2.0) (/ (- (y-point bl) (y-point tr)) 2.0))))
;    (make-point (- (x-point mid-pt) (x-point other-pt)) (+ (y-point mid-pt) (y-point other-pt)))))

;(define (top-left rectangle)
;  (let* ((bl (bottom-left rectangle))
;        (tr (top-right rectangle))
;        (mid-pt (mid-point (make-segment bl tr)))
;        (other-pt (make-point (/ (- (x-point bl) (x-point tr)) 2.0) (/ (- (y-point bl) (y-point tr)) 2.0))))
;    (make-point (+ (x-point mid-pt) (x-point other-pt)) (- (y-point mid-pt) (y-point other-pt)))))

;(define (rect-width rectangle)
;  (let ((bl (bottom-left rectangle))
;        (br (bottom-right rectangle))
;        (tl (top-left rectangle)))
;    (lenCal (make-segment bl br))))

;(define (rect-height rectangle)
;  (let ((bl (bottom-left rectangle))
;        (br (bottom-right rectangle))
;        (tl (top-left rectangle)))
;    (lenCal (make-segment bl tl))))

(define (bottom-right rect) 
   (make-point (x-point (cdr rect)) 
               (y-point (car rect)))) 
 (define (top-left rect) 
   (make-point (x-point (car rect)) 
               (y-point (cdr rect)))) 

(define (rect-width rect) 
   (abs (- (x-point (bottom-left rect)) 
           (x-point (bottom-right rect)))))

 (define (rect-height rect) 
   (abs (- (y-point (bottom-left rect)) 
           (y-point (top-left rect))))) 

(define (area-rect rectangle)
  (* (rect-width rectangle) (rect-height rectangle)))

(define (perimeter-rect rectangle)
  (* 2 (+ (rect-width rectangle) (rect-height rectangle))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Usage: 
 (define r (make-rectangle (make-point 1 1) 
                      (make-point 3 7)))
 
 (define r1 (make-rectangle (make-point 1 1) 
                      (make-point 9 3)))
 
 ;(print-point (mid-point (make-segment (bottom-left r) (top-right r))))
 ;(newline)
 ;(print-point (make-point 
 ;              (/ (abs (- (x-point (bottom-left r)) (x-point (top-right r)))) 2.0) 
 ;              (/ (abs (- (y-point (bottom-left r)) (y-point (top-right r)))) 2.0)))
 ;(newline)
 (rect-width r)
 (rect-height r)
 (area-rect r)
 (perimeter-rect r)
 (newline)
 ;(print-point (mid-point (make-segment (bottom-left r1) (top-right r1))))
 ;(newline)
 ;(print-point (make-point 
 ;              (/ (abs (- (x-point (bottom-left r1)) (x-point (top-right r1)))) 2.0) 
 ;              (/ (abs (- (y-point (bottom-left r1)) (y-point (top-right r1)))) 2.0)))
 ;(newline)
 (rect-width r1)
 (rect-height r1)
 (area-rect r1)
 (perimeter-rect r1)
 
 ; Alternate Implementation
 ; the old area functions and perimeter would work fine on the new representation
 ; if we change the name of the height and width represenations to the original ones
 ; the new definitions are just for simplicity and to distinguish
 
 (define (make-rect-alt bottom-left height width) 
   (cons bottom-left (cons height width ))) 
  
 (define (rect-height-alt rect) (car (cdr rect)))  
 (define (rect-width-alt rect) (cdr (cdr rect))) 
 
 (define (area-rect-alt rectangle)
  (* (rect-width-alt rectangle) (rect-height-alt rectangle)))

(define (perimeter-rect-alt rectangle)
  (* 2 (+ (rect-width-alt rectangle) (rect-height-alt rectangle))))
  
 ;; Usage for alternate implementation: 
 (define r2 (make-rect-alt (make-point 1 1) 3 5))
 (newline)
 (area-rect-alt r2)
 (perimeter-rect-alt r2)