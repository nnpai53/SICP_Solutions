(define (make-vect x-cord y-cord)
  (cons x-cord y-cord))

(define (xcor-vect some-vect)
  (car some-vect))

(define (ycor-vect some-vect)
  (cdr some-vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect factor some-vect)
  (make-vect (* factor (xcor-vect some-vect)) (* factor (ycor-vect some-vect))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-segment vector1 vector2)
  (cons vector1 vector2))

(define (start-segment some-segment)
  (car some-segment))
  
(define (end-segment some-segment)
  (cdr some-segment))


;; part a
;; solution taken from here
;; http://www.billthelizard.com/2012/02/sicp-252-levels-of-language-for-robust.html

(define wave-segments
 (list
  (make-segment    (make-vect 0.006 0.840)
                   (make-vect 0.155 0.591))
  (make-segment    (make-vect 0.006 0.635)
                   (make-vect 0.155 0.392))
  (make-segment    (make-vect 0.304 0.646)
                   (make-vect 0.155 0.591))
  (make-segment    (make-vect 0.298 0.591)
                   (make-vect 0.155 0.392))
  (make-segment    (make-vect 0.304 0.646)
                   (make-vect 0.403 0.646))
  (make-segment    (make-vect 0.298 0.591)
                   (make-vect 0.354 0.492))
  (make-segment    (make-vect 0.403 0.646)  ;left face
                   (make-vect 0.348 0.845))
  (make-segment    (make-vect 0.354 0.492)
                   (make-vect 0.249 0.000))
  (make-segment    (make-vect 0.403 0.000)
                   (make-vect 0.502 0.293))
  (make-segment    (make-vect 0.502 0.293)
                   (make-vect 0.602 0.000))
  (make-segment    (make-vect 0.348 0.845)
                   (make-vect 0.403 0.999))
  (make-segment    (make-vect 0.602 0.999)
                   (make-vect 0.652 0.845))
  (make-segment    (make-vect 0.652 0.845)
                   (make-vect 0.602 0.646))
  (make-segment    (make-vect 0.602 0.646)
                   (make-vect 0.751 0.646))
  (make-segment    (make-vect 0.751 0.646)
                   (make-vect 0.999 0.343))
  (make-segment    (make-vect 0.751 0.000)
                   (make-vect 0.597 0.442))
  (make-segment    (make-vect 0.597 0.442)
                   (make-vect 0.999 0.144))
  (make-segment    (make-vect 0.395 0.916) ;eye
                   (make-vect 0.410 0.916))
  (make-segment    (make-vect 0.376 0.746) ;smile
                   (make-vect 0.460 0.790))))


;; part b
;; solution take from here
;; http://www.billthelizard.com/2012/02/sicp-252-levels-of-language-for-robust.html
(define (corner-split painter n)
 (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1)))
           (corner (corner-split painter (- n 1))))
       (beside (below painter up)
               (below right corner)))))

;; part c
;; solution taken from here
;; http://www.billthelizard.com/2012/02/sicp-252-levels-of-language-for-robust.html

(define (square-limit painter n)
 (let ((quarter (rotate180 (corner-split painter n))))
   (let ((half (beside (flip-horiz quarter) quarter)))
     (below (flip-vert half) half))))