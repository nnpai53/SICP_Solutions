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

(define (for-each-new proc some-list)
  (cond ((null? some-list) #t)
        ((proc (car some-list))
         (for-each-new proc (cdr some-list)))))

(define (make-segment vector1 vector2)
  (cons vector1 vector2))

(define (start-segment some-segment)
  (car some-segment))
  
(define (end-segment some-segment)
  (cdr some-segment))

(define (segments->painters segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line 
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

;; actual 2.49 a

(define draw-frame-outline
  (segments->painters
   (list (make-segment (make-vect 0 0)
                       (make-vect 0 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 1))
         (make-segment (make-vect 1 1)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 0 0)))))

;; actual 2.49 b

(define draw-x-opposite-corner
  (segments->painters
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 0)))))

;; actual 2.49 c

(define draw-diamond
  (segments->painters
   (list (make-segment (make-vect 0 0.5)
                       (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1)
                       (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5)
                       (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0)
                       (make-vect 0 0.5)))))

;; actual 2.49 d
;; taken from https://wizardbook.wordpress.com/2010/12/03/exercise-2-49/

(define painter-wave
  (segments->painters
   (list (make-segment (make-vect 0.2 0.0) 
                       (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) 
                       (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) 
                       (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3)
                       (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) 
                       (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5)
                       (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6)
                       (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6)
                       (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8)
                       (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0)
                       (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8)
                       (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) 
                       (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6)
                       (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2)
                       (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4)
                       (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0)
                       (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3)
                       (make-vect 0.3 0.0)))))