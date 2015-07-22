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

(define (make-segment vector1 vector2)
  (cons vector1 vector2))

(define (start-segment some-segment)
  (car some-segment))
  
(define (end-segment some-segment)
  (cdr some-segment))