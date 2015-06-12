(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-one origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame some-frame)
  (car some-frame))

(define (edge1-frame some-frame)
  (cadr some-frame))

(define (edge2-frame some-frame)
  (caddr some-frame))

(define (origin-frame-one some-frame)
  (car some-frame))

(define (edge1-frame-one some-frame)
  (cadr some-frame))

(define (edge2-frame-one some-frame)
  (cddr some-frame))

;; used for testing
(define example-frame-one (make-frame 1 2 3))

(define example-frame-two (make-frame-one 4 5 6))