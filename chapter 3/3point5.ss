(define (square x)
    (* x x))

(define (estimate-integral some-predicate? lower-x lower-y upper-x upper-y number-of-trials)
    (* (- upper-y lower-y) (- upper-x lower-x) (monte-carlo number-of-trials some-predicate?)))

(define (inside-Circle xCentre yCentre xPoint yPoint radius)
    (< (+ (square (- xCentre xPoint)) (square (- yCentre yPoint))) (square radius)))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)))
              (else (iter (- trials-remaining 1)
                          trials-passed))))
    (iter trials 0))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))