(define (square x)
  (* x x))

(define (cube x)
  (* x (square x)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (triaverage (/ x (square guess)) (* 2 guess)))

(define (triaverage a b)
  (/ (+ a b) 3))

(define (goodenough? guess x)
   (< (abs (- (cube guess) x)) 0.001))

(define (cubeiter guess x)
  (if(goodenough? guess x)
     guess
     (cubeiter (improve guess x) x)))

(define (cuberoot x)
  (cubeiter 1.0 x))