(define-module
  (dds base-impl transform))

(use-modules
  (ice-9 match))

(define deg->rad (/ 3.145 180))
(define (rotate deg)
  (define cos* (cos (* deg->rad deg)))
  (define sin* (sin (* deg->rad deg)))
  (vector cos* sin* 0.
          (- sin*) cos* 0.))

(define (translate x y)
  (vector 1. 0. x
          0. 1. y))

(define (scale xk yk)
  (vector xk 0. 0.
          0. yk 0.))

(define (combine transform . rest)
  (cond
    ((null? rest) transform)
    (else (match-let ((#(a11 a21 a31
                         a12 a22 a32) (car rest))
                      (#(b11 b21 b31
                         b12 b22 b32) transform))
              (define new-transform (vector (+ (* a11 b11) (* a21 b12))
                                            (+ (* a11 b21) (* a21 b22))
                                            (+ (* a11 b31) (* a21 b32) a31)
                                            (+ (* a12 b11) (* a22 b12))
                                            (+ (* a12 b21) (* a22 b22))
                                            (+ (* a12 b31) (* a22 b32) a32)))
              (apply combine (cons new-transform (cdr rest)))))))

(define (transform-point transform point)
  (cons
    (+ (* (vector-ref transform 0) (car point))
       (* (vector-ref transform 1) (cdr point))
       (vector-ref transform 2))
    (+ (* (vector-ref transform 3) (car point))
       (* (vector-ref transform 4) (cdr point))
       (vector-ref transform 5))))


(export rotate scale translate combine transform-point)
