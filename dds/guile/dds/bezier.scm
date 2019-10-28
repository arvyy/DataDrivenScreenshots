(define-module
  (dds bezier))

(use-modules 
  (dds base)
  (srfi srfi-42)
  (ice-9 match))

(define (bezier-interp/linear p0 p1 t)
  (+ (* p0 (- 1 t))
           (* p1 t)))

(define (bezier-interp/quadratic p0 c1 p1 t)
  (+ (* p0 (expt (- 1 t) 2))
     (* c1 (* 2 (- 1 t) t))
     (* p1 (expt t 2))))

(define (bezier-interp/cubic p0 c1 c2 p1 t)
  (+ (* p0 (expt (- 1 t) 3))
     (* c1 (* 3 (expt (- 1 t) 2) t))
     (* c2 (* 3 (- 1 t) (expt t 2)))
     (* p1 (expt t 3))))

(define (bezier-interp/n points t)
  (match points
    ((or () (_)) (error "At least 2 points required"))
    ((p0 p1) (bezier-interp/linear p0 p1 t))
    ((p0 c1 p1) (bezier-interp/quadratic p0 c1 p1 t))
    ((p0 c1 c2 p1) (bezier-interp/cubic p0 c1 c2 p1 t))
    ((? list?) (let it ((points points)
                        (new-pts '()))
                 (match points
                   ((p1 p2 . _) (it (cdr points) (cons (bezier-interp/linear p1 p2 t) new-pts)))
                   (_ (bezier-interp/n (reverse new-pts) t)))))))

(define* (bezier #:key
                 (id #f)
                 (points '((0 . 0) (0 . 0)))
                 (segments 20)
                 (stroke (color 10 10 10))
                 (stroke-width 1))
         (define t-lst (compute (segments) 
                                (list-ec (: i (+ segments 1)) (/ i segments))))

         (define segment-points 
           (compute (points t-lst)
                (define x-points (map car points))
                (define y-points (map cdr points))
                (define (bez-interp t) 
                  (cons (bezier-interp/n x-points t)
                        (bezier-interp/n y-points t)))
                (map bez-interp t-lst)))

         (line #:points segment-points
               #:id id
               #:stroke stroke
               #:stroke-width stroke-width))

(export bezier bezier-interp/quadratic bezier-interp/cubic bezier-interp/n)
