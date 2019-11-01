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

(define (points->segments points)
  (define (p->s segments segment last-fixed-point points)
    (cond
      ((null? points) segments)
      (else (match points
              ((('p x y) _ ...) (p->s 
                                  (cons (reverse (cons (cons x y) segment)) segments)
                                  (list (cons x y))
                                  (cons x y)
                                  (cdr points)))
              ((('c x y) _ ...) (p->s
                                  segments
                                  (cons (cons x y) segment)
                                  last-fixed-point
                                  (cdr points)))))))
  (match points
    ((('p x y) . rest) (p->s '() (list (cons x y)) (cons x y) rest))
    (_ (error "first point must be p"))))

(define (segment->coords segment t-list)
  (define x-points (map car segment))
  (define y-points (map cdr segment))
  (define (bez-interp t) 
    (cons (bezier-interp/n x-points t)
          (bezier-interp/n y-points t)))
  (map bez-interp t-list))

(define (segments->coords segments t-list)
  (define first (caar segments))
  (define rest/lst (map (lambda(segment)
                          (cdr (segment->coords segment t-list))) 
                        segments))
  (cons first (apply append rest/lst)))

(define (points->coords points t-list)
  (segments->coords (points->segments points) t-list))

(define* (bezier #:key
                 (id #f)
                 (points '((p 0 0) (p 0 0)))
                 (segments 20)
                 (fill #f)
                 (stroke (color 10 10 10))
                 (stroke-width 1))
         (define t-lst (compute (segments) 
                                (list-ec (: i (+ segments 1)) (/ i segments))))

         (define coords 
           (compute (points t-lst)
               (points->coords points t-lst)))

         (line #:points coords
               #:id id
               #:fill fill
               #:stroke stroke
               #:stroke-width stroke-width))

(export bezier bezier-interp/quadratic bezier-interp/cubic bezier-interp/n)
