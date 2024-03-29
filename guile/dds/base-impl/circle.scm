(define-module
  (dds base-impl circle))

(use-modules
  (ice-9 match)
  (dds base-impl primitives-syntax)
  (dds base-impl color)
  (dds base-impl native))

(define-record-type+fact+bind* <circle> circle-fact bind*-circle override-circle
    (make-circle data id x y radius inner-radius start-angle end-angle fill stroke stroke-width segments)
    circle?
    ((data circle-data))
    (id circle-id)
    (x circle-x )
    (y circle-y )
    (radius circle-radius )
    (inner-radius circle-inner-radius )
    (start-angle circle-start-angle )
    (end-angle circle-end-angle )
    (fill circle-fill )
    (stroke circle-stroke )
    (stroke-width circle-stroke-width )
    (segments circle-segments ))

(define circle (circle-fact (make-circle #f #f 0 0 100 0 0 360 (color 200 100 100 255) (color 10 10 10 255) 1 20)))
(define circle-o (circle-fact (make-circle #f #f #f #f #f #f #f #f #f #f #f #f)))

(define (draw-circle circle)
  (match circle
         (($ <circle> data id x y r r-inner angl1 angl2 fill stroke thick segments)
          (draw-circle* x y r r-inner angl1 angl2 (color->vec fill) (color->vec stroke) thick segments))))

(define (hover-circle? circle mx my)
  (define hover? (match circle
     (($ <circle> data id x y r r-inner)
      (let ()
        (define dist (sqrt (+ (expt (- x mx) 2) 
                              (expt (- y my) 2))))
        (and (>= dist r-inner) (<= dist r))))))
  (if hover? (list circle) '()))

(export <circle> circle circle? circle-o draw-circle hover-circle? override-circle)
