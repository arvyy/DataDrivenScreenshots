(define-module
  (dds base-impl rect))

(use-modules
  (ice-9 match)
  (ice-9 format)
  (dds base native) 
  (dds base-impl color)
  (dds base-impl primitives-syntax))

(define-record-type+fact+bind* <rect> rect-fact bind*-rect 
    (make-rect id x y width height roundness fill stroke stroke-width segments)
    rect?
    (id rect-id)
    (x rect-x )
    (y rect-y )
    (width rect-width )
    (height rect-height )
    (roundness rect-roundness )
    (fill rect-fill )
    (stroke rect-stroke )
    (stroke-width rect-stroke-width )
    (segments rect-segments ))

(define (draw-rect rect)
  (match rect
    (($ <rect> id x y width height roundness fill stroke thick segments)
     (draw-rect* x y width height roundness (color->vec fill) (color->vec stroke) thick segments))))

(define rect (rect-fact (make-rect #f 0 0 100 100 0 (color 200 100 100) (color 10 10 10) 1 20)))
(define rect-o (rect-fact (make-rect #f #f #f #f #f #f #f #f #f #f)))

(define (hover-rect? rect mx my)
  (match rect
    (($ <rect> id x y width height)
     (and (>= mx x) 
          (>= my y) 
          (<= mx (+ x width))
          (<= my (+ y height))))))

(export <rect> draw-rect rect rect-o hover-rect?)
