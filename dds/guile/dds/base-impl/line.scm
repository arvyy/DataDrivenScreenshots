(define-module
  (dds base-impl line))

(use-modules
  (ice-9 match)
  (dds base-impl color)
  (dds base-impl primitives-syntax)
  (dds base-impl triangulation)
  (dds base-impl native))

(define-record-type+fact+bind* <line> line-fact bind*-line
    (make-line id points fill stroke stroke-width)
    line?
    (id line-id)
    (points line-points )
    (fill line-fill )
    (stroke line-stroke )
    (stroke-width line-stroke-width ))

(define line (line-fact (make-line #f '() #f (color 10 10 10 255) 1)))
(define line-o (line-fact (make-line #f #f #f #f #f)))

(define (draw-line line)
  (match line
         (($ <line> id points fill stroke thick)
          (when fill
            (let ()
              (define triangles (triangulate points))
              (for-each (lambda(t)
                          (match t
                            (#(a b c)
                             (draw-triangle* a b c (color->vec fill) #f 0))))
                        triangles)))
          (draw-line* (list->vector points) (color->vec stroke) thick)
          )))
(export <line> line line-o draw-line)
