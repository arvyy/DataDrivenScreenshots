(define-module
  (dds base-impl triangle))

(use-modules
  (ice-9 match)
  (dds base native)
  (dds base-impl color)
  (dds base-impl primitives-syntax))


(define-record-type+fact+bind* <triangle> triangle-fact bind*-triangle 
  (make-triangle id points fill stroke stroke-width)
  triangle?
  (id triangle-id)
  (points triangle-points )
  (fill triangle-fill )
  (stroke triangle-stroke )
  (stroke-width triangle-stroke-width ))

(define (draw-triangle triangle)
  (match triangle
         (($ <triangle> id points fill stroke thick)
          (let it ((points points)
                   (stroke (color->vec stroke))
                   (fill (color->vec fill)))
            (match points
                   ((a b c . _) 
                    (begin
                      (draw-triangle* a b c fill stroke thick)
                      (it (cdr points) stroke fill)))
                   (_ #f))))))

(define triangle (triangle-fact (make-triangle #f '() (color 200 100 100 255) (color 10 10 10 255) 1)))
(define triangle-o (triangle-fact (make-triangle #f #f #f #f #f)))

;TODO
(define (hover-triangle? triangle mx my)
  #f)

(export draw-triangle <triangle> triangle triangle-o hover-triangle?)
