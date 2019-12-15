(define-module
  (dds base-impl triangle))

(use-modules
  (ice-9 match)
  (ice-9 format)
  (dds base-impl native)
  (dds base-impl color)
  (dds base-impl primitives-syntax))


(define-record-type+fact+bind* <triangle> triangle-fact bind*-triangle override-triangle
  (make-triangle data id points fill stroke stroke-width)
  triangle?
  ((data triangle-data))
  (id triangle-id)
  (points triangle-points )
  (fill triangle-fill )
  (stroke triangle-stroke )
  (stroke-width triangle-stroke-width ))

(define (draw-triangle triangle)
  (match triangle
         (($ <triangle> data id points fill stroke thick)
          (let it ((points points)
                   (stroke (color->vec stroke))
                   (fill (color->vec fill)))
            (match points
                   ((a b c . _) 
                    (begin
                      (draw-triangle* a b c fill stroke thick)
                      (it (cdr points) stroke fill)))
                   (_ #f))))))

(define triangle (triangle-fact (make-triangle #f #f '() (color 200 100 100 255) (color 10 10 10 255) 1)))
(define triangle-o (triangle-fact (make-triangle #f #f #f #f #f #f)))

(define (hover-triangle-points? a b c mx my)
  (define (within-edge a b)
    (define ax (car a))
    (define ay (cdr a))
    (define bx (car b))
    (define by (cdr b))
    (define-values (ab-x ab-y) (values (- bx ax) (- by ay)))
    (define-values (nx ny) (values ab-y (- ab-x)))
    (define-values (am-x am-y) (values (- mx ax) (- my ay)))
    (define prod (+ (* am-x nx) (* am-y ny)))
    (>= prod 0))
  (and (within-edge a b)
       (within-edge b c)
       (within-edge c a)))

(define (hover-triangle? triangle mx my)
  (define (hover? lst)
    (match lst
      ((a b c . _)
       (if (hover-triangle-points? a b c mx my) (list triangle)
           (hover? (cdr lst))))
      (_ '())))
  (hover? (triangle-points triangle)))

(export draw-triangle <triangle> triangle triangle-o triangle? hover-triangle? override-triangle hover-triangle-points?)
