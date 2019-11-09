(use-modules
  (dds bezier))

(define control-points
  '((p 0 0)
    (c 50 50)
    (p 0 100)
    (p 100 50)
    (p 0 0)))

;create bezier curve
(define b (bezier #:points control-points
                  #:segments 6
                  #:fill (color 10 255 10)
                  #:stroke (color 255 10 10)
                  #:stroke-width 2))

(define (render data)
  (draw b))
