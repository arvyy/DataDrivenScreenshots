(use-modules
  (dds bezier))

(define control-points '((p 100 100)
                         (c 100 300)
                         (c 200 0)
                         (c 300 300)
                         (p 300 100)
                         (p 400 100)
                         #!
                         (c 300 -100)
                         (p 450 100)
                         (p 400 250)
                         !#
                         ))

;create bezier curve
(define b (bezier #:points control-points
                  #:fill (color 10 255 10)
                  #:stroke (color 255 10 10)
                  #:stroke-width 4))

(define (render data)
  (draw b))
