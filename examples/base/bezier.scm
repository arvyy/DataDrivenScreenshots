(use-modules
  (dds bezier))

(define control-points '((100 . 100)
                         (100 . 300)
                         (200 . 0)
                         (300 . 300)
                         (300 . 100)))

(define control-points/x (map car control-points))
(define control-points/y (map cdr control-points))


;create bezier curve
(define b (bezier #:points control-points
                  #:stroke (color 255 10 10)
                  #:stroke-width 4))

(define c (circle #:x (calc (t) (bezier-interp/n control-points/x t))
                  #:y (calc (t) (bezier-interp/n control-points/y t))
                  #:radius 10))

(define (init-data) 0)
(define (update delta data)
  (+ (* 0.5 delta) data))
(define (stop? data)
  (> data 1))
(define (render data)
  (draw b)
  (apply-data c data))
