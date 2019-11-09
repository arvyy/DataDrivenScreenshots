(use-modules
  (dds base input))
(define (render d)
  (draw (circle #:x (car (get-mouse-pos))
                #:y (cdr (get-mouse-pos))
                #:radius 10
                #:fill (if (is-mouse? 'left 'down)
                               (color 250 10 10 255)
                               (color 10 250 10 255)))))
