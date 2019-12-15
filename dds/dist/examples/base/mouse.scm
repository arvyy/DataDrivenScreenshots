(define (item-getter t)
  (circle #:x (car (get-mouse-pos))
                #:y (cdr (get-mouse-pos))
                #:radius 10
                #:fill (if (is-mouse? 'left 'down)
                               (color 250 10 10 255)
                               (color 10 250 10 255))))

(define get-dds-gif
  (dds-gif #:item-getter item-getter))
