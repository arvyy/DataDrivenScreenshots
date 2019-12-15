(use-modules
  (dds base)
  (dds gif))

;create base shape. We'll draw same shape, but with different transformations
(define base-shape (rect #:width 30
                         #:height 30
                         #:fill (color 255 10 10)
                         #:stroke #f))

;container without transformations
(define cnt1 (cnt #:items (list base-shape)))

;container moved by 50 to right and 50 down
(define cnt2 (cnt #:items (list base-shape)
                  #:transform (translate 50 50)))

;container with multiple combined transformations -- first scaled by 2, then rotated by 45, 
;and then moved by 50 right and 120 down
(define cnt3 (cnt #:items (list base-shape)
                  #:transform (combine (scale 2 2) (rotate 45) (translate 50 120))))

(define item (cnt #:items (list cnt1 cnt2 cnt3)))

(define get-dds-gif
  (dds-gif #:item-getter (const item)))
