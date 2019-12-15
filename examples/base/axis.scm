(use-modules
  (dds axis))

(define ticks
  (list-ec (: i 10) (cons (* 10 i) (number->string i) )))

(define item
  (cnt #:items (list (hor-axis #:ticks ticks
                               #:length 100))
       #:transform (translate 200 200)))

(define (render data)
  (draw (apply-data item data)))
