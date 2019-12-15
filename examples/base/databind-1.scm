; simple data binding. Data is bound to single item, and used to define single property.

(define r (rect #:x 100
                #:y (calc (data) data)
                #:width 100
                #:height 100))

(define (item-getter t)
  (apply-data r (+ 100 (* 50 t))))

(define get-dds-gif
  (dds-gif #:duration 4
           #:item-getter item-getter))
