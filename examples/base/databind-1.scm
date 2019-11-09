; simple data binding. Data is bound to single item, and used to define single property.

(define r (rect #:x 100
                #:y (calc (data) data)
                #:width 100
                #:height 100))

(define (init-data)
  100)

(define (update delta data)
  (+ data (* 50 delta))) ;move 50px per second

(define (stop? data) ;end when y > 200
  (> data 200))

(define (render data)
  (draw (apply-data r data)))
