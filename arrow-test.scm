(use-modules
  (chart derivativeshapes arrow))

(define arrow-item
  (arrow #:end (lambda(d) (cons 100 d))
         #:point-length 20
         #:point-width 20))

(define item
  (cnt #:items (list arrow-item)
       #:transform (translate 100 100)))

(define (init-data) 0)
(define (update fl data)
  (+ (* fl 50) data))

(define (stop? data)
  (> data 100))

(define (render data)
  (draw (apply-data item data)))
