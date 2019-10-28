(define r (rect #:fill (calc (hover?) (if hover? (color 255 255 10) (color 10 255 10)))))
(define cr (circle #:x 200 #:radius 50 #:inner-radius 25 #:fill (calc (hover?) (if hover? (color 255 255 10) (color 10 255 10)))))
(define c (cnt #:transform (translate 100 100)
               #:items (list r cr)
               #:onclick (calc(data) (lambda ()(not data)))))

(define (update delta data)
  data)

(define (mouse-click mouse data)
  (define onclick (get-onclick (apply-data c data) mouse))
  (if onclick (onclick)
      data))

(define (render d)
  (draw (apply-data c d)))
