(define r (rect #:width 30 #:height 30 ))
(define c (circle #:radius 20 #:inner-radius 10 #:x 60 #:y 30 #:stroke-width 1 #:stroke (color 10 10 10 100)))
(define t (triangle #:points '((20 . 40) (20 . 60) (40 . 60) (40 . 40))))
(define l (line #:points '((20 . 60) (30 . 70) (40 . 75))
                #:stroke-width 2))

(define (stop? data) #t)

(define c (cnt #:items (list r c t l)
               #:transform (translate 50 50)))

(define (render data)
  (draw c))
