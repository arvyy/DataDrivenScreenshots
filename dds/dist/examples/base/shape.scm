;create basic rectangle
(define r (rect #:width 100
                #:height 100
                #:x 100
                #:y 100
                #:fill (color 10 255 255)
                #:stroke (color 10 10 10)
                #:stroke-width 3))

;create basic circle
(define c (circle #:x 300
                  #:y 150
                  #:radius 50
                  #:fill (color 10 255 255)
                  #:stroke (color 10 10 10)
                  #:stroke-width 3))

;create basic line
(define l (line #:points '((100 . 250) (150 . 300) (200 . 250))
                #:stroke (color 10 10 10)
                #:stroke-width 10))

(define tr (triangle #:points '((250 . 200)
                                (250 . 300)
                                (400 . 250))
                     #:stroke (color 10 10 10)
                     #:stroke-width 3
                     #:fill (color 10 255 255)))

(define get-dds-gif
  (dds-gif #:item-getter 
           (lambda(t)
             (cnt #:items (list r c l tr)))))
