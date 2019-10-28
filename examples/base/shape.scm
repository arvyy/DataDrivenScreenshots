;create basic rectangle
(define r (rect #:width 100
                #:height 100
                #:x 100
                #:y 100
                #:fill (color 10 255 255)
                #:stroke (color 10 10 10)))

;create basic circle
(define c (circle #:x 300
                  #:y 150
                  #:radius 50
                  #:fill (color 10 255 255)
                  #:stroke #f))

;create basic line
(define l (line #:points '((100 . 250) (150 . 300) (200 . 250))
                   #:stroke (color 10 10 10)
                   #:stroke-width 10))

;override the template method 'render'
(define (render data) ;ignore data for now
  (cnt #:items (list r c l))) ;draw line
