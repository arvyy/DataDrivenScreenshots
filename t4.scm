(define item
  (cnt
    #:post-processing (from-premultiplied)
    #:items (list
              (cnt #:post-processing (to-premultiplied)
                   #:items (list (rect #:width 100
                                       #:height 100
                                       #:color (color 255 255 0 255))))
              (cnt #:post-processing (list 
                                       (to-premultiplied)
                                       (blur 800 2 #t))
                   #:items (list 
                             (rect #:x 0
                                   #:y 0
                                   #:width 50
                                   #:height 50
                                   #:color (color 255 0 0 200))

                             (rect #:x 50
                                   #:y 50
                                   #:width 50
                                   #:height 50
                                   #:color (color 255 0 0 200))))
              )
    #:blend blend/alpha))

(define (render data)
  (draw item))
