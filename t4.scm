(define item
  (cnt
    #:post-processing (from-premultiplied)
    #:items (list
              (cnt #:post-processing (to-premultiplied)
                   #:items (list (rect #:width 100
                                       #:height 100
                                       #:fill (color 0 255 0 255)
                                       #:stroke #f)))
              (cnt #:post-processing (list 
                                       (to-premultiplied)
                                       (blur 800 2 #t))
                   #:items (list 
                             (rect #:x 0
                                   #:y 0
                                   #:width 50
                                   #:height 50
                                   #:stroke #f
                                   #:fill (color 255 0 0 255))

                             (rect #:x 50
                                   #:y 50
                                   #:width 50
                                   #:height 50
                                   #:stroke #f
                                   #:fill (color 255 0 0 255))))
              )
    #:blend blend/add))

(define (render data)
  (draw item))
