;containers can have shaders applied to them
;some of the inbuilt shaders are opacity, grayscale, blur

(define base-item
  (cnt
    #:items (list
              (rect #:width 60
                    #:height 60
                    #:fill (color 255 10 10))
              (rect #:width 30
                    #:height 30
                    #:x 15
                    #:y 15
                    #:fill (color 10 255 10)))))

;one without shaders for comparisson
(define cnt-0 
  (cnt
    #:items (list base-item)
    #:transform (translate 100 100)))

(define cnt-1 
  (cnt
    #:items (list base-item)
    #:transform (translate 200 100)
    #:post-processing (calc (d) (opacity (- 1 d)))))

(define cnt-2
  (cnt
    #:items (list base-item)
    #:transform (translate 300 100)
    #:post-processing (calc (d) (grayscale d))))

(define cnt-3
  (cnt
    #:items (list base-item)
    #:transform (translate 400 100)
    #:post-processing (calc (d) (list
                                  (blur width (* 1.5 d) #t)
                                  (blur height (* 1.5 d) #f)))))

(define item
  (cnt
    #:items (list cnt-0 cnt-1 cnt-2 cnt-3)))

; data is shader intensity for opacity / blur / grayscale
(define (update delta data)
  (+ data (* delta 0.4)))

(define (stop? data)
  (> data 1))

(define (render data)
  (draw (apply-data item data)))
