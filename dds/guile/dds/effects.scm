(define-module (dds effects))

(use-modules
  (dds base)
  (dds stdshader))

(define (effect/shadow item-def width height x-offset y-offset blur-radius) 
  (cnt #:items (list 
                     (cnt #:items (list item-def)
                          #:post-processing (list (grayscale 1)
                                                  (blur height blur-radius #f)
                                                  (blur width blur-radius #t)
                                                  (opacity 0.4))
                          #:transform (translate x-offset y-offset))
                     item-def)))

(define (effect/outline item-def x y scale*)
  (cnt #:items 
       (list
         (cnt #:items (list item-def)
               #:post-processing (grayscale 1)
               #:transform (combine (translate (- x) (- y))
                                    (scale scale* scale*)
                                    (translate x y)))
         item-def)))

(export effect/shadow effect/outline)
