(define-module (chart effects))

(use-modules
  (chart base)
  (chart stdshader))

(define (effect/shadow item-def width height x-offset y-offset blur-radius) 
  (cnt #:items (list 
                     (cnt #:items (list item-def)
                          #:post-processing (list (grayscale 1)
                                                  (blur height blur-radius #f)
                                                  (blur width blur-radius #t)
                                                  (opacity 0.4))
                          #:transform (translate x-offset y-offset))
                     item-def)))

(export effect/shadow)
