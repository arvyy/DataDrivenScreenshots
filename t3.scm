(use-modules
  (chart effects))

(define testshader (load-shader #f "shader/testcnt.fs"))
(define cnt-loc (get-shader-loc testshader "cntTransform"))

(define texture (create-render-texture 64 64))
(define t (render-texture->texture texture))

(define (init-render)
  (draw (rect #:width 32
              #:height 64
              #:color (color 255 0 0))
        texture)
  (draw (rect #:x 32
              #:width 32
              #:height 64
              #:color (color 0 255 255))
        texture))

(define item
  (cnt #:items (list (rect #:width 50
                           #:height 50
                           #:texture t
                           #:color (color 255 255 0)
                           ))
       #:transform (calc (d) (combine  (scale (+ 1 (* 4 d)) 1) (rotate (* d 6.28)) (translate 100 200)))
       ;#:post-processing (shader testshader (list 'cnt-transform cnt-loc))
       ))

(define (render data)
  (draw (apply-data (effect/shadow item width height 0 20 4) data)))

(define (stop? d)
  (> d 1))

(define (update fd d)
  (+ fd d))
