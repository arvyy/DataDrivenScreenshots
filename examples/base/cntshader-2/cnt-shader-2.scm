(define dir (string-append (dirname (current-filename)) "/"))

(define texture/grass (load-texture (string-append dir "grass.png")))
(define texture/rock (load-texture (string-append dir "rocks.png")))

(define my-shader (load-shader #f (string-append dir "myshader.fs")))
(define loc/t (get-shader-loc my-shader "t"))
(define loc/cnt-transform (get-shader-loc my-shader "cntTransform"))
(define loc/grass (get-shader-loc my-shader "grass"))
(define loc/rock (get-shader-loc my-shader "rock"))

(define transf (calc (t) 
                     (combine 
                       (translate -0.5 -0.5)
                       (rotate (* t 10))
                       (scale 250 250)
                       (translate 250 250))))

(define item 
  (cnt #:blend blend/mult
       #:items
       (list 
             (cnt #:items 
                  (list (rect #:width 1
                              #:height 1
                              #:fill (color 255 255 255 255)
                              #:stroke #f))
                  #:transform transf)
             
             (cnt #:items 
                  (list (rect #:width 1
                              #:height 1
                              #:stroke #f))
                  #:transform transf
                  #:post-processing 
                  (calc (data)
                        (list (shader my-shader
                                      (list 'vec loc/t (vector data))
                                      (list 'cnt-transform loc/cnt-transform)
                                      (list 'texture loc/grass texture/grass)
                                      (list 'texture loc/rock texture/rock))
                              (blur width 2 #t)
                              (blur height 2 #f)))))))

(define (init-data) 0)

(define (update delta data)
  (+ delta data))

(define (render data)
  (draw (apply-data item data)))
