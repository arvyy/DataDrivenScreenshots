(use-modules
  (dds stdshader)
  (dds blend))

(define width 640)
(define height 480)

(define dir (string-append (dirname (current-filename)) "/"))

(define texture/grass (delay (load-texture (string-append dir "grass.png"))))
(define texture/rock (delay (load-texture (string-append dir "rocks.png"))))

(define my-shader (delay (load-shader #f (string-append dir "myshader.fs"))))
(define loc/t (delay (get-shader-loc (force my-shader) "t")))
(define loc/cnt-transform (delay (get-shader-loc (force my-shader) "cntTransform")))
(define loc/grass (delay (get-shader-loc (force my-shader) "grass")))
(define loc/rock (delay (get-shader-loc (force my-shader) "rock")))

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
                        (list (make-shader (force my-shader)
                                      (list
                                        (list 'vec (force loc/t) (vector data))
                                        (list 'cnt-transform (force loc/cnt-transform))
                                        (list 'texture (force loc/grass) (force texture/grass))
                                        (list 'texture (force loc/rock) (force texture/rock))))
                              (blur width 2 #t)
                              (blur height 2 #f)))))))

(define (init-data) 0)

(define (update delta data)
  (+ delta data))

(define get-dds-gif
  (dds-gif #:item-getter (lambda(t)
                           (apply-data item t))
           #:duration 20
           #:width width
           #:height height))
