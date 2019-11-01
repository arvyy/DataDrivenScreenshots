(define-module
  (dds base-impl t-rect))

(use-modules
  (ice-9 match)
  (dds base native)
  (dds base-impl color)
  (dds base-impl primitives-syntax))

(define-record-type+fact+bind* <t-rect> t-rect-fact bind*-t-rect 
  (make-t-rect id x y width height src-x src-y src-width src-height color texture)
  t-rect?
  (id t-rect-id)
  (x t-rect-x )
  (y t-rect-y )
  (width t-rect-width )
  (height t-rect-height )
  (src-x t-rect-src-x )
  (src-y t-rect-src-y )
  (src-width t-rect-src-width )
  (src-height t-rect-src-height )
  (color t-rect-color )
  (texture t-rect-texture ))

(define t-rect (t-rect-fact (make-t-rect #f 0 0 #f #f 0 0 #f #f (color 255 255 255 255) #f)))
(define t-rect-o (t-rect-fact (make-t-rect #f #f #f #f #f #f #f #f #f #f #f)))

(define (draw-t-rect t-rect)
  (match t-rect
         (($ <t-rect> id x y width height sx sy swidth sheight ($ <color> r g b a) texture) 
          (draw-t-rect* x y width height sx sy swidth sheight (vector r g b a) texture))))

;TODO
(define (hover-t-rect? tr mx my)
  #f)

(export <t-rect> t-rect t-rect-o draw-t-rect hover-t-rect?)
