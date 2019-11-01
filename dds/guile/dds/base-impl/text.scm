(define-module
  (dds base-impl text))

(use-modules
  (dds base native)
  (dds base-impl color)
  (dds base-impl primitives-syntax))

(define-record-type+fact+bind* <text> text-fact bind*-text 
  (make-text id text x y x-offset y-offset font font-size spacing color)
  text?
  (id text-id)
  (text text-text )
  (x text-x )
  (y text-y )
  (x-offset text-x-offset )
  (y-offset text-y-offset )
  (font text-font )
  (font-size text-font-size )
  (spacing text-spacing )
  (color text-color ))

(define (draw-text txt)
  (match txt
         (($ <text> id text x y x-offset y-offset font font-size spacing ($ <color> r g b a)) (draw-text* text x y x-offset y-offset font font-size spacing (vector r g b a)))))

;TODO
(define (hover-text? text mx my)
  #f)

(define text (text-fact (make-text #f "" 0 0 0 0 #f 12 2 (color 10 10 10 255))))
(define text-o (text-fact (make-text #f #f #f #f #f #f #f #f #f #f)))

(define (text-size text-def)
  (match text-def
    (($ <text> id text x y x-offset y-offset font font-size spacing ($ <color> r g b a))
     (text-size* text font font-size spacing))))

(export <text> text text-o hover-text? text-size)
