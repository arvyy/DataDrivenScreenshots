(define-module
  (dds base-impl text))

(use-modules
  (ice-9 match)
  (dds base-impl native)
  (dds base-impl color)
  (dds base-impl primitives-syntax))

(define-record-type+fact+bind* <text> text-fact bind*-text override-text
  (make-text data id text x y x-offset y-offset font font-size spacing color)
  text?
  ((data text-data))
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
         (($ <text> data id text x y x-offset y-offset font font-size spacing ($ <color> r g b a)) (draw-text* text x y x-offset y-offset font font-size spacing (vector r g b a)))))

;TODO
(define (hover-text? text mx my)
  (define size (text-size text))
  (define x (+ (text-x text)
               (* (text-x-offset text) (car size))))
  (define y (+ (text-y text)
               (* (text-y-offset text) (cdr size))))
  (define hover 
    (and (>= mx x)
         (>= my y)
         (<= mx (+ x (car size)))
         (<= my (+ y (cdr size)))))
  (if hover (list text) '()))

(define text (text-fact (make-text #f #f "" 0 0 0 0 #f 12 2 (color 10 10 10 255))))
(define text-o (text-fact (make-text #f #f #f #f #f #f #f #f #f #f #f)))

(define (text-size text-def)
  (match text-def
    (($ <text> data id text x y x-offset y-offset font font-size spacing ($ <color> r g b a))
     (text-size* text font font-size spacing))))

(export <text> text text-o text? hover-text? text-size draw-text override-text)
