;items inside container can be blended using different functions.
;some common ones are explicitly defined -- blend/alpha, blend/add, blend/mult --
;but you can use any OpenGL blend factor eg (cons 'GL_ONE 'GL_ZERO)

(define base-items
  (list
    (rect #:width 80
          #:height 80
          #:fill (color 255 100 100)
          #:stroke #f)
    (rect #:x 20
          #:y 20
          #:width 80
          #:height 80
          #:fill (color 100 255 100)
          #:stroke #f)))

(define cnt-1
  (cnt #:items base-items
       #:transform (translate 100 100)
       #:blend blend/alpha))

(define cnt-2
  (cnt #:items base-items
       #:transform (translate 250 100)
       #:blend blend/add))

(define cnt-3
  (cnt #:items base-items
       #:transform (translate 400 100)
       #:blend blend/mult))

(define item
  (cnt #:items (list cnt-1 cnt-2 cnt-3)))

(define (render data)
  item)
