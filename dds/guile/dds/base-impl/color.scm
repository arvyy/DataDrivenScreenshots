(define-module
  (dds base-impl color))

(use-modules
  (ice-9 match)
  (srfi srfi-9))

(define-record-type <color>
  (make-color r g b a)
  color?
  (r color-r)
  (g color-g)
  (b color-b)
  (a color-a))

(define (color->vec c)
  (match c
         (($ <color> r g b a) (vector r g b a))
         (_ #f)))

(define* (color r g b #:optional (a 255))
         (make-color r g b a))

(export <color> color->vec color)
