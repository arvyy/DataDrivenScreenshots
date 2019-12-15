(define-module
  (dds gif-impl slider))

(use-modules
  (dds base))

(define* (slider #:key 
                 (width 0)
                 (knob-pos 0))
    (define knob-x (compute (width knob-pos) (* width knob-pos)))
    (define bg (line #:points (list (cons 0 0) (cons width 0))
                     #:stroke-width 8
                     #:stroke (color 200 200 200)))
    (define bg/passed (line #:points (compute (knob-x)
                                        (list (cons 0 0) (cons knob-x 0)))
                            #:stroke-width 6
                            #:stroke (color 200 200 255)))
    (define knob (circle #:fill (color 250 250 250)
                         #:stroke (color 200 200 200)
                         #:radius 8
                         #:y 0
                         #:x knob-x))
    (define c (cnt #:items (list bg bg/passed knob)))
    c)

(export slider)
