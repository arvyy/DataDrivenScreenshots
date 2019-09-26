#!
(define-module (chart axis))

(use-modules
  (ice-9 format)
  (chart base)
  (chart scale))

(define (make-hor-axis domain range )
  (define sc (scale-l domain range))
  (define tick
    (cnt #:items (list (rect #:height 5
                             #:color (color 10 10 10)
                             #:width 2)
                       (text (calc d -> (number->string d))
                             #:font-size 8
                             #:y 8))
         #:transform (calc d -> (list (translate (sc d) 0)))))
  (define axis (cnt #:items (list (rect #:width (- (cdr range) (car range))
                           #:height 2
                           #:x (car range)
                           #:y 0
                           #:color (color 10 10 10))
                     (cnt #:items (cnt-items-tpl tick (lambda d (list (car domain) (cdr domain))))))))
  axis)

(export make-hor-axis)
!#
