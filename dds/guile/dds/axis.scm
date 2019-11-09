(define-module (dds axis))

(use-modules
  (ice-9 match)
  (dds base))

(define hor-tick
  (cnt
    #:transform (calc ((d . _)) (translate d 0))
    #:items (list 
              (line #:points '((0 . 0)
                               (0 . 5)))
              (text #:text (calc ((_ . label)) label)
                    #:x 0
                    #:y 10
                    #:x-offset -0.5
                    #:y-offset 1))))

(define* (hor-axis #:key
               (ticks '()) ; (cons value label)
               (length 0))
   (define base-line-points (compute (length)
                              (list (cons 0 0) (cons length 0))))
   (define base-line (line #:points base-line-points))
   (cnt #:items
        (cnt-items-combine 
          (list base-line)
          (cnt-items-tpl
            hor-tick
            ticks))))

(export hor-tick hor-axis ver-tick ver-axis)
