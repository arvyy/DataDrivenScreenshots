;example of overriding properties of any item if its branch id path matches specified predicate
;in a way similar to CSS

;use same rect, whose fill property in different containers will be overriden differently
(define base-rect (rect #:width 100
                        #:height 100
                        #:stroke #f))

(define cnt-1 (cnt #:items (list base-rect)
                   #:id 'cnt-1))

(define cnt-2 (cnt #:items (list base-rect)
                   #:transform (translate 100 0)
                   #:id 'cnt-2))

(define cnt-3 (cnt #:items (list base-rect)
                   #:transform (translate 200 0)
                   #:id 'cnt-3))

(define base-cnt (cnt #:items (list cnt-1 cnt-2 cnt-3)
                      #:transform (translate 200 200)
                      #:id 'root))

; define overrides
(define overrides
  (list
    (override (pattern 'root 'cnt-1 _) ; the `_` means match any. The last part refers to the rectangle itself, so we match on any since rectangle has no id set.
              (rect-o #:fill (color 255 10 10)))
    (override (pattern 'root 'cnt-2 _)
              (rect-o #:fill (color 10 255 10)))
    (override (pattern 'root 'cnt-3 _)
              (rect-o #:fill (color 10 10 255)))))

(define (render data)
  (draw (apply-data base-cnt data overrides)))