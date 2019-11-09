;example on how to create your own custom shape, whose properties can be both exact values, as well as functions of data
;note however this approach expands into primitives, 
;and there's no new override type created (eg how you `rect` has `rect-o` override)

;custom types should probably be as a seprate module in their own file
;note, file must in folder that's picked up as source location. Folder `extensions` is loaded automatically, 
;you can add others with (add-to-load-path pathnamehere)
;(define-module (extensiongroupname arrow))

(use-modules
  (dds mathutil))

(define (distance start end)
  (sqrt (+ (expt (- (car start) (car end)) 2)
           (expt (- (cdr start) (cdr end)) 2))))

;define function with key arguments & default values
(define* (arrow #:key
                (start '(0 . 0))
                (end '(0 . 0))
                (point-length 5)
                (point-width 3)
                (stroke (color 10 10 10))
                (stroke-width 2))

    ;any calculations that even partially depend on values that could be functions of data (eg `arrow` function arguments), 
    ;must be done using `compute` syntax or `compute*` procedure
    ;parameters are exact values inside compute body; ie it's executed when data was already applied to the parameters.
    ;compute itself returns function of data, and thus can be itself as parameter to other compute value. 
    (define line-length (compute (start end point-length)
                                   (- (distance start end) point-length)))
    (define points-line (compute (line-length) 
                                   (list
                                     (cons 0 0)
                                     (cons line-length 0))))
    (define a (compute (line-length point-width)
                         (cons line-length (- (* point-width 0.5)))))
    (define b (compute (line-length point-width)
                         (cons line-length (* point-width 0.5))))
    (define c (compute (line-length point-length)
                       (cons (+ line-length point-length) 0)))
    (define points-triangle (compute (a b c) (list a b c)))
    (define line-item
      (line #:stroke stroke
            #:stroke-width stroke-width
            #:points points-line))
    (define point-item
      (triangle #:stroke stroke
                #:stroke-width stroke-width
                #:points points-triangle))
    (define transf (compute (start end)
                             (define angle (atan
                                             (- (cdr end) (cdr start))
                                             (- (car end) (car start))))
                             (define angle-deg (->deg angle))
                             (rotate (- angle-deg))))
    (cnt
      #:items (list 
                line-item
                point-item)
      #:transform transf))

;if creating module, don't forget to export the binding
;(export arrow)

;;;;;;;;;;;;;;;; end of new type

;example use
(define arrow-item
  (arrow #:end (lambda(d) (cons 100 d))
         #:point-length 20
         #:point-width 20))

(define item
  (cnt #:items (list arrow-item)
       #:transform (translate 100 100)))

(define (init-data) 0)
(define (update fl data)
  (+ (* fl 50) data))

(define (stop? data)
  (> data 100))

(define (render data)
  (draw (apply-data item data)))
