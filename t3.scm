(define testshader (load-shader #f "shader/testcnt.fs"))

(define item
  (cnt #:items (list (rect #:width 50
                           #:height 50))
       #:transform (calc (d) (combine  (scale (+ 1 (* 4 d)) 1) (rotate (* d 6.28)) (translate 100 200)))
       #:post-processing (shader testshader)
       ))

(define (render data)
  (draw (apply-data item data)))

(define (stop? d)
  (> d 1))

(define (update fd d)
  (+ fd d))
