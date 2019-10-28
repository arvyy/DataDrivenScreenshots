(define x-ticks (list-ec (: i 5) i))
(define y-ticks (list-ec (: i 10) i))

(define states
  (map (lambda(s) (list 1 (map (lambda(y x) (cons x y)) s x-ticks)))
       (list
         (list 0 1 2 3 4)
         (list 0 1 3 4 6)
         (list 1 2 3 4 6)
         (list 1 3 3 5 8)
         (list 2 3 4 5 9))))

(define seq (seq->seq/interp (make-seq states) interp))

(define chart-width 400)
(define chart-height 200)

(define x-scale (make-scale-linear (cons 0 5) 
                                   (cons 0 chart-width)))

(define y-scale (make-scale-linear (cons 0 10)
                                   (cons chart-height 0)))


(define x-tick-item
  (cnt #:id '(x tick)
       #:items 
       (list
         (line #:points '((0 . 0) (0 . 10)))
         (text #:text (calc (d) (number->string d))
               #:font-size 10
               #:x 0
               #:x-offset -0.5
               #:y 15))
       #:transform (calc (d) (translate (use-scale x-scale d) 0))))

(define x-axis
  (cnt #:items 
       (list
         (line #:points (list (cons 0 0) (cons chart-width 0)))
         (cnt #:items (cnt-items-tpl
                        x-tick-item
                        x-ticks)))
       #:transform (translate 0 chart-height)))

(define y-tick-item
  (cnt #:id '(y tick)
       #:items 
       (list
         (line #:points '((0 . 0) (-10 . 0)))
         (text #:text (calc (d) (number->string d))
               #:font-size 10
               #:x -30
               #:y 0
               #:y-offset -0.5
               ))
       #:transform (calc (d) (translate 0 (use-scale y-scale d)))))

(define y-axis
  (cnt #:items 
       (list
         (line #:points (list (cons 0 0) (cons 0 chart-height)))
         (cnt #:items (cnt-items-tpl
                        y-tick-item
                        y-ticks)))))

(define line-series
  (line #:points (calc (d) 
                       (map (calc((x . y))
                                  (cons (use-scale x-scale x)
                                        (use-scale y-scale y))) 
                                d))
        #:stroke-width 3))

(define bg-render-texture (create-render-texture chart-width chart-height))
(define bg-texture (render-texture->texture bg-render-texture))

(define bg (t-rect #:texture bg-texture
                   #:width chart-width
                   #:height chart-height))

(define chart
  (cnt #:items (list
                 (cnt #:items (list bg)
                      #:post-processing (list (opacity 0.1)))
                 x-axis
                 y-axis
                 line-series)
       #:transform (translate 50 50)))

(define (init-render)
  (clear-render-texture bg-render-texture))
(define (init-data) 0)
(define (update delta data)
  (+ delta data))
(define (stop? data)
  (seq-empty? (seq-prune seq data)))

(define overrides
  (list
    (override (pattern '(x tick))
              (list (line-o #:stroke (color 255 0 0))))
    (override (pattern '(y tick))
              (list (line-o #:stroke (color 0 255 0))))
    (override (pattern 'tick)
              (list (line-o #:stroke-width 4)))))

(define (render data)
  (define line/applied (apply-data line-series (seq/interp-get seq data)))
  (define chart/applied (apply-data chart (seq/interp-get seq data) overrides))
  (draw line/applied bg-render-texture)
  (draw chart/applied))
