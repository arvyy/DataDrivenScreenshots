(add-to-load-path (string-append
                    (dirname (current-filename))
                    "/guile"))

(use-modules
  (srfi srfi-42)
  (ice-9 format)
  (chart base)
  (chart sequence)
  (chart stdshader)
  (chart interp))

(define item
  (cnt #:transform (combine (translate 20 100) (scale 3 3))
       #:id 'foo
       #:items (cnt-items-tpl 
                 (cnt 
                   #:post-processing  (calc ((i . a)) 
                                             (list
                                               (grayscale (/ (- 10 i) 10))
                                               (blur 800. (/ i 5) #t)
                                               (blur 450. (/ i 5) #f)
                                               (opacity a)))
                   #:transform (calc ((x . a)) (combine (translate (inexact->exact (truncate (* 30 x))) 0) (rotate x)))
                   #:items (list
                             (text #:text (calc (d) (number->string (inexact->exact (car d))))
                                   #:font-size 20
                                   ;#:x (calc (d) (inexact->exact (truncate (* 30 (car d)))))
                                   ;#:y (calc (d) (inexact->exact (truncate (* 10 (cdr d)))))
                                   )
                             )
                   )
                 identity)))

(define overrides
  (list (override (pattern _ ...)
                  (text-o #:color (color 0 255 0 255)
                          #:font-size 4))
        (override (pattern 'foo _ ...)
                  (text-o #:color (color 255 0 0 255)))))

(define data 
  (map (lambda(i) (list (cons i 1.0)))
       (list-ec (: i 10) i)))

(define bridge
  (let ()
    (define (enter d) (cons (car d) 0))
    (define (exit d) (cons (car d) 0))
    (define (id d) (car d))
    (interp-bridge enter exit id)))

(define (transition a* b* f)
  (define-values (a b) (bridge a* b*))
  (map (lambda(a* b*)
         (interp a* b* (ease f))) 
       a 
       b))

(define wrap
  (let it ((w (state-wrap transition (car data)))
           (d (cdr data)))
    (cond
      ((null? d) w)
      (else (it (state-wrap-enqueue w (car d) 1.0) 
                (cdr d))))))

(define (init-data)
  wrap)

(define (update frame-time data)
  (state-wrap-update data frame-time))

(define (key-press key data)
  (state-wrap-next data #t))

(define (render data)
  (draw (apply-data item (state-wrap-get data) overrides)))
