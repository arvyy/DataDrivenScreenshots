(use-modules
  (chart effects))

(define base-color (color 50 50 255))
(define shade-color (color 0 0 230))

(define bubble-r 34)
(define bubble-r* 30)
(define radius 100)

(define start-circles
  (map
    (lambda (i)
      (define radius (if (= 1 i) 0 1))
      (define angle (* i (/ 360 8)))
      (list radius angle))
    (list-ec (: i 6) (+ 1 i))))

(define start-rects
  ; radius angle roundness extra-height opacity
  (list 
    (list 1 (* 7 360 (/ 8)) 1 0 1)
    (list 1 360 0.2 2 1)))

(define start-data (vector start-rects start-circles))

(define end-circles
  (map
    (lambda (i)
      (define radius 1)
      (define angle (* i (/ 360 8)))
      (list radius angle))
    (list-ec (: i 6) (+ 2 i))))

(define end-rects
  (list 
    (list 1 360 0.2 2 1)
    (list 1 360 0.2 2 0.7)))

(define end-data (vector end-rects end-circles))

(define wrap (let* ((tr (lambda (a b f) (interp a b (ease f))))
                    (w (state-wrap tr start-data)))
               (state-wrap-enqueue w end-data 0.25)))

(define bubble
  (let ((base (cnt
                #:items (list
                          (circle #:radius bubble-r
                                  #:fill shade-color)
                          (circle #:radius bubble-r*
                                  #:y (- bubble-r* bubble-r)
                                  #:fill base-color
                                  #:stroke #f)))))
    (effect/outline base 0 0 1.1)))

(define rect-bubble
  (let ((base (cnt
    #:items 
    (cnt-items-tpl 
      (cnt
        #:transform
        (calc ((radius* angle _ ...))
              (combine (rotate (- angle)) (scale radius* radius*) (translate 0 radius) (rotate angle)))
        #:items 
        (list (cnt
                #:items (list (rect #:x (- bubble-r)
                                    #:y (calc ((_ _ _ extra-height _)) (- (* (+ 1 extra-height) bubble-r)))
                                    #:width (* 2 bubble-r)
                                    #:height (calc ((_ _ _ extra-height _)) (* (+ 2 extra-height) bubble-r))
                                    #:fill base-color
                                    #:roundness (calc ((_ _ roundness _ ...)) roundness)))
                #:post-processing (calc ((_ _ _ _ op)) (list 
                                                         (opacity op))))))
      (lambda (d) (vector-ref d 0))))))
    (effect/outline base 0 0 1.1)))

(define bubble-circle
  (cnt
    #:items (cnt-items-tpl
              (cnt #:items (list bubble)
                   #:transform (calc ((radius* angle))
                                     (combine (scale radius* radius*) (translate 0 radius) (rotate angle))))
              (calc (d) (vector-ref d 1))))) 

(define item 
  (cnt #:items (list rect-bubble bubble-circle)
       #:transform (combine (rotate 90) (translate 200 200))))

(define (init-data) wrap)

(define (update frame data)
  (state-wrap-update data frame))

(define (stop? data)
  (state-wrap-empty? data))

(define (render data)
  (define applied (apply-data item (state-wrap-get data)))
  (draw (effect/shadow applied width height 0 10 3)))
