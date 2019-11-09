(use-modules
  (dds bezier))

(define r 100)


(define (semi-circle color-a color-b)
  (cnt #:items
       (list
         (circle #:start-angle 0
                 #:end-angle 45
                 #:fill color-a
                 #:stroke #f
                 #:radius r)
         (circle #:start-angle 45
                 #:end-angle 180
                 #:fill color-b
                 #:stroke #f
                 #:radius r)
         (bezier #:points `((p ,(* r (cos (* (/ 45 180) 3.14)))
                               ,(* r (sin (* (/ 45 180) 3.14))))
                            (c ,(* r 1) 0)
                            (p 0 0))
                 #:fill color-a
                 #:stroke #f))))

(define full-circle
  (let ()
    (define color-a (color 250 0 0))
    (define color-b (color 0 250 0))
    (cnt #:items (list
                   (semi-circle color-a color-b)
                   (cnt #:transform (rotate 180)
                        #:items
                        (list
                          (semi-circle color-b color-a)))
                   ))))
(define part
  (let ()
    (define (pos-angle d)
      (* (/ d 6) 360))
    (define (pos d)
      (define angle (pos-angle d))
      (cons (* r (cos (* (/ angle 180) 3.14)))
            (* r (sin (* (/ angle 180) 3.14)))))
    (cnt
      #:items (list full-circle)
      #:transform (calc (d)
                        (define angle (pos-angle d))
                        (define p (pos d))
                        (combine (rotate angle) (translate (car p) (cdr p)))))))

(define view
  (cnt #:items 
       (cnt-items-combine 
         (list )
         (cnt-items-tpl part (lambda (d) (list d (+ 3 d)))))
       #:transform (translate 400 200)))

(define (render data)
  (draw (apply-data view data)))
