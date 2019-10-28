;changing bound data

(define cell (rect #:width 20
                   #:height 20
                   #:stroke #f
                   #:fill (calc ((red green)) (color (floor (inexact->exact red))
                                                     (floor (inexact->exact green))
                                                     10))))

;simple data transform.
;transform i, j into offset and colors for the cell
(define cell-cnt
  (cnt #:items 
       (cnt-items-transf 
         (list cell) ;items 
         (calc ((n i j)) ;new data
               (list (* i (/ 255 n))
                     (* j (/ 255 n)))))
       #:transform (calc ((_ _ j)) (translate (* j 20) 0))))

;transform data into list. 
;Then for each datum in list, add specified item / template with that datum as data.
(define row (cnt #:items (cnt-items-tpl 
                           cell-cnt ;template item
                           (calc ((n i)) ;data transformation into a list
                                 (list-ec (: j n) (list n i j))))
                 #:transform (calc ((_ i)) 
                                   (translate 0 (* 20 i)))))

(define table (cnt #:items (cnt-items-tpl
                             row
                             (calc (n) (list-ec (: i n) (list n i))))
                   #:transform (translate 200 100)))

(define (init-data)
  1)

(define (update delta data)
  (+ (* 2 delta) data))

(define (stop? data)
  (> data 8))

(define (render data)
  (define n (floor data))
  (apply-data table n))
