(define-module 
  (chart derivativeshapes arrow))

(use-modules
  (chart base))

(define (distance start end)
  (sqrt (+ (expt (- (car start) (car end)) 2)
           (expt (- (cdr start) (cdr end)) 2))))

(define* (arrow #:key
                (start '(0 . 0))
                (end '(0 . 0))
                (point-length 5)
                (point-width 3)
                (stroke (color 10 10 10))
                (stroke-width 2))
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
                             (define angle-deg (* rad->deg angle))
                             (rotate (- angle-deg))))
    (cnt
      #:items (list 
                line-item
                point-item)
      #:transform transf))

(export arrow)
