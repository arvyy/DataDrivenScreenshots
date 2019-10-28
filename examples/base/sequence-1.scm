(define item (circle #:x (calc ((x _)) x)
                     #:y (calc ((_ y)) y)
                     #:radius 30))

(define states 
  (list
    (list 0 (list 50 50))
    (list 0.5 (list 50 250))
    (list 0.8 (list 250 250))
    (list 1 (list 250 50))
    (list 1 (list 50 50))))

(define (transition a b f)
  (interp a b (ease f)))

(define seq-init (seq->seq/interp (make-seq  states) transition))

(define (init-data) 0)

(define (update delta data)
  (+ delta data))

(define (stop? data)
  (seq-empty? (seq-prune seq-init data)))

(define (render data)
  (draw (apply-data item (seq/interp-get seq-init data))))
