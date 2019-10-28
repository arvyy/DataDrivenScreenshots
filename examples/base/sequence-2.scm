(define item
  (cnt #:items (cnt-items-tpl
              (circle #:x (calc ((_ x)) (* 40 x))
                      #:y 0
                      #:radius (calc ((r _)) (* 10 r)))
              identity)))

(define item/id
  (cnt #:transform (translate 100 100)
       #:items (list item)))

(define item/no-id
  (cnt #:transform (translate 100 150)
       #:items (list item)))

(define states
  (list
    (list 0 (list))
    (list 1 (list 1 3 5 6))
    (list 1 (list 2 5 7 8))
    (list 1 (list))))

(define seq (make-seq states))

(define (transition a b f)
  (interp a b (ease f)))

(define bridge/id (interp-bridge 
                    #:enter (lambda (d) (list 0 d))
                    #:update (lambda (d) (list 1 d))
                    #:exit (lambda (d) (list 0 d))
                    #:id (lambda (d) d)))

(define bridge/no-id (interp-bridge 
                    #:enter (lambda (d) (list 0 d))
                    #:update (lambda (d) (list 1 d))
                    #:exit (lambda (d) (list 0 d))))

(define seq/interp/id (seq->seq/interp seq transition bridge/id))
(define seq/interp/no-id (seq->seq/interp seq transition bridge/no-id))

(define (init-data) 0)
(define (update delta data)
  (+ delta data))
(define (stop? data)
  (seq-empty? (seq-prune seq/interp/id data)))
(define (render data)
  (draw (apply-data item/id (seq/interp-get seq/interp/id data)))
  (draw (apply-data item/no-id (seq/interp-get seq/interp/no-id data))))
