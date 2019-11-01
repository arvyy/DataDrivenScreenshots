(use-modules
  (dds triangulation))

(define (render data)
  (draw (text #:text (format #f "~a" (triangulate '((0 . 0) 
                                                    (10 . 0) 
                                                    (13 . 5)
                                                    (10 . 10) 
                                                    (0 . 10)))))))
