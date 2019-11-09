(define (make-number-datum number max-number)
  (define from (/ (+ number -1) max-number))
  (define to (/ (+ number 1) max-number))
  (lambda (t)
    (define a (interp
      90
      -90
      (start-end from to (ease t))))
    (if a 
     (vector a number)   
     #f)))

(define (make-numbers-data max)
  (list-ec (: i max) (make-number-datum i max)))

(define data (make-numbers-data 10))

(define number 
  (text #:text (calc (#(_ num)) (number->string num))
        #:font-size 30))
(define number-cnt
  (cnt #:items (list number)
       #:transform (calc (#(a _))
                     (combine (translate 100 0)
                              (rotate a)))))

(define container 
  (cnt #:items (cnt-items-tpl
                 number-cnt
                 identity)
       #:transform (translate 200 100)))

(define (render t)
  (draw (apply-data container 
                    (filter (lambda(a) a) 
                            (map (lambda(datum)
                                     (datum (/ t 10))) 
                                 data)))))

(define (stop? t)
  (>= t 10))
