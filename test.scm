(add-to-load-path (string-append
                    (dirname (current-filename))
                    "/guile"))

(define-module (test))

(use-modules 
  (srfi srfi-9)
  (ice-9 match)
  (ice-9 format)
  (chart base native)
  (chart base)
  (chart scale)
  (system vm trace))

(define domain '("1" "2" "3"))

(define x-padding 50)

(define scale-x
  (make-scale-point domain `(,x-padding . 300)))

(define scale-y
  (make-scale-linear '(15 . 0) '(0 . 200)))

(define error-bar-shader (load-shader "errorbar.vs" #f))
(define minHeight (get-shader-loc error-bar-shader "minheight"))
(define maxHeight (get-shader-loc error-bar-shader "maxheight"))

(define axis-x-def
  (let* ((base-line (rect 
                      #:x 0
                      #:width (let ((r (scale-range scale-x)))
                                (+ x-padding (cdr r)))
                      #:height 1))
         (tick-text (text (calc (x) x)
                          #:y 10))
         (tick-line (rect #:width 1
                          #:height 5))
         (tick (cnt #:items (list tick-text tick-line)
                    #:transform (calc (x) (list (translate (use-scale scale-x x) 0)))))
         (ticks (cnt #:items (cnt-items-tpl tick (const domain)))))
    (cnt #:transform (list (translate 0 (cdr (scale-range scale-y))))
         #:items (list base-line ticks))))

(define axis-y-def
  (let* ((base-line (rect
                      #:width 1
                      #:height (let ((r (scale-range scale-y)))
                                 (cdr r))))
         (tick-text (text (calc (y) (number->string y))
                          #:x -15
                          #:y -8))
         (tick-line (rect #:x -5
                          #:width 5
                          #:height 1))
         (tick (cnt #:items (list tick-text tick-line)
                    #:transform (calc (y) (list (translate 0 (use-scale scale-y y))))))
         (ticks (cnt #:items (cnt-items-tpl tick (let ((d (scale-domain scale-y)))
                                                   (const (list (car d) (cdr d))))))))
    (cnt #:items (list base-line ticks))))

(define bars-def
  (let* ((main-bar (rect
                #:x -10
                #:y (calc ((_ y _)) (use-scale scale-y y))
                #:width 20
                #:height (calc ((_ y _)) (abs (- (use-scale scale-y 0) 
                                                 (use-scale scale-y y))))))
         (marker (rect
                   #:x -10
                   #:y (calc ((_ y err)) (use-scale scale-y (+ y (* 0.5 err))))
                   #:width 20
                   #:height 1
                   #:color (color 0 0 0)))
         (error-bar (cnt 
                      #:shader (calc ((_ y err)) (let ()
                                                   (define shader (cons error-bar-shader
                                                       (list
                                                         (cons minHeight (vector (use-scale scale-y (+ y err))))
                                                         (cons maxHeight (vector (use-scale scale-y y))))))
                                                   shader))
                      #:items (list
                                (rect
                                  #:x -10
                                  #:y (calc ((_ y err)) (use-scale scale-y (+ y err)))
                                  #:width 20
                                  #:height (calc ((_ y err)) (abs (- (use-scale scale-y y)
                                                                     (use-scale scale-y (+ y err)))))))))
         (bar (cnt
                #:items (list main-bar error-bar marker)
                #:transform (calc ((x _ _)) (list (translate (use-scale scale-x x) 0)))))
         (data-interpolator (lambda (time)
                              (lambda(old-datum new-datum)
                                (match-let (((c1 v1 e1) old-datum)
                                            ((_ v2 e2) new-datum))
                                           (list c1 
                                                 (use-scale (make-scale-linear '(0 . 1) `(,v1 . ,v2)) time)
                                                 (use-scale (make-scale-linear '(0 . 1) `(,e1 . ,e2)) time))))))
         (bars (cnt
                 #:items (calc ((time old new))
                               (define interp-data (map (data-interpolator time) old new))
                               (map (lambda(datum)
                                      (bind* bar datum)) 
                                    interp-data)))))
    bars))

(define chart-item
  (cnt #:transform (calc ((t . _)) (list (translate 100 100)))
       #:items (cnt-items-transf (list axis-x-def axis-y-def bars-def) cdr)))

(define (gen-data)
  (let it ((lst '())
           (domain domain))
    (if (null? domain) lst
        (it (cons (list (car domain) 
                        (+ (random 4.0) 6.0)
                        (random 3.0)) lst)
            (cdr domain)))))

(define (init-data)
  (define d (gen-data))
  (list 0.0 0.0 d d))

(define (change-data data)
  (match data
         ((total time old new) (list total 0.0 new (gen-data)))))

(define (update frame-time data)
  (match data
         ((total time old new) (list (+ total frame-time) (min (+ (/ frame-time 0.3) time) 1) old new))))

(define (key-press key data)
  (change-data data))

(define (render data)
  (draw (apply-data chart-item data)))

(export init-data update key-press render)
