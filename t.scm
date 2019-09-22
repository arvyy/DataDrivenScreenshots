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

(define texture (load-texture "mask.png"))
(define s (load-shader #f "shader/testshader.fs"))
(define sl (get-shader-loc s "mask"))

(define item
  (cnt
    #:post-processing (list (shader s `((texture . ,sl) . ,texture)))
    #:items (list (rect #:x 10
        #:y 10
        #:width 700
        #:height 300
        #:color (color 255 0 0))))
  )

(define (init-data)
 0)

(define (update frame-time data)
  0)

(define (key-press key data)
  0)

(define (render data)
  (draw item))
