(define-module
  (dds stdshader))

(use-modules
  (ice-9 textual-ports)
  (ice-9 format)
  (ice-9 match)
  (dds base))

(define (get-shader-path filename)
  (define path (%search-load-path filename))
  path)

(define (get-shader filename)
  (load-shader #f (get-shader-path filename)))

(define shd/opacity (delay (get-shader "dds/shaders/opacity.fs")))
(define shd/opacity/alpha (delay (get-shader-loc (force shd/opacity) "alpha")))
(define opacity-builder
    (lambda (config)
      (match config
        (('alpha alpha) 
         (make-shader (force shd/opacity) (list (list 'vec (force shd/opacity/alpha) (vector alpha))))))))

(define (opacity alpha)
  (make-shader-setup opacity-builder (list 'alpha alpha)))

(define shd/prem (delay (get-shader "dds/shaders/premultiply.fs")))
(define (to-premultiplied)
  (define (builder config)
    (make-shader (force shd/prem) (list)))
  (make-shader-setup builder 'premultiply))

(define shd/unprem (delay (get-shader "dds/shaders/unpremultiply.fs")))
(define (from-premultiplied)
  (define (builder config)
    (make-shader (force shd/prem) (list)))
  (make-shader-setup builder 'unpremultiply))

(define shd/grayscale (delay (get-shader "dds/shaders/grayscale.fs")))
(define shd/grayscale/mag (delay (get-shader-loc (force shd/grayscale) "magnitude")))
(define (grayscale k)
  (define builder
      (lambda (config)
        (match config
           (('grayscale k)
            (make-shader (force shd/grayscale) (list (list 'vec (force shd/grayscale/mag) (vector k))))))))
  (make-shader-setup builder (list 'grayscale k)))

(define shd/blur (delay (get-shader "dds/shaders/blur.fs")))
(define shd/blur/resolution (delay (get-shader-loc (force shd/blur) "resolution")))
(define shd/blur/radius (delay (get-shader-loc (force shd/blur) "radius")))
(define shd/blur/dir (delay (get-shader-loc (force shd/blur) "dir")))
(define (blur resolution radius hor?)
  (define (builder config)
    (match config
      (('blur res radius hor?)
       (make-shader (force shd/blur) 
                    (list
                      (list 'vec (force shd/blur/resolution) (vector res))
                      (list 'vec (force shd/blur/radius) (vector radius))
                      (list 'vec (force shd/blur/dir) (if hor? (vector 1. 0.) (vector 0. 1.))))))))
  (make-shader-setup builder (list 'blur resolution radius hor?)))

(export opacity grayscale blur to-premultiplied from-premultiplied)
