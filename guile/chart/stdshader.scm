(define-module
  (chart stdshader))

(use-modules
  (chart base))

(define shd/opacity (load-shader #f "shader/opacity.fs"))
(define shd/opacity/alpha (get-shader-loc shd/opacity "alpha"))

(define (opacity alpha)
  (shader shd/opacity (cons shd/opacity/alpha (vector alpha))))

(define shd/grayscale (load-shader #f "shader/grayscale.fs"))
(define shd/grayscale/magnitude (get-shader-loc shd/grayscale "magnitude"))
(define (grayscale magnitude)
  (shader shd/grayscale
          (cons shd/grayscale/magnitude (vector magnitude))))

(define shd/blur (load-shader #f "shader/blur.fs"))
(define shd/blur/resolution (get-shader-loc shd/blur "resolution"))
(define shd/blur/radius (get-shader-loc shd/blur "radius"))
(define shd/blur/dir (get-shader-loc shd/blur "dir"))
(define (blur res radius hor?)
  (shader shd/blur
          (cons shd/blur/resolution (vector res))
          (cons shd/blur/radius (vector radius))
          (cons shd/blur/dir (if hor? (vector 1. 0.) (vector 0. 1.)))))

(export opacity grayscale blur)
