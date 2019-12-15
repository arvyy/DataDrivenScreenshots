(define-module
  (dds base-impl shader))

(use-modules
  (dds base-impl fieldbind)
  (srfi srfi-9))

(define-record-type <shader>
  (make-shader id uniforms)
  shader?
  (id shader-id)
  (uniforms shader-uniforms))

(define-record-type <shader-setup>
  (make-shader-setup builder config)
  shader-setup?
  (builder shader-setup-builder)
  (config shader-setup-config))

(define (shader-setup-apply s)
  ((shader-setup-builder s) (shader-setup-config s)))

(define (shader-setup-map mapper s)
  (make-shader-setup (shader-setup-builder s)
                     (mapper (shader-setup-config s))))

(export <shader> shader-id shader-uniforms make-shader-setup make-shader shader-setup-apply shader-setup-map shader? shader-setup?)
