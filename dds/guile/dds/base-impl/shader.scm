(define-module
  (dds base-impl shader))

(use-modules
  (srfi srfi-9))

(define-record-type <shader>
  (make-shader id uniforms)
  shader?
  (id shader-id)
  (uniforms shader-uniforms))

(define (bind*-shader shader-def datum)
  (define uniforms 
    (map (lambda(uni)
           (cons (car uni) (fv (cdr uni) datum))) 
         (shader-uniforms shader-def)))
  (make-shader (shader-id shader-def) uniforms))

(define (shader id . uniforms)
  (make-shader id uniforms))

(export <shader> bind*-shader shader)
