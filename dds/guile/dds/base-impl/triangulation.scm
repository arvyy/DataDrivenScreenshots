(define-module
  (dds base-impl triangulation))

(use-modules
  (ice-9 match)
  (ice-9 format))

(define (dot a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))

(define (dir a b)
  (cons (- (car b) (car a))
        (- (cdr b) (cdr a))))

(define (perp dir-vec)
  (match dir-vec
    ((x . y) (cons (- y)
                   x))))

(define (is-ear? a b c)
  (define ac (dir a c))
  (define ac* (perp ac))
  (define ab (dir a b))
  (define proj (dot ab ac*))
  (>= proj 0))

(define (triangulate* triangles points skipped-points loop-productive?)
  (match points
     ((a b c . rest)
      (cond
        ((is-ear? a b c)
         (triangulate* (cons (vector a b c) triangles) (cons a (cons c rest)) skipped-points #t))
        (else (triangulate* triangles (cdr points) (cons (car points) skipped-points) loop-productive?))))
     ((a b)
      (if (or (null? skipped-points) (not loop-productive?))
          triangles
          (triangulate* triangles (reverse (cons b (cons a skipped-points))) '() #f)))))

(define (triangulate points)
  (triangulate* '() points '() #f))

(export triangulate)
