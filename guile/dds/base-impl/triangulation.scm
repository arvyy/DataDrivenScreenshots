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

(define (len^2 a)
  (+ (* (car a) (car a))
     (* (cdr a) (cdr a))))

(define (perp dir-vec)
  (match dir-vec
    ((x . y) (cons (- y)
                   x))))

(define (is-ear? a b c d)
  (define ac (dir a c))
  (define ac* (perp ac))
  (define bc* (perp (dir c b)))
  (define ab (dir a b))
  (define ad (dir a d))
  (define proj (dot ab ac*))
  (and (> proj 0) (not (and (> (dot ad ac*) 0)
                             (> (dot ad bc*) 0)))))

(define (triangulate* triangles points skipped-points max-it)
  (match points
     ((a b c d . rest)
      (cond
        ((is-ear? a b c d)
         (triangulate* (cons (vector a b c) triangles) (append (list a c d) rest) skipped-points max-it))
        (else (triangulate* triangles (cdr points) (cons (car points) skipped-points) max-it))))
     ((a b c)
      (if (or (null? skipped-points) (< max-it 0))
          (cons (vector a b c) triangles)
          ;triangles
          (triangulate* triangles (append (list b c) (reverse skipped-points) (list a)) '() (- max-it 1))))))

(define* (triangulate points #:optional (max-it #f))
  (define max-it* (if max-it max-it (* 20 (length points))))
  (triangulate* '() points '() max-it*))

(export triangulate)
