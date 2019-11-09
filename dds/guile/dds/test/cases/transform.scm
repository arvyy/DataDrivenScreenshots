(use-modules
  (dds base-impl transform)
  (dds test util))

(define (test-scale)
  (define scale1 (scale 2 2))
  (define scale2 (scale 0.5 0.5))
  (assert-equal? (combine scale1 scale2) (scale 1. 1.) "transform back and forth")
  (assert-equal? (transform-point scale1 (cons 1. 1.)) (cons 2. 2.) "applying transformation on point"))

(define (test-translate)
  (define t1 (translate 10 0))
  (define t2 (translate -10 0))
  (assert-equal? (combine t1 t2) (translate 0. 0.) "transform back and forth")
  (assert-equal? (transform-point t1 (cons 0. 0.)) (cons 10. 0.) "applying transformation on point"))

(define (test-rotate)
  (define r1 (rotate 90))
  (define r2 (rotate -90))
  (assert-equal? (combine r1 r2) (translate 0. 0.) "transform back and forth")
  (let ((p (transform-point r1 (cons 1. 0.))))
    (assert (< (abs (car p)) 0.01) "apply transformation on point /x")
    (assert (< (abs (- (cdr p) -1)) 0.01) "apply transformation on point /y")))

(run-cases
  (cons "test-scale" test-scale)
  (cons "test-translate" test-translate)
  (cons "test-rotate" test-rotate))
