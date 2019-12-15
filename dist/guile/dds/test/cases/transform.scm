(use-modules
  (dds base-impl transform)
  (srfi srfi-64))

(define (test-scale)
  (define scale1 (scale 2 2))
  (define scale2 (scale 0.5 0.5))
  (test-equal "transform back and forth" (combine scale1 scale2) (scale 1. 1.))
  (test-equal "applying transformation on point" (transform-point scale1 (cons 1. 1.)) (cons 2. 2.)))

(define (test-translate)
  (define t1 (translate 10 0))
  (define t2 (translate -10 0))
  (test-equal "transform back and forth" (combine t1 t2) (translate 0. 0.))
  (test-equal "applying transformation on point" (transform-point t1 (cons 0. 0.)) (cons 10. 0.)))

(define (test-rotate)
  (define r1 (rotate 90))
  (define r2 (rotate -90))
  (test-equal "transform back and forth" (combine r1 r2) (translate 0. 0.))
  (let ((p (transform-point r1 (cons 1. 0.))))
    (test-assert "apply transformation on point /x" (< (abs (car p)) 0.01))
    (test-assert  "apply transformation on point /y"(< (abs (- (cdr p) -1)) 0.01))))

(test-begin "Transform test")
(test-group "test-scale"
            (test-scale))
(test-group "test-rotate"
            (test-rotate))
(test-group "test-translate"
            (test-translate))
(test-end "Transform test")
