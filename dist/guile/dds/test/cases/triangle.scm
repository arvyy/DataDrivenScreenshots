(use-modules
  (srfi srfi-64)
  (dds base)
  (dds base-impl triangle))

(test-begin "Triangle test")

(test-group "hover-points"
   (define-values (a b c) (values '(0 . 0) '(0 . 10) '(10 . 0)))
   (test-assert (hover-triangle-points? a b c 2 2))
   (test-assert (hover-triangle-points? a b c 0 0))
   (test-assert (not (hover-triangle-points? a b c -1 -1)))
   (test-assert (not (hover-triangle-points? b c a -1 -1)))
   (test-assert (not (hover-triangle-points? c a b -1 -1))))

(test-group "hover-triangle"
   (define t (triangle #:points '((0 . 0)
                                  (0 . 10)
                                  (10 . 0))))
   (test-assert (not (null? (hover-triangle? t 2 2))))
   (test-assert (null? (hover-triangle? t -1 -1))))

(test-end "Triangle test")
