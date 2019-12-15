(use-modules
  (srfi srfi-64)
  (dds base)
  (dds base-impl line))

(test-begin "Line test")

(test-group "hover"
            (define l (line #:points '((0 . 0)
                                       (10 . 10)
                                       (10 . 20)
                                       (20 . 20)
                                       (20 . 0))))
            (define l+fill (override-line l (line-o #:fill (color 0 0 0))))
            (test-assert "shouldn't hover without fill" (null? (hover-line? l 15 15)))
            (test-assert "should hover with fill" (not (null? (hover-line? l+fill 15 15))))
            (test-assert "shouldn't hover outside" (null? (hover-line? l+fill -10 -1))))

(test-end "Line test")
