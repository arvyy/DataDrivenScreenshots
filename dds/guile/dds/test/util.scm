(define-module
  (dds test util))

(define (assert value error-message)
  (when (not value)
    (error error-message)))

(define (assert-equal? value expected error-message)
  (assert (equal? value expected) (format #f "Expected ~a, got ~a; " value expected error-message)))

(define (run-cases . cases)
  (for-each
    (lambda(c)
      (format #t "~a\n" (car c))
      ((cdr c)))
    cases))


(export assert assert-equal? run-cases)
