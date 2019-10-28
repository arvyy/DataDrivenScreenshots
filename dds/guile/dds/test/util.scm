(define-module
  (dds test util))

(define (assert value error-message)
  (when (not value)
    (error error-message)))

(define (assert-equal? value expected error-message)
  (assert (equal? value expected) error-message))

(export assert assert-equal?)
