(define-module
  (dds base-impl fieldbind))

(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (dds base-impl override))

; get field value
(define (fv field datum)
  (if (procedure? field)
      (field datum)
      field))

(define (fv* item field-getters datum)
  (map (lambda(get-field)
         (fv (get-field item) datum)) 
       field-getters))

(export fv fv* fv*/overrides)
