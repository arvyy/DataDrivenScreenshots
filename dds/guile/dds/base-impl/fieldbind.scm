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

(define (fv*/overrides item item-pred field-getters datum cnt-branch overrides)
  (define (merge-fields fields1 fields2)
    (map (lambda(f1 f2)
           (or f1 f2)) 
         fields1 
         fields2))
  (define applicable-overrides 
    (filter (lambda(override)
              (and (matches-leaf-type? item-pred override)
                   (matches-branch? cnt-branch override)))
            overrides))
  (define overrides/by-field 
    (map (lambda(override)
           (define def (override-item-def override))
           (map (lambda(field-getter) 
                  (field-getter def)) 
                field-getters)) 
         applicable-overrides))
  (define empty-override (map (const #f) field-getters))
  (define override/by-field
    (reduce merge-fields empty-override overrides/by-field))
  (define init-field-values (fv* item field-getters datum))
  (define overriden-values
    (map (lambda(val override)
           (cond
             ((not override) val)
             ((procedure? override) (override val datum))
             (else override))) 
         init-field-values 
         override/by-field))
  overriden-values)

(export fv fv* fv*/overrides)
