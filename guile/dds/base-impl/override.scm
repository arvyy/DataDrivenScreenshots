(define-module
  (dds base-impl override))

;(define override cons)
;
;(define (matches-leaf-type? item-pred override)
;  (item-pred (override-item-def override)))
;
;(define (override-item-def o)
;  (cdr o))
;
;(define (override-pred o)
;  (car o))
;
;(define (matches-branch? cnt-branch override)
;  ((car override) cnt-branch))
;
;(define (pattern-matches? pattern branch)
;  (define (matches? part branch-root)
;    (cond
;      ((not (list? part)) (matches? (list part) branch-root))
;      ((not (list? branch-root)) (matches? part (list branch-root)))
;      (else (let it ((ids part))
;              (cond
;                ((null? ids) #t)
;                (else (if (member (car ids) branch-root)
;                          (it (cdr ids))
;                          #f)))))))
;  (cond
;    ((null? pattern) #t)
;    ((null? branch) #f)
;    ((matches? (car pattern) (car branch)) (pattern-matches? (cdr pattern) (cdr branch)))
;    (else (pattern-matches? pattern (cdr branch)))))
;
;(define (pattern . lst)
;  (lambda (branch)
;    (pattern-matches? lst branch)))
;
;(define (flatten-overrides overrides)
;  (define lst (map (lambda(o)
;                     (if (list? (override-item-def o))
;                         (map (lambda(item-def)
;                                (override (override-pred o)
;                                          item-def))
;                              (override-item-def o))
;                         (list o))) 
;                   overrides))
;  (apply append lst))

(define (override-field field or-field)
  (cond
    ((not or-field) field)
    ((procedure? or-field)
     (cond
       ((procedure? field)
        (lambda(datum)
          (or-field (field datum))))
       (else (or-field field))))
    (else or-field)))

(define (getters->overriden_fields getters item-def override-def)
  (define (getters->values def)
    (map (lambda(getter)
           (getter def))
         getters))
  (define new-fields
    (map override-field
         (getters->values item-def)
         (getters->values override-def)))
  new-fields)

(define (matches-pred-chain? pred-lst lst)
  (cond
    ((null? pred-lst) #t)
    ((null? lst) #f)
    (else (let ((pred (car pred-lst))
                (item (car lst)))
            (if (pred item)
                (matches-pred-chain? (cdr pred-lst) (cdr lst))
                (matches-pred-chain? pred-lst (cdr lst)))))))

(export override-field getters->overriden_fields matches-pred-chain?)
