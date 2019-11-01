(define-module
  (dds base-impl override))

(define override cons)

(define (matches-leaf-type? item-pred override)
  (item-pred (override-item-def override)))

(define (override-item-def o)
  (cdr o))

(define (override-pred o)
  (car o))

(define (matches-branch? cnt-branch override)
  ((car override) cnt-branch))

(define (pattern-matches? pattern branch)
  (define (matches? part branch-root)
    (cond
      ((not (list? part)) (matches? (list part) branch-root))
      ((not (list? branch-root)) (matches? part (list branch-root)))
      (else (let it ((ids part))
              (cond
                ((null? ids) #t)
                (else (if (member (car ids) branch-root)
                          (it (cdr ids))
                          #f)))))))
  (cond
    ((null? pattern) #t)
    ((null? branch) #f)
    ((matches? (car pattern) (car branch)) (pattern-matches? (cdr pattern) (cdr branch)))
    (else (pattern-matches? pattern (cdr branch)))))

(define (pattern . lst)
  (lambda (branch)
    (pattern-matches? lst branch)))

(define (flatten-overrides overrides)
  (define lst (map (lambda(o)
                     (if (list? (override-item-def o))
                         (map (lambda(item-def)
                                (override (override-pred o)
                                          item-def))
                              (override-item-def o))
                         (list o))) 
                   overrides))
  (apply append lst))

(export override matches-leaf-type? override-item-def override-pred matches-branch? pattern flatten-overrides)
