(define-module
  (dds sequence))

(use-modules
  (srfi srfi-1)
  (srfi srfi-9)
  (ice-9 match)
  (dds base))

(define (seq-get seq t)
  (vector-ref (seq-get* seq t) 0))

(define (seq-get* seq t)
  (match seq
    (((start state) (start* state*) . _)
     (cond 
       ((< t start*) (vector state state* (/ (- t start) (- start* start))))
       (else (seq-get* (cdr seq) t))))
    (((start state)) (vector state #f #f))))

(define (seq-prune seq t)
  (match seq
    ((el1 el2  . _)
     (cond
       ((>= t (car el2)) (seq-prune (cdr seq) t))
       (else seq)))
    ((el1) seq)
    (_ #f)))

(define (make-seq states)
  (seq-append (list (list 0 (cadar states))) (cdr states)))

(define (seq-append seq states)
  (define rev-seq (reverse seq))
  (define last (car rev-seq))
  (let it ((new-rev-seq rev-seq)
           (last-start (car last))
           (states states))
    (cond
      ((null? states) (reverse new-rev-seq))
      (else (let* ((state (car states))
                   (start (+ last-start (car state)))
                   (new-state (list start (cadr state))))
              (it (cons new-state new-rev-seq)
                  start
                  (cdr states)))))))

(define (seq-empty seq)
  (list (car seq)))

(define (seq-empty? seq)
  (< (length seq) 2))

(define* (seq->seq/interp seq interp-fn #:optional (bridge-fn (lambda(a b)(values a b))))
  (let it ((new-states '())
           (states seq))
    (match states
      (((start state) (start* state*) . _) 
       (let ()
         (define-values (state/b state*/b) (bridge-fn state state*))
         (define len (- start* start))
         (define new-state-val (lambda(t)
           (define f (/ (- t start) len))
           (interp-fn state/b state*/b f)))
         (define new-state (list start new-state-val))
         (it (cons new-state new-states) (cdr states))))
      (((start state)) (let ()
                           (define new-state (list start (const state)))
                           (reverse (cons new-state new-states))))
      (_ (const #f)))))

(define (seq/interp-get seq t)
  ((seq-get seq t) t))

(export seq-get* seq-get seq-prune seq-append seq->seq/interp seq/interp-get make-seq seq-empty? seq-empty)
