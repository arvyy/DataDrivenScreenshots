(define-module (chart scale))

(use-modules
  (srfi srfi-9)
  (ice-9 vlist))

(define-record-type <scale>
  (make-scale domain range fn)
  scale?
  (domain scale-domain)
  (range scale-range)
  (fn scale-fn))

(define (use-scale scale input)
  ((scale-fn scale) input))

(define (make-scale-linear domain range)
  (define step (/ (- (cdr range) (car range))
                  (- (cdr domain) (car domain))))
  (define fn (lambda(in)
    (define val (+ (car range) (* step (- in (car domain)))))
    val))
  (make-scale domain range fn))

(define (make-scale-point domain range)
  (define l (length domain))
  (define step (/ (- (cdr range) (car range)) (- l 1)))
  (define points
    (let it ((i 0)
             (lst '()))
      (cond
        ((< i l) (it (+ i 1) 
                     (cons (+ (car range) 
                              (* i step)) 
                           lst)))
        (else (reverse lst)))))
  (define alst (map (lambda(key val) (cons key val)) domain points))
  (define scale (alist->vhash alst))
  (define (fn in)
    (cdr (vhash-assoc in scale)))
  (make-scale domain range fn))

(export make-scale-linear make-scale-point use-scale scale-domain scale-range scale-fn)
