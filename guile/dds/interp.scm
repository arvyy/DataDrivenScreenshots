(define-module (dds interp))

(use-modules
  (dds base)
  (dds base-impl color)
  (srfi srfi-43)
  (ice-9 vlist)
  (ice-9 format))

(define* (interp-bridge #:key (enter identity) 
                              (update identity)
                              (exit identity)
                              (id #f))
  (define index/id-adder
    (if (not id)
        (lambda (lst)
          (let it ((lst lst)
                   (index 0)
                   (new-lst '()))
            (cond
              ((null? lst) (reverse new-lst))
              (else (it (cdr lst) (+ 1 index) (cons (cons index (car lst)) new-lst))))))
        identity))
  (define id-fn (if id id car))
  (lambda (lst-1* lst-2*)
    (define enter-fn (if id enter (lambda(d) (enter (cdr d)))))
    (define update-fn (if id update (lambda(d) (update (cdr d)))))
    (define exit-fn (if id exit (lambda(d) (exit (cdr d)))))
    (define lst-1 (index/id-adder lst-1*))
    (define lst-2 (index/id-adder lst-2*))
    (define-values (enter-hash exit-hash) (bucket lst-1 lst-2 id-fn))
    (let it ((lst-1 lst-1)
             (lst-2 lst-2)
             (lst-1* '())
             (lst-2* '()))
      (cond
        ((and (null? lst-1) (null? lst-2)) (values lst-1* lst-2*))
        ((and (null? lst-1) (not (null? lst-2))) 
            (let ((i (car lst-2)))
              (it '() 
                  (cdr lst-2)
                  (cons (enter-fn i) lst-1*)
                  (cons (update-fn i) lst-2*))))
        ((and (not (null? lst-1)) (null? lst-2))
         (let ((i (car lst-1)))
           (it (cdr lst-1) 
               '()
               (cons (update-fn i) lst-1*)
               (cons (exit-fn i) lst-2*))))
        (else (let* ((i1 (car lst-1))
                     (i2 (car lst-2))
                     (id1 (id-fn i1))
                     (id2 (id-fn i2)))
                (cond
                  ((vhash-assoc id1 exit-hash) (it (cdr lst-1)
                                                   lst-2
                                                   (cons (update-fn i1) lst-1*)
                                                   (cons (exit-fn i1) lst-2*)))
                  ((vhash-assoc id2 enter-hash) (it lst-1
                                                    (cdr lst-2)
                                                    (cons (enter-fn i2) lst-1*)
                                                    (cons (update-fn i2) lst-2*)))
                  ((equal? id1 id2) (it (cdr lst-1)
                                        (cdr lst-2)
                                        (cons (update-fn i1) lst-1*)
                                        (cons (update-fn i2) lst-2*)))
                  (else (error "Misaligned position of old and new data")))))))))

(define (hash-lst lst id)
  (define entries (map (lambda(i) (cons (id i) i)) lst))
  (alist->vhash entries))

(define (bucket lst-1 lst-2 id)
  (define h1 (hash-lst lst-1 id))
  (define h2 (hash-lst lst-2 id))
  (let it ((enter '())
           (exit '())
           (lst (append lst-1 lst-2)))
    (cond
      ((null? lst) (values 
                     (alist->vhash enter)
                     (alist->vhash exit)))
      (else (let* ((i (car lst))
                   (rest (cdr lst))
                   (e (cons (id i) i))
                   (a1 (vhash-assoc (id i) h1))
                   (a2 (vhash-assoc (id i) h2)))
              (cond
                ((and a1 a2) (it enter exit rest))
                ((and a1 (not a2)) (it enter (cons e exit) rest))
                ((and (not a1) a2) (it (cons e enter) exit rest))))))))

(define (interp-num a b f ed)
  (+ a (* (- b a) f)))

(define (interp-vec a b f ed)
  (vector-map (lambda(index a b) (interp-pr a b f ed)) a b))

(define (interp-pair a b f ed)
  (cons (interp-pr (car a) (car b) f ed)
        (interp-pr (cdr a) (cdr b) f ed)))

(define (interp-color a b f ed)
  (vec->color (interp-vec (color->vec a)
                          (color->vec b)
                          f
                          ed)))

(define (interp-pr a b f extra-dispatch)
  (define fn 
    (cond
      ((and extra-dispatch ((car extra-dispatch) a b)) (cdr extra-dispatch))
      ((and (number? a) (number? b)) interp-num)
      ((and (vector? a) (vector? b)) interp-vec)
      ((and (null? a) (null? b)) (const '()))
      ((and (pair? a) (pair? b)) interp-pair)
      ((and (string? a) (string? b)) (const b))
      ((and (symbol? a) (symbol? b)) (const b))
      ((and (color? a) (color? b)) interp-color)
      (else (error (format #f "Can't interpolate ~a and ~a" a b)))))
  (fn a b f extra-dispatch))

(define* (interp a b f #:key (extra-dispatch #f))
  (cond
    ((or (< f 0) (> f 1)) #f)
    (else (interp-pr a b f extra-dispatch))))

(define* (interp/clamp a b f #:key (extra-dispatch #f))
    (interp-pr a b (min 1 (max 0 f)) extra-dispatch))

(define (ease f)
  (* f f (- 3 (* 2 f))))

(define (start-end start end f)
  (min 1 (max 0 (/ (- f start) 
     (- end start)))))


(export interp-bridge interp interp/clamp ease start-end)
