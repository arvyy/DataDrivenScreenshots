(define-module (chart interp))

(use-modules
  (chart base)
  (ice-9 vlist)
  (ice-9 format))

(define (interp-bridge enter exit id)
  (lambda (lst-1 lst-2)
    (define-values (enter-hash exit-hash) (bucket lst-1 lst-2 id))
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
                  (cons (enter i) lst-1*)
                  (cons i lst-2*))))
        ((and (not (null? lst-1)) (null? lst-2))
         (let ((i (car lst-1)))
           (it (cdr lst-1) 
               '()
               (cons i lst-1*)
               (cons (exit i) lst-2*))))
        (else (let* ((i1 (car lst-1))
                     (i2 (car lst-2))
                     (id1 (id i1))
                     (id2 (id i2)))
                (cond
                  ((vhash-assoc id1 exit-hash) (it (cdr lst-1)
                                                   lst-2
                                                   (cons i1 lst-1*)
                                                   (cons (exit i1) lst-2*)))
                  ((vhash-assoc id2 enter-hash) (it lst-1
                                                    (cdr lst-2)
                                                    (cons (enter i2) lst-1*)
                                                    (cons i2 lst-2*)))
                  ((equal? id1 id2) (it (cdr lst-1)
                                        (cdr lst-2)
                                        (cons i1 lst-1*)
                                        (cons i2 lst-2*)))
                  (else (error "Misaligned position of old and new data")))))
        ))))

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

(define (interp-num a b f)
  (+ a (* (- b a) f)))

(define (interp-vec a b f)
  (vector-map (lambda(a b) (interp a b f)) a b))

(define (interp-pair a b f)
  (cons (interp (car a) (car b) f)
        (interp (cdr a) (cdr b) f)))

(define (interp a b f)
  (define fn 
    (cond
      ((and (number? a) (number? b)) interp-num)
      ((and (vector? a) (vector? b)) interp-vec)
      ((and (null? a) (null? b)) (const '()))
      ((and (pair? a) (pair? b)) interp-pair)
      (else (error (format #f "Can't interpolate ~a and ~a" a b)))))
  (fn a b f))

(define (ease f)
  (* f f (- 3 (* 2 f))))

(define (start-end start end f)
  (min 1 
       (max 0 
            (/ (- f start) 
               (- end start)))))


(export interp-bridge interp ease start-end)
