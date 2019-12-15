(define-module (dds base))

(use-modules 
  (srfi srfi-42)
  (srfi srfi-9)
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 format)
  (dds base-impl native)
  (dds base-impl boot)
  (dds base-impl input)
  (dds base-impl color)
  (dds base-impl fieldbind)
  (dds base-impl override) 
  (dds base-impl triangle)
  (dds base-impl rect)
  (dds base-impl t-rect)
  (dds base-impl line)
  (dds base-impl text)
  (dds base-impl circle)
  (dds base-impl primitives-syntax)
  (dds base-impl shader)
  (dds base-impl triangulation)
  (dds base-impl transform))

(define-record-type <dynamic-item>
  (dynamic-item fn)
  dynamic-item?
  (fn dynamic-item-fn))

(define-record-type+fact+bind* <container> cnt-fact bind*-cnt/no-items override-cnt
  (make-cnt data id transform blend post-processing items)
  cnt?
  ((data cnt-data))
  (id cnt-id)
  (transform cnt-transform)
  (blend cnt-blend)
  (post-processing cnt-post-processing)
  (items cnt-items))

(define-syntax compute
  (syntax-rules ()
    ((_ (args ... ) body ...)
     (compute* (lambda(args ...) body ...)
              args ...))))

(define (compute* fn . args)
  (define all-known?
    (fold (lambda(a b)
            (and (not (procedure? a)) b)) 
          #t 
          args))
  (cond
    (all-known? (apply fn args))
    (else (lambda (datum)
            (define args-value (map (lambda(arg)(fv arg datum)) args))
            (apply fn args-value)))))

(define cnt (cnt-fact (make-cnt #f #f (translate 0. 0.) #f #f '())))
(define cnt-o (cnt-fact (make-cnt #f #f #f #f #f #f)))

(define (draw-cnt cnt transform-lst)
  (define-values (id transform blend pp items)
    (match cnt (($ <container> data id transform blend pp items) (values id transform blend pp items))))
  (define new-transform (combine transform transform-lst))
  (apply-transform new-transform)
  (when (or pp blend)
    (push-pp-texture blend))
  (for-each (lambda (item) 
              (apply-transform new-transform)
              (draw* item new-transform)) 
            items)
  (when (or blend pp)
    (let ((pp-list (cond 
                     ((not pp) '())
                     ((list? pp) pp) 
                     (else (list pp)))))
      (begin-pp-chain)
      (for-each 
        (lambda(pp-setup-or-shader)
          (define pp 
            (cond
              ((shader-setup? pp-setup-or-shader) (shader-setup-apply pp-setup-or-shader))
              ((shader? pp-setup-or-shader) pp-setup-or-shader)
              (else (error (format #f "Expected shader or shader setup; got: ~a\n" pp-setup-or-shader)))))
          (define id (shader-id pp))
          (let it ((uniforms (shader-uniforms pp))
                   (texture-index 0))
            (cond
              ((null? uniforms) #t)
              (else (let ((uniform (car uniforms))
                          (rest (cdr uniforms)))
                      (match uniform
                             (('vec loc val) 
                              (set-shader-value id loc val)
                              (it rest texture-index))
                             (('cnt-transform loc) 
                              (set-shader-value-matrix/invert id loc new-transform)
                              (it rest texture-index))
                             (('matrix loc val) 
                              (set-shader-value-matrix id loc val)
                              (it rest texture-index))
                             (('matrix/invert loc val) 
                              (set-shader-value-matrix/invert id loc val)
                              (it rest texture-index))
                             (('texture loc val) 
                              (set-shader-value-texture id loc texture-index val)
                              (it rest (+ texture-index 1))))))))
          (pp-chain-next (shader-id pp)))
        pp-list)
      (pop-pp-texture))))

(define (hover-cnt? cnt mouse transf with-pos)
  (define new-transf (combine (cnt-transform cnt) transf))
  (define hover-items/lst 
    (map (lambda(item)
           (hover-lst* item mouse new-transf with-pos)) 
         (cnt-items cnt)))
  (define hover-items (apply append hover-items/lst))
  (define rez (if (null? hover-items) 
                  '()
                  (cons cnt hover-items)))
  rez)

(define (bind*-dynamic-item item datum )
  (bind* ((dynamic-item-fn item) datum) datum))

; bind* dispatcher
(define (bind* item-def datum)
  (define fn
    (cond
      ((dynamic-item? item-def) bind*-dynamic-item)
      ((rect? item-def) bind*-rect)
      ((triangle? item-def) bind*-triangle)
      ((circle? item-def) bind*-circle)
      ((line? item-def) bind*-line)
      ((t-rect? item-def) bind*-t-rect)
      ((cnt? item-def) bind*-cnt)
      ((text? item-def) bind*-text)))
  (fn item-def datum))

(define (bind*-cnt cnt-def datum)
  (define items (cnt-items cnt-def))
  (define lst
    (cond
      ((list? items) (map (lambda(i) (bind* i datum)) items))
      ((procedure? items) (map (lambda(item+datum) 
                                 (if (pair? item+datum)
                                     (bind* (car item+datum) (cdr item+datum))
                                     (bind* item+datum datum))) 
                               (items datum)))
      (else (list (bind* items datum)))))
  (override-cnt (bind*-cnt/no-items cnt-def datum) (cnt-o #:items lst)))

(define (map-item mapper item-def)
  (map-item* mapper item-def '()))

(define (map-item* mapper item-def cnt-branch)
  (define new-item-def (mapper item-def cnt-branch))
  (if (cnt? item-def)
      (let ((new-branch (append cnt-branch (list item-def ))))
        (override-cnt item-def 
                      (cnt-o #:items 
                             (lambda(items)
                               (map (lambda(item)
                                      (map-item* mapper item new-branch))
                                    items)))))
      new-item-def))

(define (draw* item transform-lst) 
  (match item
     ((? rect?) (draw-rect item)) 
     ((? circle?) (draw-circle item))
     ((? triangle?) (draw-triangle item))
     ((? line?) (draw-line item))
     ((? t-rect?) (draw-t-rect item))
     ((? text?) (draw-text item))
     ((? cnt?) (draw-cnt item transform-lst))))

(define (hover-lst* item mouse-pos transf with-pos)
  (define m (transform-point (invert-transform transf) mouse-pos))
  (define mx (car m))
  (define my (cdr m))
  (define rez
    (if with-pos
        (lambda (i) (cons i m))
        identity))
  (match item
    ((? rect?) (rez (hover-rect? item mx my)))
    ((? line?) (rez (hover-line? item mx my)))
    ((? circle?) (rez (hover-circle? item mx my)))
    ((? triangle?) (rez (hover-triangle? item mx my)))
    ((? t-rect?) (rez (hover-t-rect? item mx my)))
    ((? text?) (rez (hover-text? item mx my)))
    ((? cnt?) (rez (hover-cnt? item mouse-pos transf with-pos)))))

(define-syntax calc
  (syntax-rules ()
    ((calc (datum) body ...) (lambda (arg) (match arg (datum body ...))))))

(define (apply-data item-def datum)
  (bind* item-def datum))

(define* (hover-lst item-def mouse-pos #:optional (with-pos #f))
  (hover-lst* item-def mouse-pos (translate 0. 0.) with-pos))

(define* (draw item #:optional (texture #f))
  (set-draw-target texture)
  (draw* item (translate 0. 0.))
  (when texture
    (set-draw-target #f)))

(define* (cnt-items-tpl item-def children-datum)
  (lambda(datum)
    (define datums (if (procedure? children-datum)
                       (children-datum datum)
                       children-datum))
    (map (lambda(child-datum) (cons item-def child-datum)) datums)))

(define (cnt-items-transf items datum-transform-fn)
  (lambda(datum)
    (define new-datum (datum-transform-fn datum))
    (map (lambda(item) (cons item new-datum)) items)))

(define (cnt-items-combine . combinations)
  (lambda (datum)
    (define lst (map (lambda(comb)
                       (cond
                         ((procedure? comb) (comb datum))
                         ((list? comb) (map (lambda(c)(cons c datum)) comb))
                         (else (list)))) 
                     combinations))
    (apply append lst)))

(define item-data
  (let ((item-pred/item-data (list
                             (cons rect? rect-data)
                             (cons triangle? triangle-data)
                             (cons circle? circle-data)
                             (cons line? line-data)
                             (cons text? text-data)
                             (cons t-rect? t-rect-data)
                             (cons cnt? cnt-data))))
    (lambda (item)
      (let it ((lst item-pred/item-data))
        (match lst
               (() #f)
               (((pred . data) . rest)
                (if (pred item)
                    (data item)
                    (it rest))))))))

(define item-id
  (let ((item-pred/item-id (list
                             (cons rect? rect-id)
                             (cons triangle? triangle-id)
                             (cons circle? circle-id)
                             (cons line? line-id)
                             (cons text? text-id)
                             (cons t-rect? t-rect-id)
                             (cons cnt? cnt-id))))
    (lambda (item)
      (let it ((lst item-pred/item-id))
        (match lst
               (() #f)
               (((pred . id) . rest)
                (if (pred item)
                    (id item)
                    (it rest))))))))

(define (id-pred id)
  (lambda(item)
    (equal? id (item-id item))))

(export 
  apply-data 
  cnt cnt-o cnt? override-cnt
  calc
  item-id
  id-pred
  cnt-items-tpl 
  cnt-items-transf 
  cnt-items-combine 
  compute 
  dynamic-item 
  hover-lst 
  draw 
  map-item)

(re-export 
  rect rect-o rect? override-rect
  circle circle-o circle? override-circle
  triangle triangle-o triangle? override-triangle
  line line-o line? override-line
  t-rect t-rect-o t-rect? override-t-rect
  text text-o text? override-text text-size
  
  take-screenshot
  load-texture 
  load-shader 
  load-shader-text 
  get-shader-loc 
  load-texture 
  create-render-texture 
  clear-render-texture 
  render-texture->texture
  load-font 
  combine 
  rotate 
  translate
  scale
  color color-r color-g color-b color-a
  shader-setup-map
  make-shader-setup
  make-shader
  matches-pred-chain?
  get-mouse-pos is-mouse? is-key? get-key-pressed
  init-window window-close? rlgl-draw begin-draw end-draw clear-bg get-frame-time)
