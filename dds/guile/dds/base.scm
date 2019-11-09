(define-module (dds base))

(use-modules 
  (srfi srfi-42)
  (srfi srfi-9)
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 format)
  (dds base-impl native)
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

(define-record-type+fact <container> cnt-fact
  (make-cnt id transform blend post-processing items)
  cnt?
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

(define cnt (cnt-fact (make-cnt #f (translate 0. 0.) #f #f '())))
(define cnt-o (cnt-fact (make-cnt #f #f #f #f #f)))

(define (draw-cnt cnt transform-lst)
  (define-values (id transform blend pp items)
    (match cnt (($ <container> id transform blend pp items) (values id transform blend pp items))))
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
        (lambda(pp)
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

(define (hover-cnt? cnt mouse transf)
  (define new-transf (combine (cnt-transform cnt) transf))
  (define hover-items (map (lambda(item)
                             (hover-tree* item mouse new-transf)) 
                           (cnt-items cnt)))
  (define all-#f? (let it ((lst hover-items))
                    (cond
                      ((null? lst) #t)
                      ((car lst) #f)
                      (else (it (cdr lst))))))
  (if all-#f? #f hover-items))


#!
(define (flatten-list lst)
  (define (flatten-list* lst)
    (let it ((lst lst)
             (new '()))
      (cond
        ((null? lst) new)
        (else (let ((e (car lst)))
          (if (pair? e)
              (it (cdr lst) (append (flatten-list e) new))
              (it (cdr lst) (cons e new))))))))
  (reverse (flatten-list* lst)))
!#

(define (bind*-dynamic-item item datum cnt-branch overrides)
  (bind* ((dynamic-item-fn item) datum) datum cnt-branch overrides))

; bind* dispatcher
(define (bind* item-def datum cnt-branch overrides)
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
  (fn item-def datum cnt-branch overrides))

(define (bind*-cnt cnt-def datum cnt-branch overrides)
  (define id (cnt-id cnt-def))
  (define new-cnt-branch (append cnt-branch (list id)))
  (define getters (list cnt-transform  cnt-blend cnt-post-processing))
  (define fields (fv*/overrides cnt-def cnt? getters datum new-cnt-branch overrides))
  (define items (cnt-items cnt-def))
  (define items* 
    (cond
      ((list? items) (map (lambda(item) 
                            (bind* item datum new-cnt-branch overrides)) 
                          items ))
      ((procedure? items)
       (let ()
         (define item/data-list (items datum))
         (map (lambda(item/data)
                (define item (car item/data))
                (define datum (cdr item/data))
                (bind* item datum new-cnt-branch overrides)) 
              item/data-list)))
      (else (error (format #f "Expected list or procedure; got: ~a" items)))))
  (match fields
    ((t blend pp ) (make-cnt id t
                        blend
                        (if pp
                            (map (lambda(shd) 
                                   (bind*-shader 
                                     (fv shd datum)
                                     datum))
                                 (if (list? pp) pp (list pp))) 
                            #f) 
                         items*
                         ))))

(define (draw* item transform-lst) 
  (match item
     ((? rect?) (draw-rect item)) 
     ((? circle?) (draw-circle item))
     ((? triangle?) (draw-triangle item))
     ((? line?) (draw-line item))
     ((? t-rect?) (draw-t-rect item))
     ((? text?) (draw-text item))
     ((? cnt?) (draw-cnt item transform-lst))))

(define (hover-tree* item mouse-pos transf)
  (define m (transform-point (invert-transform transf) mouse-pos))
  (define mx (car m))
  (define my (cdr m))
  (match item
    ((? rect?) (hover-rect? item mx my))
    ((? circle?) (hover-circle? item mx my))
    ((? triangle?) (hover-triangle? item mx my))
    ((? t-rect?) (hover-t-rect? item mx my))
    ((? text?) (hover-text? item mx my))
    ((? cnt?) (hover-cnt? item mouse-pos transf))))

(define-syntax calc
  (syntax-rules ()
    ((calc (datum) body ...) (lambda (arg) (match arg (datum body ...))))))

(define* (apply-data item-def datum #:optional (overrides '()))
  (bind* item-def datum '() (flatten-overrides overrides)))

(define (hover-tree item-def mouse-pos)
  (hover-tree* item-def mouse-pos (translate 0. 0.)))

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


(export apply-data cnt cnt-o calc cnt-items-tpl cnt-items-transf cnt-items-combine compute dynamic-item hover-tree text-size draw)
(re-export override pattern rect rect-o circle circle-o triangle triangle-o line line-o t-rect t-rect-o text text-o load-texture load-shader get-shader-loc load-texture create-render-texture clear-render-texture render-texture->texture load-font script-args combine rotate translate scale shader color)
