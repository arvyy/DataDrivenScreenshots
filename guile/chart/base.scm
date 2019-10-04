(define-module (chart base))

(use-modules 
  (srfi srfi-9)
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 format)
  (chart base native))

(define debug-enable #t)

(define (trace frmat . args)
  (when debug-enable
    (apply format (append (list #t frmat) args))))

(define-syntax define-record-type+fact
  (syntax-rules ()
    ((_ type fact-name
        (constr key* ...)
        pred
        (key getter) ...)
     (begin
       (define-record-type type
         (constr key* ...)
         pred
         (key getter) ...)
       (define (fact-name base)
         (lambda* (#:key
                   (key (getter base)) ...)
            (constr key ...)))))))

(define-syntax define-record-type+fact+bind*
  (syntax-rules()
    ((_ type fact-name bind-name
        (constr key* ...)
        pred
        (id id-getter)
        (key getter) ...)
     (begin
       (define-record-type+fact type fact-name
            (constr key* ...)
            pred
            (id id-getter)
            (key getter) ...)
       (define bind-name
         (let ((getters (list getter ...)))
           (lambda (def datum branch overrides)
             (define _id (id-getter def))
             (define new-branch (append branch (list _id)))
             (define fields (fv*/overrides def pred getters datum new-branch overrides))
             (define _id+fields (cons _id fields))
             (apply constr _id+fields))))))))

#!
(define-syntax define-record-type+fact+bind*+check
  (syntax-rules()
    ((_ type fact-name bind-name
        (constr key ...)
        pred
        (key getter) ...)
     (begin
       (define-record-type+fact+bind* type fact-name bind-name
            (constr key ...)
            pred
            (key getter) ...)))))
!#

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

(define-record-type+fact+bind* <rect> rect-fact bind*-rect
    (make-rect id x y width height roundness fill stroke stroke-width segments)
    rect?
    (id rect-id)
    (x rect-x)
    (y rect-y)
    (width rect-width)
    (height rect-height)
    (roundness rect-roundness)
    (fill rect-fill)
    (stroke rect-stroke)
    (stroke-width rect-stroke-width)
    (segments rect-segments))

(define-record-type+fact+bind* <t-rect> t-rect-fact bind*-t-rect
  (make-t-rect id x y width height color texture)
  t-rect?
  (id t-rect-id)
  (x t-rect-x)
  (y t-rect-y)
  (width t-rect-width)
  (height t-rect-height)
  (color t-rect-color)
  (texture t-rect-texture))

(define-record-type+fact+bind* <text> text-fact bind*-text
  (make-text id text x y font font-size spacing color)
  text?
  (id text-id)
  (text text-text)
  (x text-x)
  (y text-y)
  (font text-font)
  (font-size text-font-size)
  (spacing text-spacing)
  (color text-color))

(define-record-type+fact+bind* <triangle> triangle-fact bind*-triangle
  (make-triangle id points fill stroke stroke-width)
  triangle?
  (id triangle-id)
  (points triangle-points)
  (fill triangle-fill)
  (stroke triangle-stroke)
  (stroke-width triangle-stroke-width))

(define-record-type+fact+bind* <circle> circle-fact bind*-circle
    (make-circle id x y radius inner-radius start-angle end-angle fill stroke stroke-width segments)
    circle?
    (id circle-id)
    (x circle-x)
    (y circle-y)
    (radius circle-radius)
    (inner-radius circle-inner-radius)
    (start-angle circle-start-angle)
    (end-angle circle-end-angle)
    (fill circle-fill)
    (stroke circle-stroke)
    (stroke-width circle-stroke-width)
    (segments circle-segments))

(define-record-type+fact+bind* <line> line-fact bind*-line
    (make-line id points stroke stroke-width)
    line?
    (id line-id)
    (points line-points)
    (stroke line-stroke)
    (stroke-width line-stroke-width))

(define-record-type <color>
  (make-color r g b a)
  color?
  (r color-r)
  (g color-g)
  (b color-b)
  (a color-a))

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

(define (color->vec c)
  (match c
         (($ <color> r g b a) (vector r g b a))
         (_ #f)))

(define-record-type <shader>
  (make-shader id uniforms)
  shader?
  (id shader-id)
  (uniforms shader-uniforms))

(define deg->rad (/ 3.145 180))
(define rad->deg (/ deg->rad))
(define (rotate deg)
  (define cos* (cos (* deg->rad deg)))
  (define sin* (sin (* deg->rad deg)))
  (vector cos* sin* 0.
          (- sin*) cos* 0.))

(define (translate x y)
  (vector 1. 0. x
          0. 1. y))

(define (scale xk yk)
  (vector xk 0. 0.
          0. yk 0.))

(define rect (rect-fact (make-rect #f 0 0 0 0 0 (make-color 200 100 100 255) (make-color 10 10 10 255) 1 20)))
(define rect-o (rect-fact (make-rect #f #f #f #f #f #f #f #f #f #f)))

(define triangle (triangle-fact (make-triangle #f '() (make-color 200 100 100 255) (make-color 10 10 10 255) 1)))
(define triangle-o (triangle-fact (make-triangle #f #f #f #f #f)))

(define line (line-fact (make-line #f '() (make-color 10 10 10 255) 1)))
(define line-o (line-fact (make-line #f #f #f #f)))

(define circle (circle-fact (make-circle #f 0 0 0 0 0 360 (make-color 200 100 100 255) (make-color 10 10 10 255) 1 20)))
(define circle-o (circle-fact (make-circle #f #f #f #f #f #f #f #f #f #f #f)))

(define t-rect (t-rect-fact (make-t-rect #f 0 0 0 0 (make-color 200 100 100 255) #f)))
(define t-rect-o (t-rect-fact (make-t-rect #f #f #f #f #f #f #f)))

(define text (text-fact (make-text #f "" 0 0 #f 12 5 (make-color 10 10 10 255))))
(define text-o (text-fact (make-text #f #f #f #f #f #f #f #f)))

(define cnt (cnt-fact (make-cnt #f (translate 0. 0.) #f #f '())))
(define cnt-o (cnt-fact (make-cnt #f #f #f #f #f)))

(define (draw-rect rect)
  (match rect
    (($ <rect> id x y width height roundness fill stroke thick segments)
     (draw-rect* x y width height roundness (color->vec fill) (color->vec stroke) thick segments))))

(define (draw-triangle triangle)
  (match triangle
         (($ <triangle> id points fill stroke thick)
          (let it ((points points)
                   (stroke (color->vec stroke))
                   (fill (color->vec fill)))
            (match points
                   ((a b c . _) 
                    (begin
                      (draw-triangle* a b c fill stroke thick)
                      (it (cdr points) stroke fill)))
                   (_ #f))))))

(define (draw-line line)
  (match line
         (($ <line> id points stroke thick)
          (draw-line* (list->vector points) (color->vec stroke) thick))))

(define (draw-circle circle)
  (match circle
         (($ <circle> id x y r r-inner angl1 angl2 fill stroke thick segments)
          (draw-circle* x y r r-inner angl1 angl2 (color->vec fill) (color->vec stroke) thick segments))))

(define (draw-t-rect t-rect)
  (match t-rect
         (($ <t-rect> id x y width height ($ <color> r g b a) texture) (draw-t-rect* x y width height (vector r g b a) texture))))

(define (draw-text txt)
  (match txt
         (($ <text> id text x y font font-size spacing ($ <color> r g b a)) (draw-text* text x y font font-size spacing (vector r g b a)))))

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
          (for-each (lambda(uniform) 
                      (define id (shader-id pp))
                      (match uniform
                             (('vec loc val) (set-shader-value id loc val))
                             (('cnt-transform loc) (set-shader-value-matrix/invert id loc new-transform))
                             (('matrix loc val) (set-shader-value-matrix id loc val))
                             (('matrix/invert loc val) (set-shader-value-matrix/invert id loc val))
                             (('texture loc val) (set-shader-value-texture id loc val))))
                    (shader-uniforms pp))
          (pp-chain-next (shader-id pp)))
        pp-list)
      (pop-pp-texture))))

(define (combine transform . rest)
  (cond
    ((null? rest) transform)
    (else (match-let ((#(a11 a21 a31
                         a12 a22 a32) (car rest))
                      (#(b11 b21 b31
                         b12 b22 b32) transform))
              (define new-transform (vector (+ (* a11 b11) (* a21 b12))
                                            (+ (* a11 b21) (* a21 b22))
                                            (+ (* a11 b31) (* a21 b32) a31)
                                            (+ (* a12 b11) (* a22 b12))
                                            (+ (* a12 b21) (* a22 b22))
                                            (+ (* a12 b31) (* a22 b32) a32)))
              (apply combine (cons new-transform (cdr rest)))))))

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

; get field value
(define (fv field datum)
  (if (procedure? field)
      (field datum)
      field))

(define (fv* item field-getters datum)
  (map (lambda(get-field)
         (fv (get-field item) datum)) 
       field-getters))

(define (matches-leaf-type? item-pred override)
  (item-pred (override-item-def override)))

(define (override-item-def override)
  (cdr override))

(define (matches-branch? cnt-branch override)
  ((car override) cnt-branch))

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

(define (bind*-shader shader-def datum)
  (define uniforms 
    (map (lambda(uni)
           (cons (car uni) (fv (cdr uni) datum))) 
         (shader-uniforms shader-def)))
  (make-shader (shader-id shader-def) uniforms))

(define (bind*-cnt cnt-def datum cnt-branch overrides)
  (define id (cnt-id cnt-def))
  (define new-cnt-branch (append cnt-branch (list id)))
  (define getters (list cnt-transform  cnt-blend cnt-post-processing))
  (define fields (fv*/overrides cnt-def cnt? getters datum new-cnt-branch overrides))
  (define items (cnt-items cnt-def))
  (define items* 
    (cond
      ((list? items) (map (lambda(item) (bind* item datum new-cnt-branch overrides)) items))
      ((procedure? items)
       (let ()
         (define-values (item-lst datums) (items datum))
         (map (lambda(item datum)
                (bind* item datum new-cnt-branch overrides)) 
              item-lst
              datums)))
      (else (error (format #f "Expected list or procedure; got: ~a" items)))))
  (match fields
    ((t blend pp) (make-cnt id t
                        blend
                        (if pp
                            (map (lambda(shd) 
                                   (bind*-shader 
                                     (fv shd datum)
                                     datum))
                                 (if (list? pp) pp (list pp))) 
                            #f) 
                         items*))))

(define (draw* item transform-lst)
  (match item
     ((? rect?) (draw-rect item))
     ((? circle?) (draw-circle item))
     ((? triangle?) (draw-triangle item))
     ((? line?) (draw-line item))
     ((? t-rect?) (draw-t-rect item))
     ((? text?) (draw-text item))
     ((? cnt?) (draw-cnt item transform-lst)))
  )

(define-syntax calc
  (syntax-rules ()
    ((calc (datum) body ...) (lambda (arg) (match arg (datum body ...))))))

(define-syntax pattern
  (syntax-rules ()
    ((_ pat ... )
     (lambda (path)
       (match path
         ((pat ...) #t)
         (_ #f))))))

(define* (apply-data item-def datum #:optional (overrides '()))
  (bind* item-def datum '() overrides))

(define* (draw item #:optional (texture #f))
  (set-draw-target texture)
  (draw* item (translate 0. 0.))
  (when texture
    (set-draw-target #f)))

(define* (cnt-items-tpl item-def children-datum-getter 
                       #:optional (child-parent-datum-combiner (lambda(child parent) child)))
  (lambda(datum)
    (define datums (children-datum-getter datum))
    (values 
      (map (lambda(child-datum) item-def) datums)
      (map (lambda(child-datum) (child-parent-datum-combiner child-datum datum)) datums))))

(define (cnt-items-transf items datum-transform-fn)
  (lambda(datum)
    (define new-datum (datum-transform-fn datum))
    (values
      items
      (map (lambda(item) new-datum) items))))

(define* (color r g b #:optional (a 255))
         (make-color r g b a))

(define (shader id . uniforms)
  (make-shader id uniforms))

(define override cons)

(export apply-data pattern draw rect rect-o circle circle-o triangle triangle-o line line-o t-rect t-rect-o text text-o cnt cnt-o shader color combine rotate translate scale calc cnt-items-tpl cnt-items-transf override fv compute* compute dynamic-item deg->rad rad->deg)
(re-export load-texture load-shader get-shader-loc load-texture create-render-texture render-texture->texture load-font)
