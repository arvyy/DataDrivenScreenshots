;example of overriding properties of any item if its branch id path matches specified predicate
;in a way similar to CSS

;use same rect, whose fill property in different containers will be overriden differently
(define base-rect (rect #:width 100
                        #:height 100
                        #:stroke #f))

(define cnt-1 (cnt #:items (list base-rect)
                   #:id 'cnt-1))

(define cnt-2 (cnt #:items (list base-rect)
                   #:transform (translate 100 0)
                   #:id 'cnt-2))

(define cnt-3 (cnt #:items (list base-rect)
                   #:transform (translate 200 0)
                   #:id 'cnt-3))

(define base-cnt (cnt #:items (list cnt-1 cnt-2 cnt-3)
                      #:transform (translate 200 200)
                      #:id 'root))

(define overriden-base-cnt
  (map-item (lambda(item cnt-branch)
              (cond
                ((not (rect? item)) item)
                (else (cond
                        ((matches-pred-chain? 
                           (list (id-pred 'root) (id-pred 'cnt-1)) 
                           cnt-branch)
                         (override-rect item (rect-o #:fill (color 255 10 10))))
                        ((matches-pred-chain? 
                           (list (id-pred 'root) (id-pred 'cnt-2)) 
                           cnt-branch)
                         (override-rect item (rect-o #:fill (color 10 255 10))))
                        ((matches-pred-chain? 
                           (list (id-pred 'root) (id-pred 'cnt-3)) 
                           cnt-branch)
                         (override-rect item (rect-o #:fill (color 10 10 255))))
                        (else item))))) 
            base-cnt))

(define get-dds-gif
  (dds-gif #:item-getter (const overriden-base-cnt)))
