(define-module
  (dds base-impl primitives-syntax)
  #:export (define-record-type+fact define-record-type+fact+bind*))

(use-modules
  (srfi srfi-9)
  (dds base-impl fieldbind)
  (dds base-impl override))

(define-syntax define-record-type+fact
  (syntax-rules ()
    ((_ type fact-name
        (constr key* ...)
        pred
        (key getter) ...)
     (define-record-type+fact type fact-name
        (constr key* ...)
        pred
        ()
        (key getter) ...))
    ((_ type fact-name
        (constr key* ...)
        pred
        ((key/no-fac getter/no-fac) ...)
        (key getter) ...)
     (begin
       (define-record-type type
         (constr key* ...)
         pred
         (key/no-fac getter/no-fac) ...
         (key getter) ...)
       (define (fact-name base)
         (lambda* (#:key
                   (key (getter base)) ...)
                  (constr (getter/no-fac base) ... key ...)))
       (export getter ...)
       (export getter/no-fac ...)
       (export fact-name pred)))))

(define-syntax define-record-type+fact+bind*
  (syntax-rules()
    ((_ type fact-name bind-name override-name
        (constr key* ...)
        pred
        ((data data-getter)
         (key/no-fac getter/no-fac) ...)
        (key getter) ...)
     (begin
       (define-record-type+fact type fact-name
                                (constr key* ...)
                                pred
                                ((data data-getter)
                                 (key/no-fac getter/no-fac) ...)
                                (key getter) ...)
       (define bind-name
         (let ((getters (list getter ...))
               (getters/no-fac (list getter/no-fac ...)))
           (lambda (def datum)
             (define fields/no-fac (map (lambda(get) 
                                          (get def)) 
                                        getters/no-fac))
             (define fields (fv* def getters datum))
             (apply constr (append (list datum) fields/no-fac fields)))))
       (define override-name
         (let ((getters (list getter ...))
               (getters/no-fac (list getter/no-fac ...)))
           (lambda (item-def override-def)
             (define new-fields/no-fac (map (lambda(get) (get item-def)) getters/no-fac))
             (define new-fields (getters->overriden_fields getters item-def override-def))
             (apply constr (append (list (data-getter item-def)) new-fields/no-fac new-fields)))))
       (export bind-name override-name)))))
