(define-module
  (dds base-impl primitives-syntax)
  #:export (define-record-type+fact define-record-type+fact+bind*))

(use-modules
  (srfi srfi-9)
  (dds base-impl fieldbind))

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
            (constr key ...)))
       (export getter ...)
       (export fact-name pred)))))

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
             (apply constr _id+fields))))
       (export bind-name)))))
