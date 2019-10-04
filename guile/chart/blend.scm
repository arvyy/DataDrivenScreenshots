(define-module
  (chart blend))

(define blend/alpha (cons 'GL_SRC_ALPHA 'GL_ONE_MINUS_SRC_ALPHA))
(define blend/add (cons 'GL_ONE 'GL_ONE))
(define blend/mult (cons 'GL_DST_COLOR 'GL_ONE_MINUS_SRC_ALPHA))

(export blend/alpha blend/add blend/mult)
