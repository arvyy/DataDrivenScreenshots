(define-module
  (dds mathutil))

(define PI 3.14159265)

(define (->deg rad)
  (* rad (/ 180.0 PI)))

(define (->rad deg)
  (* deg (/ PI 180.0)))

(export ->deg ->rad PI)
