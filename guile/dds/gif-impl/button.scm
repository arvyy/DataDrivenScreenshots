(define-module
  (dds gif-impl button))

(use-modules
  (srfi srfi-9)
  (srfi srfi-9 gnu)
  (dds base)
  (dds interp))

(define-record-type <button-state>
  (bs last next t tooltip-delay)
  bs?
  (last bs-last)
  (next bs-next)
  (t bs-t)
  (tooltip-delay bs-tooltip-delay))

(define dur 0.4)
(define tooltip-delay 1)
(define button-init-state (bs 'none 'none 0 tooltip-delay))

(define (update-button-state state delta hover? clicked?)
  (let* ((s (update-button-state/base state delta hover? clicked?))
         (s (update-button-state/tooltip s delta hover? clicked?)))
    s))

(define (update-button-state/base state delta hover? clicked?)
  (define last (bs-last state))
  (define next (bs-next state))
  (define tt-del (bs-tooltip-delay state))
  (define new-last/next
    (cond
      (clicked? (cons 'click 'hover))
      (hover? (cons last 'hover))
      (else (cons last 'none))))
  (define new-last (car new-last/next))
  (define new-next (cdr new-last/next))
  (cond
    ((equal? new-last new-next) (bs new-last new-last 0 tt-del))
    ((and (equal? last new-last)
         (equal? next new-next))
     (let ((new-t (+ (/ delta dur) (bs-t state))))
       (if (> new-t 1)
           (bs next next 0 tt-del)
           (bs last next new-t tt-del))))
    (else (bs new-last new-next 0 tt-del))))

(define (update-button-state/tooltip state delta hover? clicked?)
  (let ((last (bs-last state))
        (tt-delay (bs-tooltip-delay state)))
    (if (not (equal? last 'none))
        (set-fields state
                    ((bs-tooltip-delay) (- tt-delay delta)))
        (set-fields state
                    ((bs-tooltip-delay) tooltip-delay)))))

(define (button-fill state)
  (define (fill-c s)
    (cond
      ((equal? 'none s) (color 200 200 200 100))
      ((equal? 'hover s) (color 200 200 200))
      ((equal? 'click s) (color 200 200 255))))
  (interp (fill-c (bs-last state))
          (fill-c (bs-next state))
          (ease (bs-t state))))

(define (button-stroke state)
  (color 100 100 100))

(define (button-stroke-width state)
  2)

(define (icon-fill state)
  (color 100 100 100))

(define (icon-stroke state)
  #f)

(define (icon-stroke-width state)
  0)


(define (button-base state size)
  (circle #:fill (compute(state) (button-fill state))
          #:stroke (compute(state) (button-stroke state))
          #:stroke-width (compute(state) (button-stroke-width state))
          #:radius size))

(define (button-icon state icon-lines k)
  (cnt #:transform (compute (k) (combine (translate -50 -50) (scale k k)))
       #:items
       (compute (icon-lines state)
                (map (lambda(l)
                       (override-line l (line-o #:fill (icon-fill state)
                                                #:stroke (icon-stroke state)
                                                #:stroke-width (icon-stroke-width state))))
                     icon-lines))))

(define (button-tooltip tooltip size)
  (text #:text tooltip
        #:x 0
        #:y (compute (size) (+ (- size) -10))
        #:x-offset 0.5
        #:y-offset 1))

(define* (button #:key 
                 (id #f)
                 (x 0)
                 (y 0)
                 (icon-lines '())
                 (size 32)
                 (state #f)
                 (tooltip #f))
         (define k (compute (size) (/ size 100)))
         (define items (compute (tooltip state)
                                (define without-tooltip
                                  (list (button-base state size)
                                        (button-icon state icon-lines k)))
                                (define lst (if (and 
                                      tooltip
                                      (< (bs-tooltip-delay state) 0))
                                    (cons (button-tooltip tooltip size) without-tooltip)
                                    without-tooltip))
                                lst))
         (cnt #:items items
              #:transform (translate x y)
              #:id id))

(export button button-init-state update-button-state)
