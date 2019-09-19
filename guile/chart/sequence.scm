(define-module
  (chart sequence))

(use-modules
  (srfi srfi-1)
  (srfi srfi-9)
  (ice-9 match)
  (chart base))

#!
(define* (make-seq-renderer item-def transition-fn states #:optional (overrides '()))
  (define-values (states* last-datum)
    (let it ((total 0)
             (rest states)
             (states '())
             (last-datum #f))
      (cond
        ((null? rest) (values (reverse states) last-datum))
        (else (let* ((s (car rest))
                     (new-state (list total (+ total (car s)) (cdr s))))
                (it (+ total (car s))
                    (cdr rest)
                    (cons new-state states)
                    (cdr s)))))))
  (define (find-state t)
    (find-tail (match-lambda ((start end datum) (and (>= t start) (< t end)))) states*))
  (define (render t)
    (match (find-state t)
      (((start1 end1 data1) (_ _ data2) _ ...) 
       (let ((frac (/ (- t start1) (- end1 start1))))
         (draw (apply-data item-def (transition-fn data1 data2 frac) overrides))))
      (_ (draw (apply-data item-def last-datum overrides)))))
  render)

!#

(define-record-type <wrap>
  ; state (list (dur . state)) ellapsed transition-fn 
  (make-wrap state queue e transition-fn)
  wrap?
  (state wrap-state)
  (queue wrap-queue)
  (e wrap-e)
  (transition-fn wrap-transition-fn))

(define (state-wrap transition init)
  (make-wrap init '() 0 transition))


(define (state-wrap-enqueue wrap new-state duration)
  (match wrap
     (($ <wrap> state queue e tr)
      (make-wrap state (append queue (list (cons new-state duration))) e tr))))

(define (state-wrap-update wrap delta)
  (match wrap
     (($ <wrap> state queue e tr)
      (cond
        ((null? queue) wrap)
        (else (let ((new-state (car (car queue)))
                    (dur (cdr (car queue)))
                    (new-e (+ e delta)))
                (if (> new-e dur)
                    (make-wrap new-state (cdr queue) (- new-e dur) tr)
                    (make-wrap state queue new-e tr))))))))

(define (state-wrap-next wrap smooth?)
  (match wrap
     (($ <wrap> state queue e tr)
      (cond
        ((null? queue) wrap)
        (else (let ((new-state (if smooth? (state-wrap-get wrap) (car (car queue)))))
                (make-wrap new-state (cdr queue) 0 tr)))))))

(define (state-wrap-get wrap)
  (match wrap
     (($ <wrap> state queue e tr)
      (cond
        ((null? queue) state)
        (else (tr state (car (car queue)) (/ e (cdr (car queue)))))))))

(export #! make-seq-renderer !# state-wrap state-wrap-enqueue state-wrap-update state-wrap-next state-wrap-get)
