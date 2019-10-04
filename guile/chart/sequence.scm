(define-module
  (chart sequence))

(use-modules
  (srfi srfi-1)
  (srfi srfi-9)
  (ice-9 match)
  (chart base))

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
                    ; call recursively self, since delta might be bigger than next state's duration
                    (state-wrap-update (make-wrap new-state (cdr queue) 0 tr) (- new-e dur))
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

(define (state-wrap-empty? wrap)
  (null? (wrap-queue wrap)))

(export state-wrap state-wrap-enqueue state-wrap-update state-wrap-next state-wrap-get state-wrap-empty?)
