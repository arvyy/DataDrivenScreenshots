(define-module
  (dds gif-impl runner))

(use-modules
  (srfi srfi-1)
  (srfi srfi-9)
  (ice-9 format)
  (ice-9 match)
  (srfi srfi-9 gnu)
  (dds base)
  (dds gif-impl record)
  (dds gif-impl slider)
  (dds gif-impl button)
  (dds gif-impl icons))

(define-record-type <gif-state>
  (make-gif-state t duration paused? playspeed using-seek? seek-range loop? recording buttons)
  gif-state?
  (t gif-state-t)
  (duration gif-state-duration)
  (paused? gif-state-paused?)
  (playspeed gif-state-playspeed)
  (using-seek? gif-state-using-seek?)
  (seek-range gif-state-seek-range)
  (loop? gif-state-loop?)
  (recording gif-state-recording)
  (buttons gif-state-buttons))

(define overlay-height 80)
(define seeker-height 20)
(define buttons-height 40)

(define (get-overlay range height allow-ss?)
  (define range-width (- (cdr range) (car range)))
  (define (time-text t speed)
    (define minute (floor (/ t 60)))
    (define minute-txt
      (format #f "~a~a" (if (< minute 10) "0" "") (inexact->exact minute)))
    (define sec (floor (floor-remainder t 60)))
    (define seconds-txt
      (format #f "~a~a" (if (< sec 10) "0" "") (inexact->exact sec)))
    (format #f "~a:~a x~a" minute-txt seconds-txt speed))
  (define btn-center (+ (car range) (* 0.5 (- (cdr range) (car range)))))
  (define not-duronly-items
    (if allow-ss?
        (list
          (button
            #:id 'record
            #:size 16
            #:x (cdr range)
            #:y (+ seeker-height (* 0.5 buttons-height))
            #:icon-lines rec-icon
            #:state (calc (d)
                          (define s (find (lambda(d) (equal? 'record (car d)))
                                          (gif-state-buttons d)))
                          (car (cdr s)))))
        (list)))
  (define duronly-items
    (list (cnt #:items (slider #:width range-width
                  #:knob-pos (calc (($ <gif-state> t duration)) (/ t duration)))
               #:transform (translate (car range) (/ seeker-height 2)))
          (rect #:fill #f
                #:stroke #f
                #:width range-width
                #:y 0
                #:height seeker-height
                #:id 'seeker-area)
          (text #:text (lambda(d)
                         (time-text (gif-state-t d)
                                    (gif-state-playspeed d)))
                #:y 15
                #:x (car range))
          (button
            #:id '+speed-btn
            #:x (+ btn-center (* 0.75 buttons-height))
            #:y (+ seeker-height (* 0.25 buttons-height))
            #:icon-lines +speed-icon
            #:size (* 0.25 buttons-height)
            #:state (calc (d)
                          (define s (find (lambda(d) (equal? '+speed-btn (car d)))
                                          (gif-state-buttons d)))
                          (car (cdr s))))
          (button
            #:id '-speed-btn
            #:x (+ btn-center (* 0.75 buttons-height))
            #:y (+ seeker-height (* 0.75 buttons-height))
            #:icon-lines -speed-icon
            #:size (* 0.25 buttons-height)
            #:state (calc (d)
                          (define s (find (lambda(d) (equal? '-speed-btn (car d)))
                                          (gif-state-buttons d)))
                          (car (cdr s))))

          (button 
            #:id 'play-btn
            #:x btn-center
            #:y (+ seeker-height (* 0.5 buttons-height))
            #:size (* 0.5 buttons-height)
            #:icon-lines (calc (d) 
                               (if (gif-state-paused? d)
                                   play-icon
                                   pause-icon))
            #:state (calc (d)
                          (define s (find (lambda(d) (equal? 'play-btn (car d)))
                                          (gif-state-buttons d)))
                          (car (cdr s))))
          ))
  (cnt #:items (lambda(d)
                 (define dur (gif-state-duration d))
                 (if (or (not dur) (<= dur 0))
                     not-duronly-items
                     (append not-duronly-items duronly-items)))
       #:transform (translate 0 height)))

(define (update-buttons state overlay-cnt)
  (define hover (hover-lst overlay-cnt (get-mouse-pos)))
  (define click? (is-mouse? 'left 'pressed))
  (define delta (get-frame-time))
  (define new-buttons+callbacks
    (map (calc((id btn-state callback))
           (define btn-hover (find (lambda(d) (equal? id (item-id d))) hover))
           (define new-btn (list id (update-button-state btn-state delta btn-hover (and btn-hover click?)) callback))
           (define cb (if (and click? btn-hover) callback #f))
           (cons new-btn cb))
         (gif-state-buttons state)))
  (define new-buttons (map car new-buttons+callbacks))
  (define callbacks (filter identity (map cdr new-buttons+callbacks)))
  (define callback
    (if (null? callbacks)
        identity
        (apply compose callbacks)))
  (define state-post-callback
    (callback state))
  (set-fields state-post-callback
              ((gif-state-buttons) new-buttons)))

(define (update-t/frame-time state fps)
  (let ((t (gif-state-t state))
        (playspeed (gif-state-playspeed state))
        (paused? (gif-state-paused? state))
        (loop? (gif-state-loop? state))
        (duration (gif-state-duration state)))
    (set-fields state
                ((gif-state-t) (let ((newt (+ t (* playspeed (/ 1 fps)))))
                                 (cond
                                   (paused? t)
                                   (else newt)))))))

(define (update-t/loop state)
  (let ((t (gif-state-t state))
        (loop? (gif-state-loop? state))
        (duration (gif-state-duration state))
        (using-seek? (gif-state-using-seek? state)))
    (set-fields state
                ((gif-state-t) (cond
                                 ((> t duration)
                                  (if (and loop? (not using-seek?))
                                      0
                                      duration))
                                 (else t))))))

(define (update-t/seek state)
  (let ((using-seek? (gif-state-using-seek? state))
        (seek-range (gif-state-seek-range state))
        (duration (gif-state-duration state)))
    (cond
      ((not using-seek?) state)
      (else (let* ((mouse (get-mouse-pos))
                   (mouse-x (car mouse))
                   (part (/ (- mouse-x (car seek-range)) (- (cdr seek-range) (car seek-range))))
                   (part (min 1 (max 0 part))))
              (set-fields state
                          ((gif-state-t) (* part duration))))))))

(define (update-seek? state overlay-cnt)
  (match state
         (($ <gif-state> t duration paused? playspeed using-seek? seek-range)
          (cond
            ((and (find (lambda(i)
                          (and (rect? i)
                               (equal? (item-id i) 'seeker-area)))
                        (hover-lst overlay-cnt (get-mouse-pos)))
                  (is-mouse? 'left 'down)
                  (not using-seek?))
             (set-fields state
                         ((gif-state-using-seek?) #t)))
            ((and using-seek? (not (is-mouse? 'left 'down)))
             (set-fields state
                         ((gif-state-using-seek?) #f)))
            (else state)))))

(define (toggle-paused state)
  (set-fields state
              ((gif-state-paused?) (not (gif-state-paused? state)))))

(define (begin-recording state)
  (set-fields state
              ((gif-state-recording) 0)
              ((gif-state-loop?) #f)
              ((gif-state-paused?) #f)
              ((gif-state-playspeed) 1)))

(define (end-recording state)
  (set-fields state
              ((gif-state-recording) #f)
              ((gif-state-loop?) #t)))

(define (speed-up state)
  (define s (gif-state-playspeed state))
  (define new-s (min 16 (* 2 s)))
  (set-fields state
              ((gif-state-playspeed) new-s)))

(define (speed-down state)
  (define s (gif-state-playspeed state))
  (define new-s (max 0.125 (* 0.5 s)))
  (set-fields state
              ((gif-state-playspeed) new-s)))

(define (update-gif-state state overlay-cnt fps rec-output)
  (if (gif-state-recording state)
      (update-gif-state/recording state rec-output fps)
      (update-gif-state/normal state overlay-cnt fps)))

(define (update-gif-state/normal state overlay-cnt fps)
  (define dur (gif-state-duration state))
  (define has-dur? (and dur (> dur 0)))
  (cond
    (has-dur?
      (let ()
        (define (update-seek?/pr state)
          (update-seek? state overlay-cnt))
        (define (update-buttons/pr state)
          (update-buttons state overlay-cnt))
        (define (update-t/frame-time/pr state)
          (update-t/frame-time state fps))
        (define new-state ((compose update-t/frame-time/pr update-t/loop update-t/seek update-seek?/pr update-buttons/pr) state))
        new-state
        ))
    (else (let ()
        (define (update-buttons/pr state)
          (update-buttons state overlay-cnt))
        (update-buttons/pr state)))))

(define (update-gif-state/recording state rec-output fps)
  (take-screenshot (format #f "~a/frame_~a.png" rec-output (gif-state-recording state)))
  (let* ((state* (update-t/frame-time state fps))
         (t (gif-state-t state*))
         (duration (gif-state-duration state*)))
    (if (or (not duration) (= 0 duration) (> t duration))
        (end-recording state*)
        (set-fields state*
                    ((gif-state-recording) (+ 1 (gif-state-recording state*)))))))

(define* (run record #:optional (module-reloader #f))
  (define dur (dds-gif-duration record))
  (define width (dds-gif-width record))
  (define height (dds-gif-height record))
  (define fps (dds-gif-fps record))
  (define slider-range (cons 50 (- width 50)))
  (define overlay-cnt (get-overlay slider-range height (dds-gif-rec-output record)))
  (define rec-output (dds-gif-rec-output record))
  (define gif-state (make-gif-state 0 dur #f 1 #f slider-range #t #f
                                    (list 
                                      (list 'record button-init-state begin-recording)
                                      (list 'play-btn button-init-state toggle-paused)
                                      (list '+speed-btn button-init-state speed-up)
                                      (list '-speed-btn button-init-state speed-down))))
  (init-window width (+ overlay-height height) fps "DDS Gif Runner")
  (let it ((close #f)
           (gif-state gif-state)
           (until-reload 0.25))
    (define item-getter (dds-gif-item-getter record))
    (define over-cnt/applied (apply-data overlay-cnt gif-state))
    (define new-until-reload
      (let ((r (- until-reload (get-frame-time))))
        (if (< r 0)
            (begin
              (when (and module-reloader (not (gif-state-recording gif-state)))
                (module-reloader))
              0.25)
            r)))
    (begin-draw)
    (clear-bg (vector 255 255 255))
    (draw (item-getter (gif-state-t gif-state)))
    (when (not (gif-state-recording gif-state))
      (draw over-cnt/applied))
    (end-draw)
    (if close #f
        (it (window-close?)
            (update-gif-state gif-state over-cnt/applied fps rec-output)
            new-until-reload))))

(export run)
