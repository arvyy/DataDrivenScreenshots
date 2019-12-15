(use-modules
  (srfi srfi-1)
  (srfi srfi-42)
  (ice-9 match)
  (dds interp)
  (dds mathutil))

(define seg 10)
(define r 100)
(define w r)
(define duration 2)

;start & end -- degrees
(define (slice-points start end)
  (define step (/ (- end start) seg))
  (define arc-points (reverse (list-ec (: i 0 (+ seg 1)) 
                              (let ((a (+ start (* i step))))
                                (cons
                                  (* r (cos (->rad a)))
                                  (* r (sin (->rad a))))))))
  (define center-point (cons 0 0))
  (append arc-points (list center-point center-point)))

;start & end -- y pos
(define (bar-points start end)
  (define step (/ (- end start) seg))
  (define right-points (list-ec (: i 0 (+ seg 1))
                              (cons w (+ start (* step i)))))
  (define left-points (list (cons 0 end)
                            (cons 0 start)))
  (append right-points left-points))

; returns function of t that returns points
(define (t->points total offset value)
  (define start-angle (* 360 (/ offset total)))
  (define end-angle (* 360 (/ (+ offset value) total)))
  (define points/slice (slice-points start-angle end-angle))
  (define points/bar (bar-points (+ value offset) offset))
  (lambda(t)
    (interp/clamp points/slice points/bar t)))

; takes entries (list (vector stroke fill value))
; returns (list (vector stroke fill t->points))
(define (layout-entries entries)
  (define total (fold (lambda(entry sum)
                        (match entry
                               (#(_ _ value) (+ sum value))))
                      0
                      entries))
  (define (it entries rez partial-total)
    (match entries
       (() (reverse rez))
       ((#(stroke fill value) . rest)
        (it rest
            (cons (vector stroke fill (t->points total partial-total value))
                  rez)
            (+ partial-total value)))))
  (it entries '() 0))

(define (get-entries-at-time entries t)
  (map
    (calc(#(stroke fill t->points))
      (vector stroke fill (t->points t)))
    entries))

(define chart-entry
  (line #:stroke (calc(#(stroke _ _)) stroke)
        #:stroke-width 3
        #:fill (calc(#(_ fill _)) fill)
        #:points (calc(#(_ _ points)) points)))

(define chart
  (cnt #:items (cnt-items-tpl
                 chart-entry
                 identity)
       #:transform (translate 200 200)))

;====================

(define data
  (list
    (vector (color 255 100 100)
            (color 200 0 0)
            50)
    (vector (color 100 255 100)
            (color 0 200 0)
            60)
    (vector (color 100 100 255)
            (color 0 0 200)
            30)
    (vector (color 255 255 100)
            (color 200 200 0)
            40)))

(define data/layed-out
  (layout-entries data))

(define (item-getter t)
  (apply-data chart (get-entries-at-time data/layed-out (ease (start-end 0.3 0.9 (/ t duration))))))

(define get-dds-gif
  (dds-gif #:duration duration
           #:item-getter item-getter))
