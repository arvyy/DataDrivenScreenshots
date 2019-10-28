(add-to-load-path (dirname (current-filename)))
(add-to-load-path "./extensions")

(use-modules
  (srfi srfi-42)
  (srfi srfi-19)
  (ice-9 format)
  (dds base)
  (dds sequence)
  (dds stdshader)
  (dds interp)
  (dds scale)
  (dds blend))

(define width 800)
(define height 450)
(define bg-color #(250 250 250))
(define fps 120)
(define loop? #t)

(define output-folder  "./export")

(define max-frames 1000)

(define (output-frame script-file i)
  (define now (current-date))
  (define last-slash (string-rindex script-file #\/))
  (define name (substring script-file 
                          (if last-slash (+ 1 last-slash) 0)
                          (string-rindex script-file #\.)))
  (format #f "~a_~a-~a-~a_frame-~a.png" 
          name 
          (date-year now)
          (date-month now)
          (date-day now)
          (string-pad (number->string i) 5 #\0)))

(define (init-data) 
  0)

(define (init-render) 
  #f)

(define (render data)
  (text #:text "(define (render data) ...)"
        #:x 50
        #:y 50))

(define (debug-data data)
  data)

(define (update delta data)
  (+ delta data))

(define (stop? data)
  #f)

