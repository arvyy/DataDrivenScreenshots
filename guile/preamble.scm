(add-to-load-path (dirname (current-filename)))

(use-modules
  (srfi srfi-42)
  (srfi srfi-19)
  (ice-9 format)
  (chart base)
  (chart sequence)
  (chart stdshader)
  (chart interp))

(define width 800)
(define height 450)
(define bg-color #(250 250 250))
(define fps 60)

(define (output-folder script-file)
  (define now (current-date))
  (define name (substring script-file 0 (string-rindex script-file ".")))
  (format #f "~a_~a-~a-~a" 
          name 
          (date-year now)
          (date-month now)
          (date-day now)))

(define max-frames 1000)

(define (output-frame i)
  (format #f "frame-~a.png" i))

(define (init-data) 
  0)

(define (key-press key data)
  data)

(define (debug-info data)
  data)

(define (update frame-time data)
  data)

(define (stop? data)
  #f)
