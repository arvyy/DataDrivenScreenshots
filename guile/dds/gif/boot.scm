(define-module
  (dds gif boot))

(use-modules
  (dds base)
  (dds gif))

(when (< (length (command-line)) 1)
  (error "No script file specified"))

(define script-file (cadr (command-line)))
(define this-module (current-module))
(define get-dds-gif #f)
(define last-load-time #f)

(define (sleep-while-not-exists)
  (if (file-exists? script-file)
      #t
      (begin
        (usleep 10000)
        (sleep-while-not-exists))))

(define (load-script-file)
  (cond
    ((or (not last-load-time)
         (not (file-exists? script-file))
         (< last-load-time (stat:mtime (stat script-file))))
     (let ()
       (define prev (set-current-module this-module))
       (sleep-while-not-exists)
       (set! last-load-time (stat:mtime (stat script-file)))
       (format #t "reloading script ~a\n" script-file)
       (primitive-load script-file)
       (set-current-module prev)))
    (else #t)))

(load-script-file)

(when (not get-dds-gif)
  (error "Script file doesn't override 'get-dds-gif' function."))

(run get-dds-gif load-script-file)
