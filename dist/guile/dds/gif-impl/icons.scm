(define-module
  (dds gif-impl icons))

(use-modules
  (dds base)
  (dds bezier))

(define play-icon
  (list 
    (bezier 
    #:points '((p 0 0)
               (p 0 100)
               (p 100 50)))))

(define pause-icon
    (list
      (line #:points '((0 . 0)
                       (0 . 100)
                       (33 . 100)
                       (33 . 0)))
      (line #:points '((66 . 0)
                       (66 . 100)
                       (100 . 100)
                       (100 . 0)))))

(define +speed-icon
  (list
    (line #:points '((50 . 0)
                     (0 . 100)
                     (100 . 100)))))

(define -speed-icon
  (list
    (line #:points '((0 . 0)
                     (50 . 100)
                     (100 . 0)))))

(define rec-icon
  (list
    (line #:points '((0 . 20)
                     (0 . 80)
                     (60 . 80)
                     (60 . 20)))
    (line #:points '((50 . 50)
                     (100 . 80)
                     (100 . 20)))))

(export play-icon pause-icon +speed-icon -speed-icon rec-icon)
