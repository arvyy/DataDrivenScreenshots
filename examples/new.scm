(use-modules
  (dds base boot)
  (dds base))

(init-window 640 480 "test")

(let it ((close? #f))
  (if close? #t
      (begin
        (begin-draw)
        (clear-bg #(0 255 0))
        (draw (circle #:x 50 #:y 50))
        (end-draw)
        (it (window-close?)))))
