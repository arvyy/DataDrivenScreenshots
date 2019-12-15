(define-module
  (dds gif-impl record))

(use-modules
  (dds base-impl primitives-syntax)
  (dds base)
  (srfi srfi-9))

(define-record-type+fact <dds-gif> dds-gif-fact
  (make-dds-gif width height duration fps rec-output item-getter)
  dds-gif?
  (width dds-gif-width)
  (height dds-gif-height)
  (duration dds-gif-duration)
  (fps dds-gif-fps)
  (rec-output dds-gif-rec-output)
  (item-getter dds-gif-item-getter))

(define dds-gif (dds-gif-fact (make-dds-gif 600 480 #f 60 #f (const (text #:x 100 #:y 100 #:text "Hello world")))))

(export <dds-gif> dds-gif dds-gif? dds-gif-width dds-gif-height dds-gif-duration dds-gif-item-getter)
