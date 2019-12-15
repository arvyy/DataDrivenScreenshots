(define-module
  (dds base-impl boot))

(load-extension "dds_native" "boot_init")

(export init-window window-close? rlgl-draw begin-draw end-draw clear-bg get-frame-time)
