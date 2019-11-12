(define-module
  (dds base input))

(load-extension "dds_native" "input_init")

(export get-mouse-pos is-mouse? is-key? get-key-pressed)
