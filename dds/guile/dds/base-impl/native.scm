(define-module
  (dds base-impl native))

(load-extension "dds/dds_native" "base_init")

(export 
    script-args
    set-draw-target 
    draw-rect* draw-text* draw-t-rect* draw-circle* draw-line* draw-triangle*
    apply-transform invert-transform
    load-shader get-shader-loc 
    load-font text-size*
    set-shader-value set-shader-value-texture set-shader-value-matrix set-shader-value-matrix/invert
    create-render-texture clear-render-texture render-texture->texture load-texture
    push-pp-texture pop-pp-texture begin-pp-chain pp-chain-next)
