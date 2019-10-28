#lang scribble/base

@(require "util.rkt")

@(begin
(define item-def-note (list (bold "Note: ") "All key arguments, aside from id, are item definition properties. They either can be expressed as values, or as functions that take data as argument. Only item definitions that have values for all properties can be drawn; see " (secref "apply-data") " for converting item with properties as function expressions into values"))
)

@title{(dds base) module reference.}

The base module of dds. Module is included by default.

@render-fn[
"apply-data"
'(("item-def" ("item definition to apply data on."))
  ("data" "data to apply."))
'(("overrides" "list of overrides to use when applying data." "empty list"))
'()
"Return item definition with properties calculated"
]{
Applies given data to item definition with given overrides. All properties expressed as functions of data get calculated, and returned value has all properties as values. Returns a copy - the argument is not modified.
}

@render-fn[
"draw"
`(("item-def" ("item definition to be drawn; see item definition. All properties must have been resolved into exact values; see " ,(secref "apply-data"))))
'(("render texture" "render texture to drawn on, or #f to draw on screen." "#f"))
'()
"Return value unspecified"
]{
Renders item definition either to screen or onto render texture; see @secref{load-render-texture}.
}

@render-fn[
"rect"
'()
'()
`(,id
  ("x" "left side x position" "0")
  ("y" "top y position" "0")
  ("width" "width" "100")
  ("height" "height" "100")
  ("roundness" "scale on how much rounding is done on corners, interval [0, 1]" "0")
  ,@fill+stroke
  ("segments" "amount of segments used for rendering rounded corners" "20"))
"Returns rectangle item definition."
]{
Create rectangle item definition.
@item-def-note
}

@section{rect-o}

Same as @secref{rect}, except all defaults are #f instead. Meant to be used in overrides.

@render-fn[
"circle"
'()
'()
`(,id
  ("x" "center x position" "0")
  ("y" "center y position" "0")
  ("radius" "outter radius of the circle" "100")
  ("inner-radius" "inner radius of the circle" "0")
  ("start-angle" "sector start angle" "0")
  ("end-angle" "sector end angle" "0")
  ,@fill+stroke
  ("segments" "number of segments used" "20"))
"Returns circle item definition"
]{
Create circle item definition.
@item-def-note
}

@section{circle-o}

Same as @secref{circle}, except all defaults are #f instead. Meant to be used in overrides. See override.

@render-fn[
"triangle"
'()
'()
`(,id
  ("points" "list of points. Each point is a pair (cons) of numbers. Can be have more than 3 points, in which case a triangle strip is made." "empty list")
  ,@fill+stroke)
"Returns triangle item definition."
]{
Create triangle item definition.
@item-def-note
}

@section{triangle-o}

Same as @secref{triangle}, except all defaults are #f instead. Mean to be used in overrides.

@render-fn[
"line"
'()
'()
`(,id
  ("points" "list of points. Each point is a pair (cons) of numbers. Can be have more than 3 points, in which case a line strip is made." "empty list")
  ,@stroke)
"Returns line item definition."
]{
Create line item definition.
@item-def-note
}

@section{line-o}

Same as @secref{line-o}, except all defaults are #f instead. Meant to be used in overrides.

@render-fn[
"t-rect"
'()
'()
`(,id
  ("x" "left side x position" "0")
  ("y" "top y position" "0")
  ("width" "width. Can be #f, in which case texture width is used" "#f")
  ("height" "height. Can be #f, in which case texture height is used" "#f")
  ("src-x" "left side x position in texture" "0")
  ("src-y" "top y position in texture" "0")
  ("src-width" "width of texture region. Can be #f, in which case texture height is used" "#f")
  ("src-height" "height texture region. Can be #f, in which case texture height is used" "#f")
  ("color" "color to tint the texture" "(color 255 255 255)")
  ("texture" "texture to be drawn" "#f"))
"Returns texture rectangle definition."
]{
Draw a texture. x,y,width,height define rectangle in draw target dimension onto which the texture will be drawn. src-x, src-y, src-width, src-height define rectangle in texture dimension, which texture region will be drawn.
}

@section{t-rect-o}

Same as @secref{t-rect}, except all defaults are #f instead. Meant to be used in overrides.

@render-fn[
"text"
'()
'()
`(,id
  ("text" "The text to be drawn" "\"\"")
  ("x" "x position" "0")
  ("y" "y position" "0")
  ("font" "font to use, or #f for default" "#f")
  ("font-size" "font size" "12")
  ("spacing" "space between letters in text" "2")
  ("color" "text color" "(color 10 10 10)"))
"Returns text item definition"
]{
Draws text. See load-font.
}

@section{text-o}

Same as @secref{text}, except all defaults are #f instead. Meant to be used in overrides.

@render-fn[
"cnt"
'()
'()
`(,id
  ("transform" "3*2 transformation matrix of the container. See combine, rotate, translate, scale" "(translate 0 0)")
  ("blend" "blending used for items, expressed as a pair of source and destination factors represented as symbols. Symbols are same as OpenGL constants; eg. (cons 'GL_ONE 'GL_SRC_ALPHA). Can be #f to disable blending. See blending" "#f")
  ("post-processing" "list of shaders to use. Shader can be shader, or can be a function of data that returns a shader. See shader, shaders." "#f")
  ("items" (,(bold "Not a property.") " Either a list of child item definitions, or a function for data propagation. Function takes data as argument, and returns list of pairs - each pair is of child item def and child datum. See cnt-items-tpl, cnt-items-transf" "empty list") "empty list"))
"Returns container item definition"
]{
Container for grouping items, applying transformations, and shaders.
}

@section{cnt-o}

Same as @secref{cnt-o}, except all defaults are #f instead. Meant to be used in overrides. Note, that "items" is not overrideable.

@render-fn[
"color"
'(("r" "red; 0-255")
  ("g" "green; 0-255")
  ("b" "blue; 0-255"))
'(("a" "alpha; 0-255" "255"))
'()
"Returns a color structure."
]{
Create RGB color structure. 
}

@render-fn[
"combine"
'(("transform ... " "One or more 3*2 transformation matrices to combine. See translate, scale, rotate"))
'()
'()
"Returns new transformation matrix"
]{
Combine multiple transformation matrices into one.
}

@render-fn[
"rotate"
'(("angle" "angle in degrees by which rotate around origin clock wise"))
'()
'()
"Returns transformation matrix"
]{
Create rotation transformation matrix.
}

@render-fn[
"translate"
'(("x" "translation along x axis")
  ("y" "translation along y axis"))
'()
'()
"Returns tranformation matrix"
]{
Create translation transformation matrix.
}

@render-fn[
"scale"
'(("xk" "scale factor along x axis")
  ("yk" "scale factor along y axis"))
'()
'()
"Returns transformation matrix."
]{
Create scaling transformation matrix.
}

@section{calc}

@bold{calc} is same as a lambda with one argument, except instead of a simple variable binding, a pattern match is used.

ie (calc (@italic{pattern}) ...) is equivalent to (lambda (arg) (match arg (@italic{pattern} ...)))

See Guile pattern matching href.

@render-fn[
"cnt-items-tpl"
'(("item-def" "item definition used as template")
  ("children-data" "either a list of data, or a function of data that returns list of data"))
'()
'()
"Returns function conformant with cnt #:items"
]{
Allows creating multiple container items with same definition, but different data. See cnt.
}

@render-fn[
"cnt-items-transf"
'(("item-def-list" "list of items")
  ("data-transformer" "function that takes data, and returns transformed data"))
'()
'()
"Returns function conformant with cnt items"
]{
Allows converting data into another data. Might be useful for using a common subtree which has defined data format.
}

@section{compute}

Syntax for deriving property from existing properties. 

(compute (arguments ...)  computation-body) . Arguments can be either values, or functions of data that returns value. Syntax produces a function of data, which when applied executes the computation body, which is suitable to be used as a property in item definitions.

@render-fn[
"shader"
'(("shader-id" "shader to use. See load-shader")
  ("uniforms ... " ("zero or more uniforms for given shader.")))
'()
'()
"Returns a shader for use in cnt #:post-processing"
]{
Prepares to use a shader inside container. Each uniform must match one of the following patterns. See get-shader-loc for obtaining @italic{location} value.
@itemlist[
@item{('vec location value) - Set vector uniform. Must be a scheme vector, eg (vector 1 2). Floats are also set this way using a vector of length 1. }
@item{('cnt-transform location) - Set matrix uniform with the value of current container transformation. Useful when shader needs to be applied not in screen dimension, but in container. }
@item{('matrix location value) - Set matrix uniform.}
@item{('matrix/invert location value) - Set matrix uniform, which gets inverted before being set.}
@item{('texture location value) - Set texture (sampler2d) uniform. Note, that max 3 textures may be used in same shader. See load-texture, render-texture->texture}
]
}

@render-fn[
"override"
'(("cnt-branch-pred" "predicate function for testing if this override matches a given container branch. Branch is expressed as a list of id of containers in the branch; last item in branch might another item. See pattern")
  ("override-item-def" "override item definition. Each item property must be either a value, or a function of initial (before override) value and datum for the item"))
'()
'()
"Returns an override definition to be used in apply-data"
]{
"Override any item matching the path and type with provided properties. In some way similar to CSS. "
}

@section{pattern}

Syntax for defining predicate function that returns true if pattern matches input. Used with @secref{override}.

(pattern @italic{pattern} ...) is equivalent to (lambda(branch) (match branch (@italic{pattern} #t) (_ #f)))

@render-fn[
"dynamic-item"
'(("item-fact" "function that takes data as argument, and returns item definition"))
'()
'()
"Returns dynamic item definition"
]{
Create dynamic depending on provided data. Note, that returned item gets data applied to it at later step.
}

@render-fn[
"load-texture"
'(("file" "path to file to load as a texture"))
'()
'()
"Texture reference"
]{
Load texture from file. 
}

@render-fn[
"create-render-texture"
'(("width" "width of texture in pixels")
  ("height" "height of texture in pixels"))
'()
'()
"Returns render texture reference"
]{
Create a render texture, onto which you can draw stuff. See @secref{draw}.
}

@render-fn[
"clear-render-texture"
'(("renderTextre" "reference to render texture. See load-render-texture"))
'()
'()
"Return is unspecified."
]{
Clears render texture with blank color.
}

@render-fn[
"render-texture->texture"
'(("renderTexture" "render texture reference. See create-render-texture"))
'()
'()
"Returns reference to texture"
]{
Prepare render texture to be used as texture. Note that this function doesn't copy - changes to render texture will also mean changes to this returned texture.
}

@render-fn[
"load-shader"
'(("vertexFile" "vertex shader file path, or #f to use a default vertex shader")
  ("fragmentFile" "fragment shader file path, or #f to use a default fragment shader"))
'()
'()
"Returns shader reference"
]{
Load shader from file. 
}

@render-fn[
"get-shader-loc"
'(("shader" "shader reference. See load-shader")
  ("uniformName" "uniform name in the shader"))
'()
'()
"Returns shader location id"
]{
Get shader location using uniform name.
}


@render-fn[
"load-font"
'(("font-file" "font file path"))
'()
'()
"Returns font reference"
]
