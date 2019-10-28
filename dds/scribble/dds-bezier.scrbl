#lang scribble/base

@(require "util.rkt")

@title{(dds-bezier) module reference. }

This module provides bezier interpolation, as well as bezier line construction. You have to include module explicitly to use.

@render-fn[
"bezier"
'()
'()
`(,id
  ("points" "list of points. Each point is a pair (cons) of numbers. Only first and last points are generally on the curve, middle ones are control points" "empty list")
  ("segments" "amount of lines used to render the curve" "20")
  ,@stroke)
"Returns line item definition."
]{
Create a line item definition using bezier control points. The degree depends on the amount of points provided. 
}

@render-fn[
"bezier-interp/quadratic"
'(("p0" "starting value")
  ("c" "control value")
  ("p1" "ending value")
  ("t" "coeficient along the curve, must be in range [0, 1]"))
'()
'()
"Returns interpolated value"
]{
Interpolate value using quadratic bezier function
}

@render-fn[
"bezier-interp/cubic"
'(("p0" "starting value")
  ("c0" "first control value")
  ("c1" "second control value")
  ("p1" "ending value")
  ("t" "coeficient along the curve, must be in range [0, 1]"))
'()
'()
"Returns interpolated value"
]{
Interpolate value using cubic bezier function
}

@render-fn[
"bezier-interp/n"
'(("points" "list of points")
  ("t" "coeficient along the curve, must be in range [0, 1]"))
'()
'()
"Returns interpolated value"
]{
Interpolate value using any degree bezier function (degree depends on amount of points provided).
}
