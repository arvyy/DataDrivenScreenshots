#lang scribble/base

@(require "util.rkt")

@title{(dds-interp) module reference}

This module provides interpolation between different values. Module is included by default.

@render-fn[
"interp-bridge"
'()
'()
'(("id" "function that returns id for item, by which it's determined the items that entered, stayed, or exited. If id is #f, index is used" "#f")
  ("enter" "function for entering items" "identity")
  ("update" "function for update items" "identity")
  ("exit" "function for exit items" "identity"))
"Returns function, that takes two lists as argument, and returns two lists as bridging result"
]


@render-fn[
"interp"
'(("a" "first value to interpolate from")
  ("b" "second value to interpolate towards")
  ("f" "interpolation factor"))
'(("extra-dispatch" "additional cases for interpolation. Must be a pair of two functions. First function takes a and b, and returns #t if it's suitable for interpolation. Second function takes does the interpolation; its arguments are same as to this base one. (a, b, f, extra-dispatch)" "#f"))
'()
"Returns interpolated value"
]{
}

@render-fn[
"ease"
'(("f" "value from 0 to 1 to be eased"))
'()
'()
"Returns eased value"
]{
}

@render-fn[
"start-end"
'(("start" "lower bound")
  ("end" "upper bound")
  ("f" "value [0 - 1]"))
'()
'()
"Returns interpolated value"
]{
Maps range [0, 1] to [start, end]. Start and end must be within range [0,1]
}
