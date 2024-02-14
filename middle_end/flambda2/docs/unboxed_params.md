# Unboxed function parameters and returns

WARNING: unboxed function parameters and returns only work with Flambda2,
the closure and flambda1 backends ignore the annotations.

## What can be unboxed ?

Function parameters as well as function's return values can be unboxed. However,
this only applies when the type/layout of the unboxed value is one of the following:

- boxed numbers (Float, Int32, Int64, Nativeint, Vec128)
- records/pairs[1]


## How does one unbox a parameter/return value ?

The `[@unboxed]` annotations are used to specify what to unbox in this context.
These annotations can be put either on a function parameter, or on a function declaration
to unbox its return value.

Here are some examples:

```ocaml
(* A regular function with boxed floats as arguments, and returning a boxed float *)
let f_boxed x y = x +. y

(* The same function, but this time with the return value unboxed *)
let[@unboxed] f_return x y = x +. y

(* Again, `f`, but with only the first argument unboxed.
   Note the parentheses around `x`, so that the annotation attaches to the parameter *)
let f_first (x[@unboxed]) y = x +. y

(* Let's define a version of `f` with the first parameter unboxed, and the return
   value unboxed, and mark it as never inline. *)
let[@unboxed] f (x[@unboxed]) y = x +. y [@@inline never]

(* Using the definition of `f` just above, this `main` function does not allocate,
   even with `f` not being inlined. *)
let main t y =
  let x = t +. 1. in
  f x y = 0.
```


## What exactly happens ? What does it mean to unbox a parameter/return ?

Contrary to layouts/kinds/jkinds, the `[@unboxed]` annotation does not really change
the calling convention of functions. However, these annotations alter the compilation
strategy just enough so that the annotated function actually becomes a thin wrapper
around the unboxed version of the function. Later, that wrapper can be inlined (without
inlining the unboxed function itself), so that the boxing of arguments/return values
can be simplified away if possible.

For instance, considering our `f` and `main` function above, here is what the actual generated
code will look like:
```ocaml
(* ***** Source code ***** *)

(* here are the original `f` and `main` functions *)
let[@unboxed] f (x[@unboxed]) y = x +. y [@@inline never]

let main t y =
  let x = t +. 1. in
  f x y = 0.


(* ***** Before Simplification ***** *)
(*
 * Here is the equivalent generated code in pseudo-code, before simplification.
 *
 * Note that the [@inline never] annotation only applies to the inner `f_unboxed`
 * version, therefore allowing `f`, which is actually a small wrapper, to be inlined.
 *
 * The code of `f_unboxed` appear to be bigger and more complex than the original
 * code of `f`, and in general it is true that the transformation applied to the code
 * is not beneficial. However, if the code of `f` only uses the unboxed versions
 * of its unboxed parameters (and/or direclty allocates its return value), then
 * the Flambda2 simplifier will be able to adequately simplify the code.
 *)
let f_unboxed (x : unboxed_float) (y: boxed_float) : unboxed_float =
  let x = box_float unboxed_x in
  let ret =
    (* original code of `f` in pseudo code with explicit allocations *)
    box_float (unbox_float x +. unbox_float y)
    (* end of original code of `f` *)
  in
  unbox_float ret
[@@inline never]

let f (x: boxed_float) (y: boxed_float) : boxed_float =
    let unboxed_x = load_float x in
    box_float (f_unboxed unboxed_x y)



(* ***** After Simplification ***** *)
(*
 * Here is the code after simplification. Notice that the code for the `f_unboxed`
 * function is much better (and simpler) than it was before simplification.
 *
 * Also note that the code for the wrapper of `f` has not changed.
 *)
let f_unboxed (unboxed_x : unboxed_float) (y: boxed_float) : unboxed_float =
  unboxed_x +. (unbox_float y) (* this returns an unboxed float *)
[@@inline never]

let f (x: boxed_float) (y: boxed_float) : boxed_float =
    let unboxed_x = unbox_float x in
    box_float (f_unboxed unboxed_x y)

let main t y =
    let unboxed_x = unbox_float t +. 1. in
    (f_unboxed unboxed_x y) = 0.
```


[1]: The exact criterion is that the type/layout is that of a variant with no
constant cases, and exactly one block case with tag zero.

