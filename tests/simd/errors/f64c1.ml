open Stdlib

type t = float64x2

external const1 : float -> t = "" "caml_float64x2_const1"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const1 (Sys.opaque_identity 1.0)
