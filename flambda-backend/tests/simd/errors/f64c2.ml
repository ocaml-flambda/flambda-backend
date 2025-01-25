open Stdlib

type t = float64x2

external const2 : float -> float -> t = "" "caml_float64x2_const2"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const2 0.0 (Sys.opaque_identity 1.0)
