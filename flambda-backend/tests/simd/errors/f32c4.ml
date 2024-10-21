open Stdlib

type t = float32x4

external const4 : float -> float -> float -> float -> t = "" "caml_float32x4_const4"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const4 0.0 0.0 (Sys.opaque_identity 1.0) 0.0
