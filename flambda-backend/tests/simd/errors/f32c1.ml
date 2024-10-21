open Stdlib

type t = float32x4

external const1 : float -> t = "" "caml_float32x4_const1"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const1 (Sys.opaque_identity 1.0)
