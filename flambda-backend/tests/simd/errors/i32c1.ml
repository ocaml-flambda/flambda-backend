open Stdlib

type t = int32x4

external const1 : int32 -> t = "" "caml_int32x4_const1"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const1 (Sys.opaque_identity 1l)
