open Stdlib

type t = int64x2

external const1 : int64 -> t = "" "caml_int64x2_const1"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const1 (Sys.opaque_identity 1L)
