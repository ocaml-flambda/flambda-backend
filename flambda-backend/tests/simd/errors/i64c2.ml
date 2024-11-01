open Stdlib

type t = int64x2

external const2 : int64 -> int64 -> t = "" "caml_int64x2_const2"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const2 0L (Sys.opaque_identity 1L)
