open Stdlib

type t = int32x4

external const4 : int32 -> int32 -> int32 -> int32 -> t = "" "caml_int32x4_const4"
        [@@noalloc] [@@unboxed] [@@builtin]

let _ = const4 0l 0l (Sys.opaque_identity 1l) 0l
