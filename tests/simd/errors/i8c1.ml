open Stdlib

type t = int8x16

external const1 : (int[@untagged]) -> (t[@unboxed]) = "" "caml_int8x16_const1"
        [@@noalloc] [@@builtin]

let _ = const1 (Sys.opaque_identity 1)
