open Stdlib

type t = int16x8

external const1 : (int[@untagged]) -> (t[@unboxed]) = "" "caml_int16x8_const1"
        [@@noalloc] [@@builtin]

let _ = const1 (Sys.opaque_identity 1)
