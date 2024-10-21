open Stdlib

type t = int16x8

external const8 : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (t[@unboxed]) = "" "caml_int16x8_const8"
        [@@noalloc] [@@builtin]

let _ = const8 0 0 0 0 0 0 0 (Sys.opaque_identity 1)
