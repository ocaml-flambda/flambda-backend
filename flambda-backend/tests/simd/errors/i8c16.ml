open Stdlib

type t = int8x16

external const16 : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (t[@unboxed]) = "" "caml_int8x16_const16"
        [@@noalloc] [@@builtin]

let _ = const16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Sys.opaque_identity 1)
