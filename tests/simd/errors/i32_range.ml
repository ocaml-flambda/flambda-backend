open Stdlib

type t = int32x4

external const1 : (int[@untagged]) -> (t[@unboxed]) = "" "caml_int32x4_const1"
        [@@noalloc] [@@builtin]

let _ = const1 0x100000000
