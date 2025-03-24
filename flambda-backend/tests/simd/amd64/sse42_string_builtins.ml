module SSE42_String = struct
  (* These also work with int16x8s, given the 16-bit char encoding immediate
     bit *)

  external cmpestrm :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int8x16[@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrm"
    [@@noalloc] [@@builtin]

  external cmpestra :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestra"
    [@@noalloc] [@@builtin]

  external cmpestrc :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrc"
    [@@noalloc] [@@builtin]

  external cmpestri :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestri"
    [@@noalloc] [@@builtin]

  external cmpestro :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestro"
    [@@noalloc] [@@builtin]

  external cmpestrs :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrs"
    [@@noalloc] [@@builtin]

  external cmpestrz :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrz"
    [@@noalloc] [@@builtin]

  external cmpistrm :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrm"
    [@@noalloc] [@@builtin]

  external cmpistra :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistra"
    [@@noalloc] [@@builtin]

  external cmpistrc :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrc"
    [@@noalloc] [@@builtin]

  external cmpistri :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistri"
    [@@noalloc] [@@builtin]

  external cmpistro :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistro"
    [@@noalloc] [@@builtin]

  external cmpistrs :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrs"
    [@@noalloc] [@@builtin]

  external cmpistrz :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrz"
    [@@noalloc] [@@builtin]
end
