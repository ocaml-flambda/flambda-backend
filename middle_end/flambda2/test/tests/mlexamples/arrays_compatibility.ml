(* Example from comby: an array access was improperly simplified to Invalid.

   The issue was that the array is seen as an array of immediates by the
   primitive, but its type from the environment is an array of values. The two
   kinds are compatible, so the array access is valid. *)

module Offset : sig
  type index_t

  val empty : index_t

  val index : source:string -> index_t

  val convert_fast : offset:int -> index_t -> int * int
end = struct
  type index_t = int array

  let empty = [||]

  let[@inline never] [@local never] index ~source =
    Sys.opaque_identity [| 0; 0 |]

  let[@inline always] convert_fast ~offset index =
    let line = Sys.opaque_identity 0 in
    let col =
      if line = 0 || line = 1 then offset + 1 else offset - index.(line - 1) + 1
    in
    line, col
end

let f fast source offset =
  let index = if fast then Offset.index ~source else Offset.empty in
  (Offset.convert_fast [@inlined always]) ~offset index
