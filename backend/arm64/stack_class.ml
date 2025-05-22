[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

module T = struct
  type t =
    | Int64
    | Float64
    | Vector128

  let all = [Int64; Float64; Vector128]

  (* Preserves original ordering: int below float. *)
  let frame_order = [| Vector128; Int64; Float64 |]

  let equal : t -> t -> bool =
   fun left right ->
    match left, right with
    | Int64, Int64 -> true
    | Float64, Float64 -> true
    | Vector128, Vector128 -> true
    | (Int64 | Float64 | Vector128), _ -> false

  let hash : t -> int = function Int64 -> 0 | Float64 -> 1 | Vector128 -> 2

  let tag : t -> string = function
    | Int64 -> "i"
    | Float64 -> "f"
    | Vector128 -> "x"

  let print : Format.formatter -> t -> unit =
   fun ppf stack_class ->
    Format.fprintf ppf "%s"
      (match stack_class with
      | Int64 -> "int64"
      | Float64 -> "float64"
      | Vector128 -> "vector128")

  let size_in_bytes : t -> int = function
    | Int64 -> 8
    | Float64 -> 8
    | Vector128 -> 16

  let of_machtype : Cmm.machtype_component -> t = function
    | Val | Int | Addr -> Int64
    | Float | Float32 -> Float64
    | Vec128 | Valx2 -> Vector128
    | Vec256 | Vec512 ->
      Misc.fatal_error "Vec256 and Vec512 not supported on ARM64"
end

include T
module Tbl = Stack_class_utils.Make_tbl (T)
