[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

module T = struct
  type t =
    | Int64
    | Float64
    | Vector128
    | Vector256
    | Vector512

  let all = [Int64; Float64; Vector128; Vector256; Vector512]

  (* Preserves original ordering (int -> float) *)
  let frame_order = [| Vector512; Vector256; Vector128; Int64; Float64 |]

  let equal : t -> t -> bool =
   fun left right ->
    match left, right with
    | Int64, Int64 -> true
    | Float64, Float64 -> true
    | Vector128, Vector128 -> true
    | Vector256, Vector256 -> true
    | Vector512, Vector512 -> true
    | (Int64 | Float64 | Vector128 | Vector256 | Vector512), _ -> false

  let hash : t -> int = function
    | Int64 -> 0
    | Float64 -> 1
    | Vector128 -> 2
    | Vector256 -> 3
    | Vector512 -> 4

  let tag : t -> string = function
    | Int64 -> "i"
    | Float64 -> "f"
    | Vector128 -> "x"
    | Vector256 -> "y"
    | Vector512 -> "z"

  let print : Format.formatter -> t -> unit =
   fun ppf stack_class ->
    Format.fprintf ppf "%s"
      (match stack_class with
      | Int64 -> "int64"
      | Float64 -> "float64"
      | Vector128 -> "vector128"
      | Vector256 -> "vector256"
      | Vector512 -> "vector512")

  let size_in_bytes : t -> int = function
    | Int64 -> 8
    | Float64 -> 8
    | Vector128 -> 16
    | Vector256 -> 32
    | Vector512 -> 64

  let of_machtype : Cmm.machtype_component -> t = function
    | Val | Int | Addr -> Int64
    | Float | Float32 -> Float64
    | Vec128 | Valx2 -> Vector128
    | Vec256 -> Vector256
    | Vec512 -> Vector512
end

include T
module Tbl = Stack_class_utils.Make_tbl (T)
