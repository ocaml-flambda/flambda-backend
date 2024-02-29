[@@@ocaml.warning "+a-30-40-41-42"]

module T = struct

  type t =
    | Int64
    | Float64
    | Vector128

  let all = [
    Int64;
    Float64;
    Vector128;
  ]

  let equal
    : t -> t -> bool
    = fun left right ->
      match left, right with
      | Int64, Int64 -> true
      | Float64, Float64 -> true
      | Vector128, Vector128 -> true
      | (Int64 | Float64 | Vector128), _ -> false

  let hash : t -> int = function
    | Int64 -> 0
    | Float64 -> 1
    | Vector128 -> 2

  let tag
    : t -> string
    = function
      | Int64 -> "i"
      | Float64 -> "f"
      | Vector128 ->
        Arch.assert_simd_enabled ();
        "x"

  let print
    : Format.formatter -> t -> unit
    = fun ppf stack_class ->
      Format.fprintf ppf "%s"
        (match stack_class with
         | Int64 -> "int64"
         | Float64 -> "float64"
         | Vector128 -> "vector128")

  let size_in_bytes
    : t -> int
    = function
      | Int64 -> 8
      | Float64 -> 8
      | Vector128 -> 16

  let of_machtype
    : Cmm.machtype_component -> t
    = function
      | Val | Addr | Int -> Int64
      | Float -> Float64
      | Vec128 ->
        Arch.assert_simd_enabled ();
        Vector128

end

include T
module Tbl = Stack_class_utils.Make_tbl (T)

let offset_in_bytes_for_class
  : stack_class:t -> slot:int -> int
  = fun ~stack_class ~slot ->
    (match stack_class with
     | Int64 | Float64 -> ()
     | Vector128 ->
       Arch.assert_simd_enabled ());
    slot * size_in_bytes stack_class

let offset_in_bytes
  : int Tbl.t -> stack_class:t -> slot:int -> int
  = fun tbl ~stack_class ~slot ->
    match stack_class with
    | Vector128 ->
      offset_in_bytes_for_class ~stack_class:Vector128 ~slot
    | Int64 ->
      Tbl.total_size_in_bytes_for_class tbl ~stack_class:Vector128
      + offset_in_bytes_for_class ~stack_class:Int64 ~slot
    | Float64 ->
      Tbl.total_size_in_bytes_for_class tbl ~stack_class:Vector128
      + Tbl.total_size_in_bytes_for_class tbl ~stack_class:Int64
      + offset_in_bytes_for_class ~stack_class:Float64 ~slot
