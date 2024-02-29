[@@@ocaml.warning "+a-30-40-41-42"]

let fatal_vec128 () =
  Misc.fatal_error "arm64: got vec128 register"

module T = struct

  type t =
    | Int64
    | Float64

  let all = [
    Int64;
    Float64;
  ]

  let equal
    : t -> t -> bool
    = fun left right ->
      match left, right with
      | Int64, Int64 -> true
      | Float64, Float64 -> true
      | (Int64 | Float64), _ -> false

  let hash : t -> int = function
    | Int64 -> 0
    | Float64 -> 1

  let tag
    : t -> string
    = function
      | Int64 -> "i"
      | Float64 -> "f"

  let print
    : Format.formatter -> t -> unit
    = fun ppf stack_class ->
      Format.fprintf ppf "%s"
        (match stack_class with
         | Int64 -> "int64"
         | Float64 -> "float64")

  let size_in_bytes
    : t -> int
    = function
      | Int64 -> 8
      | Float64 -> 8

  let of_machtype
    : Cmm.machtype_component -> t
    = function
      | Val | Int | Addr  -> Int64
      | Float -> Float64
      (* CR mslater: (SIMD) arm64 *)
      | Vec128 -> fatal_vec128 ()

end

include T
module Tbl = Stack_class_utils.Make_tbl (T)

let offset_in_bytes_for_class
  : stack_class:t -> slot:int -> int
  = fun ~stack_class ~slot ->
    slot * size_in_bytes stack_class

let offset_in_bytes
  : int Tbl.t -> stack_class:t -> slot:int -> int
  = fun tbl ~stack_class ~slot ->
    match stack_class with
    | Int64 ->
      offset_in_bytes_for_class ~stack_class:Int64 ~slot
    | Float64 ->
      Tbl.total_size_in_bytes_for_class tbl ~stack_class:Int64
      + offset_in_bytes_for_class ~stack_class:Float64 ~slot
