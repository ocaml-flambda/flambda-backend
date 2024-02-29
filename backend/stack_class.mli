[@@@ocaml.warning "+a-30-40-41-42"]

include Stack_class_utils.T

module Tbl : Stack_class_utils.Tbl with type stack_class = t

val offset_in_bytes : int Tbl.t -> stack_class:t -> slot:int -> int
