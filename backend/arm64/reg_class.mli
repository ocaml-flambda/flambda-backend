[@@@ocaml.warning "+a-40-41-42"]

type t =
  | Int64
  | Float128

include Reg_class_utils.T with type t := t

module Tbl : Reg_class_utils.Tbl with type reg_class = t
