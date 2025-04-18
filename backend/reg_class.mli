[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_class_utils.T

module Tbl : Reg_class_utils.Tbl with type reg_class = t
