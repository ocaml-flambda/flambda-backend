[@@@ocaml.warning "+a-40-41-42"]

include Stack_class_utils.T

module Tbl : Stack_class_utils.Tbl with type stack_class = t
