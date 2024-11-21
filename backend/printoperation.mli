[@@@ocaml.warning "+a-4-9-40-41-42"]

val operation :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Operation.t ->
  Reg.t array ->
  Format.formatter ->
  Reg.t array ->
  unit
