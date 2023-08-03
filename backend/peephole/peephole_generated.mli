[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

val apply :
  Cfg.basic Cfg.instruction DLL.cell ->
  Cfg.basic Cfg.instruction DLL.cell option

val generated_rule_names : string list
