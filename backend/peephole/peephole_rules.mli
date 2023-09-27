[@@@ocaml.warning "+a-29-40-41-42"]

open! Peephole_utils

val apply :
  Cfg.basic Cfg.instruction DLL.cell ->
  Cfg.basic Cfg.instruction DLL.cell option
