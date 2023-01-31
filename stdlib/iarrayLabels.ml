(* CR aspectorzabusky: This needs a copyright header; should I copy [array.ml]? *)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Module [IarrayLabels]: labelled Iarray module *)

[@@@ocaml.nolabels]

include Iarray
