(* CR aspectorzabusky: This needs a copyright header; should I copy [array.ml]? *)

open! Stdlib

module Iarray = Stdlib__Iarray

[@@@ocaml.flambda_o3]

(* Module [IarrayLabels]: labelled Iarray module *)

[@@@ocaml.nolabels]

include Iarray
