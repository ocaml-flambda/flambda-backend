(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lambda
open Mode

let transl_locality_mode = function
  | Locality.Const.Global -> alloc_heap
  | Locality.Const.Local -> alloc_local

let transl_locality_mode_l locality =
  Locality.zap_to_floor locality |> transl_locality_mode

let transl_locality_mode_r locality =
  (* r mode are for allocations; [optimise_allocations] should have pushed it
     to ceil and determined; here we push it again just to get the constant. *)
  Locality.zap_to_ceil locality |> transl_locality_mode

let transl_alloc_mode_l mode =
  (* we only take the locality axis *)
  Alloc.proj (Comonadic Areality) mode |> transl_locality_mode_l

let transl_alloc_mode_r mode =
  (* we only take the locality axis *)
  Alloc.proj (Comonadic Areality) mode |> transl_locality_mode_r

let transl_alloc_mode (mode : Typedtree.alloc_mode) =
  transl_alloc_mode_r mode.mode

let transl_modify_mode locality =
  match Locality.zap_to_floor locality with
  | Global -> modify_heap
  | Local -> modify_maybe_stack

let transl_unique_barrier barrier =
  match Typedtree.Unique_barrier.resolve barrier with
  | Uniqueness.Const.Aliased -> May_be_pushed_down
  | Uniqueness.Const.Unique ->
    (* CR uniqueness: this is a temporary measure to ensure that we can roll
       the compiler without breaking existing code. Once uniqueness is stable,
       we can simplify this to always return [Must_stay_here]. *)
    if Language_extension.is_at_least Unique
         Language_extension.maturity_of_unique_for_destruction
    then Must_stay_here
    else assert false
