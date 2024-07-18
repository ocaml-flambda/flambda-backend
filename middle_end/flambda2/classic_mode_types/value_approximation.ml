(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Pierre Chambart and Vincent Laviron, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Value_unknown
  | Value_symbol of Symbol.t
  | Value_const of Reg_width_const.t
  | Closure_approximation of
      { code_id : Code_id.t;
        function_slot : Function_slot.t;
        all_function_slots : Function_slot.Set.t;
        all_value_slots : Value_slot.Set.t;
        code : 'code;
        symbol : Symbol.t option
      }
  | Block_approximation of
      Tag.Scannable.t
      * Flambda_kind.Scannable_block_shape.t
      * 'code t array
      * Alloc_mode.For_types.t

let rec print fmt = function
  | Value_unknown -> Format.fprintf fmt "?"
  | Value_symbol sym -> Symbol.print fmt sym
  | Value_const i -> Reg_width_const.print fmt i
  | Closure_approximation { code_id; _ } ->
    Format.fprintf fmt "[%a]" Code_id.print code_id
  | Block_approximation (tag, _shape, fields, _) ->
    let len = Array.length fields in
    if len < 1
    then Format.fprintf fmt "{}"
    else (
      Format.fprintf fmt "@[<hov 2>{%a:%a" Tag.Scannable.print tag print
        fields.(0);
      for i = 1 to len - 1 do
        Format.fprintf fmt "@ %a" print fields.(i)
      done;
      Format.fprintf fmt "}@]")

let is_unknown = function
  | Value_unknown -> true
  | Value_symbol _ | Value_const _ | Closure_approximation _
  | Block_approximation _ ->
    false

let rec free_names ~code_free_names approx =
  match approx with
  | Value_unknown | Value_const _ -> Name_occurrences.empty
  | Value_symbol sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
  | Block_approximation (_tag, _shape, approxs, _) ->
    Array.fold_left
      (fun names approx ->
        Name_occurrences.union names (free_names ~code_free_names approx))
      Name_occurrences.empty approxs
  | Closure_approximation
      { code_id;
        function_slot;
        all_function_slots;
        all_value_slots;
        code;
        symbol
      } ->
    let free_names = code_free_names code in
    let free_names =
      match symbol with
      | None -> free_names
      | Some sym -> Name_occurrences.add_symbol free_names sym Name_mode.normal
    in
    let free_names =
      Name_occurrences.add_code_id free_names code_id Name_mode.normal
    in
    let free_names =
      Name_occurrences.add_function_slot_in_types free_names function_slot
    in
    let free_names =
      Function_slot.Set.fold
        (fun function_slot free_names ->
          Name_occurrences.add_function_slot_in_types free_names function_slot)
        all_function_slots free_names
    in
    Value_slot.Set.fold
      (fun value_slot free_names ->
        Name_occurrences.add_value_slot_in_types free_names value_slot)
      all_value_slots free_names
