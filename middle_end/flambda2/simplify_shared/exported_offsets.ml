(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Vincent Laviron and Guillaume Bury, OCamlPro              *)
(*                                                                        *)
(*   Copyright 2019--2020 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Offsets for function and value slots inside sets of closures. They're
    computed for elements defined in the current compilation unit by
    [function_slot_offsets], and read from cmx files for external symbols.
    Because an external cmx can reference elements from another cmx that the
    current compilation cannot see, all offsets that occur in the current
    compilation unit should be re-exported. *)

type function_slot_info =
  | Dead_function_slot
  | Live_function_slot of
      { offset : int;
        size : int
            (* Number of fields taken for the function:

               2 fields (code pointer + arity) for function of arity one

               3 fields (caml_curry + arity + code pointer) otherwise *)
      }

type value_slot_info =
  | Dead_value_slot
  | Live_value_slot of { offset : int }

type t =
  { function_slot_offsets : function_slot_info Function_slot.Map.t;
    value_slot_offsets : value_slot_info Value_slot.Map.t
  }

let print_function_slot_info fmt = function
  | Dead_function_slot -> Format.fprintf fmt "@[<h>(dead)@]"
  | Live_function_slot { offset; size } ->
    Format.fprintf fmt "@[<h>(o:%d, s:%d)@]" offset size

let print_value_slot_info fmt (info : value_slot_info) =
  match info with
  | Dead_value_slot -> Format.fprintf fmt "@[<h>(removed)@]"
  | Live_value_slot { offset } -> Format.fprintf fmt "@[<h>(o:%d)@]" offset

let [@ocamlformat "disable"] print fmt env =
  Format.fprintf fmt "{@[<v>closures: @[<v>%a@]@,value_slots: @[<v>%a@]@]}"
    (Function_slot.Map.print print_function_slot_info) env.function_slot_offsets
    (Value_slot.Map.print print_value_slot_info) env.value_slot_offsets

let empty =
  { function_slot_offsets = Function_slot.Map.empty;
    value_slot_offsets = Value_slot.Map.empty
  }

let equal_function_slot_info (info1 : function_slot_info)
    (info2 : function_slot_info) =
  match info1, info2 with
  | Dead_function_slot, Dead_function_slot -> true
  | ( Live_function_slot { offset = o1; size = s1 },
      Live_function_slot { offset = o2; size = s2 } ) ->
    o1 = o2 && s1 = s2
  | Dead_function_slot, Live_function_slot _
  | Live_function_slot _, Dead_function_slot ->
    false

let equal_value_slot_info (info1 : value_slot_info) (info2 : value_slot_info) =
  match info1, info2 with
  | Dead_value_slot, Dead_value_slot -> true
  | Live_value_slot { offset = o1 }, Live_value_slot { offset = o2 } -> o1 = o2
  | Dead_value_slot, Live_value_slot _ | Live_value_slot _, Dead_value_slot ->
    false

let add_function_slot_offset env closure offset =
  match Function_slot.Map.find closure env.function_slot_offsets with
  | o ->
    assert (equal_function_slot_info o offset);
    env
  | exception Not_found ->
    let function_slot_offsets =
      Function_slot.Map.add closure offset env.function_slot_offsets
    in
    { env with function_slot_offsets }

let add_value_slot_offset env value_slot offset =
  match Value_slot.Map.find value_slot env.value_slot_offsets with
  | o ->
    assert (equal_value_slot_info o offset);
    env
  | exception Not_found ->
    let value_slot_offsets =
      Value_slot.Map.add value_slot offset env.value_slot_offsets
    in
    { env with value_slot_offsets }

let function_slot_offset env closure =
  match Function_slot.Map.find closure env.function_slot_offsets with
  | exception Not_found -> None
  | res -> Some res

let value_slot_offset env value_slot =
  match Value_slot.Map.find value_slot env.value_slot_offsets with
  | exception Not_found -> None
  | res -> Some res

let map_function_slot_offsets env f =
  Function_slot.Map.mapi f env.function_slot_offsets

let map_value_slot_offsets env f = Value_slot.Map.mapi f env.value_slot_offsets

let current_offsets = ref empty

let imported_offsets () = !current_offsets

let merge env1 env2 =
  let function_slot_offsets =
    Function_slot.Map.disjoint_union ~eq:equal_function_slot_info
      ~print:print_function_slot_info env1.function_slot_offsets
      env2.function_slot_offsets
  in
  let value_slot_offsets =
    Value_slot.Map.disjoint_union ~eq:equal_value_slot_info
      ~print:print_value_slot_info env1.value_slot_offsets
      env2.value_slot_offsets
  in
  { function_slot_offsets; value_slot_offsets }

let import_offsets env = current_offsets := merge env !current_offsets
