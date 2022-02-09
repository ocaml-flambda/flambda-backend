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

(** Offsets for closures and environment variables inside sets of closures.
    They're computed for elements defined in the current compilation unit by
    [Closure_offsets], and read from cmx files for external symbols. Because an
    external cmx can reference elements from another cmx that the current
    compilation cannot see, all offsets that occur in the current compilation
    unit should be re-exported. *)

type closure_info =
  | Dead_id
  | Id_slot of
      { offset : int;
        size : int
            (* Number of fields taken for the function:

               2 fields (code pointer + arity) for function of arity one

               3 fields (caml_curry + arity + code pointer) otherwise *)
      }

type env_var_info =
  | Dead_var
  | Var_slot of { offset : int }

type t =
  { closure_offsets : closure_info Closure_id.Map.t;
    env_var_offsets : env_var_info Var_within_closure.Map.t
  }

let print_closure_info fmt = function
  | Dead_id -> Format.fprintf fmt "@[<h>(dead)@]"
  | Id_slot { offset; size } ->
    Format.fprintf fmt "@[<h>(o:%d, s:%d)@]" offset size

let print_env_var_info fmt (info : env_var_info) =
  match info with
  | Dead_var -> Format.fprintf fmt "@[<h>(removed)@]"
  | Var_slot { offset } -> Format.fprintf fmt "@[<h>(o:%d)@]" offset

let [@ocamlformat "disable"] print fmt env =
  Format.fprintf fmt "{@[<v>closures: @[<v>%a@]@,env_vars: @[<v>%a@]@]}"
    (Closure_id.Map.print print_closure_info) env.closure_offsets
    (Var_within_closure.Map.print print_env_var_info) env.env_var_offsets

let empty =
  { closure_offsets = Closure_id.Map.empty;
    env_var_offsets = Var_within_closure.Map.empty
  }

let equal_closure_info (info1 : closure_info) (info2 : closure_info) =
  match info1, info2 with
  | Dead_id, Dead_id -> true
  | Id_slot { offset = o1; size = s1 }, Id_slot { offset = o2; size = s2 } ->
    o1 = o2 && s1 = s2
  | Dead_id, Id_slot _ | Id_slot _, Dead_id -> false

let equal_env_var_info (info1 : env_var_info) (info2 : env_var_info) =
  match info1, info2 with
  | Dead_var, Dead_var -> true
  | Var_slot { offset = o1 }, Var_slot { offset = o2 } -> o1 = o2
  | Dead_var, Var_slot _ | Var_slot _, Dead_var -> false

let add_closure_offset env closure offset =
  match Closure_id.Map.find closure env.closure_offsets with
  | o ->
    assert (equal_closure_info o offset);
    env
  | exception Not_found ->
    let closure_offsets =
      Closure_id.Map.add closure offset env.closure_offsets
    in
    { env with closure_offsets }

let add_env_var_offset env env_var offset =
  match Var_within_closure.Map.find env_var env.env_var_offsets with
  | o ->
    assert (equal_env_var_info o offset);
    env
  | exception Not_found ->
    let env_var_offsets =
      Var_within_closure.Map.add env_var offset env.env_var_offsets
    in
    { env with env_var_offsets }

(* Missing definitions for [Closure_id]s and [Var_within_closure]s are not
   treated as errors. Doing so would mean relying on perfect removal of unused
   [Let_symbol] bindings. *)

let closure_offset env closure =
  match Closure_id.Map.find closure env.closure_offsets with
  | exception Not_found -> None
  | res -> Some res

let env_var_offset env env_var =
  match Var_within_closure.Map.find env_var env.env_var_offsets with
  | exception Not_found -> None
  | res -> Some res

let map_closure_offsets env f = Closure_id.Map.mapi f env.closure_offsets

let map_env_var_offsets env f =
  Var_within_closure.Map.mapi f env.env_var_offsets

let current_offsets = ref empty

let imported_offsets () = !current_offsets

let merge env1 env2 =
  let closure_offsets =
    Closure_id.Map.disjoint_union ~eq:equal_closure_info
      ~print:print_closure_info env1.closure_offsets env2.closure_offsets
  in
  let env_var_offsets =
    Var_within_closure.Map.disjoint_union ~eq:equal_env_var_info
      ~print:print_env_var_info env1.env_var_offsets env2.env_var_offsets
  in
  { closure_offsets; env_var_offsets }

let import_offsets env = current_offsets := merge env !current_offsets
