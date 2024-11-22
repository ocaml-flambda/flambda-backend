(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module BP = Bound_parameter

type t =
  { len : int;
    new_params_indexed : BP.t BP.Map.t
  }

let print ppf { len = _; new_params_indexed } =
  Format.fprintf ppf "@[<hov 1>(@[<hov 1>(new_params_indexed@ %a)@])@]"
    (BP.Map.print BP.print) new_params_indexed

let empty = { len = 0; new_params_indexed = BP.Map.empty }

let is_empty { len; new_params_indexed = _ } = len = 0

let length { len; new_params_indexed = _ } = len

let new_param t ~replay_history bound_param =
  let key =
    match Replay_history.replay_variable_mapping replay_history with
    | Still_recording -> bound_param
    | Replayed variable_mapping ->
      let original_var =
        Variable.Map.find (BP.var bound_param) variable_mapping
      in
      BP.create original_var (BP.kind bound_param)
  in
  let new_params_indexed = BP.Map.add key bound_param t.new_params_indexed in
  { len = t.len + 1; new_params_indexed }

let rename t =
  let bindings = BP.Map.bindings t.new_params_indexed in
  let keys, bound_param_list = List.split bindings in
  let bound_params = Bound_parameters.create bound_param_list in
  let new_bound_params = Bound_parameters.rename bound_params in
  let renaming =
    Bound_parameters.renaming bound_params ~guaranteed_fresh:new_bound_params
  in
  let new_params_indexed =
    BP.Map.of_list
      (List.combine keys (Bound_parameters.to_list new_bound_params))
  in
  { t with new_params_indexed }, renaming

let fold_aux ~init ~f { len = _; new_params_indexed } =
  BP.Map.fold f new_params_indexed init

let fold ~init ~f t = fold_aux ~init ~f:(fun _ param acc -> f param acc) t

let rec find_arg bp = function
  | [] ->
    Misc.fatal_errorf
      "Missing lifted param id: %a not found in lifted_cont_params stack"
      BP.print bp
  | { len = _; new_params_indexed } :: r -> (
    match BP.Map.find_opt bp new_params_indexed with
    | Some param -> BP.simple param
    | None -> find_arg bp r)

(* NOTE about the order of the returned args/params for the {args} and
   {bound_parameters} functions: The exact order does not matter as long as both
   functions return params (or their corresponding arguments) **in the same
   order**.

   The current implementations return the parameters/arguments in the reverse
   order of the bindings in the Map, but that's fine since it is the case for
   both functions. *)
let args ~callee_lifted_params ~caller_stack_lifted_params =
  fold_aux callee_lifted_params ~init:[] ~f:(fun bp_key _callee_param acc ->
      find_arg bp_key caller_stack_lifted_params :: acc)

let bound_parameters t =
  Bound_parameters.create
  @@ fold t ~init:[] ~f:(fun bound_param acc -> bound_param :: acc)
