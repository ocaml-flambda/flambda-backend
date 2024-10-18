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

module Id : sig
  type t

  val fresh : unit -> t

  val print : Format.formatter -> t -> unit

  module Map : Container_types.Map with type key = t
end = struct
  type t = int

  let print = Numbers.Int.print

  let fresh =
    let r = ref 0 in
    fun () ->
      incr r;
      !r

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Map = Tree.Map
end

type t =
  { len : int;
    new_params_indexed : Bound_parameter.t Id.Map.t
  }

let print ppf { len = _; new_params_indexed } =
  Format.fprintf ppf "@[<hov 1>(@[<hov 1>(new_params_indexed@ %a)@])@]"
    (Id.Map.print Bound_parameter.print)
    new_params_indexed

let empty = { len = 0; new_params_indexed = Id.Map.empty }

let is_empty { len; new_params_indexed = _ } = len = 0

let length { len; new_params_indexed = _ } = len

let new_param t bound_param =
  (* create a fresh var/bound_param to index the new parameter *)
  let id = Id.fresh () in
  let new_params_indexed = Id.Map.add id bound_param t.new_params_indexed in
  { len = t.len + 1; new_params_indexed }

let rename t =
  let bindings = Id.Map.bindings t.new_params_indexed in
  let keys, bound_param_list = List.split bindings in
  let bound_params = Bound_parameters.create bound_param_list in
  let new_bound_params = Bound_parameters.rename bound_params in
  let renaming =
    Bound_parameters.renaming bound_params ~guaranteed_fresh:new_bound_params
  in
  let new_params_indexed =
    Id.Map.of_list
      (List.combine keys (Bound_parameters.to_list new_bound_params))
  in
  { t with new_params_indexed }, renaming

let fold_aux ~init ~f { len = _; new_params_indexed } =
  Id.Map.fold f new_params_indexed init

let fold ~init ~f t = fold_aux ~init ~f:(fun _ param acc -> f param acc) t

let rec find_arg id = function
  | [] ->
    Misc.fatal_errorf
      "Missing lifted param id: %a not found in lifted_cont_params stack"
      Id.print id
  | { len = _; new_params_indexed } :: r -> (
    match Id.Map.find_opt id new_params_indexed with
    | Some param -> Bound_parameter.simple param
    | None -> find_arg id r)

(* NOTE about the order of the returned args/params for the {args} and
   {bound_parameters} functions: The exact order does not matter as long as both
   functions return params (or their corresponding arguments) **in the same
   order**.

   The current implementations return the parameters/arguments in the reverse
   order of the bindings in the Map, but that's fine since it is the case for
   both functions. *)
let args ~callee_lifted_params ~caller_stack_lifted_params =
  fold_aux callee_lifted_params ~init:[] ~f:(fun id _callee_param acc ->
      find_arg id caller_stack_lifted_params :: acc)

let bound_parameters t =
  Bound_parameters.create
  @@ fold t ~init:[] ~f:(fun bound_param acc -> bound_param :: acc)
