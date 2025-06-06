(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Graph = Global_flow_graph

type continuation_info =
  { is_exn_handler : bool;
    params : Variable.t list;
    arity : Flambda_kind.With_subkind.t list
  }

module Env : sig
  type cont_kind = Normal of Variable.t list

  type t =
    { parent : Rev_expr.rev_expr_holed;
      conts : cont_kind Continuation.Map.t;
      current_code_id : Code_id.t option;
      le_monde_exterieur : Name.t;
      all_constants : Name.t
    }
end

type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t; (* Dummy variable representing exn return value *)
    is_tupled : bool;
    call_witnesses : Code_id_or_name.t list
  }

type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_args : Simple.t list;
    apply_closure : Simple.t option;
    params_of_apply_return_cont : Variable.t list option;
    param_of_apply_exn_cont : Variable.t;
    not_pure_call_witness : Variable.t
  }

type t

val create : unit -> t

val kind : Name.t -> Flambda_kind.t -> t -> unit

val bound_parameter_kind : Bound_parameter.t -> t -> unit

val alias_kind : Name.t -> Simple.t -> t -> unit

val kinds : t -> Flambda_kind.t Name.Map.t

val simple_to_name : t -> denv:Env.t -> Simple.t -> Name.t

val alias_dep : denv:Env.t -> Variable.t -> Simple.t -> t -> unit

val root : Variable.t -> t -> unit

val used : denv:Env.t -> Simple.t -> t -> unit

val used_code_id : Code_id.t -> t -> unit

val called : denv:Env.t -> Code_id.t -> t -> unit

val fixed_arity_continuation : t -> Continuation.t -> unit

val fixed_arity_continuations : t -> Continuation.Set.t

val continuation_info : t -> Continuation.t -> continuation_info -> unit

val get_continuation_info : t -> continuation_info Continuation.Map.t

val add_apply : apply_dep -> t -> unit

val add_set_of_closures_dep :
  Name.t -> Code_id.t -> only_full_applications:bool -> t -> unit

val add_code : Code_id.t -> code_dep -> t -> unit

val find_code : t -> Code_id.t -> code_dep

val code_deps : t -> code_dep Code_id.Map.t

val graph : t -> Graph.graph

val deps :
  t ->
  get_code_metadata:(Code_id.t -> Code_metadata.t) ->
  le_monde_exterieur:Name.t ->
  all_constants:Name.t ->
  Graph.graph
