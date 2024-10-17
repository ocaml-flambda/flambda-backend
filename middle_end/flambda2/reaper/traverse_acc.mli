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

type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t (* Dummy variable representing exn return value *)
  }

type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_args : Simple.t list;
    apply_closure : Simple.t option;
    params_of_apply_return_cont : Variable.t list option;
    param_of_apply_exn_cont : Variable.t
  }

type t

val create : unit -> t

val kind : Name.t -> Flambda_kind.t -> t -> unit

val bound_parameter_kind : Bound_parameter.t -> t -> unit

val alias_kind : Name.t -> Simple.t -> t -> unit

val kinds : t -> Flambda_kind.t Name.Map.t

val record_dep :
  denv:Traverse_denv.t -> Code_id_or_name.t -> Graph.Dep.t -> t -> unit

val record_deps :
  denv:Traverse_denv.t -> Code_id_or_name.t -> Graph.Dep.Set.t -> t -> unit

val alias_dep : denv:Traverse_denv.t -> Variable.t -> Simple.t -> t -> unit

val root : Variable.t -> t -> unit

val used : denv:Traverse_denv.t -> Simple.t -> t -> unit

val used_code_id : Code_id.t -> t -> unit

val called : denv:Traverse_denv.t -> Code_id.t -> t -> unit

val add_apply : apply_dep -> t -> unit

val add_set_of_closures_dep : Name.t -> Code_id.t -> t -> unit

val add_code : Code_id.t -> code_dep -> t -> unit

val find_code : t -> Code_id.t -> code_dep

val code_deps : t -> code_dep Code_id.Map.t

val deps : t -> Graph.graph
