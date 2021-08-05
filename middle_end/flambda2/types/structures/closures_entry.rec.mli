(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
   : function_decls : Function_declaration_type.t Closure_id.Map.t
  -> closure_types : Product.Closure_id_indexed.t
  -> closure_var_types : Product.Var_within_closure_indexed.t
  -> t

val map_function_decl_types
   : t
  -> f:(Function_declaration_type.t
    -> Function_declaration_type.t Or_bottom.t)
  -> t Or_bottom.t

val find_function_declaration
   : t
  -> Closure_id.t
  -> Function_declaration_type.t Or_bottom.t

val closure_types : t -> Type_grammar.t Closure_id.Map.t

val map_closure_types
   : t
  -> f:(Type_grammar.t -> Type_grammar.t Or_bottom.t)
  -> t Or_bottom.t

val function_decl_types : t -> Function_declaration_type.t Closure_id.Map.t

val closure_var_types : t -> Type_grammar.t Var_within_closure.Map.t

val fields_kind : t -> Flambda_kind.t

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Type_grammar.t
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type join_env := Join_env.t
  with type typing_env_extension := Typing_env_extension.t
