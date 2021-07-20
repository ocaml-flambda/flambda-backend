(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Index : Product_intf.Index)
  : Product_intf.S
    with module Index := Index
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t

module Int_indexed : sig
  include Product_intf.S_base
    with module Index := Numeric_types.Int
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t

  val create_from_list : Flambda_kind.t -> Type_grammar.t list -> t
end

module Closure_id_indexed
  : Product_intf.S
    with module Index := Closure_id
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t

module Var_within_closure_indexed
  : Product_intf.S
    with module Index := Var_within_closure
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t
