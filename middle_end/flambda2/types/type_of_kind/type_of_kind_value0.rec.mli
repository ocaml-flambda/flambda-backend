(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell type Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Variant of Variant.t
  | Boxed_float of Type_grammar.t
  | Boxed_int32 of Type_grammar.t
  | Boxed_int64 of Type_grammar.t
  | Boxed_nativeint of Type_grammar.t
  | Closures of
      { by_closure_id :
          Row_like.For_closures_entry_by_set_of_closures_contents.t
      }
  | String of String_info.Set.t
  (* CR-someday mshinwell: [Array] should know what kind of array it is. *)
  | Array of { length : Type_grammar.t }

include
  Type_head_intf.S
    with type t := t
    with type type_grammar := Type_grammar.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
