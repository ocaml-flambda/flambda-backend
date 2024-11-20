(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2025 OCamlPro SAS                                          *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This module provides facilities for checking that two types are equal, for a
   {b semantic} definition of equality: aliases are resolved with respect to a
   typing environment.

   {b Warning}: This module should only be used for debugging purposes. It has
   high computational complexity, and no guarantees is made on the precision of
   the equality test, in particular for types containing env extensions.

   {b Note}: The functions operating on levels treat all variables defined by
   the levels as existentials. *)

val equal_type :
  meet_type:Typing_env.meet_type ->
  Typing_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  bool

val equal_env_extension :
  meet_type:Typing_env.meet_type ->
  Typing_env.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t ->
  bool

val names_with_non_equal_types_env_extension :
  meet_type:Typing_env.meet_type ->
  Typing_env.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t ->
  Name.Set.t

val equal_level_ignoring_name_mode :
  meet_type:Typing_env.meet_type ->
  Typing_env.t ->
  Typing_env_level.t ->
  Typing_env_level.t ->
  bool

val names_with_non_equal_types_level_ignoring_name_mode :
  meet_type:Typing_env.meet_type ->
  Typing_env.t ->
  Typing_env_level.t ->
  Typing_env_level.t ->
  Name.Set.t
