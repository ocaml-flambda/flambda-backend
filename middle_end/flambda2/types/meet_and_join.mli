(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val meet :
  Typing_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t * Typing_env_extension.t) Or_bottom.t

val meet_type : Typing_env.meet_type

val meet_shape :
  Typing_env.t ->
  Type_grammar.t ->
  shape:Type_grammar.t ->
  result_var:Bound_var.t ->
  result_kind:Flambda_kind.t ->
  Typing_env_extension.t Or_bottom.t

val meet_env_extension :
  Typing_env.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t ->
  Typing_env_extension.t Or_bottom.t

val join :
  ?bound_name:Name.t ->
  Typing_env.Join_env.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  Type_grammar.t Or_unknown.t
