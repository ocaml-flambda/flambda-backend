(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                       Basile Clément, OCamlPro                         *)
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
  (Type_grammar.t * Typing_env.t) Or_bottom.t

val meet_type : unit -> Typing_env.meet_type

val meet_shape :
  Typing_env.t ->
  Type_grammar.t ->
  shape:Type_grammar.t ->
  Typing_env.t Or_bottom.t
