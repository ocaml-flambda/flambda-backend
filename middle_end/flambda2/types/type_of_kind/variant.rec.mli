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

[@@@ocaml.warning "+a-30-40-41-42"]

type t = private {
  immediates : Type_grammar.t Or_unknown.t;
  blocks : Row_like.For_blocks.t Or_unknown.t;
  is_unique : bool;
}

val create
   : is_unique:bool
  -> immediates:Type_grammar.t Or_unknown.t
  -> blocks:Row_like.For_blocks.t Or_unknown.t
  -> t
