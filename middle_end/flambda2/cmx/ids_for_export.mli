(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Simple = Reg_width_things.Simple

type t = private {
  symbols : Symbol.Set.t;
  variables : Variable.Set.t;
  simples : Simple.Set.t;
  consts : Reg_width_things.Const.Set.t;
  code_ids : Code_id.Set.t;
  continuations : Continuation.Set.t;
}

val empty : t

val create
   : ?symbols: Symbol.Set.t
  -> ?variables: Variable.Set.t
  -> ?simples: Simple.Set.t
  -> ?consts: Reg_width_things.Const.Set.t
  -> ?code_ids: Code_id.Set.t
  -> ?continuations: Continuation.Set.t
  -> unit
  -> t

val singleton_code_id : Code_id.t -> t

val singleton_continuation : Continuation.t -> t

val singleton_symbol : Symbol.t -> t

val from_simple : Simple.t -> t

val add_variable : t -> Variable.t -> t

val add_symbol : t -> Symbol.t -> t

val add_name : t -> Name.t -> t

val add_simple : t -> Simple.t -> t

val add_code_id : t -> Code_id.t -> t

val add_continuation : t -> Continuation.t -> t

val union : t -> t -> t

val union_list : t list -> t
