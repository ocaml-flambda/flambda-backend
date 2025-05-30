(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = private
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

val create :
  params:Bound_parameters.t -> handler:Flambda.Expr.t -> is_cold:bool -> t

val print : Format.formatter -> t -> unit

val with_handler : Flambda.Expr.t -> t -> t
