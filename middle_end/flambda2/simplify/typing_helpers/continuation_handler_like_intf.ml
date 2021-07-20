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

(** A generic notion of "continuation handler". *)

module type S = sig
  type t

  val print : Format.formatter -> t -> unit

  val is_exn_handler : t -> bool

  val stub : t -> bool

  val arity : t -> Flambda_arity.t

  type behaviour = private
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Apply_cont_with_constant_arg of {
        cont : Continuation.t;
        arg : Reg_width_const.t;
        arity : Flambda_arity.t;
      }
    | Unknown of { arity : Flambda_arity.t; }

  val behaviour : t -> behaviour

  val real_handler : t -> Flambda.Continuation_handler.t option

  module Opened : sig
    type t

    val params : t -> Kinded_parameter.List.t
  end

  val pattern_match : t -> f:(Opened.t -> 'a) -> 'a
end
