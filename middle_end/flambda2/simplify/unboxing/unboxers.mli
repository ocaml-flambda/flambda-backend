(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

type number_decider =
  { param_name : string;
    kind : K.Naked_number_kind.t;
    prove_is_a_boxed_number : TE.t -> T.t -> unit T.proof_of_property
  }

type unboxer =
  { var_name : string;
    poison_const : Const.t;
    unboxing_prim : Simple.t -> P.t;
    prove_simple :
      TE.t -> min_name_mode:Name_mode.t -> T.t -> Simple.t T.meet_shortcut
  }

module type Number_S = sig
  val decider : number_decider

  val unboxing_prim : Simple.t -> P.t

  val unboxer : unboxer
end

module Immediate : Number_S

module Float32 : Number_S

module Float : Number_S

module Int32 : Number_S

module Int64 : Number_S

module Nativeint : Number_S

module Vec128 : Number_S

module Field : sig
  val unboxing_prim :
    P.Block_access_kind.t -> block:Simple.t -> index:Targetint_31_63.t -> P.t

  val unboxer :
    poison_const:Const.t ->
    P.Block_access_kind.t ->
    index:Targetint_31_63.t ->
    unboxer
end

module Closure_field : sig
  val unboxing_prim : Function_slot.t -> closure:Simple.t -> Value_slot.t -> P.t

  val unboxer : Function_slot.t -> Value_slot.t -> unboxer
end
