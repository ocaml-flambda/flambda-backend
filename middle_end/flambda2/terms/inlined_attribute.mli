(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Call site (not function declaration) inlining annotations. *)

module Use_info : sig
  type t =
    | Expected_to_be_used
    | Unused_because_of_call_site_decision
    | Unused_because_function_unknown

  val explanation : t -> string option
end

type t =
  | Always_inlined of Use_info.t
  | Hint_inlined
  | Never_inlined
  | Unroll of int * Use_info.t
  | Default_inlined

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val from_lambda : Lambda.inlined_attribute -> t

val with_use_info : t -> Use_info.t -> t

val use_info : t -> Use_info.t option
