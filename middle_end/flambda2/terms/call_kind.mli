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

(** Classification of application expressions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Function_call : sig
  type t = private
    | Direct of {
        code_id : Code_id.t;
        (** The [code_id] uniquely determines the function symbol that must
            be called. *)
        closure_id : Closure_id.t;
        (** The [closure_id] identifies which closure is to be passed to the
            function. *)
        return_arity : Flambda_arity.With_subkinds.t;
        (** [return_arity] describes what the callee returns.  It matches up
            with the arity of [continuation] in the enclosing [Apply.t]
            record. *)
      }
    | Indirect_unknown_arity
    | Indirect_known_arity of {
        param_arity : Flambda_arity.With_subkinds.t;
        return_arity : Flambda_arity.With_subkinds.t;
      }
end

type method_kind = Self | Public | Cached

(** Whether an application expression corresponds to an OCaml function
    invocation, an OCaml method invocation, or an external call. *)
type t = private
  | Function of Function_call.t
  | Method of { kind : method_kind; obj : Simple.t; }
  | C_call of {
      alloc : bool;
      param_arity : Flambda_arity.t;
      return_arity : Flambda_arity.t;
    }

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val direct_function_call
   : Code_id.t
  -> Closure_id.t
  -> return_arity:Flambda_arity.With_subkinds.t
  -> t

val indirect_function_call_unknown_arity : unit -> t

val indirect_function_call_known_arity
   : param_arity:Flambda_arity.With_subkinds.t
  -> return_arity:Flambda_arity.With_subkinds.t
  -> t

val method_call : method_kind -> obj:Simple.t -> t

val c_call
   : alloc:bool
  -> param_arity:Flambda_arity.t
  -> return_arity:Flambda_arity.t
  -> t

val return_arity : t -> Flambda_arity.With_subkinds.t
