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

module Function_call : sig
  type t = private
    | Direct of Code_id.t
        (** The [code_id] uniquely determines the function symbol that
            must be called. *)
    | Indirect_unknown_arity
    | Indirect_known_arity
end

(** Whether an application expression corresponds to an OCaml function
    invocation, an OCaml method invocation, or an external call. *)
type t = private
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_types.t
      }
  | C_call of
      { alloc : bool;
        is_c_builtin : bool
            (* CR mshinwell: This should have the effects and coeffects
               fields *)
      }

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val direct_function_call : Code_id.t -> Alloc_mode.For_types.t -> t

val indirect_function_call_unknown_arity : Alloc_mode.For_types.t -> t

val indirect_function_call_known_arity : Alloc_mode.For_types.t -> t

val c_call : alloc:bool -> is_c_builtin:bool -> t
