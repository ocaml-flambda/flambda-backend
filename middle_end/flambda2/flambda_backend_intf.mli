(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Knowledge that the Flambda 2 middle end needs about the backend. *)

module type S = sig
  (** Compute the symbol for the given identifier. *)
  val symbol_for_global' : Ident.t -> Symbol.t

  val all_predefined_exception_symbols : Symbol.Set.t

  val division_by_zero : Symbol.t

  val invalid_argument : Symbol.t

  (** The natural size of an integer on the target architecture (cf.
      [Arch.size_int] in the native code backend). *)
  val size_int : int

  (** [true] iff the target architecture is big endian. *)
  val big_endian : bool

  (** The maximum number of arguments that is reasonable for a function to have.
      This should be fewer than the threshold that causes non-self tail call
      optimization to be inhibited (in particular, if it would entail passing
      arguments on the stack; see [Selectgen]). *)
  val max_sensible_number_of_arguments : int

  val set_global_info : Flambda_cmx_format.t -> unit

  val get_global_info :
    Flambda2_compilenv_deps.Compilation_unit.t -> Flambda_cmx_format.t option
end
