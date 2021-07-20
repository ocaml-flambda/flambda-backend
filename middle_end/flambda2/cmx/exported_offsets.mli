(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Vincent Laviron and Guillaume Bury, OCamlPro              *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t
(** Public state to store the mapping from elements of a closure to offset. *)

type closure_info = {
  offset : int;
  size : int; (* Number of fields taken for the function:
                 2 fields (code pointer + arity) for function of arity one
                 3 fields (caml_curry + arity + code pointer) otherwise *)
}

type env_var_info = {
  offset : int;
}

val empty : t
(** The empty environment *)

val print : Format.formatter -> t -> unit
(** Printing function for environment. *)

val env_var_offset : t -> Var_within_closure.t -> env_var_info option
(** Returns the offset computed for an environment variable, in
    terms of target architecture words.
    If [None] is returned, there is no closure in the program containing the
    given closure variable. *)

val closure_offset : t -> Closure_id.t -> closure_info option
(** Returns the offset computed for a closure id, in terms of
    target architecture words.
    This points to the first field of the closure representation
    within the sets of closures block. Notably, if the offset is not 0,
    an infix header should be placed just before the returned offset.
    If [None] is returned, there is no closure in the program containing the
    given closure ID. *)

val add_closure_offset : t -> Closure_id.t -> closure_info -> t
val add_env_var_offset : t -> Var_within_closure.t -> env_var_info -> t
(** Record the assignment of the given offset to the given element *)

val map_closure_offsets :
     t
  -> (Closure_id.t -> closure_info -> 'a)
  -> 'a Closure_id.Map.t
val map_env_var_offsets :
     t
  -> (Var_within_closure.t -> env_var_info -> 'a)
  -> 'a Var_within_closure.Map.t
(** Build maps from the underlying data *)

val import_offsets : t -> unit
(** Take the offsets read from a cmx file and add them to the current state *)

val imported_offsets : unit -> t
(** Return all the offsets read from cmx files so far *)

val merge : t -> t -> t
(** Merge the offsets from two files *)

