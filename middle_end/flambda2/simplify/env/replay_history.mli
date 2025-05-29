(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Guillaume Bury, Pierre Chambart and Nathanaëlle Courant, OCamlPro    *)
(*                                                                        *)
(*   Copyright 2013--2024 OCamlPro SAS                                    *)
(*   Copyright 2014--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Replay Histories}

    In some cases (e.g. continuation specialization for match-in-match), we are
    interested in doing multiple downwards pass on a single term. In such
    cases, it might be necessary to relate the names of variables and
    continuations from the first pass to those from the subsequent passes. The
    predominant use of that is currently to correctly handle continuations that
    have been lifted during the first pass, and whose calls should be rewritten
    in subsequent passes (including their lifted arguments).

    This module provides a way to "replay" a downwards pass and more specifically
    its sequence of bindings, so that the names of continuations and variables
    generated during the second pass can be mapped to the names generated during
    the first pass.

    As a safety measure, before mapping two names, we check that they are in
    the same "renaming equivalence class", though note that this is not a
    guarantee (for isntance, by default all continuation have no names and
    therefore will all be in the same "renaming equivalence class").

    Additionally, and this is more specific to continuation specialization but
    generalizable to other future uses, it is necessary to have some kind of
    replayability of inlining decisions. Indeed each inlining decision will
    change the sequence of bound names that are opened during the downwards
    pass, which would currently break the hypothesis of replay histories that
    exactly the same sequence of binders are opened during both downwards
    passes. In the case of match-in-match this is simple: the handler that we
    want to specialize ends with a switch, which means that any call inside the
    handler have been inlined, so the replay history has a boolean to denote
    that we want to inline everything while replaying the handler downwards
    pass. For other more complex uses, inlining decisions could be stored
    alongside the bound variables and continuations, so that they can be
    replayed, either as is, or within some notion of compatibility. *)

(** The type of a replay history, used both when recording during the first
    downwards pass **and** when replaying during a subsequent downwards pass
    on the same term. *)
type t

val print : Format.formatter -> t -> unit

(** {2 Creation API} *)

(** Create an empty replay history for a first downwards pass. *)
val first_pass : t

(** Given a history generated during a first downwards pass on an expression [expr], create a new history
    that is suitable for use for subsequent passes on [expr]. *)
val replay : always_inline:bool -> t -> t

(** Define a new bound variable. *)
val define_variable : Variable.t -> t -> t

(** Define a new set of bound mutually recursive continuations. *)
val define_continuations : Continuation.t list -> t -> t

(** {2 Inspection API} *)

(** Returns [true] is the replay history was created with the `always_inline` flag. (first pass
    histories have the flag set to false). *)
val must_inline : t -> bool

(** Type of results for inspection functions for which the result only makes sense
    when replaying a downwards pass. *)
type 'a replay_result =
  | Still_recording
  | Replayed of 'a

(** If called on a history created with {!replay}, returns the mapping from variables
    of the current pass to variables bound during the first pass. *)
val replay_variable_mapping : t -> Variable.t Variable.Map.t replay_result

(** If called on a history created with {!replay}, returns the mapping from continuations
    of the current pass to continuations bound during the first pass. *)
val replay_continuation_mapping :
  t -> Continuation.t Continuation.Map.t replay_result
