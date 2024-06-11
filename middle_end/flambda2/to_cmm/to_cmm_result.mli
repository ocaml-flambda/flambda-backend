(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Result structure used during Flambda to Cmm translation. *)

(** An accumulator for the Cmm phrases and GC roots that result from translating
    statically-allocated blocks and function bodies.

    Values of type [t] contain both older, "archived" data and "current" data
    which is still being worked on. The [archive_data] function transfers
    current data into the archived part, clearing the current data area for
    subsequent use. *)
type t

(** Create a result structure.

    [reachable_names] specifies which names are reachable from outside the
    compilation unit (same terminology as used in [Flambda_cmx]).
*)
val create : module_symbol:Symbol.t -> reachable_names:Name_occurrences.t -> t

(** Translate an existing [Symbol.t] to a Cmm symbol. *)
val symbol : t -> Symbol.t -> Cmm.symbol

(** Produce the Cmm function symbol for a piece of code. *)
val symbol_of_code_id :
  t -> Code_id.t -> currently_in_inlined_body:bool -> Cmm.symbol

(** Create a Cmm symbol, not arising from a [Symbol.t]. *)
val raw_symbol : t -> global:Cmm.is_global -> string -> t * Cmm.symbol

(** Archive the current data into the list of already-translated data. *)
val archive_data : t -> t

(** Add already-translated Cmm data items into the archived part of the result
    structure. *)
val add_archive_data_items : t -> Cmm.data_item list -> t

(** Update the current data part of the result structure. *)
val update_data : t -> (Cmm.data_item list -> Cmm.data_item list) -> t

(** Set the current data to the given list. Raises a fatal error if the current
    data is not empty, to avoid inadvertently losing data. *)
val set_data : t -> Cmm.data_item list -> t

(** Register one or more symbols as required GC roots. *)
val add_gc_roots : t -> Symbol.t list -> t

(** Register a function that has been translated to Cmm. *)
val add_function : t -> Cmm.fundecl -> t

(** Record the given symbol as having been defined. This is used to keep track
    of whether the module block symbol for the current unit has been defined. *)
val check_for_module_symbol : t -> Symbol.t -> t

(** Caching of symbols associated with [Invalid] messages. *)
val add_invalid_message_symbol : t -> Symbol.t -> message:string -> t

val invalid_message_symbol : t -> message:string -> Symbol.t option

(** Determine whether a region is used, as per [mark_region_as_used] below. *)
val region_is_used :
  t -> Variable.t -> resolve_alias:(Variable.t -> Variable.t) -> bool

(** Mark that we have seen an occurrence of a given region as specified by
    an allocation mode.
    (Occurrences in [End_region] primitives do not count as uses.) *)
val mark_region_as_used :
  t ->
  Alloc_mode.For_allocations.t ->
  resolve_alias:(Variable.t -> Variable.t) ->
  t

type result = private
  { data_items : Cmm.phrase list;
    gc_roots : Cmm.symbol list;
    functions : Cmm.phrase list
  }

(** Archive the current data and then return the translated data present in the
    given result structure. *)
val to_cmm : t -> result
