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

[@@@ocaml.warning "+a-30-40-41-42"]

(** An accumulator for the Cmm phrases and GC roots that result from translating
    statically-allocated blocks and function bodies.

    Values of type [t] contain both older, "archived" data and "current" data
    which is still being worked on. The [archive_data] function transfers
    current data into the archived part, clearing the current data area for
    subsequent use. *)
type t

(** Create a result structure. *)
val create :
  module_symbol:Symbol.t -> data_symbol:Symbol.t -> Exported_offsets.t -> t

(** Archive the current data into the list of already-translated data. See note
    on [archive_offset_data] below too. *)
val archive_data : t -> t

(** Like [archive_data] but must be used if the current data item list contains
    symbols that have been translated to offsets from the single data symbol. It
    is not permissible to mix such symbols with those not translated in that
    manner in the same current data list. *)
val archive_offset_data : t -> t

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
    of whether the module block symbol for the current unit has been defined.
    Returns whether the current symbol is the module symbol. *)
val check_for_module_symbol : t -> Symbol.t -> t * bool

val is_module_symbol : t -> Symbol.t -> bool

val record_symbol_offset :
  t -> Symbol.t -> size_in_words_excluding_header:int -> t

val increment_symbol_offset : t -> size_in_words:int -> t

(** Get the Cmm data item to fetch the address of a [Symbol] (which may be an
    offset load from another symbol). *)
val static_symbol_address : t -> Symbol.t -> Cmm.data_item

(** Like [static_symbol_address] but for expressions. *)
val expr_symbol_address : t -> Symbol.t -> Debuginfo.t -> Cmm.expression

type result = private
  { data_items : Cmm.phrase list;
    gc_roots : Symbol.t list;
    functions : Cmm.phrase list;
    offsets : Exported_offsets.t
  }

(** Archive the current data and then return the translated data present in the
    given result structure. *)
val to_cmm : t -> result
