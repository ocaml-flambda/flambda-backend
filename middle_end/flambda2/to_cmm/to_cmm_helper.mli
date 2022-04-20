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

(** Cmm helpers for flambda to cmm translation. *)

(** {2 Useful misc values} *)

val unsupported_32_bits : unit -> 'a

(** [arch32] is [true] iff we are compiling for a 32-bit target. *)
val arch32 : bool

(** [arch64] is [true] iff we are compiling for a 64-bit target. *)
val arch64 : bool

(** An adequate Cmm machtype for an int64 (including on a 32-bit target). *)
val typ_int64 : Cmm.machtype

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

(** {2 Data items} *)

(** Static integer. *)
val cint : nativeint -> Cmm.data_item

(** Static float. *)
val cfloat : float -> Cmm.data_item

(** Static symbol. *)
val symbol_address : string -> Cmm.data_item

(** Definition for a statis symbol. *)
val define_symbol : global:bool -> string -> Cmm.data_item list

(** {2 Kinds and types} *)

val check_arity : Flambda_arity.With_subkinds.t -> _ list -> bool

val machtype_of_kind : Flambda_kind.t -> Cmm.machtype_component array

val machtype_of_kinded_parameter :
  Bound_parameter.t -> Cmm.machtype_component array

(** {2 Cmm values} *)

(** The void (i.e. empty tuple) cmm value. Not to be confused with [() : unit]. *)
val void : Cmm.expression

(** Create the single unit value. *)
val unit : dbg:Debuginfo.t -> Cmm.expression

(** Create an expression from a variable. *)
val var : Backend_var.t -> Cmm.expression

(** Create a constant int expression from a targetint. *)
val targetint : ?dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

(** {2 Block creation} *)

(** Create an array using the given fields. *)
val make_array :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Alloc_mode.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a block using the given fields. *)
val make_block :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Block_kind.t ->
  Alloc_mode.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a closure block. *)
val make_closure_block :
  ?dbg:Debuginfo.t -> Alloc_mode.t -> Cmm.expression list -> Cmm.expression

(** {2 Boxed numbers} *)

(** Box an unboxed number *)
val box_number :
  ?dbg:Debuginfo.t ->
  Flambda_kind.Boxable_number.t ->
  Alloc_mode.t ->
  Cmm.expression ->
  Cmm.expression

(** Shortcut for [box_number Flambda_kind.Boxable_number.Naked_int64] *)
val box_int64 :
  ?dbg:Debuginfo.t -> Alloc_mode.t -> Cmm.expression -> Cmm.expression

(** Unbox a boxed number. *)
val unbox_number :
  ?dbg:Debuginfo.t ->
  Flambda_kind.Boxable_number.t ->
  Cmm.expression ->
  Cmm.expression

(** {2 Block access} *)

val convert_alloc_mode : Alloc_mode.t -> Lambda.alloc_mode

(** [infix_field_address ptr n dbg] returns an expression for the address of the
    [n]-th field of the set of closures block pointed to by [ptr]. This function
    assumes that the [n-1]-th field of the block is an infix header, so that the
    returned address is in fact a correct ocaml value. *)
val infix_field_address :
  dbg:Debuginfo.t -> Cmm.expression -> int -> Cmm.expression

(** Return an expression that computes the length of the given block. *)
val block_length : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Load a field from a block. Cmm argument order: - block - field number as a
    tagged integer *)
val block_load :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Block_access_kind.t ->
  Mutability.t ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Set a field in a block. Cmm argument order: * - block * - field number as a
    tagged integer * - new value for the field. *)
val block_set :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Block_access_kind.t ->
  Flambda_primitive.Init_or_assign.t ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression
(* CR mshinwell: These functions should have labelled arguments so we don't need
   comments *)

(** {2 Array access} *)

(** Return an expression that computes the length of the given array. *)
val array_length : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Load a field from an array. Cmm argument order: * - array * - field number
    as a tagged integer *)
val array_load :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Set a field in an array. Cmm argument order: * - array * - field number as a
    tagged integer * - new value for the field. *)
val array_set :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Flambda_primitive.Init_or_assign.t ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** {2 String and Bytes access} *)

(** Load the given number of bits from a string-like value (string, bytes, *
    bigstring). Cmm arguments order: * - string-like value * - index within the
    string as a tagged integer *)
val string_like_load :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.string_like_value ->
  Flambda_primitive.string_accessor_width ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Set the given number of bits in a bytes-like value (bytes, bigstring). * Cmm
    arguments order: * - string-like value * - index within the string as a
    tagged integer * - new value for the bits set *)
val bytes_like_set :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.bytes_like_value ->
  Flambda_primitive.string_accessor_width ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** {2 Bigarrays} *)

(** [bigarray_load kind ~bigarray ~offset] loads the element of the bigarray at
    the given offset. The translation from multi-dimension addressing to linear
    addressing is supposed to already have been done (typically during
    conversion from lambda to flambda). This returns the raw value contained in
    the array (e.g. a single byte), *except* for bigarrays containing complex
    numbers, in which case it returns a boxed complex (i.e. a caml value). *)
val bigarray_load :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.bigarray_kind ->
  bigarray:Cmm.expression ->
  offset:Cmm.expression ->
  Cmm.expression

(** [bigarray_store kind ~bigarray ~offset ~new_value] stores the given value at
    the given offset in the given bigarray. The translation from multi-dimension
    addressing to linear addressing is supposed to already have been done
    (typically during conversion from lambda to flambda). This takes as argument
    the raw value to be stored (e.g. a single byte), *except* in the case of
    complex numbers, in which case it takes a boxed complex (i.e. a caml
    value). *)
val bigarray_store :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.bigarray_kind ->
  bigarray:Cmm.expression ->
  offset:Cmm.expression ->
  new_value:Cmm.expression ->
  Cmm.expression

(** {2 Numeric conversions} *)

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val targetint_of_imm : Targetint_31_63.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

(** {2 [Simple]s, constants, etc.} *)

val symbol_from_linkage_name :
  ?dbg:Debuginfo.t -> Linkage_name.t -> Cmm.expression

val symbol : ?dbg:Debuginfo.t -> Symbol.t -> Cmm.expression

val name :
  To_cmm_env.t ->
  Name.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val const : To_cmm_env.t -> Reg_width_const.t -> Cmm.expression

val simple :
  To_cmm_env.t ->
  Simple.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val arg_list :
  To_cmm_env.t ->
  Simple.t list ->
  Cmm.expression list * To_cmm_env.t * Effects_and_coeffects.t

val param_list :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

(** {2 Expression combinators} *)

(** Translate the raise kind found on Pop trap actions into a Cmm raise kind *)
val raise_kind : Trap_action.raise_kind option -> Lambda.raise_kind

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

(** {2 Static jumps} *)

(** Opaque type for static handlers. *)
type static_handler

(** [handler id vars body] creates a static handler for exit number [id],
    binding variables [vars] in [body]. *)
val handler :
  ?dbg:Debuginfo.t ->
  int ->
  (Backend_var.With_provenance.t * Cmm.machtype) list ->
  Cmm.expression ->
  static_handler

(** [cexit id args] creates the cmm expression for static to a static handler
    with exit number [id], with arguments [args]. *)
val cexit : int -> Cmm.expression list -> Cmm.trap_action list -> Cmm.expression

(** [trap_return res traps] creates the cmm expression for returning [res] after
    applying the trap actions in [traps]. *)
val trap_return : Cmm.expression -> Cmm.trap_action list -> Cmm.expression

(** Enclose a body with some static handlers. *)
val ccatch :
  rec_flag:bool ->
  handlers:static_handler list ->
  body:Cmm.expression ->
  Cmm.expression

(** {2 Arithmetic/Logic helpers} *)

(** Conversion function. *)
val primitive_boxed_int_of_standard_int :
  Flambda_kind.Standard_int.t -> Primitive.boxed_integer

(** {2 Arithmetic/Logic operations} *)

val lsl_int_caml_raw :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val lsr_int_caml_raw :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Shift operations. take as first argument a tagged caml integer, and as
    second argument an untagged machine intger which is the amount to shift the
    first argument by. *)
val asr_int_caml_raw :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val int_of_float : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Conversions functions between integers and floats. *)
val float_of_int : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** {2 Static structure helpers} *)

(** [fundecl name args body codegen_options dbg] creates a cmm function
    declaration for a function [name] with binding [args] over [body]. *)
val fundecl :
  string ->
  (Backend_var.With_provenance.t * Cmm.machtype) list ->
  Cmm.expression ->
  Cmm.codegen_option list ->
  Debuginfo.t ->
  Cmm.fundecl

(** Create a cmm phrase for a function declaration. *)
val cfunction : Cmm.fundecl -> Cmm.phrase

(** Create a cmm phrase for a static data item. *)
val cdata : Cmm.data_item list -> Cmm.phrase

(** Create the gc root table from a list of root symbols. *)
val gc_root_table :
  make_symbol:(?unitname:string -> string option -> string) ->
  string list ->
  Cmm.phrase

(** Prepend constants that were populated in cmmgen_state (mostly by indirect
    use through functions from Cmm_helpers). *)
val flush_cmmgen_state : unit -> Cmm.phrase list

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  Cmm.memory_chunk ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:Cmm.expression option ->
  To_cmm_env.t * Cmm.expression option

val simple_static :
  To_cmm_env.t ->
  Simple.t ->
  To_cmm_env.t * [`Data of Cmm.data_item list | `Var of Variable.t]
