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

(** {2 Cmm values} *)

(** An invalid/dummy cmm expression that can be used for unreachable code. *)
val unreachable : Cmm.expression

(** The void (i.e. empty tuple) cmm value. Not to be confused with [() : unit]. *)
val void : Cmm.expression

(** Create the single unit value. *)
val unit : dbg:Debuginfo.t -> Cmm.expression

(** Create an expression from a variable. *)
val var : Backend_var.t -> Cmm.expression

(** Create an expression from a symbol. *)
val symbol : ?dbg:Debuginfo.t -> string -> Cmm.expression

(** Create a constant float expression. *)
val float : ?dbg:Debuginfo.t -> float -> Cmm.expression

(** Create a constant int expression. *)
val int : ?dbg:Debuginfo.t -> int -> Cmm.expression

(** Create a constant int expression from an int32. *)
val int32 : ?dbg:Debuginfo.t -> int32 -> Cmm.expression

(** Create a constant int expression from an int64. *)
val int64 : ?dbg:Debuginfo.t -> int64 -> Cmm.expression

(** Create a constant int expression from a targetint. *)
val targetint : ?dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

(** Create a constant int expression from a nativeint. *)
val nativeint : ?dbg:Debuginfo.t -> Nativeint.t -> Cmm.expression

(** {2 Block creation} *)

(** Create an array using the given fields. *)
val make_array :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a block using the given fields. *)
val make_block :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Block_kind.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a closure block. *)
val make_closure_block :
  ?dbg:Debuginfo.t -> Cmm.expression list -> Cmm.expression

(** {2 Boxed numbers} *)

(** Box an unboxed number *)
val box_number :
  ?dbg:Debuginfo.t ->
  Flambda_kind.Boxable_number.t ->
  Cmm.expression ->
  Cmm.expression

(** Shortcut for [box_number Flambda_kind.Boxable_number.Naked_int64] *)
val box_int64 : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Unbox a boxed number. *)
val unbox_number :
  ?dbg:Debuginfo.t ->
  Flambda_kind.Boxable_number.t ->
  Cmm.expression ->
  Cmm.expression

(** {2 Block access} *)

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
val array_length :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Cmm.expression ->
  Cmm.expression

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

(** [bigarray_load dimensions kind layout ba offset] loads the element of the
    bigarray at the given offset. The translation from multi-dimension
    addressing to linear addressing is supposed to already have been done
    (typically during conversion from lambda to flambda). This returns the raw
    value contained in the array (e.g. a single byte), *except* for bigarrays
    containing complex numbers, in which case it returns a boxed complex (i.e. a
    caml value). *)
val bigarray_load :
  ?dbg:Debuginfo.t ->
  int ->
  Flambda_primitive.bigarray_kind ->
  Flambda_primitive.bigarray_layout ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** [bigarray_store dimensions kind layout ba offset newval] stores the given
    value at the given offset in the given bigarray. The translation from
    multi-dimension addressing to linear addressing is supposed to already have
    been done (typically during conversion from lambda to flambda). This takes
    as argument the raw value to be stored (e.g. a single byte), *except* in the
    case of complex numbers, in which case it takes a boxed complex (i.e. a caml
    value). *)
val bigarray_store :
  ?dbg:Debuginfo.t ->
  int ->
  Flambda_primitive.bigarray_kind ->
  Flambda_primitive.bigarray_layout ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** {2 Expression combinators} *)

(** Cassign *)
val assign : Backend_var.t -> Cmm.expression -> Cmm.expression

(** Create a sequence of expressions. Will erase void expressions as needed. *)
val sequence : Cmm.expression -> Cmm.expression -> Cmm.expression

(** [letin v e body] binds [v] to [e] in [body]. *)
val letin :
  Backend_var.With_provenance.t ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** [letin_mut v ty e body] binds a mutable variable [v] of machtype [ty] to [e]
    in [body]. *)
val letin_mut :
  Backend_var.With_provenance.t ->
  Cmm.machtype ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Creates a conditional branching on the given condition. *)
val ite :
  ?dbg:Debuginfo.t ->
  ?then_dbg:Debuginfo.t ->
  then_:Cmm.expression ->
  ?else_dbg:Debuginfo.t ->
  else_:Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Create a load expression. *)
val load :
  ?dbg:Debuginfo.t ->
  Cmm.memory_chunk ->
  Asttypes.mutable_flag ->
  Cmm.expression ->
  Cmm.expression

(** Create a store expression. *)
val store :
  ?dbg:Debuginfo.t ->
  Cmm.memory_chunk ->
  Lambda.initialization_or_assignment ->
  Cmm.expression ->
  Cmm.expression ->
  Cmm.expression

(** Create a try_with structure. The [exn_var] is the variable bound to the
    catched exception in the handler. *)
val trywith :
  ?dbg:Debuginfo.t ->
  kind:Cmm.trywith_kind ->
  body:Cmm.expression ->
  exn_var:Backend_var.With_provenance.t ->
  handler:Cmm.expression ->
  unit ->
  Cmm.expression

(** Translate the raise kind found on Pop trap actions into a Cmm raise kind *)
val raise_kind : Trap_action.raise_kind option -> Lambda.raise_kind

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

val eq : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Integer arithmetic (dis)equality of cmm expressions. Returns an untagged
    integer (either 0 or 1) to represent the result of the comparison. *)
val neq : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val lt : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val le : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val gt : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Integer arithmetic signed comparisons on cmm expressions. Returns an
    untagged integer (either 0 or 1) to represent the result of the comparison. *)
val ge : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val ult : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val ule : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val ugt : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Integer arithmetic unsigned comparisons on cmm expressions. Returns an
    untagged integer (either 0 or 1) to represent the result of the comparison. *)
val uge : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Asbolute value on floats. *)
val float_abs : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Arithmetic negation on floats. *)
val float_neg : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

val float_add :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_sub :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_mul :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Float arithmetic operations. *)
val float_div :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_eq :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Float arithmetic (dis)equality of cmm expressions. Returns an untagged
    integer (either 0 or 1) to represent the result of the comparison. *)
val float_neq :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_lt :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_le :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val float_gt :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

(** Float arithmetic comparisons on cmm expressions. Returns an untagged integer
    (either 0 or 1) to represent the result of the comparison. *)
val float_ge :
  ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression -> Cmm.expression

val int_of_float : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** Conversions functions between integers and floats. *)
val float_of_int : ?dbg:Debuginfo.t -> Cmm.expression -> Cmm.expression

(** {2 Function calls} *)

(** [direct_call ty f_code args] creates a direct call to the function code
    [f_code] with arguments [args], with a return value of type [ty].

    If a closure needs to be passed, it must be included in [args]. *)
val direct_call :
  ?dbg:Debuginfo.t ->
  Cmm.machtype ->
  Cmm.expression ->
  Cmm.expression list ->
  Cmm.expression

(** Same as {!direct_call} but for an indirect call. *)
val indirect_call :
  ?dbg:Debuginfo.t ->
  Cmm.machtype ->
  Cmm.expression ->
  Cmm.expression list ->
  Cmm.expression

(** Same as {!direct_call} but for an indirect call that is know to be a full
    application (since this enables a few optimisations). *)
val indirect_full_call :
  ?dbg:Debuginfo.t ->
  Cmm.machtype ->
  Cmm.expression ->
  Cmm.expression list ->
  Cmm.expression

(** Create a C function call. *)
val extcall :
  ?dbg:Debuginfo.t ->
  returns:bool ->
  alloc:bool ->
  is_c_builtin:bool ->
  ty_args:Cmm.exttype list ->
  string ->
  Cmm.machtype ->
  Cmm.expression list ->
  Cmm.expression

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

(** Create a cmm phrase for a satic data item. *)
val cdata : Cmm.data_item list -> Cmm.phrase

(** Create the gc root table from a list of root symbols. *)
val gc_root_table :
  make_symbol:(?unitname:string -> string option -> string) ->
  string list ->
  Cmm.phrase

(** Prepend constants that were populated in cmmgen_state (mostly by indirect
    use through functions from Cmm_helpers). *)
val flush_cmmgen_state : unit -> Cmm.phrase list
