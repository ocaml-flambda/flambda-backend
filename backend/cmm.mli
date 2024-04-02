(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Second intermediate language (machine independent) *)

type machtype_component = Cmx_format.machtype_component =
  | Val
  | Addr
  | Int
  | Float
  | Vec128
  | Float32

(* - [Val] denotes a valid OCaml value: either a pointer to the beginning
     of a heap block, an infix pointer if it is preceded by the correct
     infix header, or a 2n+1 encoded integer.
   - [Int] is for integers (not necessarily 2n+1 encoded) and for
     pointers outside the heap.
   - [Addr] denotes pointers that are neither [Val] nor [Int], i.e.
     pointers into the heap that point in the middle of a heap block.
     Such derived pointers are produced by e.g. array indexing.
   - [Float] is for unboxed floating-point numbers.

The purpose of these types is twofold.  First, they guide register
allocation: type [Float] goes in FP registers, the other types go
into integer registers.  Second, they determine how local variables are
tracked by the GC:
   - Variables of type [Val] are GC roots.  If they are pointers, the
     GC will not deallocate the addressed heap block, and will update
     the local variable if the heap block moves.
   - Variables of type [Int] and [Float] are ignored by the GC.
     The GC does not change their values.
   - Variables of type [Addr] must never be live across an allocation
     point or function call.  They cannot be given as roots to the GC
     because they don't point after a well-formed block header of the
     kind that the GC needs.  However, the GC may move the block pointed
     into, invalidating the value of the [Addr] variable.
*)

type machtype = machtype_component array

val typ_void: machtype
val typ_val: machtype
val typ_addr: machtype
val typ_int: machtype
val typ_float: machtype
val typ_float32: machtype
val typ_vec128: machtype

(** Least upper bound of two [machtype_component]s. *)
val lub_component
   : machtype_component
  -> machtype_component
  -> machtype_component

(** Returns [true] iff the first supplied [machtype_component] is greater than
    or equal to the second under the relation used by [lub_component]. *)
val ge_component
   : machtype_component
  -> machtype_component
  -> bool

type exttype =
  | XInt                                (**r OCaml value, word-sized integer *)
  | XInt32                              (**r 32-bit integer *)
  | XInt64                              (**r 64-bit integer  *)
  | XFloat32                            (**r single-precision FP number *)
  | XFloat                              (**r double-precision FP number  *)
  | XVec128                             (**r 128-bit vector *)
(** A variant of [machtype] used to describe arguments
    to external C functions *)

val machtype_of_exttype: exttype -> machtype
val machtype_of_exttype_list: exttype list -> machtype

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

val negate_integer_comparison: integer_comparison -> integer_comparison
val swap_integer_comparison: integer_comparison -> integer_comparison

type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

val negate_float_comparison: float_comparison -> float_comparison
val swap_float_comparison: float_comparison -> float_comparison

type label = int
val new_label: unit -> label
val set_label: label -> unit
val cur_label: unit -> label

type exit_label =
  | Return_lbl
  | Lbl of Lambda.static_label

type rec_flag = Nonrecursive | Recursive

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type atomic_op = Fetch_and_add | Compare_and_swap

type atomic_bitwidth = Thirtytwo | Sixtyfour | Word

type effects = No_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type phantom_defining_expr =
  (* CR-soon mshinwell: Convert this to [Targetint.OCaml.t] (or whatever the
     representation of "target-width OCaml integers of type [int]"
     becomes when merged). *)
  | Cphantom_const_int of Targetint.t
  (** The phantom-let-bound variable is a constant integer.
      The argument must be the tagged representation of an integer within
      the range of type [int] on the target.  (Analogously to [Cconst_int].) *)
  | Cphantom_const_symbol of string
  (** The phantom-let-bound variable is an alias for a symbol. *)
  | Cphantom_var of Backend_var.t
  (** The phantom-let-bound variable is an alias for another variable.  The
      aliased variable must not be a bound by a phantom let. *)
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

type trywith_shared_label = Lambda.static_label (* Same as Ccatch handlers *)

type trap_action =
  | Push of trywith_shared_label
  (** Add the corresponding handler to the trap stack. *)
  | Pop of trywith_shared_label
  (** Remove the last handler from the trap stack. *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type initialization_or_assignment =
  | Initialization
  | Assignment

type float_width =
  | Float64
  | Float32

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int                           (* integer or pointer outside heap *)
  | Word_val                           (* pointer inside heap or encoded int *)
  | Single of { reg : float_width }    (* F32 in memory, may be F32 or F64 in registers. *)
  | Double                             (* word-aligned 64-bit float
                                          see PR#10433 *)
  | Onetwentyeight_unaligned           (* word-aligned 128-bit vector *)
  | Onetwentyeight_aligned             (* 16-byte-aligned 128-bit vector *)

type vector_cast =
  | Bits128

type scalar_cast =
  | Float_to_float32
  | Float_of_float32
  | Float_to_int of float_width
  | Float_of_int of float_width
  | V128_to_scalar of Primitive.vec128_type
  | V128_of_scalar of Primitive.vec128_type

type operation =
    Capply of machtype * Lambda.region_close
  | Cextcall of
      { func: string;
        ty: machtype;
        ty_args : exttype list;
        alloc: bool;
        builtin: bool;
        returns: bool;
        effects: effects;
        coeffects: coeffects;
      }
      (** The [machtype] is the machine type of the result.
          The [exttype list] describes the unboxing types of the arguments.
          An empty list means "all arguments are machine words [XInt]".
          The boolean indicates whether the function may allocate. *)
  | Cload of
      { memory_chunk: memory_chunk;
        mutability: Asttypes.mutable_flag;
        is_atomic: bool;
      }
  | Calloc of Lambda.alloc_mode
  | Cstore of memory_chunk * initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi of { signed: bool }  | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Cbswap of { bitwidth: bswap_bitwidth; }
  | Ccsel of machtype
  | Cclz of { arg_is_non_zero: bool; }
  | Cctz of { arg_is_non_zero: bool; }
  | Cpopcnt
  | Cprefetch of { is_write: bool; locality: prefetch_temporal_locality_hint; }
  | Catomic of { op: atomic_op; size : atomic_bitwidth }
  | Ccmpi of integer_comparison
  | Caddv (* pointer addition that produces a [Val] (well-formed Caml value) *)
  | Cadda (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa of integer_comparison
  | Cnegf of float_width | Cabsf of float_width
  | Caddf of float_width | Csubf of float_width
  | Cmulf of float_width | Cdivf of float_width
  | Cvalueofint | Cintofvalue
  | Cvectorcast of vector_cast
  | Cscalarcast of scalar_cast
  | Ccmpf of float_width * float_comparison
  | Craise of Lambda.raise_kind
  | Cprobe of { name: string; handler_code_sym: string; enabled_at_init: bool }
  | Cprobe_is_enabled of { name: string }
  | Copaque (* Sys.opaque_identity *)
  | Cbeginregion | Cendregion
  | Ctuple_field of int * machtype array
      (* the [machtype array] refers to the whole tuple *)
  | Cdls_get

(* This is information used exclusively during construction of cmm terms by
   cmmgen, and thus irrelevant for selectgen and flambda2. *)
type kind_for_unboxing =
  | Any (* This may contain anything, including non-scannable things *)
  | Boxed_integer of Lambda.boxed_integer
  | Boxed_vector of Lambda.boxed_vector
  | Boxed_float of Lambda.boxed_float

type is_global = Global | Local
val equal_is_global : is_global -> is_global -> bool

(* Symbols are marked with whether they are local or global,
   at both definition and use sites.

   Symbols defined as [Local] may only be referenced within the same file,
   and all such references must also be [Local].

   Symbols defined as [Global] may be referenced from other files.
   References from other files must be [Global], but references from the
   same file may be [Local].

   (Marking symbols in this way speeds up linking, as many references can
   then be resolved early) *)
type symbol =
  { sym_name : string;
    sym_global : is_global }

(* SIMD vectors are untyped in the backend.
   This record holds the bitwise representation of a 128-bit value. *)
type vec128_bits = { low : int64; high: int64 }

val global_symbol : string -> symbol

(** Every basic block should have a corresponding [Debuginfo.t] for its
    beginning. *)
type expression =
    Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float32 of float * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_vec128 of vec128_bits * Debuginfo.t
  | Cconst_symbol of symbol * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Clet_mut of Backend_var.With_provenance.t * machtype
                * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  (* Cassign must refer to a variable bound by Clet_mut *)
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t * kind_for_unboxing
  | Cswitch of expression * int array * (expression * Debuginfo.t) array
      * Debuginfo.t * kind_for_unboxing
  | Ccatch of
      rec_flag
        * (Lambda.static_label * (Backend_var.With_provenance.t * machtype) list
          * expression * Debuginfo.t * bool (* is_cold *)) list
        * expression
        * kind_for_unboxing
  | Cexit of exit_label * expression list * trap_action list
  | Ctrywith of expression * trywith_shared_label
      * Backend_var.With_provenance.t * expression * Debuginfo.t
      * kind_for_unboxing
    (** Ctrywith uses "delayed handlers":
        The body starts with the previous exception handler, and only after
        going through an explicit Push-annotated Cexit will this handler become
        active.  This allows for sharing a single handler in several places, or
        having multiple entry and exit points to a single trywith block. *)

type property =
  | Zero_alloc

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Use_linscan_regalloc
  | Ignore_assert_all of property
  | Assume of { property: property; strict: bool; never_returns_normally: bool;
                loc: Location.t }
  | Check of { property: property; strict: bool; loc: Location.t }

type fundecl =
  { fun_name: symbol;
    fun_args: (Backend_var.With_provenance.t * machtype) list;
    fun_body: expression;
    fun_codegen_options : codegen_option list;
    fun_poll: Lambda.poll_attribute;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of symbol
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Cvec128 of vec128_bits
  | Csymbol_address of symbol
  | Csymbol_offset of symbol * int
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

val ccatch :
     label * (Backend_var.With_provenance.t * machtype) list
       * expression * expression * Debuginfo.t * kind_for_unboxing
       * bool
  -> expression

val reset : unit -> unit

val iter_shallow_tail: (expression -> unit) -> expression -> bool
  (** Either apply the callback to all immediate sub-expressions that
      can produce the final result for the expression and return
      [true], or do nothing and return [false].  Note that the notion
      of "tail" sub-expression used here does not match the one used
      to trigger tail calls; in particular, try...with handlers are
      considered to be in tail position (because their result become
      the final result for the expression).  *)

val map_shallow_tail: ?kind:kind_for_unboxing -> (expression -> expression) -> expression -> expression
  (** Apply the transformation to those immediate sub-expressions of an
      expression that are in tail position, using the same definition of "tail"
      as [iter_shallow_tail] *)

val map_tail: ?kind:kind_for_unboxing -> (expression -> expression) -> expression -> expression
  (** Apply the transformation to an expression, trying to push it
      to all inner sub-expressions that can produce the final result,
      by recursively applying map_shallow_tail *)

val iter_shallow: (expression -> unit) -> expression -> unit
  (** Apply the callback to each immediate sub-expression. *)

val map_shallow: (expression -> expression) -> expression -> expression
  (** Apply the transformation to each immediate sub-expression. *)

val equal_machtype_component : machtype_component -> machtype_component -> bool
val equal_exttype : exttype -> exttype -> bool
val equal_scalar_cast : scalar_cast -> scalar_cast -> bool
val equal_float_width : float_width -> float_width -> bool
val equal_float_comparison : float_comparison -> float_comparison -> bool
val equal_memory_chunk : memory_chunk -> memory_chunk -> bool
val equal_integer_comparison : integer_comparison -> integer_comparison -> bool
val all_properties : property list

val caml_flambda2_invalid : string
