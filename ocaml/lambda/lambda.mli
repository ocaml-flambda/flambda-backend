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

(* The "lambda" intermediate code *)

open Asttypes

(* Overriding Asttypes.mutable_flag *)
type mutable_flag = Immutable | Immutable_unique | Mutable

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

type immediate_or_pointer =
  | Immediate
  | Pointer

type alloc_mode = private
  | Alloc_heap
  | Alloc_local

val alloc_heap : alloc_mode

(* Actually [Alloc_heap] if [Config.stack_allocation] is [false] *)
val alloc_local : alloc_mode

type initialization_or_assignment =
  (* [Assignment Alloc_local] is a mutation of a block that may be heap or local.
     [Assignment Alloc_heap] is a mutation of a block that's definitely heap. *)
  | Assignment of alloc_mode
  (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
  | Heap_initialization
  (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type field_read_semantics =
  | Reads_agree
  | Reads_vary

(* Tail calls can close their enclosing region early *)
type region_close =
  | Rc_normal         (* do not close region, may TCO if in tail position *)
  | Rc_nontail        (* do not close region, must not TCO *)
  | Rc_close_at_apply (* close region and tail call *)

type primitive =
  | Pidentity
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
  | Prevapply of region_close
  | Pdirapply of region_close
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape * alloc_mode
  | Pmakefloatblock of mutable_flag * alloc_mode
  | Pfield of int * field_read_semantics
  | Pfield_computed of field_read_semantics
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int * field_read_semantics * alloc_mode
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  (* Comparions that return int (not bool like above) for ordering *)
  | Pcompare_ints | Pcompare_floats | Pcompare_bints of boxed_integer
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint of alloc_mode
  | Pnegfloat of alloc_mode | Pabsfloat of alloc_mode
  | Paddfloat of alloc_mode | Psubfloat of alloc_mode
  | Pmulfloat of alloc_mode | Pdivfloat of alloc_mode
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag * alloc_mode
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint of { variant_only : bool }
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer * alloc_mode
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
                * alloc_mode
  | Pnegbint of boxed_integer * alloc_mode
  | Paddbint of boxed_integer * alloc_mode
  | Psubbint of boxed_integer * alloc_mode
  | Pmulbint of boxed_integer * alloc_mode
  | Pdivbint of { size : boxed_integer; is_safe : is_safe; mode: alloc_mode }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe; mode: alloc_mode }
  | Pandbint of boxed_integer * alloc_mode
  | Porbint of boxed_integer * alloc_mode
  | Pxorbint of boxed_integer * alloc_mode
  | Plslbint of boxed_integer * alloc_mode
  | Plsrbint of boxed_integer * alloc_mode
  | Pasrbint of boxed_integer * alloc_mode
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on Bigarrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a Bigarray *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool * alloc_mode
  | Pstring_load_64 of bool * alloc_mode
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool * alloc_mode
  | Pbytes_load_64 of bool * alloc_mode
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool * alloc_mode
  | Pbigstring_load_64 of bool * alloc_mode
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer * alloc_mode
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Inhibition of optimisation *)
  | Popaque
  (* Statically-defined probes *)
  | Pprobe_is_enabled of { name: string }

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval
  | Pvariant of {
      consts : int list;
      non_consts : (int * value_kind list) list;
      (** [non_consts] must be non-empty.  For constant variants [Pintval]
          must be used.  This causes a small loss of precision but it is not
          expected to be significant. *)
    }
  | Parrayval of array_kind

and block_shape =
  value_kind list option

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

val equal_primitive : primitive -> primitive -> bool

val equal_value_kind : value_kind -> value_kind -> bool

val equal_boxed_integer : boxed_integer -> boxed_integer -> bool

type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
  | Const_float_block of string list

type tailcall_attribute =
  | Tailcall_expectation of bool
    (* [@tailcall] and [@tailcall true] have [true],
       [@tailcall false] has [false] *)
  | Default_tailcall (* no [@tailcall] attribute *)

(* Function declaration inlining annotations *)
type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Available_inline (* [@inline available] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

(* Call site inlining annotations *)
type inlined_attribute =
  | Always_inlined (* [@inlined] or [@inlined always] *)
  | Never_inlined (* [@inlined never] *)
  | Hint_inlined (* [@inlined hint] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inlined (* no [@inlined] attribute *)

val equal_inline_attribute : inline_attribute -> inline_attribute -> bool
val equal_inlined_attribute : inlined_attribute -> inlined_attribute -> bool

type probe_desc = { name: string }
type probe = probe_desc option

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

val equal_specialise_attribute
   : specialise_attribute
  -> specialise_attribute
  -> bool

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type function_kind = Curried of {nlocal: int} | Tupled
(* [nlocal] determines how many arguments may be partially applied
   before the resulting closure must be locally allocated.
   See [check_lfunction] for details *)

type let_kind = Strict | Alias | StrictOpt | Variable
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effects; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e'
 *)

type meth_kind = Self | Public | Cached

val equal_meth_kind : meth_kind -> meth_kind -> bool

type shared_code = (int * int) list     (* stack size -> code label *)

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  is_a_functor: bool;
  stub: bool;
}

type scoped_location = Debuginfo.Scoped_location.t

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * scoped_location
  | Lswitch of lambda * lambda_switch * scoped_location * value_kind
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * scoped_location * value_kind
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * lambda * value_kind
  | Ltrywith of lambda * Ident.t * lambda * value_kind
(* Lifthenelse (e, t, f) evaluates t if e evaluates to 0, and
   evaluates f if e evaluates to any other value *)
  | Lifthenelse of lambda * lambda * lambda * value_kind
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list
             * region_close * alloc_mode * scoped_location
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda
  | Lregion of lambda

and lfunction =
  { kind: function_kind;
    params: (Ident.t * value_kind) list;
    return: value_kind;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc : scoped_location;
    mode : alloc_mode;     (* alloc mode of the closure itself *)
    region : bool;         (* false if this function may locally
                              allocate in the caller's region *)
  }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_region_close : region_close;
    ap_mode : alloc_mode;
    ap_loc : scoped_location;
    ap_tailcall : tailcall_attribute;
    ap_inlined : inlined_attribute; (* [@inlined] attribute in code *)
    ap_specialised : specialise_attribute;
    ap_probe : probe;
  }

and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option}      (* Action to take if failure *)
and lambda_event =
  { lev_loc: scoped_location;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.t }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo
  | Lev_module_definition of Ident.t

type program =
  { module_ident : Ident.t;
    main_module_block_size : int;
    required_globals : Ident.Set.t;    (* Modules whose initializer side effects
                                          must occur before [code]. *)
    code : lambda }
(* Lambda code for the middle-end.
   * In the closure case the code is a sequence of assignments to a
     preallocated block of size [main_module_block_size] using
     (Setfield(Getglobal(module_ident))). The size is used to preallocate
     the block.
   * In the flambda case the code is an expression returning a block
     value of size [main_module_block_size]. The size is used to build
     the module root as an initialize_symbol
     Initialize_symbol(module_name, 0,
       [getfield 0; ...; getfield (main_module_block_size - 1)])
*)

(* Sharing key *)
val make_key: lambda -> lambda option

val const_unit: structured_constant
val const_int : int -> structured_constant
val lambda_unit: lambda
val check_lfunction : lfunction -> unit
val name_lambda: let_kind -> lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list: lambda list -> (lambda list -> lambda) -> lambda

val iter_head_constructor: (lambda -> unit) -> lambda -> unit
(** [iter_head_constructor f lam] apply [f] to only the first level of
    sub expressions of [lam]. It does not recursively traverse the
    expression.
*)

val shallow_iter:
  tail:(lambda -> unit) ->
  non_tail:(lambda -> unit) ->
  lambda -> unit
(** Same as [iter_head_constructor], but use a different callback for
    sub-terms which are in tail position or not. *)

val transl_prim: string -> string -> lambda
(** Translate a value from a persistent module. For instance:

    {[
      transl_internal_value "CamlinternalLazy" "force"
    ]}
*)

val free_variables: lambda -> Ident.Set.t

val transl_module_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_value_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_extension_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_class_path: scoped_location -> Env.t -> Path.t -> lambda

val make_sequence: ('a -> lambda) -> 'a list -> lambda

val subst:
  (Ident.t -> Types.value_description -> Env.t -> Env.t) ->
  ?freshen_bound_variables:bool ->
  lambda Ident.Map.t -> lambda -> lambda
(** [subst update_env ?freshen_bound_variables s lt]
    applies a substitution [s] to the lambda-term [lt].

    Assumes that the image of the substitution is out of reach
    of the bound variables of the lambda-term (no capture).

    [update_env] is used to refresh the environment contained in debug
    events.

    [freshen_bound_variables], which defaults to [false], freshens
    the bound variables within [lt].
 *)

val rename : Ident.t Ident.Map.t -> lambda -> lambda
(** A version of [subst] specialized for the case where we're just renaming
    idents. *)

val duplicate : lambda -> lambda
(** Duplicate a term, freshening all locally-bound identifiers. *)

val map : (lambda -> lambda) -> lambda -> lambda
  (** Bottom-up rewriting, applying the function on
      each node from the leaves to the root. *)

val shallow_map  :
  tail:(lambda -> lambda) ->
  non_tail:(lambda -> lambda) ->
  lambda -> lambda
  (** Rewrite each immediate sub-term with the function. *)

val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda
val bind_with_value_kind:
  let_kind -> (Ident.t * value_kind) -> lambda -> lambda -> lambda

val negate_integer_comparison : integer_comparison -> integer_comparison
val swap_integer_comparison : integer_comparison -> integer_comparison

val negate_float_comparison : float_comparison -> float_comparison
val swap_float_comparison : float_comparison -> float_comparison

val default_function_attribute : function_attribute
val default_stub_attribute : function_attribute

val max_arity : unit -> int
  (** Maximal number of parameters for a function, or in other words,
      maximal length of the [params] list of a [lfunction] record.
      This is unlimited ([max_int]) for bytecode, but limited
      (currently to 126) for native code. *)

val join_mode : alloc_mode -> alloc_mode -> alloc_mode
val sub_mode : alloc_mode -> alloc_mode -> bool
val eq_mode : alloc_mode -> alloc_mode -> bool
val is_local_mode : alloc_mode -> bool
val is_heap_mode : alloc_mode -> bool

val primitive_may_allocate : primitive -> alloc_mode option
  (** Whether and where a primitive may allocate.
      [Some Alloc_local] permits both options: that is, primitives that
      may allocate on both the GC heap and locally report this value. *)

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int

val staticfail : lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded: lambda -> bool
val patch_guarded : lambda -> lambda -> lambda

val raise_kind: raise_kind -> string

val merge_inline_attributes
   : inline_attribute
  -> inline_attribute
  -> inline_attribute option

val reset: unit -> unit

(** Helpers for module block accesses.
    Module accesses are always immutable, except in translobj where the
    method cache is stored in a mutable module field.
*)
val mod_field: ?read_semantics: field_read_semantics -> int -> primitive
val mod_setfield: int -> primitive
