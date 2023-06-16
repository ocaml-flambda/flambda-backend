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

open Misc
open Asttypes

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

type is_safe =
  | Safe
  | Unsafe

type field_read_semantics =
  | Reads_agree
  | Reads_vary

include (struct

  type alloc_mode =
    | Alloc_heap
    | Alloc_local

  type modify_mode =
    | Modify_heap
    | Modify_maybe_stack

  let alloc_heap = Alloc_heap

  let alloc_local : alloc_mode =
    if Config.stack_allocation then Alloc_local
    else Alloc_heap

  let join_mode a b =
    match a, b with
    | Alloc_local, _ | _, Alloc_local -> Alloc_local
    | Alloc_heap, Alloc_heap -> Alloc_heap

  let modify_heap = Modify_heap

  let modify_maybe_stack : modify_mode =
    (* CR zqian: possible to move this check to a better place? *)
    (* idealy I don't want to do the checking here.
       if stack allocations are disabled, then the alloc_mode which this modify_mode
        depends on should be heap, which makes this modify_mode to be heap *)

    (* one suggestion: move the check to optimize_allocation;
      if stack_allocation not enabled, force all allocations to be heap,
        which then propagates to all the other modes.
       *)
    if Config.stack_allocation then Modify_maybe_stack
    else Modify_heap

end : sig

  type alloc_mode = private
    | Alloc_heap
    | Alloc_local

  type modify_mode = private
    | Modify_heap
    | Modify_maybe_stack

  val alloc_heap : alloc_mode

  val alloc_local : alloc_mode

  val modify_heap : modify_mode

  val modify_maybe_stack : modify_mode

  val join_mode : alloc_mode -> alloc_mode -> alloc_mode

end)

let is_local_mode = function
  | Alloc_heap -> false
  | Alloc_local -> true

let is_heap_mode = function
  | Alloc_heap -> true
  | Alloc_local -> false

let sub_mode a b =
  match a, b with
  | Alloc_heap, _ -> true
  | _, Alloc_local -> true
  | Alloc_local, Alloc_heap -> false

let eq_mode a b =
  match a, b with
  | Alloc_heap, Alloc_heap -> true
  | Alloc_local, Alloc_local -> true
  | Alloc_heap, Alloc_local -> false
  | Alloc_local, Alloc_heap -> false

type initialization_or_assignment =
  | Assignment of modify_mode
  | Heap_initialization
  | Root_initialization

type region_close =
  | Rc_normal
  | Rc_nontail
  | Rc_close_at_apply

type primitive =
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
    (* Globals *)
  | Pgetglobal of Compilation_unit.t
  | Psetglobal of Compilation_unit.t
  | Pgetpredef of Ident.t
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
  (* Force lazy values *)
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
  | Parraylength of array_kind
  | Parrayrefu of array_ref_kind
  | Parraysetu of array_set_kind
  | Parrayrefs of array_ref_kind
  | Parraysets of array_set_kind
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
  | Popaque of layout
  (* Statically-defined probes *)
  | Pprobe_is_enabled of { name: string }
  (* Primitives for [Obj] *)
  | Pobj_dup
  | Pobj_magic of layout
  | Punbox_float
  | Pbox_float of alloc_mode
  | Punbox_int of boxed_integer
  | Pbox_int of boxed_integer * alloc_mode
  (* Jane Street extensions *)
  | Parray_to_iarray
  | Parray_of_iarray

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval
  | Pvariant of {
      consts : int list;
      non_consts : (int * value_kind list) list;
    }
  | Parrayval of array_kind

and layout =
  | Ptop
  | Pvalue of value_kind
  | Punboxed_float
  | Punboxed_int of boxed_integer
  | Pbottom

and block_shape =
  value_kind list option

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and array_ref_kind =
  | Pgenarray_ref of alloc_mode
  | Paddrarray_ref
  | Pintarray_ref
  | Pfloatarray_ref of alloc_mode

and array_set_kind =
  | Pgenarray_set of modify_mode
  | Paddrarray_set of modify_mode
  | Pintarray_set
  | Pfloatarray_set

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

let equal_boxed_integer = Primitive.equal_boxed_integer

let equal_primitive =
  (* Should be implemented like [equal_value_kind] of [equal_boxed_integer],
     i.e. by matching over the various constructors but the type has more
     than 100 constructors... *)
  (=)

let rec equal_value_kind x y =
  match x, y with
  | Pgenval, Pgenval -> true
  | Pfloatval, Pfloatval -> true
  | Pboxedintval bi1, Pboxedintval bi2 -> equal_boxed_integer bi1 bi2
  | Pintval, Pintval -> true
  | Parrayval elt_kind1, Parrayval elt_kind2 -> elt_kind1 = elt_kind2
  | Pvariant { consts = consts1; non_consts = non_consts1; },
    Pvariant { consts = consts2; non_consts = non_consts2; } ->
    let consts1 = List.sort Int.compare consts1 in
    let consts2 = List.sort Int.compare consts2 in
    let compare_by_tag (tag1, _) (tag2, _) = Int.compare tag1 tag2 in
    let non_consts1 = List.sort compare_by_tag non_consts1 in
    let non_consts2 = List.sort compare_by_tag non_consts2 in
    List.equal Int.equal consts1 consts2
      && List.equal (fun (tag1, fields1) (tag2, fields2) ->
             Int.equal tag1 tag2
             && List.length fields1 = List.length fields2
             && List.for_all2 equal_value_kind fields1 fields2)
           non_consts1 non_consts2
  | (Pgenval | Pfloatval | Pboxedintval _ | Pintval | Pvariant _
      | Parrayval _), _ -> false

let equal_layout x y =
  match x, y with
  | Pvalue x, Pvalue y -> equal_value_kind x y
  | Ptop, Ptop -> true
  | Pbottom, Pbottom -> true
  | _, _ -> false

let compatible_layout x y =
  match x, y with
  | Pbottom, _
  | _, Pbottom -> true
  | Pvalue _, Pvalue _ -> true
  | Punboxed_float, Punboxed_float -> true
  | Punboxed_int bi1, Punboxed_int bi2 ->
      equal_boxed_integer bi1 bi2
  | Ptop, Ptop -> true
  | Ptop, _ | _, Ptop -> false
  | (Pvalue _ | Punboxed_float | Punboxed_int _), _ -> false

let must_be_value layout =
  match layout with
  | Pvalue v -> v
  | Pbottom ->
      (* Here, we want to get the [value_kind] corresponding to
         a [Pbottom] layout. Anything will do, we return [Pgenval]
         as a default. *)
      Pgenval
  | _ -> Misc.fatal_error "Layout is not a value"

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

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Available_inline (* [@inline available] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

type inlined_attribute =
  | Always_inlined (* [@inlined] or [@inlined always] *)
  | Never_inlined (* [@inlined never] *)
  | Hint_inlined (* [@inlined hint] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inlined (* no [@inlined] attribute *)

let equal_inline_attribute (x : inline_attribute) (y : inline_attribute) =
  match x, y with
  | Always_inline, Always_inline
  | Never_inline, Never_inline
  | Available_inline, Available_inline
  | Default_inline, Default_inline
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inline | Never_inline
    | Available_inline | Unroll _ | Default_inline), _ ->
    false

let equal_inlined_attribute (x : inlined_attribute) (y : inlined_attribute) =
  match x, y with
  | Always_inlined, Always_inlined
  | Never_inlined, Never_inlined
  | Hint_inlined, Hint_inlined
  | Default_inlined, Default_inlined
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inlined | Never_inlined
    | Hint_inlined | Unroll _ | Default_inlined), _ ->
    false

type probe_desc = { name: string; enabled_at_init: bool; }
type probe = probe_desc option

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

let equal_specialise_attribute x y =
  match x, y with
  | Always_specialise, Always_specialise
  | Never_specialise, Never_specialise
  | Default_specialise, Default_specialise ->
    true
  | (Always_specialise | Never_specialise | Default_specialise), _ ->
    false

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type property =
  | Zero_alloc

type poll_attribute =
  | Error_poll (* [@poll error] *)
  | Default_poll (* no [@poll] attribute *)

type check_attribute =
  | Default_check
  | Ignore_assert_all of property
  | Check of { property: property;
               strict: bool;
               assume: bool;
               loc: Location.t;
             }

type loop_attribute =
  | Always_loop (* [@loop] or [@loop always] *)
  | Never_loop (* [@loop never] *)
  | Default_loop (* no [@loop] attribute *)

type function_kind = Curried of {nlocal: int} | Tupled

type let_kind = Strict | Alias | StrictOpt

type meth_kind = Self | Public | Cached

let equal_meth_kind x y =
  match x, y with
  | Self, Self -> true
  | Public, Public -> true
  | Cached, Cached -> true
  | (Self | Public | Cached), _ -> false

type shared_code = (int * int) list

type static_label = int

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  check : check_attribute;
  poll: poll_attribute;
  loop: loop_attribute;
  is_a_functor: bool;
  stub: bool;
  tmc_candidate: bool;
}

type scoped_location = Debuginfo.Scoped_location.t

type lambda =
    Lvar of Ident.t
  | Lmutvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * layout * Ident.t * lambda * lambda
  | Lmutlet of layout * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * scoped_location
  | Lswitch of lambda * lambda_switch * scoped_location * layout
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * scoped_location * layout
  | Lstaticraise of static_label * lambda list
  | Lstaticcatch of lambda * (static_label * (Ident.t * layout) list) * lambda * layout
  | Ltrywith of lambda * Ident.t * lambda * layout
  | Lifthenelse of lambda * lambda * lambda * layout
  | Lsequence of lambda * lambda
  | Lwhile of lambda_while
  | Lfor of lambda_for
  | Lassign of Ident.t * lambda
  | Lsend of
      meth_kind * lambda * lambda * lambda list
      * region_close * alloc_mode * scoped_location * layout
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda
  | Lregion of lambda * layout
  | Lexclave of lambda

and lfunction =
  { kind: function_kind;
    params: (Ident.t * layout) list;
    return: layout;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc: scoped_location;
    mode: alloc_mode;
    region: bool; }

and lambda_while =
  { wh_cond : lambda;
    wh_body : lambda;
  }

and lambda_for =
  { for_id : Ident.t;
    for_from : lambda;
    for_to : lambda;
    for_dir : direction_flag;
    for_body : lambda;
  }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_result_layout : layout;
    ap_region_close : region_close;
    ap_mode : alloc_mode;
    ap_loc : scoped_location;
    ap_tailcall : tailcall_attribute;
    ap_inlined : inlined_attribute;
    ap_specialised : specialise_attribute;
    ap_probe : probe;
  }

and lambda_switch =
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_failaction : lambda option}

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

type program =
  { compilation_unit : Compilation_unit.t;
    main_module_block_size : int;
    required_globals : Compilation_unit.Set.t;
    code : lambda }

let const_int n = Const_base (Const_int n)

let const_unit = const_int 0

let max_arity () =
  if !Clflags.native_code then 126 else max_int
  (* 126 = 127 (the maximal number of parameters supported in C--)
           - 1 (the hidden parameter containing the environment) *)

let lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~region =
  assert (List.length params <= max_arity ());
  (* A curried function type with n parameters has n arrows. Of these,
     the first [n-nlocal] have return mode Heap, while the remainder
     have return mode Local, except possibly the final one.

     That is, after supplying the first [n-nlocal] arguments, further
     partial applications must be locally allocated.

     A curried function with no local parameters or returns has kind
     [Curried {nlocal=0}]. *)
  begin match mode, kind with
  | Alloc_heap, Tupled -> ()
  | Alloc_local, Tupled ->
     (* Tupled optimisation does not apply to local functions *)
     assert false
  | mode, Curried {nlocal} ->
     let nparams = List.length params in
     assert (0 <= nlocal);
     assert (nlocal <= nparams);
     if not region then assert (nlocal >= 1);
     if is_local_mode mode then assert (nlocal = nparams)
  end;
  Lfunction { kind; params; return; body; attr; loc; mode; region }

let lambda_unit = Lconst const_unit

let layout_unit = Pvalue Pintval
let layout_int = Pvalue Pintval
let layout_array kind = Pvalue (Parrayval kind)
let layout_block = Pvalue Pgenval
let layout_list =
  Pvalue (Pvariant { consts = [0] ; non_consts = [0, [Pgenval; Pgenval]] })
let layout_field = Pvalue Pgenval
let layout_exception = Pvalue Pgenval
let layout_function = Pvalue Pgenval
let layout_object = Pvalue Pgenval
let layout_class = Pvalue Pgenval
let layout_module = Pvalue Pgenval
let layout_module_field = Pvalue Pgenval
let layout_functor = Pvalue Pgenval
let layout_float = Pvalue Pfloatval
let layout_string = Pvalue Pgenval
let layout_boxedint bi = Pvalue (Pboxedintval bi)
let layout_lazy = Pvalue Pgenval
let layout_lazy_contents = Pvalue Pgenval
let layout_any_value = Pvalue Pgenval
let layout_letrec = layout_any_value

(* CR ncourant: use [Ptop] or remove this as soon as possible. *)
let layout_top = layout_any_value
let layout_bottom = Pbottom

let default_function_attribute = {
  inline = Default_inline;
  specialise = Default_specialise;
  local = Default_local;
  check = Default_check ;
  poll = Default_poll;
  loop = Default_loop;
  is_a_functor = false;
  stub = false;
  tmc_candidate = false;
}

let default_stub_attribute =
  { default_function_attribute with stub = true; check = Ignore_assert_all Zero_alloc }

(* Build sharing keys *)
(*
   Those keys are later compared with Stdlib.compare.
   For that reason, they should not include cycles.
*)

let max_raw = 32

let make_key e =
  let exception Not_simple in
  let count = ref 0   (* Used for controlling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e with
    | Lvar id
    | Lmutvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply ap ->
        Lapply {ap with ap_func = tr_rec env ap.ap_func;
                        ap_args = tr_recs env ap.ap_args;
                        ap_loc = Loc_unknown}
    | Llet (Alias,_k,x,ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt),_k,x,ex,Lvar v) when Ident.same v x ->
        tr_rec env ex
    | Llet (str,k,x,ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        Llet (str,k,y,ex,tr_rec (Ident.add x (Lvar y) env) e)
    | Lmutlet (k,x,ex,e) ->
        let ex = tr_rec env ex in
        let y = make_key x in
        Lmutlet (k,y,ex,tr_rec (Ident.add x (Lmutvar y) env) e)
    | Lprim (p,es,_) ->
        Lprim (p,tr_recs env es, Loc_unknown)
    | Lswitch (e,sw,loc,kind) ->
        Lswitch (tr_rec env e,tr_sw env sw,loc,kind)
    | Lstringswitch (e,sw,d,_,kind) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s,e) -> s,tr_rec env e) sw,
           tr_opt env d,
          Loc_unknown,kind)
    | Lstaticraise (i,es) ->
        Lstaticraise (i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2, kind) ->
        Lstaticcatch (tr_rec env e1,xs,tr_rec env e2, kind)
    | Ltrywith (e1,x,e2,kind) ->
        Ltrywith (tr_rec env e1,x,tr_rec env e2,kind)
    | Lifthenelse (cond,ifso,ifnot,kind) ->
        Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot,kind)
    | Lsequence (e1,e2) ->
        Lsequence (tr_rec env e1,tr_rec env e2)
    | Lassign (x,e) ->
        Lassign (x,tr_rec env e)
    | Lsend (m,e1,e2,es,pos,mo,_loc,layout) ->
        Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,pos,mo,Loc_unknown,layout)
    | Lifused (id,e) -> Lifused (id,tr_rec env e)
    | Lregion (e,layout) -> Lregion (tr_rec env e,layout)
    | Lexclave e -> Lexclave (tr_rec env e)
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg layout fn =
  match arg with
    Lvar id -> fn id
  | _ ->
      let id = Ident.create_local "let" in
      Llet(strict, layout, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar _ as arg, _) :: rem ->
      name_list (arg :: names) rem
  | (arg, layout) :: rem ->
      let id = Ident.create_local "let" in
      Llet(Strict, layout, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let shallow_iter ~tail ~non_tail:f = function
    Lvar _
  | Lmutvar _
  | Lconst _ -> ()
  | Lapply{ap_func = fn; ap_args = args} ->
      f fn; List.iter f args
  | Lfunction{body} ->
      f body
  | Llet(_, _k, _id, arg, body)
  | Lmutlet(_k, _id, arg, body) ->
      f arg; tail body
  | Lletrec(decl, body) ->
      tail body;
      List.iter (fun (_id, exp) -> f exp) decl
  | Lprim (Psequand, [l1; l2], _)
  | Lprim (Psequor, [l1; l2], _) ->
      f l1;
      tail l2
  | Lprim(_p, args, _loc) ->
      List.iter f args
  | Lswitch(arg, sw,_,_) ->
      f arg;
      List.iter (fun (_key, case) -> tail case) sw.sw_consts;
      List.iter (fun (_key, case) -> tail case) sw.sw_blocks;
      iter_opt tail sw.sw_failaction
  | Lstringswitch (arg,cases,default,_,_) ->
      f arg ;
      List.iter (fun (_,act) -> tail act) cases ;
      iter_opt tail default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, _, e2, _kind) ->
      tail e1; tail e2
  | Ltrywith(e1, _, e2,_) ->
      f e1; tail e2
  | Lifthenelse(e1, e2, e3,_) ->
      f e1; tail e2; tail e3
  | Lsequence(e1, e2) ->
      f e1; tail e2
  | Lwhile {wh_cond; wh_body} ->
      f wh_cond; f wh_body
  | Lfor {for_from; for_to; for_body} ->
      f for_from; f for_to; f for_body
  | Lassign(_, e) ->
      f e
  | Lsend (_k, met, obj, args, _, _, _, _) ->
      List.iter f (met::obj::args)
  | Levent (e, _evt) ->
      tail e
  | Lifused (_v, e) ->
      tail e
  | Lregion (e, _) ->
      f e
  | Lexclave e ->
      tail e

let iter_head_constructor f l =
  shallow_iter ~tail:f ~non_tail:f l

let rec free_variables = function
  | Lvar id
  | Lmutvar id -> Ident.Set.singleton id
  | Lconst _ -> Ident.Set.empty
  | Lapply{ap_func = fn; ap_args = args} ->
      free_variables_list (free_variables fn) args
  | Lfunction{body; params} ->
      Ident.Set.diff (free_variables body)
        (Ident.Set.of_list (List.map fst params))
  | Llet(_, _k, id, arg, body)
  | Lmutlet(_k, id, arg, body) ->
      Ident.Set.union
        (free_variables arg)
        (Ident.Set.remove id (free_variables body))
  | Lletrec(decl, body) ->
      let set = free_variables_list (free_variables body) (List.map snd decl) in
      Ident.Set.diff set (Ident.Set.of_list (List.map fst decl))
  | Lprim(_p, args, _loc) ->
      free_variables_list Ident.Set.empty args
  | Lswitch(arg, sw,_,_) ->
      let set =
        free_variables_list
          (free_variables_list (free_variables arg)
             (List.map snd sw.sw_consts))
          (List.map snd sw.sw_blocks)
      in
      begin match sw.sw_failaction with
      | None -> set
      | Some failaction -> Ident.Set.union set (free_variables failaction)
      end
  | Lstringswitch (arg,cases,default,_,_) ->
      let set =
        free_variables_list (free_variables arg)
          (List.map snd cases)
      in
      begin match default with
      | None -> set
      | Some default -> Ident.Set.union set (free_variables default)
      end
  | Lstaticraise (_,args) ->
      free_variables_list Ident.Set.empty args
  | Lstaticcatch(body, (_, params), handler, _kind) ->
      Ident.Set.union
        (Ident.Set.diff
           (free_variables handler)
           (Ident.Set.of_list (List.map fst params)))
        (free_variables body)
  | Ltrywith(body, param, handler, _) ->
      Ident.Set.union
        (Ident.Set.remove
           param
           (free_variables handler))
        (free_variables body)
  | Lifthenelse(e1, e2, e3, _) ->
      Ident.Set.union
        (Ident.Set.union (free_variables e1) (free_variables e2))
        (free_variables e3)
  | Lsequence(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lwhile {wh_cond; wh_body} ->
      Ident.Set.union (free_variables wh_cond) (free_variables wh_body)
  | Lfor {for_id; for_from; for_to; for_body} ->
      Ident.Set.union (free_variables for_from)
        (Ident.Set.union (free_variables for_to)
           (Ident.Set.remove for_id (free_variables for_body)))
  | Lassign(id, e) ->
      Ident.Set.add id (free_variables e)
  | Lsend (_k, met, obj, args, _, _, _, _) ->
      free_variables_list
        (Ident.Set.union (free_variables met) (free_variables obj))
        args
  | Levent (lam, _evt) ->
      free_variables lam
  | Lifused (_v, e) ->
      (* Shouldn't v be considered a free variable ? *)
      free_variables e
  | Lregion (e, _) ->
      free_variables e
  | Lexclave e ->
      free_variables e

and free_variables_list set exprs =
  List.fold_left (fun set expr -> Ident.Set.union (free_variables expr) set)
    set exprs

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0,[])

let rec is_guarded = function
  | Lifthenelse(_cond, _body, Lstaticraise (0,[]),_) -> true
  | Llet(_str, _k, _id, _lam, body) -> is_guarded body
  | Levent(lam, _ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0,[]), kind) ->
      Lifthenelse (cond, body, patch, kind)
  | Llet(str, k, id, lam, body) ->
      Llet (str, k, id, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_address loc = function
  | Env.Aunit cu -> Lprim(Pgetglobal cu, [], loc)
  | Env.Alocal id ->
      if Ident.is_predef id
      then Lprim (Pgetpredef id, [], loc)
      else Lvar id
  | Env.Adot(addr, pos) ->
      Lprim(Pfield (pos, Reads_agree), [transl_address loc addr], loc)

let transl_path find loc env path =
  match find path env with
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))
  | addr -> transl_address loc addr

(* Translation of identifiers *)

let transl_module_path loc env path =
  transl_path Env.find_module_address loc env path

let transl_value_path loc env path =
  transl_path Env.find_value_address loc env path

let transl_extension_path loc env path =
  transl_path Env.find_constructor_address loc env path

let transl_class_path loc env path =
  transl_path Env.find_class_address loc env path

let transl_prim mod_name name =
  let pers = Ident.create_persistent mod_name in
  let env = Env.add_persistent_structure pers Env.empty in
  let lid = Longident.Ldot (Longident.Lident mod_name, name) in
  match Env.find_value_by_name lid env with
  | path, _ -> transl_value_path Loc_unknown env path
  | exception Not_found ->
      fatal_error ("Primitive " ^ name ^ " not found.")

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst update_env ?(freshen_bound_variables = false) s input_lam =
  (* [s] contains a partial substitution for the free variables of the
     input term [input_lam].

     During our traversal of the term we maintain a second environment
     [l] with all the bound variables of [input_lam] in the current
     scope, mapped to either themselves or freshened versions of
     themselves when [freshen_bound_variables] is set. *)
  let bind id l =
    let id' = if not freshen_bound_variables then id else Ident.rename id in
    id', Ident.Map.add id id' l
  in
  let bind_many ids l =
    List.fold_right (fun (id, rhs) (ids', l) ->
        let id', l = bind id l in
        ((id', rhs) :: ids' , l)
      ) ids ([], l)
  in
  let rec subst s l lam =
    match lam with
    | Lvar id as lam ->
        begin match Ident.Map.find id l with
          | id' -> Lvar id'
          | exception Not_found ->
             (* note: as this point we know [id] is not a bound
                variable of the input term, otherwise it would belong
                to [l]; it is a free variable of the input term. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lmutvar id as lam ->
       begin match Ident.Map.find id l with
          | id' -> Lmutvar id'
          | exception Not_found ->
             (* Note: a mutable [id] should not appear in [s].
                Keeping the behavior of Lvar case for now. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lconst _ as l -> l
    | Lapply ap ->
        Lapply{ap with ap_func = subst s l ap.ap_func;
                      ap_args = subst_list s l ap.ap_args}
    | Lfunction lf ->
        let params, l' = bind_many lf.params l in
        Lfunction {lf with params; body = subst s l' lf.body}
    | Llet(str, k, id, arg, body) ->
        let id, l' = bind id l in
        Llet(str, k, id, subst s l arg, subst s l' body)
    | Lmutlet(k, id, arg, body) ->
        let id, l' = bind id l in
        Lmutlet(k, id, subst s l arg, subst s l' body)
    | Lletrec(decl, body) ->
        let decl, l' = bind_many decl l in
        Lletrec(List.map (subst_decl s l') decl, subst s l' body)
    | Lprim(p, args, loc) -> Lprim(p, subst_list s l args, loc)
    | Lswitch(arg, sw, loc,kind) ->
        Lswitch(subst s l arg,
                {sw with sw_consts = List.map (subst_case s l) sw.sw_consts;
                        sw_blocks = List.map (subst_case s l) sw.sw_blocks;
                        sw_failaction = subst_opt s l sw.sw_failaction; },
                loc,kind)
    | Lstringswitch (arg,cases,default,loc,kind) ->
        Lstringswitch
          (subst s l arg,
           List.map (subst_strcase s l) cases,
           subst_opt s l default,
           loc,kind)
    | Lstaticraise (i,args) ->  Lstaticraise (i, subst_list s l args)
    | Lstaticcatch(body, (id, params), handler, kind) ->
        let params, l' = bind_many params l in
        Lstaticcatch(subst s l body, (id, params),
                     subst s l' handler, kind)
    | Ltrywith(body, exn, handler,kind) ->
        let exn, l' = bind exn l in
        Ltrywith(subst s l body, exn, subst s l' handler,kind)
    | Lifthenelse(e1, e2, e3,kind) ->
        Lifthenelse(subst s l e1, subst s l e2, subst s l e3,kind)
    | Lsequence(e1, e2) -> Lsequence(subst s l e1, subst s l e2)
    | Lwhile lw -> Lwhile { wh_cond = subst s l lw.wh_cond;
                            wh_body = subst s l lw.wh_body}
    | Lfor lf ->
        let for_id, l' = bind lf.for_id l in
        Lfor {lf with for_id;
                      for_from = subst s l lf.for_from;
                      for_to = subst s l lf.for_to;
                      for_body = subst s l' lf.for_body}
    | Lassign(id, e) ->
        assert (not (Ident.Map.mem id s));
        let id = try Ident.Map.find id l with Not_found -> id in
        Lassign(id, subst s l e)
    | Lsend (k, met, obj, args, pos, mode, loc, layout) ->
        Lsend (k, subst s l met, subst s l obj, subst_list s l args,
               pos, mode, loc, layout)
    | Levent (lam, evt) ->
        let old_env = evt.lev_env in
        let env_updates =
          let find_in_old id = Env.find_value (Path.Pident id) old_env in
          let rebind id id' new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | vd -> Env.add_value_lazy id' vd new_env
          in
          let update_free id new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | vd -> update_env id vd new_env
          in
          Ident.Map.merge (fun id bound free ->
            match bound, free with
            | Some id', _ ->
                if Ident.equal id id' then None else Some (rebind id id')
            | None, Some _ -> Some (update_free id)
            | None, None -> None
          ) l s
        in
        let new_env =
          Ident.Map.fold (fun _id update env -> update env) env_updates old_env
        in
        Levent (subst s l lam, { evt with lev_env = new_env })
    | Lifused (id, e) ->
        let id = try Ident.Map.find id l with Not_found -> id in
        Lifused (id, subst s l e)
    | Lregion (e, layout) ->
        Lregion (subst s l e, layout)
    | Lexclave e ->
        Lexclave (subst s l e)
  and subst_list s l li = List.map (subst s l) li
  and subst_decl s l (id, exp) = (id, subst s l exp)
  and subst_case s l (key, case) = (key, subst s l case)
  and subst_strcase s l (key, case) = (key, subst s l case)
  and subst_opt s l = function
    | None -> None
    | Some e -> Some (subst s l e)
  in
  subst s Ident.Map.empty input_lam

let rename idmap lam =
  let update_env oldid vd env =
    let newid = Ident.Map.find oldid idmap in
    Env.add_value_lazy newid vd env
  in
  let s = Ident.Map.map (fun new_id -> Lvar new_id) idmap in
  subst update_env s lam

let duplicate lam =
  subst
    (fun _ _ env -> env)
    ~freshen_bound_variables:true
    Ident.Map.empty
    lam

let shallow_map ~tail ~non_tail:f = function
  | Lvar _
  | Lmutvar _
  | Lconst _ as lam -> lam
  | Lapply { ap_func; ap_args; ap_result_layout; ap_region_close; ap_mode; ap_loc; ap_tailcall;
             ap_inlined; ap_specialised; ap_probe } ->
      Lapply {
        ap_func = f ap_func;
        ap_args = List.map f ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
        ap_probe;
      }
  | Lfunction { kind; params; return; body; attr; loc; mode; region } ->
      Lfunction { kind; params; return; body = f body; attr; loc;
                  mode; region }
  | Llet (str, layout, v, e1, e2) ->
      Llet (str, layout, v, f e1, tail e2)
  | Lmutlet (layout, v, e1, e2) ->
      Lmutlet (layout, v, f e1, tail e2)
  | Lletrec (idel, e2) ->
      Lletrec (List.map (fun (v, e) -> (v, f e)) idel, tail e2)
  | Lprim (Psequand as p, [l1; l2], loc)
  | Lprim (Psequor as p, [l1; l2], loc) ->
      Lprim(p, [f l1; tail l2], loc)
  | Lprim (p, el, loc) ->
      Lprim (p, List.map f el, loc)
  | Lswitch (e, sw, loc, layout) ->
      Lswitch (f e,
               { sw_numconsts = sw.sw_numconsts;
                 sw_consts = List.map (fun (n, e) -> (n, tail e)) sw.sw_consts;
                 sw_numblocks = sw.sw_numblocks;
                 sw_blocks = List.map (fun (n, e) -> (n, tail e)) sw.sw_blocks;
                 sw_failaction = Option.map tail sw.sw_failaction;
               },
               loc, layout)
  | Lstringswitch (e, sw, default, loc, layout) ->
      Lstringswitch (
        f e,
        List.map (fun (s, e) -> (s, tail e)) sw,
        Option.map tail default,
        loc, layout)
  | Lstaticraise (i, args) ->
      Lstaticraise (i, List.map f args)
  | Lstaticcatch (body, id, handler, layout) ->
      Lstaticcatch (tail body, id, tail handler, layout)
  | Ltrywith (e1, v, e2, layout) ->
      Ltrywith (f e1, v, tail e2, layout)
  | Lifthenelse (e1, e2, e3, layout) ->
      Lifthenelse (f e1, tail e2, tail e3, layout)
  | Lsequence (e1, e2) ->
      Lsequence (f e1, tail e2)
  | Lwhile lw ->
      Lwhile { wh_cond = f lw.wh_cond;
               wh_body = f lw.wh_body }
  | Lfor lf ->
      Lfor { lf with for_from = f lf.for_from;
                     for_to = f lf.for_to;
                     for_body = f lf.for_body }
  | Lassign (v, e) ->
      Lassign (v, f e)
  | Lsend (k, m, o, el, pos, mode, loc, layout) ->
      Lsend (k, f m, f o, List.map f el, pos, mode, loc, layout)
  | Levent (l, ev) ->
      Levent (tail l, ev)
  | Lifused (v, e) ->
      Lifused (v, tail e)
  | Lregion (e, layout) ->
      Lregion (f e, layout)
  | Lexclave e ->
      Lexclave (tail e)

let map f =
  let rec g lam = f (shallow_map ~tail:g ~non_tail:g lam) in
  g

(* To let-bind expressions to variables *)

let bind_with_layout str (var, layout) exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, layout, var, exp, body)

let negate_integer_comparison = function
  | Ceq -> Cne
  | Cne -> Ceq
  | Clt -> Cge
  | Cle -> Cgt
  | Cgt -> Cle
  | Cge -> Clt

let swap_integer_comparison = function
  | Ceq -> Ceq
  | Cne -> Cne
  | Clt -> Cgt
  | Cle -> Cge
  | Cgt -> Clt
  | Cge -> Cle

let negate_float_comparison = function
  | CFeq -> CFneq
  | CFneq -> CFeq
  | CFlt -> CFnlt
  | CFnlt -> CFlt
  | CFgt -> CFngt
  | CFngt -> CFgt
  | CFle -> CFnle
  | CFnle -> CFle
  | CFge -> CFnge
  | CFnge -> CFge

let swap_float_comparison = function
  | CFeq -> CFeq
  | CFneq -> CFneq
  | CFlt -> CFgt
  | CFnlt -> CFngt
  | CFle -> CFge
  | CFnle -> CFnge
  | CFgt -> CFlt
  | CFngt -> CFnlt
  | CFge -> CFle
  | CFnge -> CFnle

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let merge_inline_attributes attr1 attr2 =
  match attr1, attr2 with
  | Default_inline, _ -> Some attr2
  | _, Default_inline -> Some attr1
  | _, _ ->
    if attr1 = attr2 then Some attr1
    else None

let max_arity () =
  if !Clflags.native_code then 126 else max_int
  (* 126 = 127 (the maximal number of parameters supported in C--)
           - 1 (the hidden parameter containing the environment) *)

let find_exact_application kind ~arity args =
  match kind with
  | Curried _ ->
      if arity <> List.length args
      then None
      else Some args
  | Tupled ->
      begin match args with
      | [Lprim(Pmakeblock _, tupled_args, _)] ->
          if arity <> List.length tupled_args
          then None
          else Some tupled_args
      | [Lconst(Const_block (_, const_args))] ->
          if arity <> List.length const_args
          then None
          else Some (List.map (fun cst -> Lconst cst) const_args)
      | _ -> None
      end

let reset () =
  raise_count := 0

let mod_field ?(read_semantics=Reads_agree) pos =
  Pfield (pos, read_semantics)

let mod_setfield pos =
  Psetfield (pos, Pointer, Root_initialization)

let primitive_may_allocate : primitive -> alloc_mode option = function
  | Pbytes_to_string | Pbytes_of_string
  | Parray_to_iarray | Parray_of_iarray
  | Pignore -> None
  | Pgetglobal _ | Psetglobal _ | Pgetpredef _ -> None
  | Pmakeblock (_, _, _, m) -> Some m
  | Pmakefloatblock (_, m) -> Some m
  | Pfield _ | Pfield_computed _ | Psetfield _ | Psetfield_computed _ -> None
  | Pfloatfield (_, _, m) -> Some m
  | Psetfloatfield _ -> None
  | Pduprecord _ -> Some alloc_heap
  | Pccall p ->
     if not p.prim_alloc then None
     else begin match p.prim_native_repr_res with
       | (Prim_local|Prim_poly), _ -> Some alloc_local
       | Prim_global, _ -> Some alloc_heap
     end
  | Praise _ -> None
  | Psequor | Psequand | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Poffsetint _
  | Poffsetref _ -> None
  | Pintoffloat -> None
  | Pfloatofint m -> Some m
  | Pnegfloat m | Pabsfloat m
  | Paddfloat m | Psubfloat m
  | Pmulfloat m | Pdivfloat m -> Some m
  | Pfloatcomp _ -> None
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets -> None
  | Pmakearray (_, _, m) -> Some m
  | Pduparray _ -> Some alloc_heap
  | Parraylength _ -> None
  | Parraysetu _ | Parraysets _
  | Parrayrefu (Paddrarray_ref | Pintarray_ref)
  | Parrayrefs (Paddrarray_ref | Pintarray_ref) -> None
  | Parrayrefu (Pgenarray_ref m | Pfloatarray_ref m)
  | Parrayrefs (Pgenarray_ref m | Pfloatarray_ref m) -> Some m
  | Pisint _ | Pisout -> None
  | Pintofbint _ -> None
  | Pbintofint (_,m)
  | Pcvtbint (_,_,m)
  | Pnegbint (_, m)
  | Paddbint (_, m)
  | Psubbint (_, m)
  | Pmulbint (_, m)
  | Pdivbint {mode=m}
  | Pmodbint {mode=m}
  | Pandbint (_, m)
  | Porbint (_, m)
  | Pxorbint (_, m)
  | Plslbint (_, m)
  | Plsrbint (_, m)
  | Pasrbint (_, m) -> Some m
  | Pbintcomp _ -> None
  | Pbigarrayset _ | Pbigarraydim _ -> None
  | Pbigarrayref (_, _, _, _) ->
     (* Boxes arising from Bigarray access are always Alloc_heap *)
     Some alloc_heap
  | Pstring_load_16 _ | Pbytes_load_16 _ -> None
  | Pstring_load_32 (_, m) | Pbytes_load_32 (_, m)
  | Pstring_load_64 (_, m) | Pbytes_load_64 (_, m) -> Some m
  | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_64 _ -> None
  | Pbigstring_load_16 _ -> None
  | Pbigstring_load_32 (_,m) | Pbigstring_load_64 (_,m) -> Some m
  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _ -> None
  | Pctconst _ -> None
  | Pbswap16 -> None
  | Pbbswap (_, m) -> Some m
  | Pint_as_pointer -> None
  | Popaque _ -> None
  | Pprobe_is_enabled _ -> None
  | Pobj_dup -> Some alloc_heap
  | Pobj_magic _ -> None
  | Punbox_float | Punbox_int _ -> None
  | Pbox_float m | Pbox_int (_, m) -> Some m

let constant_layout = function
  | Const_int _ | Const_char _ -> Pvalue Pintval
  | Const_string _ -> Pvalue Pgenval
  | Const_int32 _ -> Pvalue (Pboxedintval Pint32)
  | Const_int64 _ -> Pvalue (Pboxedintval Pint64)
  | Const_nativeint _ -> Pvalue (Pboxedintval Pnativeint)
  | Const_float _ -> Pvalue Pfloatval

let structured_constant_layout = function
  | Const_base const -> constant_layout const
  | Const_block _ | Const_immstring _ -> Pvalue Pgenval
  | Const_float_array _ | Const_float_block _ -> Pvalue (Parrayval Pfloatarray)

let primitive_result_layout (p : primitive) =
  match p with
  | Popaque layout | Pobj_magic layout -> layout
  | Pbytes_to_string | Pbytes_of_string -> layout_string
  | Pignore | Psetfield _ | Psetfield_computed _ | Psetfloatfield _ | Poffsetref _
  | Pbytessetu | Pbytessets | Parraysetu _ | Parraysets _ | Pbigarrayset _
  | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_64 _
  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _
    -> layout_unit
  | Pgetglobal _ | Psetglobal _ | Pgetpredef _ -> layout_module_field
  | Pmakeblock _ | Pmakefloatblock _ | Pmakearray _ | Pduprecord _
  | Pduparray _ | Pbigarraydim _ | Pobj_dup -> layout_block
  | Pfield _ | Pfield_computed _ -> layout_field
  | Pfloatfield _ | Pfloatofint _ | Pnegfloat _ | Pabsfloat _
  | Paddfloat _ | Psubfloat _ | Pmulfloat _ | Pdivfloat _
  | Pbox_float _ -> layout_float
  | Punbox_float -> Punboxed_float
  | Pccall { prim_native_repr_res = _, Untagged_int; _} -> layout_int
  | Pccall { prim_native_repr_res = _, Unboxed_float; _} -> layout_float
  | Pccall { prim_native_repr_res = _, Same_as_ocaml_repr; _} ->
      layout_any_value
  | Pccall { prim_native_repr_res = _, Unboxed_integer bi; _} ->
      layout_boxedint bi
  | Praise _ -> layout_bottom
  | Psequor | Psequand | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Poffsetint _ | Pintoffloat | Pfloatcomp _
  | Pstringlength | Pstringrefu | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytesrefs
  | Parraylength _ | Pisint _ | Pisout | Pintofbint _
  | Pbintcomp _
  | Pstring_load_16 _ | Pbytes_load_16 _ | Pbigstring_load_16 _
  | Pprobe_is_enabled _ | Pbswap16
    -> layout_int
  | Parrayrefu array_ref_kind | Parrayrefs array_ref_kind ->
      (match array_ref_kind with
       | Pintarray_ref -> layout_int
       | Pfloatarray_ref _ -> layout_float
       | Pgenarray_ref _ | Paddrarray_ref -> layout_field)
  | Pbintofint (bi, _) | Pcvtbint (_,bi,_)
  | Pnegbint (bi, _) | Paddbint (bi, _) | Psubbint (bi, _)
  | Pmulbint (bi, _) | Pdivbint {size = bi} | Pmodbint {size = bi}
  | Pandbint (bi, _) | Porbint (bi, _) | Pxorbint (bi, _)
  | Plslbint (bi, _) | Plsrbint (bi, _) | Pasrbint (bi, _)
  | Pbbswap (bi, _) | Pbox_int (bi, _) ->
      layout_boxedint bi
  | Punbox_int bi -> Punboxed_int bi
  | Pstring_load_32 _ | Pbytes_load_32 _ | Pbigstring_load_32 _ ->
      layout_boxedint Pint32
  | Pstring_load_64 _ | Pbytes_load_64 _ | Pbigstring_load_64 _ ->
      layout_boxedint Pint64
  | Pbigarrayref (_, _, kind, _) ->
      begin match kind with
      | Pbigarray_unknown -> layout_any_value
      | Pbigarray_float32 | Pbigarray_float64 -> layout_float
      | Pbigarray_sint8 | Pbigarray_uint8
      | Pbigarray_sint16 | Pbigarray_uint16
      | Pbigarray_caml_int -> layout_int
      | Pbigarray_int32 -> layout_boxedint Pint32
      | Pbigarray_int64 -> layout_boxedint Pint64
      | Pbigarray_native_int -> layout_boxedint Pnativeint
      | Pbigarray_complex32 | Pbigarray_complex64 ->
          layout_block
      end
  | Pctconst (
      Big_endian | Word_size | Int_size | Max_wosize
      | Ostype_unix | Ostype_cygwin | Ostype_win32 | Backend_type
    ) ->
      (* Compile-time constants only ever return ints for now,
         enumerate them all to be sure to modify this if it becomes wrong. *)
      layout_int
  | Pint_as_pointer ->
      (* CR ncourant: use an unboxed int64 here when it exists *)
      layout_any_value
  | (Parray_to_iarray | Parray_of_iarray) -> layout_any_value

let compute_expr_layout free_vars_kind lam =
  let rec compute_expr_layout kinds = function
    | Lvar id | Lmutvar id -> begin
        try Ident.Map.find id kinds
        with Not_found ->
        match free_vars_kind id with
        | Some kind -> kind
        | None ->
            Misc.fatal_errorf "Unbound layout for variable %a" Ident.print id
      end
    | Lconst cst -> structured_constant_layout cst
    | Lfunction _ -> layout_function
    | Lapply { ap_result_layout; _ } -> ap_result_layout
    | Lsend (_, _, _, _, _, _, _, layout) -> layout
    | Llet(_, kind, id, _, body) | Lmutlet(kind, id, _, body) ->
      compute_expr_layout (Ident.Map.add id kind kinds) body
    | Lletrec(defs, body) ->
      let kinds =
        List.fold_left (fun kinds (id, _) -> Ident.Map.add id layout_letrec kinds)
          kinds defs
      in
      compute_expr_layout kinds body
    | Lprim(p, _, _) ->
      primitive_result_layout p
    | Lswitch(_, _, _, kind) | Lstringswitch(_, _, _, _, kind)
    | Lstaticcatch(_, _, _, kind) | Ltrywith(_, _, _, kind)
    | Lifthenelse(_, _, _, kind) | Lregion (_, kind) ->
      kind
    | Lstaticraise (_, _) ->
      layout_bottom
    | Lsequence(_, body) | Levent(body, _) -> compute_expr_layout kinds body
    | Lwhile _ | Lfor _ | Lassign _ -> layout_unit
    | Lifused _ ->
        assert false
    | Lexclave e -> compute_expr_layout kinds e
  in
  compute_expr_layout Ident.Map.empty lam

let array_ref_kind mode = function
  | Pgenarray -> Pgenarray_ref mode
  | Paddrarray -> Paddrarray_ref
  | Pintarray -> Pintarray_ref
  | Pfloatarray -> Pfloatarray_ref mode

let array_set_kind mode = function
  | Pgenarray -> Pgenarray_set mode
  | Paddrarray -> Paddrarray_set mode
  | Pintarray -> Pintarray_set
  | Pfloatarray -> Pfloatarray_set
