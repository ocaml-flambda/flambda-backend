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

type machtype_component =
  | Val
  | Addr
  | Int
  | Float

type machtype = machtype_component array

let typ_void = ([||] : machtype_component array)
let typ_val = [|Val|]
let typ_addr = [|Addr|]
let typ_int = [|Int|]
let typ_float = [|Float|]

(** [machtype_component]s are partially ordered as follows:

      Addr     Float
       ^
       |
      Val
       ^
       |
      Int

  In particular, [Addr] must be above [Val], to ensure that if there is
  a join point between a code path yielding [Addr] and one yielding [Val]
  then the result is treated as a derived pointer into the heap (i.e. [Addr]).
  (Such a result may not be live across any call site or a fatal compiler
  error will result.)
*)

let lub_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> Int
  | Int, Val -> Val
  | Int, Addr -> Addr
  | Val, Int -> Val
  | Val, Val -> Val
  | Val, Addr -> Addr
  | Addr, Int -> Addr
  | Addr, Addr -> Addr
  | Addr, Val -> Addr
  | Float, Float -> Float
  | (Int | Addr | Val), Float
  | Float, (Int | Addr | Val) ->
    (* Float unboxing code must be sure to avoid this case. *)
    assert false

let ge_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> true
  | Int, Addr -> false
  | Int, Val -> false
  | Val, Int -> true
  | Val, Val -> true
  | Val, Addr -> false
  | Addr, Int -> true
  | Addr, Addr -> true
  | Addr, Val -> true
  | Float, Float -> true
  | (Int | Addr | Val), Float
  | Float, (Int | Addr | Val) ->
    assert false

type exttype =
  | XInt
  | XInt32
  | XInt64
  | XFloat

let machtype_of_exttype = function
  | XInt -> typ_int
  | XInt32 -> typ_int
  | XInt64 -> if Arch.size_int = 4 then [|Int;Int|] else typ_int
  | XFloat -> typ_float

let machtype_of_exttype_list xtl =
  Array.concat (List.map machtype_of_exttype xtl)

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

let negate_integer_comparison = Lambda.negate_integer_comparison

let swap_integer_comparison = Lambda.swap_integer_comparison

(* With floats [not (x < y)] is not the same as [x >= y] due to NaNs,
   so we provide additional comparisons to represent the negations.*)
type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

let negate_float_comparison = Lambda.negate_float_comparison

let swap_float_comparison = Lambda.swap_float_comparison
type label = int

type exit_label =
  | Return_lbl
  | Lbl of label

let init_label = 99

let label_counter = ref init_label

let set_label l =
  if (l < !label_counter) then begin
    Misc.fatal_errorf "Cannot set label counter to %d, it must be >= %d"
      l !label_counter ()
  end;
  label_counter := l

let cur_label () = !label_counter

let new_label() = incr label_counter; !label_counter

type rec_flag = Nonrecursive | Recursive

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type effects = No_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type phantom_defining_expr =
  | Cphantom_const_int of Targetint.t
  | Cphantom_const_symbol of string
  | Cphantom_var of Backend_var.t
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }

type trywith_shared_label = int

type trap_action =
  | Push of trywith_shared_label
  | Pop

type trywith_kind =
  | Regular
  | Delayed of trywith_shared_label

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type initialization_or_assignment =
  | Initialization
  | Assignment

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int
  | Word_val
  | Single
  | Double

and operation =
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
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc of Lambda.alloc_mode
  | Cstore of memory_chunk * initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi of { signed: bool } | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Cbswap of { bitwidth: bswap_bitwidth; }
  | Cclz of { arg_is_non_zero: bool; }
  | Cctz of { arg_is_non_zero: bool; }
  | Cpopcnt
  | Cprefetch of { is_write: bool; locality: prefetch_temporal_locality_hint; }
  | Ccmpi of integer_comparison
  | Caddv | Cadda
  | Ccmpa of integer_comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of Lambda.raise_kind
  | Ccheckbound
  | Cprobe of { name: string; handler_code_sym: string; }
  | Cprobe_is_enabled of { name: string }
  | Copaque
  | Cbeginregion | Cendregion

type value_kind =
  | Vval of Lambda.value_kind (* Valid OCaml values *)
  | Vint (* Untagged integers and off-heap pointers *)
  | Vaddr (* Derived pointers *)
  | Vfloat (* Unboxed floating-point numbers *)

type expression =
    Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_symbol of string * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Clet_mut of Backend_var.With_provenance.t * machtype
                * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t * value_kind
  | Cswitch of expression * int array * (expression * Debuginfo.t) array
      * Debuginfo.t * value_kind
  | Ccatch of
      rec_flag
        * (label * (Backend_var.With_provenance.t * machtype) list
          * expression * Debuginfo.t) list
        * expression * value_kind
  | Cexit of exit_label * expression list * trap_action list
  | Ctrywith of expression * trywith_kind * Backend_var.With_provenance.t
      * expression * Debuginfo.t * value_kind
  | Cregion of expression
  | Ctail of expression

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Use_linscan_regalloc

type fundecl =
  { fun_name: string;
    fun_args: (Backend_var.With_provenance.t * machtype) list;
    fun_body: expression;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of string
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

let ccatch (i, ids, e1, e2, dbg, kind) =
  Ccatch(Nonrecursive, [i, ids, e2, dbg], e1, kind)

let reset () =
  label_counter := init_label

let iter_shallow_tail f = function
  | Clet(_, _, body) | Cphantom_let (_, _, body) | Clet_mut(_, _, _, body) ->
      f body;
      true
  | Cifthenelse(_cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _value_kind) ->
      f ifso;
      f ifnot;
      true
  | Csequence(_e1, e2) ->
      f e2;
      true
  | Cswitch(_e, _tbl, el, _dbg', _value_kind) ->
      Array.iter (fun (e, _dbg) -> f e) el;
      true
  | Ccatch(_rec_flag, handlers, body, _value_kind) ->
      List.iter (fun (_, _, h, _dbg) -> f h) handlers;
      f body;
      true
  | Ctrywith(e1, _kind, _id, e2, _dbg, _value_kind) ->
      f e1;
      f e2;
      true
  | Cregion e ->
      f e;
      true
  | Ctail e ->
      f e;
      true
  | Cexit _ | Cop (Craise _, _, _) ->
      true
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ ->
      false

let map_shallow_tail ?kind f = function
  | Clet(id, exp, body) ->
      Clet(id, exp, f body)
  | Clet_mut(id, kind, exp, body) ->
      Clet_mut(id, kind, exp, f body)
  | Cphantom_let(id, exp, body) ->
      Cphantom_let (id, exp, f body)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg, kind_before) ->
      Cifthenelse
        (
          cond,
          ifso_dbg, f ifso,
          ifnot_dbg, f ifnot,
          dbg,
          Option.value kind ~default:kind_before
        )
  | Csequence(e1, e2) ->
      Csequence(e1, f e2)
  | Cswitch(e, tbl, el, dbg', kind_before) ->
      Cswitch(e, tbl, Array.map (fun (e, dbg) -> f e, dbg) el, dbg',
              Option.value kind ~default:kind_before)
  | Ccatch(rec_flag, handlers, body, kind_before) ->
      let map_h (n, ids, handler, dbg) = (n, ids, f handler, dbg) in
      Ccatch(rec_flag, List.map map_h handlers, f body,
             Option.value kind ~default:kind_before)
  | Ctrywith(e1, kind', id, e2, dbg, kind_before) ->
      Ctrywith(f e1, kind', id, f e2, dbg,
              Option.value kind ~default:kind_before)
  | Cregion e ->
      Cregion(f e)
  | Ctail e ->
      Ctail(f e)
  | Cexit _ | Cop (Craise _, _, _) as cmm ->
      cmm
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ as cmm -> cmm

let map_tail ?kind f =
  let rec loop = function
    | Cconst_int _
    | Cconst_natint _
    | Cconst_float _
    | Cconst_symbol _
    | Cvar _
    | Cassign _
    | Ctuple _
    | Cop _ as c ->
        f c
    | cmm -> map_shallow_tail ?kind loop cmm
  in
  loop

let iter_shallow f = function
  | Clet (_id, e1, e2) ->
      f e1; f e2
  | Clet_mut (_id, _kind, e1, e2) ->
      f e1; f e2
  | Cphantom_let (_id, _de, e) ->
      f e
  | Cassign (_id, e) ->
      f e
  | Ctuple el ->
      List.iter f el
  | Cop (_op, el, _dbg) ->
      List.iter f el
  | Csequence (e1, e2) ->
      f e1; f e2
  | Cifthenelse(cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _value_kind) ->
      f cond; f ifso; f ifnot
  | Cswitch (_e, _ia, ea, _dbg, _value_kind) ->
      Array.iter (fun (e, _) -> f e) ea
  | Ccatch (_rf, hl, body, _value_kind) ->
      let iter_h (_n, _ids, handler, _dbg) = f handler in
      List.iter iter_h hl; f body
  | Cexit (_n, el, _traps) ->
      List.iter f el
  | Ctrywith (e1, _kind, _id, e2, _dbg, _value_kind) ->
      f e1; f e2
  | Cregion e ->
      f e
  | Ctail e ->
      f e
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _ ->
      ()

let map_shallow f = function
  | Clet (id, e1, e2) ->
      Clet (id, f e1, f e2)
  | Clet_mut (id, kind, e1, e2) ->
      Clet_mut (id, kind, f e1, f e2)
  | Cphantom_let (id, de, e) ->
      Cphantom_let (id, de, f e)
  | Cassign (id, e) ->
      Cassign (id, f e)
  | Ctuple el ->
      Ctuple (List.map f el)
  | Cop (op, el, dbg) ->
      Cop (op, List.map f el, dbg)
  | Csequence (e1, e2) ->
      Csequence (f e1, f e2)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg, kind) ->
      Cifthenelse(f cond, ifso_dbg, f ifso, ifnot_dbg, f ifnot, dbg, kind)
  | Cswitch (e, ia, ea, dbg, kind) ->
      Cswitch (e, ia, Array.map (fun (e, dbg) -> f e, dbg) ea, dbg, kind)
  | Ccatch (rf, hl, body, kind) ->
      let map_h (n, ids, handler, dbg) = (n, ids, f handler, dbg) in
      Ccatch (rf, List.map map_h hl, f body, kind)
  | Cexit (n, el, traps) ->
      Cexit (n, List.map f el, traps)
  | Ctrywith (e1, kind, id, e2, dbg, value_kind) ->
      Ctrywith (f e1, kind, id, f e2, dbg, value_kind)
  | Cregion e ->
      Cregion (f e)
  | Ctail e ->
      Ctail (f e)
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
    as c ->
      c

let equal_machtype_component left right =
  match left, right with
  | Val, Val -> true
  | Addr, Addr -> true
  | Int, Int -> true
  | Float, Float -> true
  | Val, (Addr | Int | Float)
  | Addr, (Val | Int | Float)
  | Int, (Val | Addr | Float)
  | Float, (Val | Addr | Int) ->
    false

let equal_exttype left right =
  match left, right with
  | XInt, XInt -> true
  | XInt32, XInt32 -> true
  | XInt64, XInt64 -> true
  | XFloat, XFloat -> true
  | XInt, (XInt32 | XInt64 | XFloat)
  | XInt32, (XInt | XInt64 | XFloat)
  | XInt64, (XInt | XInt32 | XFloat)
  | XFloat, (XInt | XInt32 | XInt64) ->
    false

let equal_float_comparison left right =
  match left, right with
  | CFeq, CFeq -> true
  | CFneq, CFneq -> true
  | CFlt, CFlt -> true
  | CFnlt, CFnlt -> true
  | CFgt, CFgt -> true
  | CFngt, CFngt -> true
  | CFle, CFle -> true
  | CFnle, CFnle -> true
  | CFge, CFge -> true
  | CFnge, CFnge -> true
  | CFeq, (CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFneq, (CFeq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFlt, (CFeq | CFneq | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFnlt, (CFeq | CFneq | CFlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFgt, (CFeq | CFneq | CFlt | CFnlt | CFngt | CFle | CFnle | CFge | CFnge)
  | CFngt, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFle | CFnle | CFge | CFnge)
  | CFle, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFnle | CFge | CFnge)
  | CFnle, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFge | CFnge)
  | CFge, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFnge)
  | CFnge, (CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge) ->
    false

let equal_memory_chunk left right =
  match left, right with
  | Byte_unsigned, Byte_unsigned -> true
  | Byte_signed, Byte_signed -> true
  | Sixteen_unsigned, Sixteen_unsigned -> true
  | Sixteen_signed, Sixteen_signed -> true
  | Thirtytwo_unsigned, Thirtytwo_unsigned -> true
  | Thirtytwo_signed, Thirtytwo_signed -> true
  | Word_int, Word_int -> true
  | Word_val, Word_val -> true
  | Single, Single -> true
  | Double, Double -> true
  | Byte_unsigned, (Byte_signed | Sixteen_unsigned | Sixteen_signed | Thirtytwo_unsigned
                   | Thirtytwo_signed | Word_int | Word_val | Single | Double)
  | Byte_signed, (Byte_unsigned | Sixteen_unsigned | Sixteen_signed | Thirtytwo_unsigned
                 | Thirtytwo_signed | Word_int | Word_val | Single | Double)
  | Sixteen_unsigned, (Byte_unsigned | Byte_signed | Sixteen_signed | Thirtytwo_unsigned
                      | Thirtytwo_signed | Word_int | Word_val | Single | Double)
  | Sixteen_signed, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Thirtytwo_unsigned
                    | Thirtytwo_signed | Word_int | Word_val | Single | Double)
  | Thirtytwo_unsigned, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
                        | Thirtytwo_signed | Word_int | Word_val | Single | Double)
  | Thirtytwo_signed, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
                      | Thirtytwo_unsigned | Word_int | Word_val | Single | Double)
  | Word_int, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
              | Thirtytwo_unsigned | Thirtytwo_signed | Word_val | Single | Double)
  | Word_val, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
              | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Single | Double)
  | Single, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Double)
  | Double, (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Single) ->
    false

let equal_integer_comparison left right =
  match left, right with
  | Ceq, Ceq -> true
  | Cne, Cne -> true
  | Clt, Clt -> true
  | Cgt, Cgt -> true
  | Cle, Cle -> true
  | Cge, Cge -> true
  | Ceq, (Cne | Clt | Cgt | Cle | Cge)
  | Cne, (Ceq | Clt | Cgt | Cle | Cge)
  | Clt, (Ceq | Cne | Cgt | Cle | Cge)
  | Cgt, (Ceq | Cne | Clt | Cle | Cge)
  | Cle, (Ceq | Cne | Clt | Cgt | Cge)
  | Cge, (Ceq | Cne | Clt | Cgt | Cle) ->
    false
