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

[@@@ocaml.warning "+a-4-9-40-41-42-44-45"]

module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm
open Arch

type arity =
  { function_kind : Lambda.function_kind;
    params_layout : Lambda.layout list;
    return_layout : Lambda.layout
  }

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
  | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ -> fn arg
  | _ ->
    let id = V.create_local name in
    Clet (VP.create id, arg, fn (Cvar id))

let bind_list name args fn =
  let rec aux bound_args = function
    | [] -> fn bound_args
    | arg :: args ->
      bind name arg (fun bound_arg -> aux (bound_arg :: bound_args) args)
  in
  aux [] (List.rev args)

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8

let caml_local =
  Nativeint.shift_left (Nativeint.of_int (if Config.runtime5 then 3 else 2)) 8
(* cf. runtime/caml/gc.h *)

(* Loads *)

let mk_load_immut memory_chunk =
  Cload { memory_chunk; mutability = Immutable; is_atomic = false }

let mk_load_mut memory_chunk =
  Cload { memory_chunk; mutability = Mutable; is_atomic = false }

let mk_load_atomic memory_chunk =
  Cload { memory_chunk; mutability = Mutable; is_atomic = true }

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

(* CR mshinwell: update to use NOT_MARKABLE terminology *)
let block_header tag sz =
  Nativeint.add
    (Nativeint.shift_left (Nativeint.of_int sz) 10)
    (Nativeint.of_int tag)

(* Static data corresponding to "value"s must be marked black in case we are in
   no-naked-pointers mode. See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black

let local_block_header tag sz = Nativeint.logor (block_header tag sz) caml_local

let white_closure_header sz = block_header Obj.closure_tag sz

let black_closure_header sz = black_block_header Obj.closure_tag sz

let local_closure_header sz = local_block_header Obj.closure_tag sz

let infix_header ofs = block_header Obj.infix_tag ofs

let float_header = block_header Obj.double_tag (size_float / size_addr)

let float_local_header =
  local_block_header Obj.double_tag (size_float / size_addr)

let boxedvec128_header = block_header Obj.abstract_tag (size_vec128 / size_addr)

let boxedvec128_local_header =
  local_block_header Obj.abstract_tag (size_vec128 / size_addr)

let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0
  then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)

let string_header len =
  block_header Obj.string_tag ((len + size_addr) / size_addr)

let boxedint32_header = block_header Obj.custom_tag 2

let boxedint64_header = block_header Obj.custom_tag (1 + (8 / size_addr))

let boxedintnat_header = block_header Obj.custom_tag 2

let boxedint32_local_header = local_block_header Obj.custom_tag 2

let boxedint64_local_header =
  local_block_header Obj.custom_tag (1 + (8 / size_addr))

let boxedintnat_local_header = local_block_header Obj.custom_tag 2

let black_custom_header ~size = black_block_header Obj.custom_tag size

let custom_header ~size = block_header Obj.custom_tag size

let custom_local_header ~size = local_block_header Obj.custom_tag size

let caml_nativeint_ops = "caml_nativeint_ops"

let caml_int32_ops = "caml_int32_ops"

let caml_int64_ops = "caml_int64_ops"

let pos_arity_in_closinfo = (8 * size_addr) - 8
(* arity = the top 8 bits of the closinfo word *)

let pack_closure_info ~arity ~startenv ~is_last =
  assert (-128 <= arity && arity <= 127);
  assert (0 <= startenv && startenv < 1 lsl (pos_arity_in_closinfo - 2));
  Nativeint.(
    add
      (shift_left (of_int arity) pos_arity_in_closinfo)
      (add
         (shift_left
            (Bool.to_int is_last |> Nativeint.of_int)
            (pos_arity_in_closinfo - 1))
         (add (shift_left (of_int startenv) 1) 1n)))

let closure_info' ~arity ~startenv ~is_last =
  let arity =
    match arity with
    | Lambda.Tupled, l -> -List.length l
    | Lambda.Curried _, l -> List.length l
  in
  pack_closure_info ~arity ~startenv ~is_last

let closure_info ~(arity : arity) ~startenv ~is_last =
  closure_info'
    ~arity:(arity.function_kind, arity.params_layout)
    ~startenv ~is_last

let alloc_float_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (float_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (float_local_header, dbg)

let alloc_boxedvec128_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedvec128_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedvec128_local_header, dbg)

let alloc_floatarray_header len dbg = Cconst_natint (floatarray_header len, dbg)

let alloc_closure_header ~mode sz dbg =
  match (mode : Lambda.alloc_mode) with
  | Alloc_heap -> Cconst_natint (white_closure_header sz, dbg)
  | Alloc_local -> Cconst_natint (local_closure_header sz, dbg)

let alloc_infix_header ofs dbg = Cconst_natint (infix_header ofs, dbg)

let alloc_boxedint32_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedint32_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedint32_local_header, dbg)

let alloc_boxedint64_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedint64_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedint64_local_header, dbg)

let alloc_boxedintnat_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedintnat_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedintnat_local_header, dbg)

(* Integers *)

let max_repr_int = max_int asr 1

let min_repr_int = min_int asr 1

let int_const dbg n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int ((n lsl 1) + 1, dbg)
  else
    Cconst_natint
      (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n, dbg)

let natint_const_untagged dbg n =
  if n > Nativeint.of_int max_int || n < Nativeint.of_int min_int
  then Cconst_natint (n, dbg)
  else Cconst_int (Nativeint.to_int n, dbg)

let cint_const n =
  Cint (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let add_no_overflow n x c dbg =
  let d = n + x in
  if d = 0 then c else Cop (Caddi, [c; Cconst_int (d, dbg)], dbg)

let rec add_const c n dbg =
  if n = 0
  then c
  else
    match c with
    | Cconst_int (x, _) when Misc.no_overflow_add x n -> Cconst_int (x + n, dbg)
    | Cop (Caddi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
    | Cop (Caddi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
    | Cop (Csubi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      Cop (Csubi, [Cconst_int (n + x, dbg); c], dbg)
    | Cop (Csubi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_sub n x ->
      add_const c (n - x) dbg
    | c -> Cop (Caddi, [c; Cconst_int (n, dbg)], dbg)

let incr_int c dbg = add_const c 1 dbg

let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  match c1, c2 with
  | Cconst_int (n, _), c | c, Cconst_int (n, _) -> add_const c n dbg
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
    add_const (add_int c1 c2 dbg) n1 dbg
  | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) ->
    add_const (add_int c1 c2 dbg) n2 dbg
  | _, _ -> Cop (Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  match c1, c2 with
  | c1, Cconst_int (n2, _) when n2 <> min_int -> add_const c1 (-n2) dbg
  | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) when n2 <> min_int ->
    add_const (sub_int c1 c2 dbg) (-n2) dbg
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
    add_const (sub_int c1 c2 dbg) n1 dbg
  | c1, c2 -> Cop (Csubi, [c1; c2], dbg)

let neg_int c dbg = sub_int (Cconst_int (0, dbg)) c dbg

let rec lsl_int c1 c2 dbg =
  match c1, c2 with
  | Cop (Clsl, [c; Cconst_int (n1, _)], _), Cconst_int (n2, _)
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
    Cop (Clsl, [c; Cconst_int (n1 + n2, dbg)], dbg)
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), Cconst_int (n2, _)
    when Misc.no_overflow_lsl n1 n2 ->
    add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | _, _ -> Cop (Clsl, [c1; c2], dbg)

let is_power2 n = n = 1 lsl Misc.log2 n

and mult_power2 c n dbg = lsl_int c (Cconst_int (Misc.log2 n, dbg)) dbg

let rec mul_int c1 c2 dbg =
  match c1, c2 with
  | c, Cconst_int (0, _) | Cconst_int (0, _), c ->
    Csequence (c, Cconst_int (0, dbg))
  | c, Cconst_int (1, _) | Cconst_int (1, _), c -> c
  | c, Cconst_int (-1, _) | Cconst_int (-1, _), c ->
    sub_int (Cconst_int (0, dbg)) c dbg
  | c, Cconst_int (n, _) when is_power2 n -> mult_power2 c n dbg
  | Cconst_int (n, _), c when is_power2 n -> mult_power2 c n dbg
  | Cop (Caddi, [c; Cconst_int (n, _)], _), Cconst_int (k, _)
  | Cconst_int (k, _), Cop (Caddi, [c; Cconst_int (n, _)], _)
    when Misc.no_overflow_mul n k ->
    add_const (mul_int c (Cconst_int (k, dbg)) dbg) (n * k) dbg
  | c1, c2 -> Cop (Cmuli, [c1; c2], dbg)

(* identify cmm operations whose result is guaranteed to be small integers (e.g.
   in the range [min_int / 4; max_int / 4]) *)
let guaranteed_to_be_small_int = function
  | Cop ((Ccmpi _ | Ccmpf _), _, _) ->
    (* integer/float comparisons return either [1] or [0]. *)
    true
  | _ -> false

let ignore_low_bit_int = function
  | Cop
      ( Caddi,
        [(Cop (Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)],
        _ )
    when n > 0 ->
    c
  | Cop (Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

let lsr_int c1 c2 dbg =
  match c2 with
  | Cconst_int (0, _) -> c1
  | Cconst_int (n, _) when n > 0 -> Cop (Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ -> Cop (Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  match c2 with
  | Cconst_int (0, _) -> c1
  | Cconst_int (n, _) when n > 0 -> (
    match ignore_low_bit_int c1 with
    (* some operations always return small enough integers that it is safe and
       correct to optimise [asr (lsl x 1) 1] into [x]. *)
    | Cop (Clsl, [c; Cconst_int (1, _)], _)
      when n = 1 && guaranteed_to_be_small_int c ->
      c
    | c1' -> Cop (Casr, [c1'; c2], dbg))
  | _ -> Cop (Casr, [c1; c2], dbg)

let tag_int i dbg =
  match i with
  | Cconst_int (n, _) -> int_const dbg n
  | Cop (Casr, [c; Cconst_int (n, _)], _) when n > 0 ->
    Cop
      (Cor, [asr_int c (Cconst_int (n - 1, dbg)) dbg; Cconst_int (1, dbg)], dbg)
  | c -> incr_int (lsl_int c (Cconst_int (1, dbg)) dbg) dbg

let untag_int i dbg =
  match i with
  | Cconst_int (n, _) -> Cconst_int (n asr 1, dbg)
  | Cop (Cor, [Cop (Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < (size_int * 8) - 1 ->
    Cop (Casr, [c; Cconst_int (n + 1, dbg)], dbg)
  | Cop (Cor, [Cop (Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < (size_int * 8) - 1 ->
    Cop (Clsr, [c; Cconst_int (n + 1, dbg)], dbg)
  | c -> asr_int c (Cconst_int (1, dbg)) dbg

let mk_not dbg cmm =
  match cmm with
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg')
    -> (
    match c with
    | Cop (Ccmpi cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpa cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpf cmp, [c1; c2], dbg'') ->
      tag_int (Cop (Ccmpf (negate_float_comparison cmp), [c1; c2], dbg'')) dbg'
    | _ ->
      (* 0 -> 3, 1 -> 1 *)
      Cop
        ( Csubi,
          [Cconst_int (3, dbg); Cop (Clsl, [c; Cconst_int (1, dbg)], dbg)],
          dbg ))
  | Cconst_int (3, _) -> Cconst_int (1, dbg)
  | Cconst_int (1, _) -> Cconst_int (3, dbg)
  | c ->
    (* 1 -> 3, 3 -> 1 *)
    Cop (Csubi, [Cconst_int (4, dbg); c], dbg)

let mk_compare_ints_untagged dbg a1 a2 =
  bind "int_cmp" a2 (fun a2 ->
      bind "int_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpi Cgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpi Clt, [a1; a2], dbg) in
          sub_int op1 op2 dbg))

let mk_compare_ints dbg a1 a2 =
  match a1, a2 with
  | Cconst_int (c1, _), Cconst_int (c2, _) -> int_const dbg (Int.compare c1 c2)
  | Cconst_natint (c1, _), Cconst_natint (c2, _) ->
    int_const dbg (Nativeint.compare c1 c2)
  | Cconst_int (c1, _), Cconst_natint (c2, _) ->
    int_const dbg Nativeint.(compare (of_int c1) c2)
  | Cconst_natint (c1, _), Cconst_int (c2, _) ->
    int_const dbg Nativeint.(compare c1 (of_int c2))
  | a1, a2 -> tag_int (mk_compare_ints_untagged dbg a1 a2) dbg

let mk_compare_floats_untagged dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
      bind "float_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpf CFgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpf CFlt, [a1; a2], dbg) in
          let op3 = Cop (Ccmpf CFeq, [a1; a1], dbg) in
          let op4 = Cop (Ccmpf CFeq, [a2; a2], dbg) in
          (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1, and the
             result is op1 - op2.

             If at least one of the operands is NaN, then op1 = op2 = 0, and the
             result is op3 - op4, which orders NaN before other values.

             To detect if the operand is NaN, we use the property:

             for all x, NaN is not equal to x, even if x is NaN.

             Therefore, op3 is 0 if and only if a1 is NaN, and op4 is 0 if and
             only if a2 is NaN. See also caml_float_compare_unboxed in
             runtime/floats.c *)
          add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg))

let mk_compare_floats dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
      bind "float_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpf CFgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpf CFlt, [a1; a2], dbg) in
          let op3 = Cop (Ccmpf CFeq, [a1; a1], dbg) in
          let op4 = Cop (Ccmpf CFeq, [a2; a2], dbg) in
          (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1, and the
             result is op1 - op2.

             If at least one of the operands is NaN, then op1 = op2 = 0, and the
             result is op3 - op4, which orders NaN before other values.

             To detect if the operand is NaN, we use the property: for all x,
             NaN is not equal to x, even if x is NaN.

             Therefore, op3 is 0 if and only if a1 is NaN, and op4 is 0 if and
             only if a2 is NaN.

             See also caml_float_compare_unboxed in runtime/floats.c *)
          tag_int (add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg) dbg))

let create_loop body dbg =
  let cont = Lambda.next_raise_count () in
  let call_cont = Cexit (Lbl cont, [], []) in
  let body = Csequence (body, call_cont) in
  Ccatch (Recursive, [cont, [], body, dbg, false], call_cont, Any)

(* Turning integer divisions into multiply-high then shift. The
   [division_parameters] function is used in module Emit for those target
   platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint. Algorithm: Hacker's Delight
   section 9.3 *)

let udivmod n d =
  Nativeint.(
    if d < 0n
    then if ucompare n d < 0 then 0n, n else 1n, sub n d
    else
      let q = shift_left (div (shift_right_logical n 1) d) 1 in
      let r = sub n (mul q d) in
      if ucompare r d >= 0 then succ q, sub r d else q, r)

(* Compute division parameters. Algorithm: Hacker's Delight chapter 10, fig
   10-1. *)

let divimm_parameters d =
  Nativeint.(
    assert (d > 0n);
    let twopsm1 = min_int in
    (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
    let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
    let rec loop p (q1, r1) (q2, r2) =
      let p = p + 1 in
      let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
      let q1, r1 = if ucompare r1 nc >= 0 then succ q1, sub r1 nc else q1, r1 in
      let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
      let q2, r2 = if ucompare r2 d >= 0 then succ q2, sub r2 d else q2, r2 in
      let delta = sub d r2 in
      if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
      then loop p (q1, r1) (q2, r2)
      else succ q2, p - size
    in
    loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

   2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1) (i)

   from which it follows that

   floor(n / d) = floor(n * m / 2^(wordsize+p)), if 0 <= n < 2^(wordsize-1)

   ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1, if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below. It was
   exhaustively tested for values of d from 2 to 10^9 in the wordsize = 64
   case.

 * let add2 (xh, xl) (yh, yl) =
 *   let zl = add xl yl and zh = add xh yh in
 *   (if ucompare zl xl < 0 then succ zh else zh), zl
 *
 * let shl2 (xh, xl) n =
 *   assert (0 < n && n < size + size);
 *   if n < size
 *   then
 *     logor (shift_left xh n) (shift_right_logical xl (size - n)),
 *       shift_left xl n
 *   else shift_left xl (n - size), 0n
 *
 * let mul2 x y =
 *   let halfsize = size / 2 in
 *   let halfmask = pred (shift_left 1n halfsize) in
 *   let xl = logand x halfmask and xh = shift_right_logical x halfsize in
 *   let yl = logand y halfmask and yh = shift_right_logical y halfsize in
 *   add2
 *     (mul xh yh, 0n)
 *     (add2
 *        (shl2 (0n, mul xl yh) halfsize)
 *        (add2 (shl2 (0n, mul xh yl) halfsize) (0n, mul xl yl)))
 *
 * let ucompare2 (xh, xl) (yh, yl) =
 *   let c = ucompare xh yh in
 *   if c = 0 then ucompare xl yl else c
 *
 * let validate d m p =
 *   let md = mul2 m d in
 *   let one2 = 0n, 1n in
 *   let twoszp = shl2 one2 (size + p) in
 *   let twop1 = shl2 one2 (p + 1) in
 *   ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
 *)

let raise_symbol dbg symb =
  Cop
    (Craise Lambda.Raise_regular, [Cconst_symbol (global_symbol symb, dbg)], dbg)

let rec div_int c1 c2 is_safe dbg =
  match c1, c2 with
  | c1, Cconst_int (0, _) ->
    Csequence (c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | c1, Cconst_int (1, _) -> c1
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Cconst_int (n1 / n2, dbg)
  | c1, Cconst_int (n, _) when n <> min_int ->
    let l = Misc.log2 n in
    if n = 1 lsl l
    then
      (* Algorithm:

         t = shift-right-signed(c1, l - 1)

         t = shift-right(t, W - l)

         t = c1 + t res = shift-right-signed(c1 + t, l) *)
      Cop
        ( Casr,
          [ bind "dividend" c1 (fun c1 ->
                assert (l >= 1);
                let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
                let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
                add_int c1 t dbg);
            Cconst_int (l, dbg) ],
          dbg )
    else if n < 0
    then
      sub_int
        (Cconst_int (0, dbg))
        (div_int c1 (Cconst_int (-n, dbg)) is_safe dbg)
        dbg
    else
      let m, p = divimm_parameters (Nativeint.of_int n) in
      (* Algorithm:

         t = multiply-high-signed(c1, m) if m < 0,

         t = t + c1 if p > 0,

         t = shift-right-signed(t, p)

         res = t + sign-bit(c1) *)
      bind "dividend" c1 (fun c1 ->
          let t =
            Cop
              (Cmulhi { signed = true }, [c1; natint_const_untagged dbg m], dbg)
          in
          let t = if m < 0n then Cop (Caddi, [t; c1], dbg) else t in
          let t =
            if p > 0 then Cop (Casr, [t; Cconst_int (p, dbg)], dbg) else t
          in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1, dbg)) dbg) dbg)
  | c1, c2 when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
    Cop (Cdivi, [c1; c2], dbg)
  | c1, c2 ->
    bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
            Cifthenelse
              ( c2,
                dbg,
                Cop (Cdivi, [c1; c2], dbg),
                dbg,
                raise_symbol dbg "caml_exn_Division_by_zero",
                dbg,
                Any )))

let mod_int c1 c2 is_safe dbg =
  match c1, c2 with
  | c1, Cconst_int (0, _) ->
    Csequence (c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | c1, Cconst_int ((1 | -1), _) -> Csequence (c1, Cconst_int (0, dbg))
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Cconst_int (n1 mod n2, dbg)
  | c1, (Cconst_int (n, _) as c2) when n <> min_int ->
    let l = Misc.log2 n in
    if n = 1 lsl l
    then
      (* Algorithm:

         t = shift-right-signed(c1, l - 1)

         t = shift-right(t, W - l)

         t = c1 + t

         t = bit-and(t, -n)

         res = c1 - t *)
      bind "dividend" c1 (fun c1 ->
          assert (l >= 1);
          let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
          let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
          let t = add_int c1 t dbg in
          let t = Cop (Cand, [t; Cconst_int (-n, dbg)], dbg) in
          sub_int c1 t dbg)
    else
      bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 is_safe dbg) c2 dbg) dbg)
  | c1, c2 when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
    (* Flambda already generates that test *)
    Cop (Cmodi, [c1; c2], dbg)
  | c1, c2 ->
    bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
            Cifthenelse
              ( c2,
                dbg,
                Cop (Cmodi, [c1; c2], dbg),
                dbg,
                raise_symbol dbg "caml_exn_Division_by_zero",
                dbg,
                Any )))

(* Division or modulo on boxed integers. The overflow case min_int / -1 can
   occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
  | Cconst_int (n, _) -> n <> x
  | Cconst_natint (n, _) -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop kind is_safe mkm1 c1 c2 bi dbg =
  bind "divisor" c2 (fun c2 ->
      bind "dividend" c1 (fun c1 ->
          let c = mkop c1 c2 is_safe dbg in
          if Arch.division_crashes_on_overflow && bi <> Primitive.Pint32
             && not (is_different_from (-1) c2)
          then
            Cifthenelse
              ( Cop (Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
                dbg,
                c,
                dbg,
                mkm1 c1 dbg,
                dbg,
                kind )
          else c))

let safe_div_bi is_safe =
  safe_divmod_bi div_int Any is_safe (fun c1 dbg ->
      Cop (Csubi, [Cconst_int (0, dbg); c1], dbg))

let safe_mod_bi is_safe =
  safe_divmod_bi mod_int Any is_safe (fun _ dbg -> Cconst_int (0, dbg))

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _)
    ->
    c
  | Cconst_int (n, dbg) ->
    if n = 1 then Cconst_int (0, dbg) else Cconst_int (1, dbg)
  | c -> Cop (Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float dbg m c = Cop (Calloc m, [alloc_float_header m dbg; c], dbg)

let unbox_float dbg =
  map_tail ~kind:Any (function
    | Cop (Calloc _, [Cconst_natint (hdr, _); c], _)
      when Nativeint.equal hdr float_header
           || Nativeint.equal hdr float_local_header ->
      c
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name with
      | Some (Const_float x) -> Cconst_float (x, dbg) (* or keep _dbg? *)
      | _ -> Cop (mk_load_immut Double, [cmm], dbg))
    | cmm -> Cop (mk_load_immut Double, [cmm], dbg))

(* Vectors *)

let box_vec128 dbg m c = Cop (Calloc m, [alloc_boxedvec128_header m dbg; c], dbg)

let unbox_vec128 dbg =
  (* Boxed vectors are not 16-byte aligned by the GC, so we must use an
     unaligned load. *)
  map_tail ~kind:Any (function
    | Cop (Calloc _, [Cconst_natint (hdr, _); c], _)
      when Nativeint.equal hdr boxedvec128_header
           || Nativeint.equal hdr boxedvec128_local_header ->
      c
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name with
      | Some (Const_vec128 { low; high }) ->
        Cconst_vec128 ({ low; high }, dbg) (* or keep _dbg? *)
      | _ -> Cop (mk_load_immut Onetwentyeight_unaligned, [cmm], dbg))
    | cmm -> Cop (mk_load_immut Onetwentyeight_unaligned, [cmm], dbg))

(* Complex *)

let box_complex dbg c_re c_im =
  Cop
    (Calloc Lambda.alloc_heap, [alloc_floatarray_header 2 dbg; c_re; c_im], dbg)

let complex_re c dbg = Cop (mk_load_immut Double, [c], dbg)

let complex_im c dbg =
  Cop
    ( mk_load_immut Double,
      [Cop (Cadda, [c; Cconst_int (size_float, dbg)], dbg)],
      dbg )

(* Unit *)

let return_unit dbg c = Csequence (c, Cconst_int (1, dbg))

let field_address ?(memory_chunk = Word_val) ptr n dbg =
  if n = 0
  then ptr
  else
    let field_size_in_bytes =
      match memory_chunk with
      | Byte_unsigned | Byte_signed -> 1
      | Sixteen_unsigned | Sixteen_signed -> 2
      | Thirtytwo_unsigned | Thirtytwo_signed -> 4
      | Single ->
        assert (size_float = 8);
        (* unclear what to do if this is false *)
        size_float / 2
      | Word_int -> size_int
      | Word_val -> size_addr
      | Double -> size_float
      | Onetwentyeight_unaligned | Onetwentyeight_aligned -> size_vec128
    in
    Cop (Cadda, [ptr; Cconst_int (n * field_size_in_bytes, dbg)], dbg)

let get_field_gen_given_memory_chunk memory_chunk mutability ptr n dbg =
  Cop
    ( Cload { memory_chunk; mutability; is_atomic = false },
      [field_address ptr n dbg],
      dbg )

let get_field_gen mut ptr n dbg =
  get_field_gen_given_memory_chunk Word_val mut ptr n dbg

let get_field_codepointer mut ptr n dbg =
  get_field_gen_given_memory_chunk Word_int mut ptr n dbg

let set_field ptr n newval init dbg =
  Cop (Cstore (Word_val, init), [field_address ptr n dbg; newval], dbg)

let get_header ptr dbg =
  (* Headers can be mutated when forcing a lazy value. However, for all purposes
     that the mutability tag currently serves in the compiler, header loads can
     be marked as [Immutable], since the runtime should ensure that there is no
     data race on headers. This saves performance with ThreadSanitizer
     instrumentation by avoiding to instrument header loads. *)
  Cop
    ( mk_load_mut Word_int,
      (* CR xclerc: consider whether that could be changed to mk_load_immut *)
      [Cop (Cadda, [ptr; Cconst_int (-size_int, dbg)], dbg)],
      dbg )

let get_header_masked ptr dbg =
  if Config.reserved_header_bits > 0
  then
    let header_mask = (1 lsl (64 - Config.reserved_header_bits)) - 1 in
    Cop (Cand, [get_header ptr dbg; Cconst_int (header_mask, dbg)], dbg)
  else get_header ptr dbg

let tag_offset = if big_endian then -1 else -size_int

let get_tag ptr dbg =
  if Proc.word_addressed
  then
    (* If byte loads are slow *)
    Cop (Cand, [get_header ptr dbg; Cconst_int (255, dbg)], dbg)
  else
    (* If byte loads are efficient *)
    (* Same comment as [get_header] above *)
    Cop
      ( (if Config.runtime5
        then mk_load_immut Byte_unsigned
        else mk_load_mut Byte_unsigned),
        [Cop (Cadda, [ptr; Cconst_int (tag_offset, dbg)], dbg)],
        dbg )

let get_size ptr dbg =
  Cop (Clsr, [get_header_masked ptr dbg; Cconst_int (10, dbg)], dbg)

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr

let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9

let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array_hdr hdr dbg =
  Cop
    ( Ccmpi Cne,
      [Cop (Cand, [hdr; Cconst_int (255, dbg)], dbg); floatarray_tag dbg],
      dbg )

let addr_array_length_shifted hdr dbg =
  Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)

let float_array_length_shifted hdr dbg =
  Cop (Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg)

let lsl_const c n dbg =
  if n = 0 then c else Cop (Clsl, [c; Cconst_int (n, dbg)], dbg)

(* Produces a pointer to the element of the array [ptr] on the position [ofs]
   with the given element [log2size] log2 element size.

   [ofs] is given as a tagged int expression.

   The optional ?typ argument is the C-- type of the result. By default, it is
   Addr, meaning we are constructing a derived pointer into the heap. If we know
   the pointer is outside the heap (this is the case for bigarray indexing), we
   give type Int instead. *)

let array_indexing ?typ log2size ptr ofs dbg =
  let add =
    match typ with
    | None | Some Addr -> Cadda
    | Some Int -> Caddi
    | _ -> assert false
  in
  match ofs with
  | Cconst_int (n, _) ->
    let i = n asr 1 in
    if i = 0
    then ptr
    else Cop (add, [ptr; Cconst_int (i lsl log2size, dbg)], dbg)
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg')
    ->
    Cop (add, [ptr; lsl_const c log2size dbg], dbg')
  | Cop (Caddi, [c; Cconst_int (n, _)], dbg') when log2size = 0 ->
    Cop
      ( add,
        [Cop (add, [ptr; untag_int c dbg], dbg); Cconst_int (n asr 1, dbg)],
        dbg' )
  | Cop (Caddi, [c; Cconst_int (n, _)], _) ->
    Cop
      ( add,
        [ Cop (add, [ptr; lsl_const c (log2size - 1) dbg], dbg);
          Cconst_int ((n - 1) lsl (log2size - 1), dbg) ],
        dbg )
  | _ when log2size = 0 -> Cop (add, [ptr; untag_int ofs dbg], dbg)
  | _ ->
    Cop
      ( add,
        [ Cop (add, [ptr; lsl_const ofs (log2size - 1) dbg], dbg);
          Cconst_int (-1 lsl (log2size - 1), dbg) ],
        dbg )

(* CR Gbury: this conversion int -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int ~dbg i = natint_const_untagged dbg (Nativeint.of_int i)

let custom_ops_size_log2 =
  let lg = Misc.log2 Config.custom_ops_struct_size in
  assert (1 lsl lg = Config.custom_ops_struct_size);
  lg

(* caml_unboxed_int32_array_ops refers to the first element of an array of two
   custom ops. The array index indicates the number of (invalid) tailing int32s
   (0 or 1). *)
let custom_ops_unboxed_int32_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_int32_array_ops", Debuginfo.none)

let custom_ops_unboxed_int32_even_array = custom_ops_unboxed_int32_array

let custom_ops_unboxed_int32_odd_array =
  Cop
    ( Caddi,
      [ custom_ops_unboxed_int32_array;
        Cconst_int (Config.custom_ops_struct_size, Debuginfo.none) ],
      Debuginfo.none )

let custom_ops_unboxed_int64_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_int64_array_ops", Debuginfo.none)

let custom_ops_unboxed_nativeint_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_nativeint_array_ops", Debuginfo.none)

let unboxed_int32_array_length arr dbg =
  (* Checking custom_ops is needed to determine if the array contains an odd or
     even number of elements *)
  let res =
    bind "arr" arr (fun arr ->
        let custom_ops_var = Backend_var.create_local "custom_ops" in
        let custom_ops_index_var =
          Backend_var.create_local "custom_ops_index"
        in
        let num_words_var = Backend_var.create_local "num_words" in
        Clet
          ( VP.create num_words_var,
            (* subtract custom_operations word *)
            sub_int (get_size arr dbg) (int ~dbg 1) dbg,
            Clet
              ( VP.create custom_ops_var,
                Cop (mk_load_immut Word_int, [arr], dbg),
                Clet
                  ( VP.create custom_ops_index_var,
                    (* compute index into custom ops array *)
                    lsr_int
                      (sub_int (Cvar custom_ops_var)
                         custom_ops_unboxed_int32_array dbg)
                      (int ~dbg custom_ops_size_log2)
                      dbg,
                    (* subtract index from length in int32s *)
                    sub_int
                      (mul_int (Cvar num_words_var) (int ~dbg 2) dbg)
                      (Cvar custom_ops_index_var) dbg ) ) ))
  in
  tag_int res dbg

let unboxed_int64_or_nativeint_array_length arr dbg =
  let res =
    bind "arr" arr (fun arr ->
        (* need to subtract so as not to count the custom_operations field *)
        sub_int (get_size arr dbg) (int ~dbg 1) dbg)
  in
  tag_int res dbg

let addr_array_ref arr ofs dbg =
  Cop (mk_load_mut Word_val, [array_indexing log2_size_addr arr ofs dbg], dbg)

let int_array_ref arr ofs dbg =
  Cop (mk_load_mut Word_int, [array_indexing log2_size_addr arr ofs dbg], dbg)

let unboxed_float_array_ref arr ofs dbg =
  Cop (mk_load_mut Double, [array_indexing log2_size_float arr ofs dbg], dbg)

let float_array_ref mode arr ofs dbg =
  box_float dbg mode (unboxed_float_array_ref arr ofs dbg)

let addr_array_set_heap arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_modify";
          ty = typ_void;
          alloc = false;
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty_args = []
        },
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

let addr_array_set_local arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_modify_local";
          ty = typ_void;
          alloc = false;
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty_args = []
        },
      [arr; untag_int ofs dbg; newval],
      dbg )

let addr_array_set (mode : Lambda.modify_mode) arr ofs newval dbg =
  match mode with
  | Modify_heap -> addr_array_set_heap arr ofs newval dbg
  | Modify_maybe_stack -> addr_array_set_local arr ofs newval dbg

(* int and float arrays can be written to uniformly regardless of their mode *)

let int_array_set arr ofs newval dbg =
  Cop
    ( Cstore (Word_int, Assignment),
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

let float_array_set arr ofs newval dbg =
  Cop
    ( Cstore (Double, Assignment),
      [array_indexing log2_size_float arr ofs dbg; newval],
      dbg )

let addr_array_initialize arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_initialize";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_void;
          alloc = false;
          ty_args = []
        },
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

(* low_32 x is a value which agrees with x on at least the low 32 bits *)
let rec low_32 dbg = function
  (* Ignore sign and zero extensions, which do not affect the low bits *)
  | Cop (Casr, [Cop (Clsl, [x; Cconst_int (32, _)], _); Cconst_int (32, _)], _)
  | Cop (Cand, [x; Cconst_natint (0xFFFFFFFFn, _)], _) ->
    low_32 dbg x
  | Clet (id, e, body) -> Clet (id, e, low_32 dbg body)
  | x -> x

(* sign_extend_32 sign-extends values from 32 bits to the word size. *)
let sign_extend_32 dbg e =
  match low_32 dbg e with
  | Cop
      ( Cload
          { memory_chunk = Thirtytwo_unsigned | Thirtytwo_signed;
            mutability;
            is_atomic
          },
        args,
        dbg ) ->
    Cop
      ( Cload { memory_chunk = Thirtytwo_signed; mutability; is_atomic },
        args,
        dbg )
  | e ->
    Cop
      ( Casr,
        [Cop (Clsl, [e; Cconst_int (32, dbg)], dbg); Cconst_int (32, dbg)],
        dbg )

let unboxed_int32_array_ref arr index dbg =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          let index =
            (* Need to skip the custom_operations field. We add 2 element
               offsets not 1 since the call to [array_indexing], below, is in
               terms of 32-bit words. Then we multiply the offset by 2 to get 4
               since we are manipulating a tagged int. *)
            add_int index (int ~dbg 4) dbg
          in
          let log2_size_addr = 2 in
          (* N.B. The resulting value will be sign extended by the code
             generated for a [Thirtytwo_signed] load. *)
          Cop
            ( mk_load_mut Thirtytwo_signed,
              [array_indexing log2_size_addr arr index dbg],
              dbg )))

let unboxed_int64_or_nativeint_array_ref arr index dbg =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          let index =
            (* Need to skip the custom_operations field. 2 not 1 since we are
               manipulating a tagged int. *)
            add_int index (int ~dbg 2) dbg
          in
          int_array_ref arr index dbg))

let unboxed_int32_array_set arr ~index ~new_value dbg =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          bind "new_value" new_value (fun new_value ->
              let index =
                (* See comment in [unboxed_int32_array_ref]. *)
                add_int index (int ~dbg 4) dbg
              in
              let log2_size_addr = 2 in
              Cop
                ( Cstore (Thirtytwo_signed, Assignment),
                  [array_indexing log2_size_addr arr index dbg; new_value],
                  dbg ))))

let unboxed_int64_or_nativeint_array_set arr ~index ~new_value dbg =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          bind "new_value" new_value (fun new_value ->
              let index =
                (* See comment in [unboxed_int64_or_nativeint_array_ref]. *)
                add_int index (int ~dbg 2) dbg
              in
              int_array_set arr index new_value dbg)))

(* Get the field of a block given a possibly inconstant index *)

let get_field_computed imm_or_ptr mutability ~block ~index dbg =
  let memory_chunk =
    match imm_or_ptr with
    | Lambda.Immediate -> Word_int
    | Lambda.Pointer -> Word_val
  in
  let field_address = array_indexing log2_size_addr block index dbg in
  Cop
    (Cload { memory_chunk; mutability; is_atomic = false }, [field_address], dbg)

(* String length *)

(* Length of string block *)

let string_length exp dbg =
  bind "str" exp (fun str ->
      let tmp_var = V.create_local "tmp" in
      Clet
        ( VP.create tmp_var,
          Cop
            ( Csubi,
              [ Cop
                  ( Clsl,
                    [get_size str dbg; Cconst_int (log2_size_addr, dbg)],
                    dbg );
                Cconst_int (1, dbg) ],
              dbg ),
          Cop
            ( Csubi,
              [ Cvar tmp_var;
                Cop
                  ( mk_load_mut Byte_unsigned,
                    [Cop (Cadda, [str; Cvar tmp_var], dbg)],
                    dbg ) ],
              dbg ) ))

let bigstring_get_alignment ba idx align dbg =
  bind "ba_data"
    (Cop (mk_load_mut Word_int, [field_address ba 1 dbg], dbg))
    (fun ba_data ->
      Cop
        ( Cand,
          [Cconst_int (align - 1, dbg); Cop (Caddi, [ba_data; idx], dbg)],
          dbg ))

(* Message sending *)

let lookup_tag obj tag dbg =
  bind "tag" tag (fun tag ->
      Cop
        ( Cextcall
            { func = "caml_get_public_method";
              ty = typ_val;
              builtin = false;
              returns = true;
              effects = Arbitrary_effects;
              coeffects = Has_coeffects;
              alloc = false;
              ty_args = []
            },
          [obj; tag],
          dbg ))

let lookup_label obj lab dbg =
  bind "lab" lab (fun lab ->
      let table = Cop (mk_load_mut Word_val, [obj], dbg) in
      addr_array_ref table lab dbg)

module Extended_machtype_component = struct
  type t =
    | Val
    | Addr
    | Tagged_int
    | Any_int
    | Float
    | Vec128

  let of_machtype_component (component : machtype_component) =
    match component with
    | Val -> Val
    | Addr -> Addr
    | Int -> Any_int
    | Float -> Float
    | Vec128 -> Vec128

  let to_machtype_component t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Tagged_int | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128

  let change_tagged_int_to_val t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Tagged_int -> Val
    | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128
end

module Extended_machtype = struct
  type t = Extended_machtype_component.t array

  let typ_val = [| Extended_machtype_component.Val |]

  let typ_tagged_int = [| Extended_machtype_component.Tagged_int |]

  let typ_any_int = [| Extended_machtype_component.Any_int |]

  let typ_float = [| Extended_machtype_component.Float |]

  let typ_vec128 = [| Extended_machtype_component.Vec128 |]

  let typ_void = [||]

  let of_machtype machtype =
    Array.map Extended_machtype_component.of_machtype_component machtype

  let to_machtype t =
    Array.map Extended_machtype_component.to_machtype_component t

  let change_tagged_int_to_val t =
    Array.map Extended_machtype_component.change_tagged_int_to_val t

  let rec of_layout (layout : Lambda.layout) =
    match layout with
    | Ptop -> Misc.fatal_error "No Extended_machtype for layout [Ptop]"
    | Pbottom ->
      Misc.fatal_error "No unique Extended_machtype for layout [Pbottom]"
    | Punboxed_float -> typ_float
    | Punboxed_vector (Pvec128 _) -> typ_vec128
    | Punboxed_int _ ->
      (* Only 64-bit architectures, so this is always [typ_int] *)
      typ_any_int
    | Pvalue Pintval -> typ_tagged_int
    | Pvalue _ -> typ_val
    | Punboxed_product fields -> Array.concat (List.map of_layout fields)
end

let machtype_of_layout layout =
  layout |> Extended_machtype.of_layout |> Extended_machtype.to_machtype

let machtype_of_layout_changing_tagged_int_to_val layout =
  layout |> Extended_machtype.of_layout
  |> Extended_machtype.change_tagged_int_to_val

let machtype_identifier t =
  let char_of_component (component : machtype_component) =
    match component with
    | Val -> 'V'
    | Int -> 'I'
    | Float -> 'F'
    | Vec128 -> 'X'
    | Addr ->
      Misc.fatal_error "[Addr] is forbidden inside arity for generic functions"
  in
  String.of_seq (Seq.map char_of_component (Array.to_seq t))

let unique_arity_identifier (arity : Cmm.machtype list) =
  if List.for_all (function [| Val |] -> true | _ -> false) arity
  then Int.to_string (List.length arity)
  else String.concat "_" (List.map machtype_identifier arity)

let result_layout_suffix result =
  match result with [| Val |] -> "" | _ -> "_R" ^ machtype_identifier result

let send_function_name arity result (mode : Lambda.alloc_mode) =
  let res = result_layout_suffix result in
  let suff = match mode with Alloc_heap -> "" | Alloc_local -> "L" in
  global_symbol ("caml_send" ^ unique_arity_identifier arity ^ res ^ suff)

let call_cached_method obj tag cache pos args args_type result (apos, mode) dbg
    =
  let cache = array_indexing log2_size_addr cache pos dbg in
  Compilenv.need_send_fun
    (List.map Extended_machtype.change_tagged_int_to_val args_type)
    (Extended_machtype.change_tagged_int_to_val result)
    mode;
  Cop
    ( Capply (Extended_machtype.to_machtype result, apos),
      (* See the cases for caml_apply regarding [change_tagged_int_to_val]. *)
      Cconst_symbol
        ( send_function_name
            (List.map Extended_machtype.change_tagged_int_to_val args_type)
            (Extended_machtype.change_tagged_int_to_val result)
            mode,
          dbg )
      :: obj :: tag :: cache :: args,
      dbg )

(* Allocation *)

let make_alloc_generic ~mode set_fn dbg tag wordsize args =
  (* allocs of size 0 must be statically allocated else the Gc will bug *)
  assert (List.compare_length_with args 0 > 0);
  if Lambda.is_local_mode mode || wordsize <= Config.max_young_wosize
  then
    let hdr =
      match mode with
      | Lambda.Alloc_local -> local_block_header tag wordsize
      | Lambda.Alloc_heap -> block_header tag wordsize
    in
    Cop (Calloc mode, Cconst_natint (hdr, dbg) :: args, dbg)
  else
    let id = V.create_local "*alloc*" in
    let rec fill_fields idx = function
      | [] -> Cvar id
      | e1 :: el ->
        Csequence
          ( set_fn (Cvar id) (Cconst_int (idx, dbg)) e1 dbg,
            fill_fields (idx + 2) el )
    in
    Clet
      ( VP.create id,
        Cop
          ( Cextcall
              { func =
                  (if Config.runtime5
                  then "caml_alloc_shr_check_gc"
                  else "caml_alloc");
                ty = typ_val;
                alloc = true;
                builtin = false;
                returns = true;
                effects = Arbitrary_effects;
                coeffects = Has_coeffects;
                ty_args = []
              },
            [Cconst_int (wordsize, dbg); Cconst_int (tag, dbg)],
            dbg ),
        fill_fields 1 args )

let make_alloc ~mode dbg tag args =
  let addr_array_init arr ofs newval dbg =
    Cop
      ( Cextcall
          { func = "caml_initialize";
            ty = typ_void;
            alloc = false;
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty_args = []
          },
        [array_indexing log2_size_addr arr ofs dbg; newval],
        dbg )
  in
  make_alloc_generic ~mode addr_array_init dbg tag (List.length args) args

let make_float_alloc ~mode dbg tag args =
  make_alloc_generic ~mode float_array_set dbg tag
    (List.length args * size_float / size_addr)
    args

(* Record application and currying functions *)

let apply_function_name arity result (mode : Lambda.alloc_mode) =
  let res = result_layout_suffix result in
  let suff = match mode with Alloc_heap -> "" | Alloc_local -> "L" in
  "caml_apply" ^ unique_arity_identifier arity ^ res ^ suff

let apply_function_sym arity result mode =
  let arity = List.map Extended_machtype.change_tagged_int_to_val arity in
  let result = Extended_machtype.change_tagged_int_to_val result in
  assert (List.length arity > 0);
  Compilenv.need_apply_fun arity result mode;
  global_symbol (apply_function_name arity result mode)

let tuplify_function_name arity result =
  "caml_tuplify" ^ Int.to_string arity ^ result_layout_suffix result

let curry_function_sym_name function_kind arity result =
  match function_kind with
  | Lambda.Curried { nlocal } ->
    Compilenv.need_curry_fun function_kind arity result;
    "caml_curry"
    ^ unique_arity_identifier arity
    ^ result_layout_suffix result
    ^ if nlocal > 0 then "L" ^ Int.to_string nlocal else ""
  | Lambda.Tupled ->
    if List.exists (function [| Val |] | [| Int |] -> false | _ -> true) arity
    then
      Misc.fatal_error
        "tuplify_function is currently unsupported if arity contains non-values";
    (* Always use [Val] to ensure we don't generate duplicate tuplify functions
       when [Int] machtypes are involved. *)
    Compilenv.need_curry_fun function_kind
      (List.map (fun _ -> [| Val |]) arity)
      result;
    tuplify_function_name (List.length arity) result

let curry_function_sym function_kind arity result =
  { sym_name = curry_function_sym_name function_kind arity result;
    sym_global = Global
  }

(* Big arrays *)

let bigarray_elt_size_in_bytes : Lambda.bigarray_kind -> int = function
  | Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> 4
  | Pbigarray_float64 -> 8
  | Pbigarray_sint8 -> 1
  | Pbigarray_uint8 -> 1
  | Pbigarray_sint16 -> 2
  | Pbigarray_uint16 -> 2
  | Pbigarray_int32 -> 4
  | Pbigarray_int64 -> 8
  | Pbigarray_caml_int -> size_int
  | Pbigarray_native_int -> size_int
  | Pbigarray_complex32 -> 8
  | Pbigarray_complex64 -> 16

let bigarray_word_kind : Lambda.bigarray_kind -> memory_chunk = function
  | Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> Single
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word_int
  | Pbigarray_caml_int -> Word_int
  | Pbigarray_native_int -> Word_int
  | Pbigarray_complex32 -> Single
  | Pbigarray_complex64 -> Double

(* the three functions below assume 64-bit words *)
let () = assert (size_int = 8)

let check_64_bit_target func =
  if size_int <> 8
  then
    Misc.fatal_errorf
      "Cmm helpers function %s can only be used on 64-bit targets" func

(* Like [low_32] but for 63-bit integers held in 64-bit registers. *)
(* CR gbury: Why not use Cmm.map_tail here ? It seems designed for that kind of
   thing (and covers more cases than just Clet). *)
let rec low_63 dbg e =
  check_64_bit_target "low_63";
  match e with
  | Cop (Casr, [Cop (Clsl, [x; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
    low_63 dbg x
  | Cop (Cand, [x; Cconst_natint (0x7FFF_FFFF_FFFF_FFFFn, _)], _) ->
    low_63 dbg x
  | Clet (id, x, body) -> Clet (id, x, low_63 dbg body)
  | _ -> e

(* CR-someday mshinwell/gbury: sign_extend_63 then tag_int should simplify to
   just tag_int. Similarly, untag_int then sign_extend_63 should simplify to
   untag_int. *)
let sign_extend_63 dbg e =
  check_64_bit_target "sign_extend_63";
  let e = low_63 dbg e in
  Cop
    (Casr, [Cop (Clsl, [e; Cconst_int (1, dbg)], dbg); Cconst_int (1, dbg)], dbg)

(* zero_extend_32 zero-extends values from 32 bits to the word size. *)
let zero_extend_32 dbg e =
  (* CR mshinwell for gbury: same question as above *)
  match low_32 dbg e with
  | Cop
      ( Cload
          { memory_chunk = Thirtytwo_signed | Thirtytwo_unsigned;
            mutability;
            is_atomic
          },
        args,
        dbg ) ->
    Cop
      ( Cload { memory_chunk = Thirtytwo_unsigned; mutability; is_atomic },
        args,
        dbg )
  | e -> Cop (Cand, [e; natint_const_untagged dbg 0xFFFFFFFFn], dbg)

let zero_extend_63 dbg e =
  check_64_bit_target "zero_extend_63";
  let e = low_63 dbg e in
  Cop (Cand, [e; natint_const_untagged dbg 0x7FFF_FFFF_FFFF_FFFFn], dbg)

let and_int e1 e2 dbg =
  let is_mask32 = function
    | Cconst_natint (0xFFFF_FFFFn, _) -> true
    | Cconst_int (n, _) -> Nativeint.of_int n = 0xFFFF_FFFFn
    | _ -> false
  in
  match e1, e2 with
  | e, m when is_mask32 m -> zero_extend_32 dbg e
  | m, e when is_mask32 m -> zero_extend_32 dbg e
  | e1, e2 -> Cop (Cand, [e1; e2], dbg)

let or_int e1 e2 dbg = Cop (Cor, [e1; e2], dbg)

let xor_int e1 e2 dbg = Cop (Cxor, [e1; e2], dbg)

(* Boxed integers *)

let operations_boxed_int (bi : Primitive.boxed_integer) =
  let sym_name =
    match bi with
    | Pnativeint -> caml_nativeint_ops
    | Pint32 -> caml_int32_ops
    | Pint64 -> caml_int64_ops
  in
  global_symbol sym_name

let alloc_header_boxed_int (bi : Primitive.boxed_integer) mode dbg =
  match bi with
  | Pnativeint -> alloc_boxedintnat_header mode dbg
  | Pint32 -> alloc_boxedint32_header mode dbg
  | Pint64 -> alloc_boxedint64_header mode dbg

let box_int_gen dbg (bi : Primitive.boxed_integer) mode arg =
  let arg' =
    if bi = Primitive.Pint32
    then
      if big_endian
      then Cop (Clsl, [arg; Cconst_int (32, dbg)], dbg)
      else sign_extend_32 dbg arg
    else arg
  in
  Cop
    ( Calloc mode,
      [ alloc_header_boxed_int bi mode dbg;
        Cconst_symbol (operations_boxed_int bi, dbg);
        arg' ],
      dbg )

let alloc_matches_boxed_int bi ~hdr ~ops =
  match (bi : Primitive.boxed_integer), hdr, ops with
  | Pnativeint, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedintnat_header
    || Nativeint.equal hdr boxedintnat_local_header)
    && String.equal sym.sym_name caml_nativeint_ops
  | Pint32, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint32_header
    || Nativeint.equal hdr boxedint32_local_header)
    && String.equal sym.sym_name caml_int32_ops
  | Pint64, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint64_header
    || Nativeint.equal hdr boxedint64_local_header)
    && String.equal sym.sym_name caml_int64_ops
  | (Pnativeint | Pint32 | Pint64), _, _ -> false

let unbox_int dbg bi =
  let default arg =
    let memory_chunk =
      if bi = Primitive.Pint32 then Thirtytwo_signed else Word_int
    in
    Cop
      ( mk_load_immut memory_chunk,
        [Cop (Cadda, [arg; Cconst_int (size_addr, dbg)], dbg)],
        dbg )
  in
  map_tail ~kind:Any (function
    | Cop
        ( Calloc _,
          [hdr; ops; Cop (Clsl, [contents; Cconst_int (32, _)], _dbg')],
          _dbg )
      when bi = Primitive.Pint32 && big_endian
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend_32 dbg contents
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when bi = Primitive.Pint32 && (not big_endian)
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend_32 dbg contents
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when alloc_matches_boxed_int bi ~hdr ~ops ->
      contents
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name, bi with
      | Some (Const_nativeint n), Primitive.Pnativeint ->
        natint_const_untagged dbg n
      | Some (Const_int32 n), Primitive.Pint32 ->
        natint_const_untagged dbg (Nativeint.of_int32 n)
      | Some (Const_int64 n), Primitive.Pint64 ->
        natint_const_untagged dbg (Int64.to_nativeint n)
      | _ -> default cmm)
    | cmm -> default cmm)

let make_unsigned_int bi arg dbg =
  if bi = Primitive.Pint32 then zero_extend_32 dbg arg else arg

let unaligned_load_16 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop (mk_load_mut Sixteen_unsigned, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Cop (Cor, [lsl_int b1 (cconst_int 8) dbg; b2], dbg)

let unaligned_set_16 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop
      (Cstore (Sixteen_unsigned, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v2 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Csequence
      ( Cop (Cstore (Byte_unsigned, Assignment), [add_int ptr idx dbg; b1], dbg),
        Cop
          ( Cstore (Byte_unsigned, Assignment),
            [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
            dbg ) )

let unaligned_load_32 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop (mk_load_mut Thirtytwo_unsigned, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let v3 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 2) dbg],
          dbg )
    in
    let v4 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 3) dbg],
          dbg )
    in
    let b1, b2, b3, b4 =
      if Arch.big_endian then v1, v2, v3, v4 else v4, v3, v2, v1
    in
    Cop
      ( Cor,
        [ Cop
            ( Cor,
              [lsl_int b1 (cconst_int 24) dbg; lsl_int b2 (cconst_int 16) dbg],
              dbg );
          Cop (Cor, [lsl_int b3 (cconst_int 8) dbg; b4], dbg) ],
        dbg )

let unaligned_set_32 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop
      ( Cstore (Thirtytwo_unsigned, Assignment),
        [add_int ptr idx dbg; newval],
        dbg )
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop
        (Cand, [Cop (Clsr, [newval; cconst_int 24], dbg); cconst_int 0xFF], dbg)
    in
    let v2 =
      Cop
        (Cand, [Cop (Clsr, [newval; cconst_int 16], dbg); cconst_int 0xFF], dbg)
    in
    let v3 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v4 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4 =
      if Arch.big_endian then v1, v2, v3, v4 else v4, v3, v2, v1
    in
    Csequence
      ( Csequence
          ( Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int ptr idx dbg; b1],
                dbg ),
            Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                dbg ) ),
        Csequence
          ( Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                dbg ),
            Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                dbg ) ) )

let unaligned_load_64 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop (mk_load_mut Word_int, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let v3 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 2) dbg],
          dbg )
    in
    let v4 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 3) dbg],
          dbg )
    in
    let v5 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 4) dbg],
          dbg )
    in
    let v6 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 5) dbg],
          dbg )
    in
    let v7 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 6) dbg],
          dbg )
    in
    let v8 =
      Cop
        ( mk_load_mut Byte_unsigned,
          [add_int (add_int ptr idx dbg) (cconst_int 7) dbg],
          dbg )
    in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1
    in
    Cop
      ( Cor,
        [ Cop
            ( Cor,
              [ Cop
                  ( Cor,
                    [ lsl_int b1 (cconst_int (8 * 7)) dbg;
                      lsl_int b2 (cconst_int (8 * 6)) dbg ],
                    dbg );
                Cop
                  ( Cor,
                    [ lsl_int b3 (cconst_int (8 * 5)) dbg;
                      lsl_int b4 (cconst_int (8 * 4)) dbg ],
                    dbg ) ],
              dbg );
          Cop
            ( Cor,
              [ Cop
                  ( Cor,
                    [ lsl_int b5 (cconst_int (8 * 3)) dbg;
                      lsl_int b6 (cconst_int (8 * 2)) dbg ],
                    dbg );
                Cop (Cor, [lsl_int b7 (cconst_int 8) dbg; b8], dbg) ],
              dbg ) ],
        dbg )

let unaligned_set_64 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then Cop (Cstore (Word_int, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 7)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v2 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 6)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v3 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 5)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v4 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 4)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v5 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 3)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v6 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 2)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v7 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v8 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1
    in
    Csequence
      ( Csequence
          ( Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int ptr idx dbg; b1],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                    dbg ) ),
            Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                    dbg ) ) ),
        Csequence
          ( Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 4) dbg; b5],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 5) dbg; b6],
                    dbg ) ),
            Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 6) dbg; b7],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 7) dbg; b8],
                    dbg ) ) ) )

let unaligned_load_128 ptr idx dbg =
  assert (size_vec128 = 16);
  Cop (mk_load_mut Onetwentyeight_unaligned, [add_int ptr idx dbg], dbg)

let unaligned_set_128 ptr idx newval dbg =
  assert (size_vec128 = 16);
  Cop
    ( Cstore (Onetwentyeight_unaligned, Assignment),
      [add_int ptr idx dbg; newval],
      dbg )

let aligned_load_128 ptr idx dbg =
  assert (size_vec128 = 16);
  Cop (mk_load_mut Onetwentyeight_aligned, [add_int ptr idx dbg], dbg)

let aligned_set_128 ptr idx newval dbg =
  assert (size_vec128 = 16);
  Cop
    ( Cstore (Onetwentyeight_aligned, Assignment),
      [add_int ptr idx dbg; newval],
      dbg )

let opaque e dbg = Cop (Copaque, [e], dbg)

(* Build an actual switch (ie jump table) *)

let make_switch arg cases actions dbg kind =
  let extract_uconstant = function
    (* Constant integers loaded from a table should end in 1, so that Cload
       never produces untagged integers *)
    | Cconst_int (n, _), _dbg when n land 1 = 1 ->
      Some (Cint (Nativeint.of_int n))
    | Cconst_natint (n, _), _dbg when Nativeint.(to_int (logand n one) = 1) ->
      Some (Cint n)
    | Cconst_symbol (s, _), _dbg -> Some (Csymbol_address s)
    | _ -> None
  in
  let extract_affine ~cases ~const_actions =
    let length = Array.length cases in
    if length >= 2
    then
      match const_actions.(cases.(0)), const_actions.(cases.(1)) with
      | Cint v0, Cint v1 ->
        let slope = Nativeint.sub v1 v0 in
        let check i = function
          | Cint v -> v = Nativeint.(add (mul (of_int i) slope) v0)
          | _ -> false
        in
        if Misc.Stdlib.Array.for_alli
             (fun i idx -> check i const_actions.(idx))
             cases
        then Some (v0, slope)
        else None
      | _, _ -> None
    else None
  in
  let make_table_lookup ~cases ~const_actions arg dbg =
    let table = Compilenv.new_const_symbol () in
    Cmmgen_state.add_constant table
      (Const_table
         ( Local,
           Array.to_list (Array.map (fun act -> const_actions.(act)) cases) ));
    let table_sym = { sym_name = table; sym_global = Local } in
    addr_array_ref (Cconst_symbol (table_sym, dbg)) (tag_int arg dbg) dbg
  in
  let make_affine_computation ~offset ~slope arg dbg =
    (* In case the resulting integers are an affine function of the index, we
       don't emit a table, and just compute the result directly *)
    add_int
      (mul_int arg (natint_const_untagged dbg slope) dbg)
      (natint_const_untagged dbg offset)
      dbg
  in
  match Misc.Stdlib.Array.all_somes (Array.map extract_uconstant actions) with
  | None -> Cswitch (arg, cases, actions, dbg, kind)
  | Some const_actions -> (
    match extract_affine ~cases ~const_actions with
    | Some (offset, slope) -> make_affine_computation ~offset ~slope arg dbg
    | None -> make_table_lookup ~cases ~const_actions arg dbg)

module SArgBlocks = struct
  type primitive = operation

  let eqint = Ccmpi Ceq

  let neint = Ccmpi Cne

  let leint = Ccmpi Cle

  let ltint = Ccmpi Clt

  let geint = Ccmpi Cge

  let gtint = Ccmpi Cgt

  type arg = expression

  type test = expression

  type act = expression

  type loc = Debuginfo.t

  type layout = kind_for_unboxing

  (* CR mshinwell: GPR#2294 will fix the Debuginfo here *)

  let make_const i = Cconst_int (i, Debuginfo.none)

  let make_prim p args = Cop (p, args, Debuginfo.none)

  let make_offset arg n = add_const arg n Debuginfo.none

  let make_isout h arg = Cop (Ccmpa Clt, [h; arg], Debuginfo.none)

  let make_isin h arg = Cop (Ccmpa Cge, [h; arg], Debuginfo.none)

  let make_is_nonzero arg = arg

  let arg_as_test arg = arg

  let make_if value_kind cond ifso ifnot =
    Cifthenelse
      ( cond,
        Debuginfo.none,
        ifso,
        Debuginfo.none,
        ifnot,
        Debuginfo.none,
        value_kind )

  let make_switch dbg value_kind arg cases actions =
    let actions = Array.map (fun expr -> expr, dbg) actions in
    make_switch arg cases actions dbg value_kind

  let bind arg body = bind "switcher" arg body

  let make_catch kind handler =
    match handler with
    | Cexit (Lbl i, [], []) -> i, fun e -> e
    | _ -> (
      let dbg = Debuginfo.none in
      let i = Lambda.next_raise_count () in
      (* Printf.eprintf "SHARE CMM: %i\n" i ; Printcmm.expression
         Format.str_formatter handler ; Printf.eprintf "%s\n"
         (Format.flush_str_formatter ()) ; *)
      ( i,
        fun body ->
          match body with
          | Cexit (j, _, _) -> if Lbl i = j then handler else body
          | _ -> ccatch (i, [], body, handler, dbg, kind, false) ))

  let make_exit i = Cexit (Lbl i, [], [])
end

(* cmm store, as sharing as normally been detected in previous phases, we only
   share exits *)
(* Some specific patterns can lead to switches where several cases point to the
   same action, but this action is not an exit (see GPR#1370). The addition of
   the index in the action array as context allows to share them correctly
   without duplication. *)
module StoreExpForSwitch = Switch.CtxStore (struct
  type t = expression

  type key_info =
    | Exit of int
    | Load of
        { block : Backend_var.t;
          byte_offset : int;
          memory_chunk : memory_chunk;
          mutability : Asttypes.mutable_flag;
          is_atomic : bool
        }

  type key = key_info option * int

  type context = int

  let make_key index expr =
    let continuation =
      match expr with
      | Cexit (Lbl i, [], []) -> Some (Exit i)
      | Cop (Cload { memory_chunk; mutability; is_atomic }, [Cvar block], _) ->
        Some
          (Load { block; byte_offset = 0; memory_chunk; mutability; is_atomic })
      | Cop
          ( Cload { memory_chunk; mutability; is_atomic },
            [Cop (Cadda, [Cvar block; Cconst_int (byte_offset, _)], _)],
            _ ) ->
        Some (Load { block; byte_offset; memory_chunk; mutability; is_atomic })
      | _ -> None
    in
    Some (continuation, index)

  let compare_key (cont, index) (cont', index') =
    match cont, cont' with
    | Some (Exit i), Some (Exit i') when i = i' -> 0
    (* CR mshinwell: seems like this should match [simple_and_equal_exprs] *)
    | ( Some (Load { block; byte_offset; memory_chunk; mutability; is_atomic }),
        Some
          (Load
            { block = block';
              byte_offset = byte_offset';
              memory_chunk = memory_chunk';
              mutability = mutability';
              is_atomic = is_atomic'
            }) ) ->
      let c = Backend_var.compare block block' in
      if c <> 0
      then c
      else
        let c = Stdlib.compare byte_offset byte_offset' in
        if c <> 0
        then c
        else
          let c = Stdlib.compare memory_chunk memory_chunk' in
          if c <> 0
          then c
          else
            let c = Stdlib.compare mutability mutability' in
            if c <> 0 then c else Stdlib.compare is_atomic is_atomic'
    | Some (Exit _), Some (Load _) -> -1
    | Some (Load _), Some (Exit _) -> 1
    | _, _ -> Stdlib.compare index index'
end)

module SwitcherBlocks = Switch.Make (SArgBlocks)

let transl_switch_clambda loc value_kind arg index cases =
  let store = StoreExpForSwitch.mk_store () in
  let index = Array.map (fun j -> store.Switch.act_store j cases.(j)) index in
  let n_index = Array.length index in
  let inters = ref []
  and this_high = ref (n_index - 1)
  and this_low = ref (n_index - 1)
  and this_act = ref index.(n_index - 1) in
  for i = n_index - 2 downto 0 do
    let act = index.(i) in
    if act = !this_act
    then decr this_low
    else (
      inters := (!this_low, !this_high, !this_act) :: !inters;
      this_high := i;
      this_low := i;
      this_act := act)
  done;
  inters := (0, !this_high, !this_act) :: !inters;
  match !inters with
  | [_] -> cases.(0)
  | inters ->
    bind "switcher" arg (fun a ->
        SwitcherBlocks.zyva loc value_kind
          (0, n_index - 1)
          a (Array.of_list inters) store)

let split_arity_for_apply arity args =
  (* Decides whether a caml_applyN needs to be split. If N <= max_arity, then
     keep caml_apply as is; otherwise, split at caml_apply[max_arity] *)
  let max_arity = Lambda.max_arity () in
  if List.compare_length_with arity max_arity <= 0
  then (arity, args), None
  else
    let a1, a2 = Misc.Stdlib.List.split_at max_arity arity in
    let args1, args2 = Misc.Stdlib.List.split_at max_arity args in
    (a1, args1), Some (a2, args2)

let call_caml_apply extended_ty extended_args_type mut clos args pos mode dbg =
  (* Treat tagged int arguments and results as [typ_val], to avoid generating
     excessive numbers of caml_apply functions. *)
  let ty = Extended_machtype.to_machtype extended_ty in
  let really_call_caml_apply clos args =
    let cargs =
      Cconst_symbol (apply_function_sym extended_args_type extended_ty mode, dbg)
      :: args
      @ [clos]
    in
    Cop (Capply (ty, pos), cargs, dbg)
  in
  if !Flambda_backend_flags.caml_apply_inline_fast_path
  then
    (* Generate the following expression:
     *  (if (= clos.arity N)
     *      (app clos.direct a1 ... aN clos)
     *      (app caml_applyN a1 .. aN clos)
     *)
    (* CR-someday gyorsh: in the [else] case above, call another version of
       caml_applyN that has only the cold path. *)
    bind_list "arg" args (fun args ->
        bind "fun" clos (fun clos ->
            Cifthenelse
              ( Cop
                  ( Ccmpi Ceq,
                    [ Cop
                        ( Casr,
                          [ get_field_gen mut clos 1 dbg;
                            Cconst_int (pos_arity_in_closinfo, dbg) ],
                          dbg );
                      Cconst_int (List.length extended_args_type, dbg) ],
                    dbg ),
                dbg,
                Cop
                  ( Capply (ty, pos),
                    (get_field_codepointer mut clos 2 dbg :: args) @ [clos],
                    dbg ),
                dbg,
                really_call_caml_apply clos args,
                dbg,
                Any )))
  else really_call_caml_apply clos args

(* CR mshinwell: These will be filled in by later pull requests. *)
let placeholder_dbg () = Debuginfo.none

let maybe_reset_current_region ~dbg ~body_tail ~body_nontail old_region =
  Cifthenelse
    ( Cop (Ccmpi Ceq, [old_region; Cop (Cbeginregion, [], dbg ())], dbg ()),
      dbg (),
      body_tail,
      dbg (),
      (let res = V.create_local "result" in
       Clet
         ( VP.create res,
           body_nontail,
           Csequence (Cop (Cendregion, [old_region], dbg ()), Cvar res) )),
      dbg (),
      Any )

let apply_or_call_caml_apply result arity mut clos args pos mode dbg =
  match args with
  | [arg] ->
    bind "fun" clos (fun clos ->
        Cop
          ( Capply (Extended_machtype.to_machtype result, pos),
            [get_field_codepointer mut clos 0 dbg; arg; clos],
            dbg ))
  | _ -> call_caml_apply result arity mut clos args pos mode dbg

let rec might_split_call_caml_apply ?old_region result arity mut clos args pos
    mode dbg =
  match split_arity_for_apply arity args with
  | (arity, args), None -> (
    match old_region with
    | None -> apply_or_call_caml_apply result arity mut clos args pos mode dbg
    | Some old_region ->
      maybe_reset_current_region ~dbg:placeholder_dbg
        ~body_tail:
          (apply_or_call_caml_apply result arity mut clos args pos mode dbg)
        ~body_nontail:
          (apply_or_call_caml_apply result arity mut clos args Rc_normal
             Lambda.alloc_local dbg)
        old_region)
  | (arity, args), Some (arity', args') -> (
    let body old_region =
      bind "result"
        (call_caml_apply [| Val |] arity mut clos args Rc_normal
           Lambda.alloc_local dbg) (fun clos ->
          might_split_call_caml_apply ?old_region result arity' mut clos args'
            pos mode dbg)
    in
    (* When splitting [caml_applyM] into [caml_applyN] and [caml_applyK] it is
       possible for [caml_applyN] to allocate on the local stack. If we are not
       careful the region might be closed once [caml_applyN] returns, which
       could produce a segfault or make subsequent loads read bad data.

       To avoid doing that, when splitting a [caml_apply], we check before
       calling the last [caml_apply] if we allocated on the local stack; and if
       so, we close the region ourselves afterwards, as is already done inside
       [caml_apply]. *)
    match old_region, mode with
    | None, Lambda.Alloc_heap when Config.stack_allocation ->
      let dbg = placeholder_dbg in
      bind "region"
        (Cop (Cbeginregion, [], dbg ()))
        (fun region -> body (Some region))
    | _ -> body old_region)

let generic_apply mut clos args args_type result (pos, mode) dbg =
  might_split_call_caml_apply result args_type mut clos args pos mode dbg

let send kind met obj args args_type result akind dbg =
  let call_met obj args args_type clos =
    (* met is never a simple expression, so it never gets turned into an
       Immutable load *)
    generic_apply Asttypes.Mutable clos (obj :: args)
      (Extended_machtype.typ_val :: args_type)
      result akind dbg
  in
  bind "obj" obj (fun obj ->
      match (kind : Lambda.meth_kind), args, args_type with
      | Self, _, _ ->
        bind "met" (lookup_label obj met dbg) (call_met obj args args_type)
      | Cached, cache :: pos :: args, _ :: _ :: args_type ->
        call_cached_method obj met cache pos args args_type result akind dbg
      | _ -> bind "met" (lookup_tag obj met dbg) (call_met obj args args_type))

(*
 * CAMLprim value caml_cache_public_method (value meths, value tag,
 *                                          value *cache)
 * {
 *   int li = 3, hi = Field(meths,0), mi;
 *   while (li < hi) { // no need to check the 1st time
 *     mi = ((li+hi) >> 1) | 1;
 *     if (tag < Field(meths,mi)) hi = mi-2;
 *     else li = mi;
 *   }
 *   *cache = (li-3)*sizeof(value)+1;
 *   return Field (meths, li-1);
 * }
 *)

let cache_public_method meths tag cache dbg =
  let raise_num = Lambda.next_raise_count () in
  let cconst_int i = Cconst_int (i, dbg) in
  let li = V.create_local "*li*"
  and hi = V.create_local "*hi*"
  and mi = V.create_local "*mi*"
  and tagged = V.create_local "*tagged*" in
  Clet_mut
    ( VP.create li,
      typ_int,
      cconst_int 3,
      Clet_mut
        ( VP.create hi,
          typ_int,
          Cop (mk_load_mut Word_int, [meths], dbg),
          Csequence
            ( ccatch
                ( raise_num,
                  [],
                  create_loop
                    (Clet
                       ( VP.create mi,
                         Cop
                           ( Cor,
                             [ Cop
                                 ( Clsr,
                                   [ Cop (Caddi, [Cvar li; Cvar hi], dbg);
                                     cconst_int 1 ],
                                   dbg );
                               cconst_int 1 ],
                             dbg ),
                         Csequence
                           ( Cifthenelse
                               ( Cop
                                   ( Ccmpi Clt,
                                     [ tag;
                                       Cop
                                         ( mk_load_mut Word_int,
                                           [ Cop
                                               ( Cadda,
                                                 [ meths;
                                                   lsl_const (Cvar mi)
                                                     log2_size_addr dbg ],
                                                 dbg ) ],
                                           dbg ) ],
                                     dbg ),
                                 dbg,
                                 Cassign
                                   ( hi,
                                     Cop (Csubi, [Cvar mi; cconst_int 2], dbg)
                                   ),
                                 dbg,
                                 Cassign (li, Cvar mi),
                                 dbg,
                                 Any ),
                             Cifthenelse
                               ( Cop (Ccmpi Cge, [Cvar li; Cvar hi], dbg),
                                 dbg,
                                 Cexit (Lbl raise_num, [], []),
                                 dbg,
                                 Ctuple [],
                                 dbg,
                                 Any ) ) ))
                    dbg,
                  Ctuple [],
                  dbg,
                  Any,
                  false ),
              Clet
                ( VP.create tagged,
                  Cop
                    ( Caddi,
                      [ lsl_const (Cvar li) log2_size_addr dbg;
                        cconst_int (1 - (3 * size_addr)) ],
                      dbg ),
                  Csequence
                    ( Cop
                        ( Cstore (Word_int, Assignment),
                          [cache; Cvar tagged],
                          dbg ),
                      Cvar tagged ) ) ) ) )

let placeholder_fun_dbg ~human_name:_ = Debuginfo.none

(* Generate an application function:
 *  (defun caml_applyN (a1 ... aN clos)
 *    (if (= clos.arity N)
 *      (app clos.direct a1 ... aN clos)
 *      (let (clos1 (app clos.code a1 clos)
 *            clos2 (app clos1.code a2 clos)
 *            ...
 *            closN-1 (app closN-2.code aN-1 closN-2))
 *        (app closN-1.code aN closN-1))))
 *)

let apply_function_body arity result (mode : Lambda.alloc_mode) =
  let dbg = placeholder_dbg in
  let args = List.map (fun _ -> V.create_local "arg") arity in
  let clos = V.create_local "clos" in
  (* In the slowpath, a region is necessary in case the initial applications do
     local allocations *)
  let region =
    if not Config.stack_allocation
    then None
    else
      match mode with
      | Alloc_heap -> Some (V.create_local "region")
      | Alloc_local -> None
  in
  let rec app_fun clos args =
    match args with
    | [] -> Misc.fatal_error "apply_function_body for empty arity"
    | [arg] -> (
      let app =
        Cop
          ( Capply (result, Rc_normal),
            [ get_field_codepointer Asttypes.Mutable (Cvar clos) 0 (dbg ());
              Cvar arg;
              Cvar clos ],
            dbg () )
      in
      match region with
      | None -> app
      | Some region ->
        (* To preserve tail-call behaviour, we do a runtime check whether
           anything has been allocated in [region]. If not, then we can do a
           direct tail call without waiting to end the region afterwards. *)
        maybe_reset_current_region ~dbg ~body_tail:app ~body_nontail:app
          (Cvar region))
    | arg :: args ->
      let newclos = V.create_local "clos" in
      Clet
        ( VP.create newclos,
          Cop
            ( Capply (typ_val, Rc_normal),
              [ get_field_codepointer Asttypes.Mutable (Cvar clos) 0 (dbg ());
                Cvar arg;
                Cvar clos ],
              dbg () ),
          app_fun newclos args )
  in
  let code =
    match region with
    | None -> app_fun clos args
    | Some reg ->
      Clet (VP.create reg, Cop (Cbeginregion, [], dbg ()), app_fun clos args)
  in
  let all_args = args @ [clos] in
  ( args,
    clos,
    if List.compare_length_with arity 1 = 0
    then code
    else
      Cifthenelse
        ( Cop
            ( Ccmpi Ceq,
              [ Cop
                  ( Casr,
                    [ get_field_gen Asttypes.Mutable (Cvar clos) 1 (dbg ());
                      Cconst_int (pos_arity_in_closinfo, dbg ()) ],
                    dbg () );
                Cconst_int (List.length arity, dbg ()) ],
              dbg () ),
          dbg (),
          Cop
            ( Capply (result, Rc_normal),
              get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ())
              :: List.map (fun s -> Cvar s) all_args,
              dbg () ),
          dbg (),
          code,
          dbg (),
          Any ) )

let send_function (arity, result, mode) =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let args, clos', body = apply_function_body (typ_val :: arity) result mode in
  let cache = V.create_local "cache"
  and obj = List.hd args
  and tag = V.create_local "tag" in
  let clos =
    let cache = Cvar cache and obj = Cvar obj and tag = Cvar tag in
    let meths = V.create_local "meths" and cached = V.create_local "cached" in
    let real = V.create_local "real" in
    let mask = get_field_gen Asttypes.Mutable (Cvar meths) 1 (dbg ()) in
    let cached_pos = Cvar cached in
    let tag_pos =
      Cop
        ( Cadda,
          [ Cop (Cadda, [cached_pos; Cvar meths], dbg ());
            cconst_int ((3 * size_addr) - 1) ],
          dbg () )
    in
    let tag' = Cop (mk_load_mut Word_int, [tag_pos], dbg ()) in
    Clet
      ( VP.create meths,
        Cop (mk_load_mut Word_val, [obj], dbg ()),
        Clet
          ( VP.create cached,
            Cop
              (Cand, [Cop (mk_load_mut Word_int, [cache], dbg ()); mask], dbg ()),
            Clet
              ( VP.create real,
                Cifthenelse
                  ( Cop (Ccmpa Cne, [tag'; tag], dbg ()),
                    dbg (),
                    cache_public_method (Cvar meths) tag cache (dbg ()),
                    dbg (),
                    cached_pos,
                    dbg (),
                    Any ),
                Cop
                  ( mk_load_mut Word_val,
                    [ Cop
                        ( Cadda,
                          [ Cop (Cadda, [Cvar real; Cvar meths], dbg ());
                            cconst_int ((2 * size_addr) - 1) ],
                          dbg () ) ],
                    dbg () ) ) ) )
  in
  let body = Clet (VP.create clos', clos, body) in
  let cache = cache in
  let fun_name = send_function_name arity result mode in
  let fun_args =
    [obj, typ_val; tag, typ_int; cache, typ_addr]
    @ List.combine (List.tl args) arity
  in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) fun_args;
      fun_body = body;
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

let apply_function (arity, result, mode) =
  let args, clos, body = apply_function_body arity result mode in
  let all_args = List.combine args arity @ [clos, typ_val] in
  let fun_name = global_symbol (apply_function_name arity result mode) in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) all_args;
      fun_body = body;
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

(* Generate tuplifying functions:
 *    (defun caml_tuplifyN (arg clos)
 *      (app clos.direct #0(arg) ... #N-1(arg) clos))
 *)

let tuplify_function arity return =
  if List.exists (function [| Val |] | [| Int |] -> false | _ -> true) arity
  then
    Misc.fatal_error
      "tuplify_function is currently unsupported if arity contains non-values";
  let arity = List.length arity in
  let dbg = placeholder_dbg in
  let arg = V.create_local "arg" in
  let clos = V.create_local "clos" in
  let rec access_components i =
    if i >= arity
    then []
    else
      get_field_gen Asttypes.Mutable (Cvar arg) i (dbg ())
      :: access_components (i + 1)
  in
  let fun_name = global_symbol (tuplify_function_name arity return) in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
      fun_body =
        Cop
          ( Capply (return, Rc_normal),
            get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ())
            :: access_components 0
            @ [Cvar clos],
            dbg () );
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

(* Generate currying functions:
 *
 *   (defun caml_curryN (arg clos)
 *      (alloc HDR caml_curryN_1 <arity (N-1)> caml_curry_N_1_app arg clos))
 *   (defun caml_curryN_1 (arg clos)
 *      (alloc HDR caml_curryN_2 <arity (N-2)> caml_curry_N_2_app arg clos))
 *   ...
 *   (defun caml_curryN_N-1 (arg clos)
 *      (let (closN-2 clos.vars[1]
 *            closN-3 closN-2.vars[1]
 *            ...
 *            clos1 clos2.vars[1]
 *            clos clos1.vars[1])
 *        (app clos.direct
 *             clos1.vars[0] ... closN-2.vars[0] clos.vars[0] arg clos)))
 *
 * Special "shortcut" functions are also generated to handle the case where a
 * partially applied function is applied to all remaining arguments in one go.
 *
 *   (defun caml_curry_N_1_app (arg2 ... argN clos)
 *     (let clos' clos.vars[1]
 *        (app clos'.direct clos.vars[0] arg2 ... argN clos')))
 *
 * Those shortcuts may lead to a quadratic number of application primitives
 * being generated in the worst case, which resulted in linking time blowup in
 * practice (PR#5933), so we only generate and use them when below a fixed arity
 * 'max_arity_optimized'. *)

let max_arity_optimized = 15

let ints_per_float = size_float / Arch.size_int

let ints_per_vec128 = size_vec128 / Arch.size_int

let machtype_stored_size t =
  Array.fold_left
    (fun cur c ->
      match c with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Val | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Vec128 -> cur + ints_per_vec128)
    0 t

let machtype_non_scanned_size t =
  Array.fold_left
    (fun cur c ->
      match c with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Val -> cur
      | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Vec128 -> cur + ints_per_vec128)
    0 t

let make_tuple l = match l with [e] -> e | _ -> Ctuple l

let value_slot_given_machtype vs =
  let non_scanned, scanned =
    List.partition
      (fun (_, c) ->
        match c with
        | Int | Float | Vec128 -> true
        | Val -> false
        | Addr -> assert false)
      vs
  in
  List.map (fun (v, _) -> Cvar v) (non_scanned @ scanned)

let read_from_closure_given_machtype t clos base_offset dbg =
  let load chunk offset =
    Cop (mk_load_mut chunk, [field_address clos offset dbg], dbg)
  in
  let _, l =
    List.fold_left_map
      (fun (non_scanned_pos, scanned_pos) c ->
        match c with
        | Int ->
          (non_scanned_pos + 1, scanned_pos), load Word_int non_scanned_pos
        | Float ->
          ( (non_scanned_pos + ints_per_float, scanned_pos),
            load Double non_scanned_pos )
        | Vec128 ->
          (* Vectors stored in closures may not be 16-byte aligned. *)
          ( (non_scanned_pos + ints_per_vec128, scanned_pos),
            load Onetwentyeight_unaligned non_scanned_pos )
        | Val -> (non_scanned_pos, scanned_pos + 1), load Word_val scanned_pos
        | Addr -> Misc.fatal_error "[Addr] cannot be read")
      (base_offset, base_offset + machtype_non_scanned_size t)
      (Array.to_list t)
  in
  make_tuple l

let curry_clos_has_nary_application ~narity n =
  narity <= max_arity_optimized && n < narity - 1

let rec make_curry_apply result narity args_type args clos n =
  let dbg = placeholder_dbg in
  match args_type with
  | [] ->
    Cop
      ( Capply (result, Rc_normal),
        (get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ()) :: args)
        @ [Cvar clos],
        dbg () )
  | arg_type :: args_type ->
    let newclos = V.create_local "clos" in
    let arg_pos = if curry_clos_has_nary_application ~narity n then 3 else 2 in
    let clos_pos = arg_pos + machtype_stored_size arg_type in
    Clet
      ( VP.create newclos,
        get_field_gen Asttypes.Mutable (Cvar clos) clos_pos (dbg ()),
        make_curry_apply result narity args_type
          (read_from_closure_given_machtype arg_type (Cvar clos) arg_pos
             (dbg ())
          :: args)
          newclos (n - 1) )

let final_curry_function nlocal arity result =
  let last_arg = V.create_local "arg" in
  let last_clos = V.create_local "clos" in
  let narity = List.length arity in
  let fun_name =
    global_symbol
      (curry_function_sym_name (Lambda.Curried { nlocal }) arity result
      ^ "_"
      ^ Int.to_string (narity - 1))
  in
  let args_type = List.rev arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args =
        [VP.create last_arg, List.hd args_type; VP.create last_clos, typ_val];
      fun_body =
        make_curry_apply result narity (List.tl args_type) [Cvar last_arg]
          last_clos (narity - 1);
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

let intermediate_curry_functions ~nlocal ~arity result =
  let name1 =
    curry_function_sym_name (Lambda.Curried { nlocal }) arity result
  in
  let narity = List.length arity in
  let dbg = placeholder_dbg in
  let rec loop accumulated_args remaining_args num =
    match remaining_args with
    | [] -> Misc.fatal_error "Empty arity for [intermediate_curry_functions]"
    | [_] -> [final_curry_function nlocal arity result]
    | arg_type :: remaining_args ->
      let name2 = if num = 0 then name1 else name1 ^ "_" ^ Int.to_string num in
      let clos = V.create_local "clos" in
      let args =
        List.init (Array.length arg_type) (fun i ->
            V.create_local "arg", arg_type.(i))
      in
      let fun_dbg = placeholder_fun_dbg ~human_name:name2 in
      let mode : Lambda.alloc_mode =
        if num >= narity - nlocal then Lambda.alloc_local else Lambda.alloc_heap
      in
      let has_nary = curry_clos_has_nary_application ~narity (num + 1) in
      let function_slot_size = if has_nary then 3 else 2 in
      Cfunction
        { fun_name = global_symbol name2;
          fun_args =
            List.map (fun (arg, t) -> VP.create arg, [| t |]) args
            @ [VP.create clos, typ_val];
          fun_body =
            Cop
              ( Calloc mode,
                [ alloc_closure_header ~mode
                    (function_slot_size + machtype_stored_size arg_type + 1)
                    (dbg ());
                  Cconst_symbol
                    ( global_symbol (name1 ^ "_" ^ Int.to_string (num + 1)),
                      dbg () );
                  Cconst_natint
                    ( pack_closure_info
                        ~arity:(if has_nary then narity - num - 1 else 1)
                        ~startenv:
                          (function_slot_size
                          + machtype_non_scanned_size arg_type)
                        ~is_last:true,
                      dbg () ) ]
                @ (if has_nary
                  then
                    [ Cconst_symbol
                        ( global_symbol
                            (name1 ^ "_" ^ Int.to_string (num + 1) ^ "_app"),
                          dbg () ) ]
                  else [])
                @ value_slot_given_machtype args
                @ [Cvar clos],
                dbg () );
          fun_codegen_options = [];
          fun_dbg;
          fun_poll = Default_poll
        }
      ::
      (if has_nary
      then
        let direct_args =
          List.mapi
            (fun i ty ->
              V.create_local (Printf.sprintf "arg%d" (i + num + 2)), ty)
            remaining_args
        in
        let fun_args =
          List.map
            (fun (arg, ty) -> VP.create arg, ty)
            (direct_args @ [clos, typ_val])
        in
        let fun_name =
          global_symbol (name1 ^ "_" ^ Int.to_string (num + 1) ^ "_app")
        in
        let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
        let cf =
          Cfunction
            { fun_name;
              fun_args;
              fun_body =
                make_curry_apply result narity
                  (arg_type :: accumulated_args)
                  (List.map (fun (arg, _) -> Cvar arg) direct_args)
                  clos (num + 1);
              fun_codegen_options = [];
              fun_dbg;
              fun_poll = Default_poll
            }
        in
        [cf]
      else [])
      @ loop (arg_type :: accumulated_args) remaining_args (num + 1)
  in
  loop [] arity 0

let curry_function (kind, arity, return) =
  match kind with
  | Lambda.Tupled -> [tuplify_function arity return]
  | Lambda.Curried { nlocal } ->
    intermediate_curry_functions ~nlocal ~arity return

(* Primitives *)

type unary_primitive = expression -> Debuginfo.t -> expression

let int_as_pointer arg dbg = Cop (Caddi, [arg; Cconst_int (-1, dbg)], dbg)
(* always a pointer outside the heap *)

let raise_prim raise_kind arg dbg =
  if !Clflags.debug
  then Cop (Craise raise_kind, [arg], dbg)
  else Cop (Craise Lambda.Raise_notrace, [arg], dbg)

let negint arg dbg = Cop (Csubi, [Cconst_int (2, dbg); arg], dbg)

let arraylength kind arg dbg =
  let hdr = get_header_masked arg dbg in
  match (kind : Lambda.array_kind) with
  | Pgenarray ->
    let len =
      if wordsize_shift = numfloat_shift
      then Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
      else
        bind "header" hdr (fun hdr ->
            Cifthenelse
              ( is_addr_array_hdr hdr dbg,
                dbg,
                Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg),
                dbg,
                Cop (Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg),
                dbg,
                Any ))
    in
    Cop (Cor, [len; Cconst_int (1, dbg)], dbg)
  | Paddrarray | Pintarray ->
    Cop (Cor, [addr_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)
  | Pfloatarray | Punboxedfloatarray ->
    (* Note: we only support 64 bit targets now, so this is ok for
       Punboxedfloatarray *)
    Cop (Cor, [float_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)
  | Punboxedintarray Pint64 | Punboxedintarray Pnativeint ->
    unboxed_int64_or_nativeint_array_length arg dbg
  | Punboxedintarray Pint32 -> unboxed_int32_array_length arg dbg

(* CR-soon gyorsh: effects and coeffects for primitives are set conservatively
   to Arbitrary_effects and Has_coeffects, resp. Check if this can be improved
   (e.g., bswap). *)

let bbswap bi arg dbg =
  let bitwidth : Cmm.bswap_bitwidth =
    match (bi : Primitive.boxed_integer) with
    | Pnativeint -> if size_int = 4 then Thirtytwo else Sixtyfour
    | Pint32 -> Thirtytwo
    | Pint64 -> Sixtyfour
  in
  let op = Cbswap { bitwidth } in
  if (bi = Primitive.Pint64 && size_int = 4)
     || not (Proc.operation_supported op)
  then
    let prim, tyarg =
      match (bi : Primitive.boxed_integer) with
      | Pnativeint -> "nativeint", XInt
      | Pint32 -> "int32", XInt32
      | Pint64 -> "int64", XInt64
    in
    Cop
      ( Cextcall
          { func = Printf.sprintf "caml_%s_direct_bswap" prim;
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty = typ_int;
            alloc = false;
            ty_args = [tyarg]
          },
        [arg],
        dbg )
  else Cop (op, [arg], dbg)

let bswap16 arg dbg =
  let op = Cbswap { bitwidth = Cmm.Sixteen } in
  if Proc.operation_supported op
  then Cop (op, [arg], dbg)
  else
    Cop
      ( Cextcall
          { func = "caml_bswap16_direct";
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty = typ_int;
            alloc = false;
            ty_args = []
          },
        [arg],
        dbg )

type binary_primitive = expression -> expression -> Debuginfo.t -> expression

(* Helper for compilation of initialization and assignment operations *)

type assignment_kind =
  | Caml_modify
  | Caml_modify_local
  | Caml_initialize (* never local *)
  | Simple of initialization_or_assignment

let assignment_kind (ptr : Lambda.immediate_or_pointer)
    (init : Lambda.initialization_or_assignment) =
  match init, ptr with
  | Assignment Modify_heap, Pointer -> Caml_modify
  | Assignment Modify_maybe_stack, Pointer ->
    assert Config.stack_allocation;
    Caml_modify_local
  | Heap_initialization, Pointer -> Caml_initialize
  | Root_initialization, Pointer ->
    if Config.runtime5 then Caml_initialize else Simple Initialization
  | Assignment _, Immediate -> Simple Assignment
  | Heap_initialization, Immediate | Root_initialization, Immediate ->
    Simple Initialization

let setfield n ptr init arg1 arg2 dbg =
  match assignment_kind ptr init with
  | Caml_modify ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_modify";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [field_address arg1 n dbg; arg2],
           dbg ))
  | Caml_modify_local ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_modify_local";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [arg1; Cconst_int (n, dbg); arg2],
           dbg ))
  | Caml_initialize ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_initialize";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [field_address arg1 n dbg; arg2],
           dbg ))
  | Simple init -> return_unit dbg (set_field arg1 n arg2 init dbg)

let add_int_caml arg1 arg2 dbg = decr_int (add_int arg1 arg2 dbg) dbg

let sub_int_caml arg1 arg2 dbg = incr_int (sub_int arg1 arg2 dbg) dbg

let mul_int_caml arg1 arg2 dbg =
  (* decrementing the non-constant part helps when the multiplication is
     followed by an addition; for example, using this trick compiles

     (100 * a + 7)

     into

     (+ ( * a 100) -85)

     rather than

     (+ ( * 200 (>>s a 1)) 15) *)
  match arg1, arg2 with
  | (Cconst_int _ as c1), c2 ->
    incr_int (mul_int (untag_int c1 dbg) (decr_int c2 dbg) dbg) dbg
  | c1, c2 -> incr_int (mul_int (decr_int c1 dbg) (untag_int c2 dbg) dbg) dbg

let div_int_caml is_safe arg1 arg2 dbg =
  tag_int (div_int (untag_int arg1 dbg) (untag_int arg2 dbg) is_safe dbg) dbg

let mod_int_caml is_safe arg1 arg2 dbg =
  tag_int (mod_int (untag_int arg1 dbg) (untag_int arg2 dbg) is_safe dbg) dbg

let and_int_caml arg1 arg2 dbg = and_int arg1 arg2 dbg

let or_int_caml arg1 arg2 dbg = or_int arg1 arg2 dbg

let xor_int_caml arg1 arg2 dbg =
  Cop
    ( Cor,
      [ xor_int (ignore_low_bit_int arg1) (ignore_low_bit_int arg2) dbg;
        Cconst_int (1, dbg) ],
      dbg )

let lsl_int_caml arg1 arg2 dbg =
  incr_int (lsl_int (decr_int arg1 dbg) (untag_int arg2 dbg) dbg) dbg

let lsr_int_caml arg1 arg2 dbg =
  Cop (Cor, [lsr_int arg1 (untag_int arg2 dbg) dbg; Cconst_int (1, dbg)], dbg)

let asr_int_caml arg1 arg2 dbg =
  Cop (Cor, [asr_int arg1 (untag_int arg2 dbg) dbg; Cconst_int (1, dbg)], dbg)

type ternary_primitive =
  expression -> expression -> expression -> Debuginfo.t -> expression

let setfield_computed ptr init arg1 arg2 arg3 dbg =
  match assignment_kind ptr init with
  | Caml_modify -> return_unit dbg (addr_array_set_heap arg1 arg2 arg3 dbg)
  | Caml_modify_local ->
    return_unit dbg (addr_array_set_local arg1 arg2 arg3 dbg)
  | Caml_initialize ->
    return_unit dbg (addr_array_initialize arg1 arg2 arg3 dbg)
  | Simple _ -> return_unit dbg (int_array_set arg1 arg2 arg3 dbg)

(* Symbols *)

let cdefine_symbol sym = [Cdefine_symbol sym]

let emit_block symb white_header cont =
  (* Headers for structured constants must be marked black in case we are in
     no-naked-pointers mode. See [caml_darken]. *)
  let black_header = Nativeint.logor white_header caml_black in
  (Cint black_header :: cdefine_symbol symb) @ cont

let emit_string_constant_fields s cont =
  let n = size_int - 1 - (String.length s mod size_int) in
  Cstring s :: Cskip n :: Cint8 n :: cont

let emit_boxed_int32_constant_fields n cont =
  let n = Nativeint.of_int32 n in
  Csymbol_address (global_symbol caml_int32_ops)
  :: Cint32 n :: Cint32 0n :: cont

let emit_boxed_int64_constant_fields n cont =
  let lo = Int64.to_nativeint n in
  Csymbol_address (global_symbol caml_int64_ops) :: Cint lo :: cont

let emit_boxed_nativeint_constant_fields n cont =
  Csymbol_address (global_symbol caml_nativeint_ops) :: Cint n :: cont

let emit_float_constant symb f cont =
  emit_block symb float_header (Cdouble f :: cont)

let emit_string_constant symb s cont =
  emit_block symb
    (string_header (String.length s))
    (emit_string_constant_fields s cont)

let emit_int32_constant symb n cont =
  emit_block symb boxedint32_header (emit_boxed_int32_constant_fields n cont)

let emit_int64_constant symb n cont =
  emit_block symb boxedint64_header (emit_boxed_int64_constant_fields n cont)

let emit_nativeint_constant symb n cont =
  emit_block symb boxedintnat_header
    (emit_boxed_nativeint_constant_fields n cont)

let emit_vec128_constant symb bits cont =
  emit_block symb boxedvec128_header (Cvec128 bits :: cont)

let emit_float_array_constant symb fields cont =
  emit_block symb
    (floatarray_header (List.length fields))
    (Misc.map_end (fun f -> Cdouble f) fields cont)

let make_symbol ?compilation_unit name =
  let compilation_unit =
    match compilation_unit with
    | None -> Compilation_unit.get_current_exn ()
    | Some compilation_unit -> compilation_unit
  in
  Symbol.for_name compilation_unit name
  |> Symbol.linkage_name |> Linkage_name.to_string

(* Generate the entry point *)
(*
 * CAMLprim value caml_program()
 * {
 *   int id = 0;
 *   while (true) {
 *     if (id == len_caml_globals_entry_functions) goto out;
 *     caml_globals_entry_functions[id]();
 *     caml_globals_inited += 1;
 *     id += 1;
 *   }
 *   out:
 *   return 1;
 * }
 *)
let entry_point namelist =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let cconst_symbol sym = Cconst_symbol (sym, dbg ()) in
  let incr_global_inited () =
    Cop
      ( Cstore (Word_int, Assignment),
        [ cconst_symbol (global_symbol "caml_globals_inited");
          Cop
            ( Caddi,
              [ Cop
                  ( mk_load_mut Word_int,
                    [cconst_symbol (global_symbol "caml_globals_inited")],
                    dbg () );
                cconst_int 1 ],
              dbg () ) ],
        dbg () )
  in
  let table_symbol = global_symbol "caml_globals_entry_functions" in
  let call i =
    let f =
      Cop
        ( Cadda,
          [ cconst_symbol table_symbol;
            Cop (Cmuli, [Cconst_int (Arch.size_addr, dbg ()); i], dbg ()) ],
          dbg () )
    in
    Csequence
      ( Cop
          ( Capply (typ_void, Rc_normal),
            [Cop (mk_load_immut Word_int, [f], dbg ())],
            dbg () ),
        incr_global_inited () )
  in
  let data =
    List.map
      (fun name ->
        Csymbol_address
          (global_symbol (make_symbol ~compilation_unit:name "entry")))
      namelist
  in
  let data = Cdefine_symbol table_symbol :: data in
  let raise_num = Lambda.next_raise_count () in
  let id = VP.create (Ident.create_local "*id*") in
  let high = cconst_int (List.length namelist) in
  let body =
    let dbg = dbg () in
    let incr_i =
      Cassign
        (VP.var id, Cop (Caddi, [Cvar (VP.var id); Cconst_int (1, dbg)], dbg))
    in
    let exit_if_last_iteration =
      Cifthenelse
        ( Cop (Ccmpi Ceq, [Cvar (VP.var id); high], dbg),
          dbg,
          Cexit (Lbl raise_num, [], []),
          dbg,
          Ctuple [],
          dbg,
          Any )
    in
    Clet_mut
      ( id,
        typ_int,
        cconst_int 0,
        ccatch
          ( raise_num,
            [],
            create_loop
              (Csequence
                 ( exit_if_last_iteration,
                   Csequence (call (Cvar (VP.var id)), incr_i) ))
              dbg,
            Ctuple [],
            dbg,
            Any,
            false ) )
  in
  let fun_name = global_symbol "caml_program" in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  [ Cdata data;
    Cfunction
      { fun_name;
        fun_args = [];
        fun_body = Csequence (body, cconst_int 1);
        fun_codegen_options = [Reduce_code_size; Use_linscan_regalloc];
        fun_dbg;
        fun_poll = Default_poll
      } ]

(* Generate the table of globals *)

let cint_zero = Cint 0n

let global_table namelist =
  let mksym name =
    Csymbol_address
      (global_symbol (make_symbol ~compilation_unit:name "gc_roots"))
  in
  Cdata
    ((Cdefine_symbol (global_symbol "caml_globals") :: List.map mksym namelist)
    @ [cint_zero])

let reference_symbols namelist =
  let mksym name = Csymbol_address name in
  Cdata (List.map mksym namelist)

let global_data sym_name v =
  let symbol = { sym_name; sym_global = Global } in
  Cdata (emit_string_constant symbol (Marshal.to_string v []) [])

let globals_map v = global_data "caml_globals_map" v

(* Generate the master table of frame descriptors *)

let frame_table namelist =
  let mksym name =
    Csymbol_address
      (global_symbol (make_symbol ~compilation_unit:name "frametable"))
  in
  Cdata
    (Cdefine_symbol (global_symbol "caml_frametable")
     :: List.map mksym namelist
    @ [cint_zero])

(* Generate the table of module data and code segments *)

let segment_table namelist symbol begname endname =
  let addsyms name lst =
    Csymbol_address (global_symbol (make_symbol ~compilation_unit:name begname))
    :: Csymbol_address
         (global_symbol (make_symbol ~compilation_unit:name endname))
    :: lst
  in
  Cdata
    (Cdefine_symbol (global_symbol symbol)
    :: List.fold_right addsyms namelist [cint_zero])

let data_segment_table namelist =
  segment_table namelist "caml_data_segments" "data_begin" "data_end"

let code_segment_table namelist =
  segment_table namelist "caml_code_segments" "code_begin" "code_end"

(* Initialize a predefined exception *)

let predef_exception i name =
  let name_sym =
    { sym_name = Compilenv.new_const_symbol (); sym_global = Local }
  in
  let data_items = emit_string_constant name_sym name [] in
  let exn_sym = global_symbol ("caml_exn_" ^ name) in
  let tag = Obj.object_tag in
  let size = 2 in
  let fields = Csymbol_address name_sym :: cint_const (-i - 1) :: data_items in
  let data_items = emit_block exn_sym (block_header tag size) fields in
  Cdata data_items

(* Header for a plugin *)

let plugin_header units =
  global_data "caml_plugin_header"
    ({ dynu_magic = Config.cmxs_magic_number; dynu_units = units }
      : Cmxs_format.dynheader)

(* Build the NULL terminated array of gc roots *)

let emit_gc_roots_table ~symbols cont =
  let table_symbol = global_symbol (make_symbol "gc_roots") in
  Cdata
    (Cdefine_symbol table_symbol
     :: List.map (fun s -> Csymbol_address s) symbols
    @ [Cint 0n])
  :: cont

(* Helper functions and values used by Flambda 2. *)

let void = Ctuple []

let unit ~dbg = Cconst_int (1, dbg)

let var v = Cvar v

let symbol ~dbg sym = Cconst_symbol (sym, dbg)

let float ~dbg f = Cconst_float (f, dbg)

let int32 ~dbg i = natint_const_untagged dbg (Nativeint.of_int32 i)

(* CR Gbury: this conversion int64 -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int64 ~dbg i = natint_const_untagged dbg (Int64.to_nativeint i)

let vec128 ~dbg bits = Cconst_vec128 (bits, dbg)

let nativeint ~dbg i = natint_const_untagged dbg i

let letin v ~defining_expr ~body =
  match body with
  | Cvar v' when Backend_var.same (Backend_var.With_provenance.var v) v' ->
    defining_expr
  | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_vec128 _ | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _
  | Ctuple _ | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _
  | Cexit _ | Ctrywith _ ->
    Clet (v, defining_expr, body)

let letin_mut v ty e body = Clet_mut (v, ty, e, body)

let assign x e = Cassign (x, e)

let sequence x y =
  match x, y with
  | Ctuple [], _ -> y
  | _, Ctuple [] -> x
  | _, _ -> Csequence (x, y)

let ite ~dbg ~then_dbg ~then_ ~else_dbg ~else_ cond =
  Cifthenelse (cond, then_dbg, then_, else_dbg, else_, dbg, Any)

let trywith ~dbg ~body ~exn_var ~handler_cont ~handler () =
  Ctrywith (body, handler_cont, exn_var, handler, dbg, Any)

type static_handler =
  int
  * (Backend_var.With_provenance.t * Cmm.machtype) list
  * Cmm.expression
  * Debuginfo.t
  * bool

let handler ~dbg id vars body is_cold = id, vars, body, dbg, is_cold

let cexit id args trap_actions = Cmm.Cexit (Cmm.Lbl id, args, trap_actions)

let trap_return arg trap_actions =
  Cmm.Cexit (Cmm.Return_lbl, [arg], trap_actions)

let create_ccatch ~rec_flag ~handlers ~body =
  let rec_flag = if rec_flag then Cmm.Recursive else Cmm.Nonrecursive in
  Cmm.Ccatch (rec_flag, handlers, body, Any)

let unary op ~dbg x = Cop (op, [x], dbg)

let binary op ~dbg x y = Cop (op, [x; y], dbg)

let int_of_float = unary Cintoffloat

let float_of_int = unary Cfloatofint

let lsl_int_caml_raw ~dbg arg1 arg2 =
  incr_int (lsl_int (decr_int arg1 dbg) arg2 dbg) dbg

let lsr_int_caml_raw ~dbg arg1 arg2 =
  Cop (Cor, [lsr_int arg1 arg2 dbg; Cconst_int (1, dbg)], dbg)

let asr_int_caml_raw ~dbg arg1 arg2 =
  Cop (Cor, [asr_int arg1 arg2 dbg; Cconst_int (1, dbg)], dbg)

let eq ~dbg x y =
  match x, y with
  | Cconst_int (n, _), Cop (Csubi, [Cconst_int (m, _); c], _)
  | Cop (Csubi, [Cconst_int (m, _); c], _), Cconst_int (n, _)
    when Misc.no_overflow_sub m n ->
    (* [n = m - c] <=> [c = m - n]

       This is typically generated by expressions of the form [if not expr then
       ...], with [not expr] being compiled to [4 - c] and the condition for the
       test becomes [1 = 4 - c].

       We need to impose the side condition because the above equivalence hides
       a subtlety: While [c] is a full-blooded native integer, [m] and [n] are
       OCaml ints that will be sign-extended between now and run time. That in
       itself doesn't break the equivalence. The problem is that we intend to
       compute [m - n] right now, while [m] and [n] are one bit shorter. Thus
       there's a bit of sleight of hand going on: the [m - n] we compute now may
       not be the [m - n] that appears in the equivalence. [m - c], however,
       _is_ subtraction of full native ints (it must be, since [c] can be any
       native int). So [m - c] and [m - n] refer to two different operations and
       we're cheekily swapping one for the other. We'll get away with it,
       however, _so long as [m - n] doesn't overflow_.

       Formally, writing [se] for sign extension, we can write a version of our
       equivalence that's unconditionally true: [se(n) = se(m) - c] <=> [c =
       se(m) - se(n)], where now [-] consistently means subtraction of native
       ints. Effectively, we intend to write [c = se(m - n)] in the compiled
       code (here [-] is instead subtraction of OCaml ints). This is the same as
       [c = se(m) - se(n)] exactly when [se(m - n) = se(m) - se(n)], which is
       another way of saying that [m - n] doesn't overflow.

       The following z3 script confirms that this check is sufficient: *)
    (*
     *   (define-sort int63 () (_ BitVec 63))
     *   (define-sort int64 () (_ BitVec 64))
     *   (define-const z63 int63 ((_ int2bv 63) 0))
     *
     *   (declare-const m int63)
     *   (declare-const n int63)
     *   (declare-const c int64)
     *
     *   ; let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0
     *   (define-fun no_overflow_sub ((a int63) (b int63)) Bool
     *     (bvslt (bvor (bvxor a (bvnot b)) (bvxor b (bvsub a b))) z63))
     *
     *   (assert (no_overflow_sub m n))
     *
     *   (assert (not (=
     *     (= ((_ sign_extend 1) n) (bvsub ((_ sign_extend 1) m) c))
     *     (= c ((_ sign_extend 1) (bvsub m n)))
     *   )))
     *
     *   (check-sat)
     *)
    binary (Ccmpi Ceq) ~dbg c (Cconst_int (m - n, dbg))
  | _, _ -> binary (Ccmpi Ceq) ~dbg x y

let neq = binary (Ccmpi Cne)

let lt = binary (Ccmpi Clt)

let le = binary (Ccmpi Cle)

let gt = binary (Ccmpi Cgt)

let ge = binary (Ccmpi Cge)

let ult = binary (Ccmpa Clt)

let ule = binary (Ccmpa Cle)

let ugt = binary (Ccmpa Cgt)

let uge = binary (Ccmpa Cge)

let float_abs = unary Cabsf

let float_neg = unary Cnegf

let float_add = binary Caddf

let float_sub = binary Csubf

let float_mul = binary Cmulf

let float_div = binary Cdivf

let float_eq = binary (Ccmpf CFeq)

let float_neq = binary (Ccmpf CFneq)

let float_lt = binary (Ccmpf CFlt)

let float_le = binary (Ccmpf CFle)

let float_gt = binary (Ccmpf CFgt)

let float_ge = binary (Ccmpf CFge)

let beginregion ~dbg = Cop (Cbeginregion, [], dbg)

let endregion ~dbg region = Cop (Cendregion, [region], dbg)

let probe ~dbg ~name ~handler_code_linkage_name ~enabled_at_init ~args =
  Cop
    ( Cprobe
        { name; handler_code_sym = handler_code_linkage_name; enabled_at_init },
      args,
      dbg )

let load ~dbg memory_chunk mutability ~addr =
  Cop (Cload { memory_chunk; mutability; is_atomic = false }, [addr], dbg)

let store ~dbg kind init ~addr ~new_value =
  Cop (Cstore (kind, init), [addr; new_value], dbg)

let direct_call ~dbg ty pos f_code_sym args =
  Cop (Capply (ty, pos), f_code_sym :: args, dbg)

let indirect_call ~dbg ty pos alloc_mode f args_type args =
  might_split_call_caml_apply ty args_type Asttypes.Mutable f args pos
    alloc_mode dbg

let indirect_full_call ~dbg ty pos alloc_mode f args_type = function
  (* the single-argument case is already optimized by indirect_call *)
  | [_] as args -> indirect_call ~dbg ty pos alloc_mode f args_type args
  | args ->
    (* Use a variable to avoid duplicating the cmm code of the closure [f]. *)
    let v = Backend_var.create_local "*closure*" in
    let v' = Backend_var.With_provenance.create v in
    (* get the function's code pointer *)
    let fun_ptr =
      load ~dbg Word_int Asttypes.Mutable ~addr:(field_address (Cvar v) 2 dbg)
    in
    letin v' ~defining_expr:f
      ~body:
        (Cop
           ( Capply (Extended_machtype.to_machtype ty, pos),
             (fun_ptr :: args) @ [Cvar v],
             dbg ))

let bigarray_load ~dbg ~elt_kind ~elt_size ~elt_chunk ~bigarray ~index =
  let ba_data_f = field_address bigarray 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p index dbg
  in
  match (elt_kind : Lambda.bigarray_kind) with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int ~dbg (elt_size / 2)) in
    box_complex dbg
      (load ~dbg elt_chunk Mutable ~addr)
      (load ~dbg elt_chunk Mutable ~addr:addr')
  | _ ->
    (* Note that no sign extension operation is necessary here: if the element
       type of the bigarray is signed, then the backend will emit a
       sign-extending load instruction. *)
    load ~dbg elt_chunk Mutable ~addr

let bigarray_store ~dbg ~(elt_kind : Lambda.bigarray_kind) ~elt_size ~elt_chunk
    ~bigarray ~index ~new_value =
  let ba_data_f = field_address bigarray 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p index dbg
  in
  match elt_kind with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int ~dbg (elt_size / 2)) in
    return_unit dbg
      (sequence
         (store ~dbg elt_chunk Assignment ~addr
            ~new_value:(complex_re new_value dbg))
         (store ~dbg elt_chunk Assignment ~addr:addr'
            ~new_value:(complex_im new_value dbg)))
  | _ -> return_unit dbg (store ~dbg elt_chunk Assignment ~addr ~new_value)

(* Infix field address. Contrary to regular field addresses, these addresses are
   valid ocaml values, and can be live at gc points. *)

let infix_field_address ~dbg ptr n =
  if n = 0
  then ptr
  else Cmm.Cop (Cmm.Caddv, [ptr; int ~dbg (n * Arch.size_addr)], dbg)

(* Data items *)

let cint i = Cmm.Cint i

let cfloat f = Cmm.Cdouble f

let cvec128 bits = Cmm.Cvec128 bits

let symbol_address s = Cmm.Csymbol_address s

let symbol_offset s o = Cmm.Csymbol_offset (s, o)

let define_symbol symbol = [Cdefine_symbol symbol]

(* Cmm phrases *)

let cfunction decl = Cmm.Cfunction decl

let cdata d = Cmm.Cdata d

let fundecl fun_name fun_args fun_body fun_codegen_options fun_dbg fun_poll =
  { Cmm.fun_name; fun_args; fun_body; fun_codegen_options; fun_dbg; fun_poll }

(* Gc root table *)

let gc_root_table syms =
  let table_symbol = make_symbol ?compilation_unit:None "gc_roots" in
  cdata
    (define_symbol { sym_name = table_symbol; sym_global = Global }
    @ List.map symbol_address syms
    @ [cint 0n])

let cmm_arith_size (e : Cmm.expression) =
  let rec cmm_arith_size0 (e : Cmm.expression) =
    match e with
    | Cop
        ( ( Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor
          | Clsl | Clsr | Casr ),
          l,
          _ ) ->
      List.fold_left ( + ) 1 (List.map cmm_arith_size0 l)
    | _ -> 0
  in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _ | Cvar _
  | Cconst_vec128 _ ->
    Some 0
  | Cop _ -> Some (cmm_arith_size0 e)
  | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _ | Ctuple _ | Csequence _
  | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    None

let transl_property : Lambda.property -> Cmm.property = function
  | Zero_alloc -> Zero_alloc

let transl_attrib : Lambda.check_attribute -> Cmm.codegen_option list = function
  | Default_check -> []
  | Ignore_assert_all p -> [Ignore_assert_all (transl_property p)]
  | Assume { property; strict; never_returns_normally; loc } ->
    [ Assume
        { property = transl_property property;
          strict;
          never_returns_normally;
          loc
        } ]
  | Check { property; strict; loc; opt } ->
    if Lambda.is_check_enabled ~opt property
    then [Check { property = transl_property property; strict; loc }]
    else []

let kind_of_layout (layout : Lambda.layout) =
  match layout with
  | Pvalue Pfloatval -> Boxed_float
  | Pvalue (Pboxedintval bi) -> Boxed_integer bi
  | Pvalue (Pboxedvectorval vi) -> Boxed_vector vi
  | Pvalue (Pgenval | Pintval | Pvariant _ | Parrayval _)
  | Ptop | Pbottom | Punboxed_float | Punboxed_int _ | Punboxed_vector _
  | Punboxed_product _ ->
    Any

(* Atomics *)

let atomic_load ~dbg (imm_or_ptr : Lambda.immediate_or_pointer) atomic =
  let memory_chunk =
    match imm_or_ptr with Immediate -> Word_int | Pointer -> Word_val
  in
  Cop (mk_load_atomic memory_chunk, [atomic], dbg)

let atomic_exchange ~dbg atomic new_value =
  Cop
    ( Cextcall
        { func = "caml_atomic_exchange";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_val;
          ty_args = [];
          alloc = false
        },
      [atomic; new_value],
      dbg )

let atomic_fetch_and_add ~dbg atomic i =
  Cop
    ( Cextcall
        { func = "caml_atomic_fetch_add";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_int;
          ty_args = [];
          alloc = false
        },
      [atomic; i],
      dbg )

let atomic_compare_and_set ~dbg atomic ~old_value ~new_value =
  Cop
    ( Cextcall
        { func = "caml_atomic_cas";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_int;
          ty_args = [];
          alloc = false
        },
      [atomic; old_value; new_value],
      dbg )

type even_or_odd =
  | Even
  | Odd

let make_unboxed_int32_array_payload dbg unboxed_int32_list =
  (* CR mshinwell/gbury: potential big-endian implementations:
   *
   *  let i =
   *    if big_endian
   *    then Cop (Clsl, [a; Cconst_int (32, dbg)], dbg)
   *    else a
   *  in
   *   ...
   *  let i =
   *    if big_endian
   *    then Cop (Cor, [Cop (Clsl, [a; Cconst_int (32, dbg)], dbg); b], dbg)
   *    else Cop (Cor, [a; Cop (Clsl, [b; Cconst_int (32, dbg)], dbg)], dbg)
   *  in
   *)
  if Sys.big_endian
  then
    Misc.fatal_error "Big-endian platforms not yet supported for unboxed arrays";
  let rec aux acc = function
    | [] -> Even, List.rev acc
    | a :: [] -> Odd, List.rev (a :: acc)
    | a :: b :: r ->
      let i =
        Cop
          ( Cor,
            [ (* [a] is sign-extended by default. We need to change it to be
                 zero-extended for the `or` operation to be correct. *)
              zero_extend_32 dbg a;
              Cop (Clsl, [b; Cconst_int (32, dbg)], dbg) ],
            dbg )
      in
      aux (i :: acc) r
  in
  aux [] unboxed_int32_list

let allocate_unboxed_int32_array ~elements (mode : Lambda.alloc_mode) dbg =
  let num_elts, payload = make_unboxed_int32_array_payload dbg elements in
  let header =
    let size = 1 (* custom_ops field *) + List.length payload in
    match mode with
    | Alloc_heap -> custom_header ~size
    | Alloc_local -> custom_local_header ~size
  in
  let custom_ops =
    (* For odd-length unboxed int32 arrays there are 32 bits spare at the end of
       the block, which are never read. *)
    match num_elts with
    | Even -> custom_ops_unboxed_int32_even_array
    | Odd -> custom_ops_unboxed_int32_odd_array
  in
  Cop (Calloc mode, Cconst_natint (header, dbg) :: custom_ops :: payload, dbg)

let allocate_unboxed_int64_or_nativeint_array custom_ops ~elements
    (mode : Lambda.alloc_mode) dbg =
  let header =
    let size = 1 (* custom_ops field *) + List.length elements in
    match mode with
    | Alloc_heap -> custom_header ~size
    | Alloc_local -> custom_local_header ~size
  in
  Cop (Calloc mode, Cconst_natint (header, dbg) :: custom_ops :: elements, dbg)

let allocate_unboxed_int64_array =
  allocate_unboxed_int64_or_nativeint_array custom_ops_unboxed_int64_array

let allocate_unboxed_nativeint_array =
  allocate_unboxed_int64_or_nativeint_array custom_ops_unboxed_nativeint_array

let rec simple_and_equal_exprs e1 e2 =
  match e1, e2 with
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Int.equal n1 n2
  | Cvar var1, Cvar var2 -> Backend_var.equal var1 var2
  | Cop (op1, args1, _), Cop (op2, args2, _) ->
    Stdlib.compare op1 op2 = 0
    && List.compare_lengths args1 args2 = 0
    && List.for_all2 simple_and_equal_exprs args1 args2
  | _, _ -> false
