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

let arch_bits = Arch.size_int * 8

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

type t =
  | Regular_block
  (* Regular blocks, including closures (with unboxed fields). Closures do *not*
     need to be mixed block because they can make use of the startenv to skip
     the unboxed part of the environment. *)
  | Mixed_block of { scannable_prefix : int }
(* Mixed blocks, that need special header to specify the length of the scannable
   prefix. *)

module Mixed_block_support : sig
  val assert_mixed_block_support : unit -> unit

  val make_header : Nativeint.t -> scannable_prefix:int -> Nativeint.t
end = struct
  (* CR mixed blocks v1: This "8" is duplicated in [typedecl.ml]. We should fix
     up this duplication when we make the "8" configurable. *)
  let required_reserved_header_bits = 8

  let required_addr_size_bits = 64

  (* Many of these checks are duplicated *)

  (* CR mixed blocks v1: This is also duplicated in [typedecl.ml]. *)
  (* Why 2? We'd subtract 1 if the mixed block encoding could use all 8 bits of
     the prefix. But the all-0 prefix means "not a mixed block", so we can't use
     the all-0 pattern, and we must subtract 2 instead. *)
  let max_scannable_prefix = (1 lsl required_reserved_header_bits) - 2

  let max_header =
    (1 lsl (required_addr_size_bits - required_reserved_header_bits)) - 1
    |> Nativeint.of_int

  let assert_mixed_block_support =
    lazy
      (if not Config.native_compiler
       then Misc.fatal_error "Mixed blocks are only supported in native code";
       let reserved_header_bits = Config.reserved_header_bits in
       let addr_size_bits = Arch.size_addr * 8 in
       match
         ( reserved_header_bits = required_reserved_header_bits,
           addr_size_bits = required_addr_size_bits )
       with
       | true, true -> ()
       | false, true ->
         Misc.fatal_errorf
           "Need %d reserved header bits for mixed blocks; got %d"
           required_reserved_header_bits reserved_header_bits
       | _, false ->
         Misc.fatal_errorf
           "Mixed blocks only supported on %d bit platforms; got %d"
           required_addr_size_bits addr_size_bits)

  let assert_mixed_block_support () = Lazy.force assert_mixed_block_support

  let make_header header ~scannable_prefix =
    assert_mixed_block_support ();
    if scannable_prefix > max_scannable_prefix
    then
      Misc.fatal_errorf "Scannable prefix too big (%d > %d)" scannable_prefix
        max_scannable_prefix;
    (* This means we crash the compiler if someone tries to write a mixed record
       with too many fields, but you effectively can't: you'd need something
       like 2^46 fields. *)
    if header > max_header
    then
      Misc.fatal_errorf
        "Header too big for the mixed block encoding to be added (%nd > %nd)"
        header max_header;
    Nativeint.add
      (Nativeint.shift_left
         (Nativeint.of_int (scannable_prefix + 1))
         (required_addr_size_bits - required_reserved_header_bits))
      header
end

(* CR mshinwell: update to use NOT_MARKABLE terminology *)
let block_header ?(block_kind = Regular_block) tag sz =
  let hdr =
    Nativeint.add
      (Nativeint.shift_left (Nativeint.of_int sz) 10)
      (Nativeint.of_int tag)
  in
  match block_kind with
  | Regular_block -> hdr
  | Mixed_block { scannable_prefix } ->
    Mixed_block_support.make_header hdr ~scannable_prefix

(* Static data corresponding to "value"s must be marked black in case we are in
   no-naked-pointers mode. See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black

let black_mixed_block_header tag sz ~scannable_prefix_len =
  Nativeint.logor
    (block_header tag sz
       ~block_kind:(Mixed_block { scannable_prefix = scannable_prefix_len }))
    caml_black

let local_block_header ?block_kind tag sz =
  Nativeint.logor (block_header ?block_kind tag sz) caml_local

let white_closure_header sz = block_header Obj.closure_tag sz

let black_closure_header sz = black_block_header Obj.closure_tag sz

let local_closure_header sz = local_block_header Obj.closure_tag sz

let infix_header ofs = block_header Obj.infix_tag ofs

let boxedfloat32_header = block_header Obj.custom_tag 2

let boxedfloat32_local_header = local_block_header Obj.custom_tag 2

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

let caml_float32_ops = "caml_float32_ops"

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

let alloc_boxedfloat32_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (boxedfloat32_header, dbg)
  | Local -> Cconst_natint (boxedfloat32_local_header, dbg)

let alloc_float_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (float_header, dbg)
  | Local -> Cconst_natint (float_local_header, dbg)

let alloc_boxedvec128_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (boxedvec128_header, dbg)
  | Local -> Cconst_natint (boxedvec128_local_header, dbg)

let alloc_floatarray_header len dbg = Cconst_natint (floatarray_header len, dbg)

let alloc_closure_header ~(mode : Cmm.Alloc_mode.t) sz dbg =
  match mode with
  | Heap -> Cconst_natint (white_closure_header sz, dbg)
  | Local -> Cconst_natint (local_closure_header sz, dbg)

let alloc_infix_header ofs dbg = Cconst_natint (infix_header ofs, dbg)

let alloc_boxedint32_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (boxedint32_header, dbg)
  | Local -> Cconst_natint (boxedint32_local_header, dbg)

let alloc_boxedint64_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (boxedint64_header, dbg)
  | Local -> Cconst_natint (boxedint64_local_header, dbg)

let alloc_boxedintnat_header (mode : Cmm.Alloc_mode.t) dbg =
  match mode with
  | Heap -> Cconst_natint (boxedintnat_header, dbg)
  | Local -> Cconst_natint (boxedintnat_local_header, dbg)

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

let is_defined_shift n = 0 <= n && n < arch_bits

(** returns true only if [e + n] is definitely the same as [e | n] *)
let[@inline] can_interchange_add_with_or e n =
  match e with
  | Cop (Clsl, [_; Cconst_int (x, _)], _) -> is_defined_shift x && n asr x = 0
  | _ -> false

let[@inline] prefer_add = function
  | Cop (Cor, [e; (Cconst_int (n, _) as n')], dbg)
    when can_interchange_add_with_or e n ->
    Cop (Caddi, [e; n'], dbg)
  | e -> e

let[@inline] prefer_or = function
  | Cop (Caddi, [e; (Cconst_int (n, _) as n')], dbg)
    when can_interchange_add_with_or e n ->
    Cop (Cor, [e; n'], dbg)
  | e -> e

let rec map_tail1 e ~f =
  match e with
  | Clet (id, exp, body) -> Clet (id, exp, map_tail1 body ~f)
  | Cphantom_let (id, exp, body) -> Cphantom_let (id, exp, map_tail1 body ~f)
  | Csequence (e1, e2) -> Csequence (e1, map_tail1 e2 ~f)
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_symbol _ | Cvar _ | Ctuple _ | Cop _
  | Cifthenelse _ | Cexit _ | Ccatch _ | Ctrywith _ | Cswitch _ ->
    f e

let map_tail2 x y ~f = map_tail1 y ~f:(fun y -> map_tail1 x ~f:(fun x -> f x y))

let[@inline] is_constant = function
  | Cconst_int _ | Cconst_natint _ -> true
  | _ -> false

let rec add_const c n dbg =
  if n = 0
  then c
  else
    map_tail1 c ~f:(fun c ->
        match prefer_add c with
        | Cconst_int (x, _) when Misc.no_overflow_add x n ->
          Cconst_int (x + n, dbg)
        | Cop (Caddi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x
          ->
          add_no_overflow n x c dbg
        | Cop (Caddi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_add n x
          ->
          add_no_overflow n x c dbg
        | Cop (Csubi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x
          ->
          Cop (Csubi, [Cconst_int (n + x, dbg); c], dbg)
        | Cop (Csubi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_sub n x
          ->
          add_const c (n - x) dbg
        | _ -> Cop (Caddi, [c; Cconst_int (n, dbg)], dbg))

let incr_int c dbg = add_const c 1 dbg

let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match prefer_add c1, prefer_add c2 with
      | Cconst_int (n, _), c | c, Cconst_int (n, _) -> add_const c n dbg
      | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
        add_const (add_int c1 c2 dbg) n1 dbg
      | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) ->
        add_const (add_int c1 c2 dbg) n2 dbg
      | _, _ -> Cop (Caddi, [c1; c2], dbg))

let rec sub_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match c1, c2 with
      | c1, Cconst_int (n2, _) when n2 <> min_int -> add_const c1 (-n2) dbg
      | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) when n2 <> min_int ->
        add_const (sub_int c1 c2 dbg) (-n2) dbg
      | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
        add_const (sub_int c1 c2 dbg) n1 dbg
      | c1, c2 -> Cop (Csubi, [c1; c2], dbg))

let neg_int c dbg = sub_int (Cconst_int (0, dbg)) c dbg

(** This function conservatively approximates the number of significant bits in its signed
    argument. That is, it computes the number of bits required to represent the absolute
    value of its argument. *)
let rec max_signed_bit_length e =
  match prefer_or e with
  | Cop ((Ccmpi _ | Ccmpf _), _, _) ->
    (* integer/float comparisons return either [1] or [0]. *)
    1
  | Cop (Cand, [_; Cconst_int (n, _)], _) when n > 0 -> 1 + Misc.log2 n
  | Cop (Clsl, [c; Cconst_int (n, _)], _) when is_defined_shift n ->
    Int.min arch_bits (max_signed_bit_length c + n)
  | Cop (Casr, [c; Cconst_int (n, _)], _) when is_defined_shift n ->
    Int.max 0 (max_signed_bit_length c - n)
  | Cop (Clsr, [c; Cconst_int (n, _)], _) when is_defined_shift n ->
    if n = 0 then max_signed_bit_length c else arch_bits - n
  | Cop ((Cand | Cor | Cxor), [x; y], _) ->
    Int.max (max_signed_bit_length x) (max_signed_bit_length y)
  | _ -> arch_bits

let ignore_low_bit_int = function
  | Cop
      ( Caddi,
        [(Cop (Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)],
        _ )
    when n > 0 && is_defined_shift n ->
    c
  | Cop (Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

let[@inline] get_const = function
  | Cconst_int (i, _) -> Some (Nativeint.of_int i)
  | Cconst_natint (i, _) -> Some i
  | _ -> None

let[@inline] const_exn = function
  | Cconst_int (i, _) -> Nativeint.of_int i
  | Cconst_natint (i, _) -> i
  | _ -> Misc.fatal_error "const_exn: not a constant"

let replace x ~with_ =
  match x with
  | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ | Cvar _ | Ctuple [] ->
    with_
  | inner -> Csequence (inner, with_)

let rec xor_const e n dbg =
  match n with
  | 0n -> e
  | n ->
    map_tail1 e ~f:(fun e ->
        match get_const e with
        | Some e -> natint_const_untagged dbg (Nativeint.logxor e n)
        | None -> (
          let[@local] default () =
            (* prefer putting constants on the right *)
            Cop (Cxor, [e; natint_const_untagged dbg n], dbg)
          in
          match e with
          | Cop (Cxor, [x; y], _) -> (
            match get_const y with
            | None -> default ()
            | Some y -> xor_const x (Nativeint.logxor y n) dbg)
          | _ -> default ()))

let rec or_const e n dbg =
  match n with
  | 0n -> e
  | -1n -> replace e ~with_:(Cconst_int (-1, dbg))
  | n ->
    map_tail1 e ~f:(fun e ->
        let[@local] default () =
          (* prefer putting constants on the right *)
          Cop (Cor, [e; natint_const_untagged dbg n], dbg)
        in
        match get_const e with
        | Some e -> natint_const_untagged dbg (Nativeint.logor e n)
        | None -> (
          match e with
          | Cop (Cor, [x; y], _) -> (
            match get_const y with
            | None -> default ()
            | Some y -> or_const x (Nativeint.logor y n) dbg)
          | _ -> default ()))

let rec and_const e n dbg =
  match n with
  | 0n -> replace e ~with_:(Cconst_int (0, dbg))
  | -1n -> e
  | n ->
    map_tail1 e ~f:(fun e ->
        match get_const e with
        | Some e -> natint_const_untagged dbg (Nativeint.logand e n)
        | None -> (
          let[@local] default () =
            (* prefer putting constants on the right *)
            Cop (Cand, [e; natint_const_untagged dbg n], dbg)
          in
          match e with
          | Cop (Cand, [x; y], dbg) -> (
            match get_const y with
            | Some y -> and_const x (Nativeint.logand y n) dbg
            | None -> default ())
          | Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg) -> (
            let[@local] load memory_chunk =
              Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg)
            in
            match memory_chunk, n with
            | (Byte_signed | Byte_unsigned), 0xffn -> load Byte_unsigned
            | (Sixteen_signed | Sixteen_unsigned), 0xffffn ->
              load Sixteen_unsigned
            | (Thirtytwo_signed | Thirtytwo_unsigned), 0xffff_ffffn ->
              load Thirtytwo_unsigned
            | _ -> default ())
          | _ -> default ()))

let xor_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match get_const c1, get_const c2 with
      | Some c1, Some c2 -> natint_const_untagged dbg (Nativeint.logxor c1 c2)
      | None, Some c2 -> xor_const c1 c2 dbg
      | Some c1, None -> xor_const c2 c1 dbg
      | None, None -> Cop (Cxor, [c1; c2], dbg))

let or_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match get_const c1, get_const c2 with
      | Some c1, Some c2 -> natint_const_untagged dbg (Nativeint.logor c1 c2)
      | None, Some c2 -> or_const c1 c2 dbg
      | Some c1, None -> or_const c2 c1 dbg
      | None, None -> Cop (Cor, [c1; c2], dbg))

let and_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match get_const c1, get_const c2 with
      | Some c1, Some c2 -> natint_const_untagged dbg (Nativeint.logand c1 c2)
      | None, Some c2 -> and_const c1 c2 dbg
      | Some c1, None -> and_const c2 c1 dbg
      | None, None -> Cop (Cand, [c1; c2], dbg))

let rec lsr_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match c1, c2 with
      | c1, Cconst_int (0, _) -> c1
      | c1, Cconst_int (n, _) when is_defined_shift n -> (
        let c1 = ignore_low_bit_int c1 in
        match get_const c1 with
        | Some x ->
          natint_const_untagged dbg (Nativeint.shift_right_logical x n)
        | None -> (
          match prefer_or c1 with
          | Cop (Clsr, [inner; Cconst_int (n', _)], _) when is_defined_shift n'
            ->
            if is_defined_shift (n + n')
            then lsr_const inner (n + n') dbg
            else replace inner ~with_:(Cconst_int (0, dbg))
          | Cop ((Cor | Cxor), [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right_logical (const_exn y) n = 0n ->
            lsr_int x c2 dbg
          | Cop (Cand, [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right (const_exn y) n = 0n ->
            replace x ~with_:(Cconst_int (0, dbg))
          | _ -> Cop (Clsr, [c1; c2], dbg)))
      | Cop (Clsr, [x; (Cconst_int (n', _) as y)], dbg'), c2
        when is_defined_shift n' ->
        (* prefer putting the constant shift on the outside to help enable
           further peephole optimizations *)
        Cop (Clsr, [Cop (Clsr, [x; c2], dbg); y], dbg')
      | c1, c2 -> Cop (Clsr, [c1; c2], dbg))

and asr_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match c1, c2 with
      | c1, Cconst_int (0, _) -> c1
      | c1, Cconst_int (n, _) when is_defined_shift n -> (
        let c1 = ignore_low_bit_int c1 in
        match get_const c1 with
        | Some x -> natint_const_untagged dbg (Nativeint.shift_right x n)
        | None -> (
          match prefer_or c1 with
          | Cop (Casr, [inner; Cconst_int (n', _)], _) when is_defined_shift n'
            ->
            (* saturating add, since the sign bit extends to the left. This is
               different from the logical shifts because arithmetic shifting
               [arch_bits] times or more is the same as shifting [arch_bits - 1]
               times *)
            asr_const inner (Int.min (n + n') (arch_bits - 1)) dbg
          | Cop (Clsr, [_; Cconst_int (n', _)], _)
            when n' > 0 && is_defined_shift n' ->
            (* If the argument is guaranteed non-negative, then we know the sign
               bit is 0 and we can weaken this operation to a logical shift *)
            lsr_const c1 n dbg
          | Cop (Clsl, [c; Cconst_int (x, _)], _)
            when is_defined_shift x && max_signed_bit_length c + x < arch_bits
            ->
            (* some operations always return small enough integers that it is
               safe and correct to combine [asr (lsl x y) z] into [asr x (z -
               y)]. *)
            if x > n then lsl_const c (x - n) dbg else asr_const c (n - x) dbg
          | Cop ((Cor | Cxor), [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right (const_exn y) n = 0n ->
            asr_int x c2 dbg
          | Cop (Cor, [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right (const_exn y) n = -1n ->
            replace x ~with_:(Cconst_int (-1, dbg))
          | Cop (Cand, [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right (const_exn y) n = -1n ->
            asr_int x c2 dbg
          | Cop (Cand, [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_right (const_exn y) n = 0n ->
            replace x ~with_:(Cconst_int (0, dbg))
          | _ -> Cop (Casr, [c1; c2], dbg)))
      | Cop (Casr, [x; (Cconst_int (n', _) as y)], z), c2
        when is_defined_shift n' ->
        (* prefer putting the constant shift on the outside to help enable
           further peephole optimizations *)
        Cop (Casr, [Cop (Casr, [x; c2], dbg); y], z)
      | _ -> Cop (Casr, [c1; c2], dbg))

and lsl_int c1 c2 dbg =
  map_tail2 c1 c2 ~f:(fun c1 c2 ->
      match c1, c2 with
      | c1, Cconst_int (0, _) -> c1
      | c1, Cconst_int (n, _) when is_defined_shift n -> (
        match get_const c1 with
        | Some c1 -> natint_const_untagged dbg (Nativeint.shift_left c1 n)
        | None -> (
          match c1 with
          | Cop (Clsl, [inner; Cconst_int (n', _)], dbg)
            when is_defined_shift n' ->
            if is_defined_shift (n + n')
            then lsl_const inner (n + n') dbg
            else replace inner ~with_:(Cconst_int (0, dbg))
          | Cop (Caddi, [c1; Cconst_int (offset, _)], _)
            when Misc.no_overflow_lsl offset n ->
            add_const (lsl_int c1 c2 dbg) (offset lsl n) dbg
          | Cop ((Cor | Cxor), [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_left (const_exn y) n = 0n ->
            lsl_int x c2 dbg
          | Cop (Cand, [x; ((Cconst_int _ | Cconst_natint _) as y)], _)
            when Nativeint.shift_left (const_exn y) n = 0n ->
            replace x ~with_:(Cconst_int (0, dbg))
          | c1 -> Cop (Clsl, [c1; c2], dbg)))
      | Cop (Clsl, [x; (Cconst_int (n', _) as y)], dbg'), c2
        when is_defined_shift n' ->
        (* prefer putting the constant shift on the outside to help enable
           further peephole optimizations *)
        Cop (Clsl, [Cop (Clsl, [x; c2], dbg); y], dbg')
      | _, _ -> Cop (Clsl, [c1; c2], dbg))

and lsl_const c n dbg = lsl_int c (Cconst_int (n, dbg)) dbg

and asr_const c n dbg = asr_int c (Cconst_int (n, dbg)) dbg

and lsr_const c n dbg = lsr_int c (Cconst_int (n, dbg)) dbg

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

let tag_int i dbg =
  match i with
  | Cconst_int (n, _) -> int_const dbg n
  | c -> incr_int (lsl_const c 1 dbg) dbg

let untag_int i dbg =
  match i with
  | Cconst_int (n, _) -> Cconst_int (n asr 1, dbg)
  | Cop (Cor, [Cop (Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && is_defined_shift (n + 1) ->
    asr_const c (n + 1) dbg
  | Cop (Cor, [Cop (Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && is_defined_shift (n + 1) ->
    lsr_const c (n + 1) dbg
  | c -> asr_const c 1 dbg

let mk_not dbg cmm =
  match cmm with
  | Cop
      ( (Caddi | Cor),
        [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)],
        dbg' ) -> (
    match c with
    | Cop (Ccmpi cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpa cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpf (w, cmp), [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpf (w, negate_float_comparison cmp), [c1; c2], dbg''))
        dbg'
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

let mk_compare_floats_gen ~tag_result ~width dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
      bind "float_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpf (width, CFgt), [a1; a2], dbg) in
          let op2 = Cop (Ccmpf (width, CFlt), [a1; a2], dbg) in
          let op3 = Cop (Ccmpf (width, CFeq), [a1; a1], dbg) in
          let op4 = Cop (Ccmpf (width, CFeq), [a2; a2], dbg) in
          (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1, and the
             result is op1 - op2.

             If at least one of the operands is NaN, then op1 = op2 = 0, and the
             result is op3 - op4, which orders NaN before other values.

             To detect if the operand is NaN, we use the property:

             for all x, NaN is not equal to x, even if x is NaN.

             Therefore, op3 is 0 if and only if a1 is NaN, and op4 is 0 if and
             only if a2 is NaN. See also caml_float_compare_unboxed in
             runtime/floats.c *)
          let result =
            add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg
          in
          if tag_result then tag_int result dbg else result))

let mk_compare_floats = mk_compare_floats_gen ~tag_result:true ~width:Float64

let mk_compare_floats_untagged =
  mk_compare_floats_gen ~tag_result:false ~width:Float64

let mk_compare_float32s_untagged =
  mk_compare_floats_gen ~tag_result:false ~width:Float32

(* Turning integer divisions into multiply-high then shift. The
   [division_parameters] function is used in module Emit for those target
   platforms that support this optimization. *)

let divimm_parameters d =
  (* Signed division and modulus at type nativeint. Algorithm: Hacker's Delight,
     2nd ed, Figure 10-1. *)
  let open Nativeint in
  let udivmod n d =
    let q = unsigned_div n d in
    q, sub n (mul q d)
  in
  let ad = abs d in
  assert (ad > 1n);
  let t = add min_int (shift_right_logical d (size - 1)) in
  let anc = sub (pred t) (unsigned_rem t ad) in
  let step (q, r) x =
    let q = shift_left q 1 and r = shift_left r 1 in
    if unsigned_compare r x >= 0 then succ q, sub r x else q, r
  in
  let rec loop p qr1 qr2 =
    let p = p + 1 in
    let q1, r1 = step qr1 anc in
    let q2, r2 = step qr2 ad in
    let delta = sub ad r2 in
    if unsigned_compare q1 delta < 0 || (q1 = delta && r1 = 0n)
    then loop p (q1, r1) (q2, r2)
    else
      let m = succ q2 in
      let m = if d < 0n then neg m else m in
      m, p - size
  in
  loop (size - 1) (udivmod min_int anc) (udivmod min_int ad)

(* For d > 1, the result [(m, p)] of [divimm_parameters d] satisfies the following
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
 *   (if unsigned_compare zl xl < 0 then succ zh else zh), zl
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
 * let unsigned_compare2 (xh, xl) (yh, yl) =
 *   let c = unsigned_compare xh yh in
 *   if c = 0 then unsigned_compare xl yl else c
 *
 * let validate d m p =
 *   let md = mul2 m d in
 *   let one2 = 0n, 1n in
 *   let twoszp = shl2 one2 (size + p) in
 *   let twop1 = shl2 one2 (p + 1) in
 *   unsigned_compare2 twoszp md < 0 && unsigned_compare2 md (add2 twoszp twop1) <= 0
 *)

let raise_symbol dbg symb =
  Cop
    (Craise Lambda.Raise_regular, [Cconst_symbol (global_symbol symb, dbg)], dbg)

let[@inline] get_const = function
  | Cconst_int (i, _) -> Some (Nativeint.of_int i)
  | Cconst_natint (i, _) -> Some i
  | _ -> None

(** Division or modulo on registers. The overflow case min_int / -1 can
    occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513).
    In typical cases, [operator] is used to compute the result.

    However, if division crashes on overflow, we will insert a runtime check for a divisor
    of -1, and fall back to [if_divisor_is_minus_one]. *)
let make_safe_divmod operator ~if_divisor_is_negative_one
    ?(dividend_cannot_be_min_int = false) c1 c2 ~dbg =
  if dividend_cannot_be_min_int || not Arch.division_crashes_on_overflow
  then Cop (operator, [c1; c2], dbg)
  else
    bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
            Cifthenelse
              ( Cop (Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
                dbg,
                Cop (operator, [c1; c2], dbg),
                dbg,
                if_divisor_is_negative_one ~dividend:c1 ~dbg,
                dbg,
                Any )))

let is_power_of_2_or_zero n = Nativeint.logand n (Nativeint.pred n) = 0n

let divide_by_zero dividend ~dbg =
  bind "dividend" dividend (fun _ ->
      raise_symbol dbg "caml_exn_Division_by_zero")

let div_int ?dividend_cannot_be_min_int c1 c2 dbg =
  let if_divisor_is_negative_one ~dividend ~dbg = neg_int dividend dbg in
  match get_const c1, get_const c2 with
  | _, Some 0n -> divide_by_zero c1 ~dbg
  | _, Some 1n -> c1
  | Some n1, Some n2 -> natint_const_untagged dbg (Nativeint.div n1 n2)
  | _, Some -1n -> if_divisor_is_negative_one ~dividend:c1 ~dbg
  | _, Some divisor ->
    if divisor = Nativeint.min_int
    then
      (* integer division by min_int always returns 0 unless the dividend is
         also min_int, in which case it's 1. *)
      Cifthenelse
        ( Cop (Ccmpi Ceq, [c1; Cconst_natint (divisor, dbg)], dbg),
          dbg,
          Cconst_int (1, dbg),
          dbg,
          Cconst_int (0, dbg),
          dbg,
          Any )
    else if is_power_of_2_or_zero divisor
    then
      (* [divisor] must be positive be here since we already handled zero and
         min_int (the only negative power of 2) *)
      let l = Misc.log2_nativeint divisor in
      (* Algorithm:

         t = shift-right-signed(c1, l - 1)

         t = shift-right(t, W - l)

         t = c1 + t

         res = shift-right-signed(c1 + t, l) *)
      asr_const
        (bind "dividend" c1 (fun c1 ->
             assert (l >= 1);
             let t = asr_const c1 (l - 1) dbg in
             let t = lsr_const t (Nativeint.size - l) dbg in
             add_int c1 t dbg))
        l dbg
    else
      bind "dividend" c1 (fun n ->
          (* Algorithm:

             q = smulhi n, M

             if m < 0 && d > 0: q += n

             if m > 0 && d < 0: q -= n

             q >>= s

             q += sign-bit(q) *)
          let m, s = divimm_parameters divisor in
          let q =
            Cop (Cmulhi { signed = true }, [n; natint_const_untagged dbg m], dbg)
          in
          let q =
            if m < 0n && divisor >= 0n
            then add_int q n dbg
            else if m >= 0n && divisor < 0n
            then sub_int q n dbg
            else q
          in
          let q = asr_const q s dbg in
          let sign_bit =
            (* we can use n instead of q when the divisor is non-negative. This
               makes the instruction dependency graph shallower. *)
            lsr_const (if divisor >= 0n then n else q) (Nativeint.size - 1) dbg
          in
          add_int q sign_bit dbg)
  | _, _ ->
    make_safe_divmod ?dividend_cannot_be_min_int ~if_divisor_is_negative_one
      Cdivi c1 c2 ~dbg

let mod_int ?dividend_cannot_be_min_int c1 c2 dbg =
  let if_divisor_is_positive_or_negative_one ~dividend ~dbg =
    bind "dividend" dividend (fun _ -> Cconst_int (0, dbg))
  in
  match get_const c1, get_const c2 with
  | _, Some 0n -> divide_by_zero c1 ~dbg
  | _, Some (1n | -1n) ->
    if_divisor_is_positive_or_negative_one ~dividend:c1 ~dbg
  | Some n1, Some n2 -> natint_const_untagged dbg (Nativeint.rem n1 n2)
  | _, Some n ->
    if n = Nativeint.min_int
    then
      (* Similarly to the division by min_int almost always being 0, modulo
         min_int is almost always the identity, the exception being when the
         divisor is min_int *)
      bind "dividend" c1 (fun c1 ->
          let min_int = Cconst_natint (Nativeint.min_int, dbg) in
          Cifthenelse
            ( Cop (Ccmpi Ceq, [c1; min_int], dbg),
              dbg,
              Cconst_int (0, dbg),
              dbg,
              c1,
              dbg,
              Any ))
    else if is_power_of_2_or_zero n
    then
      (* [divisor] must be positive be here since we already handled zero and
         min_int (the only negative power of 2). *)
      let l = Misc.log2_nativeint n in
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
          let t = Cop (Cand, [t; Cconst_natint (Nativeint.neg n, dbg)], dbg) in
          sub_int c1 t dbg)
    else
      bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 dbg) c2 dbg) dbg)
  | _, _ ->
    make_safe_divmod ?dividend_cannot_be_min_int
      ~if_divisor_is_negative_one:if_divisor_is_positive_or_negative_one Cmodi
      c1 c2 ~dbg

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop
      ( (Caddi | Cor),
        [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)],
        _ ) ->
    c
  | Cconst_int (n, dbg) ->
    if n = 1 then Cconst_int (0, dbg) else Cconst_int (1, dbg)
  | c -> Cop (Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float32 dbg mode exp =
  Cop
    ( Calloc (mode, Alloc_block_kind_float32),
      [ alloc_boxedfloat32_header mode dbg;
        Cconst_symbol (global_symbol caml_float32_ops, dbg);
        exp ],
      dbg )

let unbox_float32 dbg =
  map_tail ~kind:Any (function
    | Cop (Calloc _, [Cconst_natint (hdr, _); Cconst_symbol (sym, _); c], _)
      when (Nativeint.equal hdr boxedfloat32_header
           || Nativeint.equal hdr boxedfloat32_local_header)
           && String.equal sym.sym_name caml_float32_ops ->
      c
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name with
      | Some (Const_float32 x) -> Cconst_float32 (x, dbg) (* or keep _dbg? *)
      | _ ->
        Cop
          ( mk_load_immut (Single { reg = Float32 }),
            [Cop (Cadda, [cmm; Cconst_int (size_addr, dbg)], dbg)],
            dbg ))
    | cmm ->
      Cop
        ( mk_load_immut (Single { reg = Float32 }),
          [Cop (Cadda, [cmm; Cconst_int (size_addr, dbg)], dbg)],
          dbg ))

let box_float dbg m c =
  Cop (Calloc (m, Alloc_block_kind_float), [alloc_float_header m dbg; c], dbg)

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

let box_vec128 dbg m c =
  Cop
    ( Calloc (m, Alloc_block_kind_vec128),
      [alloc_boxedvec128_header m dbg; c],
      dbg )

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

(* Conversions for 16-bit floats *)

let float_of_float16 dbg c =
  Cop
    ( Cextcall
        { func = "caml_double_of_float16";
          ty = typ_float;
          alloc = false;
          builtin = false;
          returns = true;
          effects = No_effects;
          coeffects = No_coeffects;
          ty_args = [XInt]
        },
      [c],
      dbg )

let float16_of_float dbg c =
  Cop
    ( Cextcall
        { func = "caml_float16_of_double";
          ty = typ_int;
          alloc = false;
          builtin = false;
          returns = true;
          effects = No_effects;
          coeffects = No_coeffects;
          ty_args = [XFloat]
        },
      [c],
      dbg )

(* Complex *)

let box_complex dbg c_re c_im =
  Cop
    ( Calloc (Cmm.Alloc_mode.Heap, Alloc_block_kind_float_array),
      [alloc_floatarray_header 2 dbg; c_re; c_im],
      dbg )

let complex_re c dbg = Cop (mk_load_immut Double, [c], dbg)

let complex_im c dbg =
  Cop
    ( mk_load_immut Double,
      [Cop (Cadda, [c; Cconst_int (size_float, dbg)], dbg)],
      dbg )

(* Unit *)

let return_unit dbg c =
  match c with
  | Csequence (_, Cconst_int (1, _)) as c -> c
  | c -> Csequence (c, Cconst_int (1, dbg))

let memory_chunk_width_in_bytes : memory_chunk -> int = function
  | Byte_unsigned | Byte_signed -> 1
  | Sixteen_unsigned | Sixteen_signed -> 2
  | Thirtytwo_unsigned | Thirtytwo_signed -> 4
  | Single { reg = Float64 | Float32 } -> 4
  | Word_int -> size_int
  | Word_val -> size_addr
  | Double -> size_float
  | Onetwentyeight_unaligned | Onetwentyeight_aligned -> size_vec128

let strided_field_address ptr ~index ~stride dbg =
  if index * stride = 0
  then ptr
  else Cop (Cadda, [ptr; Cconst_int (index * stride, dbg)], dbg)

let field_address ?(memory_chunk = Word_val) ptr n dbg =
  strided_field_address ptr dbg ~index:n
    ~stride:(memory_chunk_width_in_bytes memory_chunk)

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
  match Config.reserved_header_bits with
  | 0 -> get_header ptr dbg
  | bits -> lsr_const (lsl_const (get_header ptr dbg) bits dbg) bits dbg

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

let get_size ptr dbg = lsr_const (get_header_masked ptr dbg) 10 dbg

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr

let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9

let numfloat_shift = 9 + log2_size_float - log2_size_addr

let addr_array_length_shifted hdr dbg = lsr_const hdr wordsize_shift dbg

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
  | Cop
      ( (Caddi | Cor),
        [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)],
        dbg' ) ->
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

(* caml_unboxed_float32_array_ops refers to the first element of an array of two
   custom ops. The array index indicates the number of (invalid) tailing
   float32s (0 or 1). *)
let custom_ops_unboxed_float32_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_float32_array_ops", Debuginfo.none)

let custom_ops_unboxed_float32_even_array = custom_ops_unboxed_float32_array

let custom_ops_unboxed_float32_odd_array =
  Cop
    ( Caddi,
      [ custom_ops_unboxed_float32_array;
        Cconst_int (Config.custom_ops_struct_size, Debuginfo.none) ],
      Debuginfo.none )

let custom_ops_unboxed_int64_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_int64_array_ops", Debuginfo.none)

let custom_ops_unboxed_nativeint_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_nativeint_array_ops", Debuginfo.none)

let custom_ops_unboxed_vec128_array =
  Cconst_symbol
    (Cmm.global_symbol "caml_unboxed_vec128_array_ops", Debuginfo.none)

let unboxed_packed_array_length arr dbg ~custom_ops_base_symbol
    ~elements_per_word =
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
                      (sub_int (Cvar custom_ops_var) custom_ops_base_symbol dbg)
                      (int ~dbg custom_ops_size_log2)
                      dbg,
                    (* subtract index from length in elements *)
                    sub_int
                      (mul_int (Cvar num_words_var)
                         (int ~dbg elements_per_word)
                         dbg)
                      (Cvar custom_ops_index_var) dbg ) ) ))
  in
  tag_int res dbg

let unboxed_int32_array_length =
  unboxed_packed_array_length
    ~custom_ops_base_symbol:custom_ops_unboxed_int32_array ~elements_per_word:2

let unboxed_float32_array_length =
  unboxed_packed_array_length
    ~custom_ops_base_symbol:custom_ops_unboxed_float32_array
    ~elements_per_word:2

let unboxed_int64_or_nativeint_array_length arr dbg =
  let res =
    bind "arr" arr (fun arr ->
        (* need to subtract so as not to count the custom_operations field *)
        sub_int (get_size arr dbg) (int ~dbg 1) dbg)
  in
  tag_int res dbg

let unboxed_vec128_array_length arr dbg =
  let res =
    bind "arr" arr (fun arr ->
        (* need to subtract so as not to count the custom_operations field *)
        sub_int (get_size arr dbg) (int ~dbg 1) dbg)
  in
  tag_int (lsr_int res (int ~dbg 1) dbg) dbg

let addr_array_ref arr ofs dbg =
  Cop (mk_load_mut Word_val, [array_indexing log2_size_addr arr ofs dbg], dbg)

let int_array_ref arr ofs dbg =
  Cop (mk_load_mut Word_int, [array_indexing log2_size_addr arr ofs dbg], dbg)

let unboxed_mutable_float_array_ref arr ofs dbg =
  Cop (mk_load_mut Double, [array_indexing log2_size_float arr ofs dbg], dbg)

let unboxed_immutable_float_array_ref arr ofs dbg =
  Cop (mk_load_immut Double, [array_indexing log2_size_float arr ofs dbg], dbg)

let unboxed_mutable_float32_unboxed_product_array_ref arr ~array_index dbg =
  bind "arr" arr (fun arr ->
      bind "index" array_index (fun index ->
          Cop
            ( mk_load_mut (Single { reg = Float32 }),
              [array_indexing log2_size_addr arr index dbg],
              dbg )))

(* CR mshinwell/mslater: if we're writing zeros to the top 32 bits of float32
   product fields, should we do the same for mixed block record fields? *)

let unboxed_mutable_float32_unboxed_product_array_set arr ~array_index
    ~new_value dbg =
  bind "arr" arr (fun arr ->
      bind "index" array_index (fun index ->
          bind "new_value" new_value (fun new_value ->
              Csequence
                ( Cop
                    ( Cstore (Word_int, Assignment),
                      [ array_indexing log2_size_addr arr index dbg;
                        Cconst_int (0, dbg) ],
                      dbg ),
                  Cop
                    ( Cstore (Single { reg = Float32 }, Assignment),
                      [array_indexing log2_size_addr arr index dbg; new_value],
                      dbg ) ))))

let unboxed_float_array_ref (mutability : Asttypes.mutable_flag) ~block:arr
    ~index:ofs dbg =
  match mutability with
  | Immutable -> unboxed_immutable_float_array_ref arr ofs dbg
  | Mutable -> unboxed_mutable_float_array_ref arr ofs dbg

let float_array_ref mode arr ofs dbg =
  box_float dbg mode (unboxed_mutable_float_array_ref arr ofs dbg)

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

(** [get_const_bitmask x] returns [Some (y, mask)] if [x] is [y & mask] *)
let get_const_bitmask = function
  | Cop (Cand, ([x; Cconst_natint (mask, _)] | [Cconst_natint (mask, _); x]), _)
    ->
    Some (x, mask)
  | Cop (Cand, ([x; Cconst_int (mask, _)] | [Cconst_int (mask, _); x]), _) ->
    Some (x, Nativeint.of_int mask)
  | _ -> None

(** [low_bits ~bits x] is a (potentially simplified) value which agrees with x on at least
    the low [bits] bits. E.g., [low_bits ~bits x & mask = x & mask], where [mask] is a
    bitmask of the low [bits] bits . *)
let rec low_bits ~bits ~dbg x =
  assert (bits > 0);
  if bits >= arch_bits
  then x
  else
    let unused_bits = arch_bits - bits in
    let does_mask_keep_low_bits mask =
      (* If the mask has all the low bits set, then the low bits are unchanged.
         This could happen from zero-extension. *)
      let low_bits = Nativeint.pred (Nativeint.shift_left 1n bits) in
      Nativeint.equal low_bits (Nativeint.logand mask low_bits)
    in
    (* Ignore sign and zero extensions which do not affect the low bits *)
    map_tail
      (function
        | Cop
            ( (Casr | Clsr),
              [Cop (Clsl, [x; Cconst_int (left, _)], _); Cconst_int (right, _)],
              _ )
          when 0 <= right && right <= left && left <= unused_bits ->
          (* these sign-extensions can be replaced with a left shift since we
             don't care about the high bits that it changed *)
          low_bits ~bits (lsl_const x (left - right) dbg) ~dbg
        | x -> (
          match get_const_bitmask x with
          | Some (x, bitmask) when does_mask_keep_low_bits bitmask ->
            low_bits ~bits x ~dbg
          | _ -> x))
      x

(** [zero_extend ~bits dbg e] returns [e] with the most significant [arch_bits - bits]
    bits set to 0 *)
let zero_extend ~bits ~dbg e =
  assert (0 < bits && bits <= arch_bits);
  let mask = Nativeint.pred (Nativeint.shift_left 1n bits) in
  let zero_extend_via_mask e =
    Cop (Cand, [e; natint_const_untagged dbg mask], dbg)
  in
  if bits = arch_bits
  then e
  else
    map_tail
      (function
        | Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg) as e
          -> (
          let load memory_chunk =
            Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg)
          in
          match memory_chunk, bits with
          | (Byte_signed | Byte_unsigned), 8 -> load Byte_unsigned
          | (Sixteen_signed | Sixteen_unsigned), 16 -> load Sixteen_unsigned
          | (Thirtytwo_signed | Thirtytwo_unsigned), 32 ->
            load Thirtytwo_unsigned
          | _ -> zero_extend_via_mask e)
        | e -> zero_extend_via_mask e)
      (low_bits ~bits e ~dbg)

let rec sign_extend ~bits ~dbg e =
  assert (0 < bits && bits <= arch_bits);
  let unused_bits = arch_bits - bits in
  let sign_extend_via_shift e =
    asr_const (lsl_const e unused_bits dbg) unused_bits dbg
  in
  if bits = arch_bits
  then e
  else
    map_tail
      (fun e ->
        match prefer_or e with
        | Cop (Cand, [x; y], _) when is_constant y ->
          and_int (sign_extend ~bits x ~dbg) (sign_extend ~bits y ~dbg) dbg
        | Cop (Cor, [x; y], _) when is_constant y ->
          or_int (sign_extend ~bits x ~dbg) (sign_extend ~bits y ~dbg) dbg
        | Cop (Cxor, [x; y], _) when is_constant y ->
          xor_int (sign_extend ~bits x ~dbg) (sign_extend ~bits y ~dbg) dbg
        | Cop (((Casr | Clsr) as op), [inner; Cconst_int (n, _)], _) as e
          when is_defined_shift n ->
          (* see middle_end/flambda2/z3/sign_extension.py for proof *)
          if n = unused_bits
          then
            match op with
            | Casr -> e
            | Clsr -> asr_const inner unused_bits dbg
            | _ -> assert false
          else if n > unused_bits
          then
            (* sign-extension is a no-op since the top n bits already match *)
            e
          else
            let e = lsl_const inner (unused_bits - n) dbg in
            asr_const e unused_bits dbg
        | Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg) as e
          -> (
          let load memory_chunk =
            Cop (Cload { memory_chunk; mutability; is_atomic }, args, dbg)
          in
          match memory_chunk, bits with
          | (Byte_signed | Byte_unsigned), 8 -> load Byte_signed
          | (Sixteen_signed | Sixteen_unsigned), 16 -> load Sixteen_signed
          | (Thirtytwo_signed | Thirtytwo_unsigned), 32 -> load Thirtytwo_signed
          | _ -> sign_extend_via_shift e)
        | e -> sign_extend_via_shift e)
      (low_bits ~bits e ~dbg)

let unboxed_packed_array_ref arr index dbg ~memory_chunk ~elements_per_word =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          let index =
            (* Need to skip the custom_operations field. We add
               elements_per_word offsets not 1 since the call to
               [array_indexing], below, is in terms of elements. Then we
               multiply the offset by 2 since we are manipulating a tagged
               int. *)
            add_int index (int ~dbg (elements_per_word * 2)) dbg
          in
          let log2_size_addr = 2 in
          Cop
            ( mk_load_mut memory_chunk,
              [array_indexing log2_size_addr arr index dbg],
              dbg )))

let unboxed_int32_array_ref =
  (* N.B. The resulting value will be sign extended by the code generated for a
     [Thirtytwo_signed] load. *)
  unboxed_packed_array_ref ~memory_chunk:Thirtytwo_signed ~elements_per_word:2

let unboxed_mutable_int32_unboxed_product_array_ref arr ~array_index dbg =
  bind "arr" arr (fun arr ->
      bind "index" array_index (fun index ->
          sign_extend ~bits:32
            (Cop
               ( mk_load_mut Thirtytwo_signed,
                 [array_indexing log2_size_addr arr index dbg],
                 dbg ))
            ~dbg))

let unboxed_mutable_int32_unboxed_product_array_set arr ~array_index ~new_value
    dbg =
  bind "arr" arr (fun arr ->
      bind "index" array_index (fun index ->
          bind "new_value" new_value (fun new_value ->
              let new_value = sign_extend ~bits:32 new_value ~dbg in
              Cop
                ( Cstore (Word_int, Assignment),
                  [array_indexing log2_size_addr arr index dbg; new_value],
                  dbg ))))

let unboxed_float32_array_ref =
  unboxed_packed_array_ref
    ~memory_chunk:(Single { reg = Float32 })
    ~elements_per_word:2

let unboxed_int64_or_nativeint_array_ref ~has_custom_ops arr ~array_index dbg =
  bind "arr" arr (fun arr ->
      bind "index" array_index (fun index ->
          let index =
            if has_custom_ops
            then
              (* Need to skip the custom_operations field. 2 not 1 since we are
                 manipulating a tagged int. *)
              add_int index (int ~dbg 2) dbg
            else index
          in
          int_array_ref arr index dbg))

let unboxed_packed_array_set arr ~index ~new_value dbg ~memory_chunk
    ~elements_per_word =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          bind "new_value" new_value (fun new_value ->
              let index =
                (* See comment in [unboxed_packed_array_ref]. *)
                add_int index (int ~dbg (elements_per_word * 2)) dbg
              in
              let log2_size_addr = 2 in
              Cop
                ( Cstore (memory_chunk, Assignment),
                  [array_indexing log2_size_addr arr index dbg; new_value],
                  dbg ))))

let unboxed_int32_array_set =
  unboxed_packed_array_set ~memory_chunk:Thirtytwo_signed ~elements_per_word:2

let unboxed_float32_array_set =
  unboxed_packed_array_set
    ~memory_chunk:(Single { reg = Float32 })
    ~elements_per_word:2

let unboxed_int64_or_nativeint_array_set ~has_custom_ops arr ~index ~new_value
    dbg =
  bind "arr" arr (fun arr ->
      bind "index" index (fun index ->
          bind "new_value" new_value (fun new_value ->
              let index =
                if has_custom_ops
                then
                  (* See comment in [unboxed_int64_or_nativeint_array_ref]. *)
                  add_int index (int ~dbg 2) dbg
                else index
              in
              int_array_set arr index new_value dbg)))

let get_field_unboxed ~dbg memory_chunk mutability block ~index_in_words =
  if Arch.big_endian && memory_chunk_width_in_bytes memory_chunk <> size_addr
  then
    (* CR layouts v5.1: Properly support big-endian. *)
    Misc.fatal_error
      "Unboxed non-word size integer fields are only supported on \
       little-endian architectures";
  (* CR layouts v5.1: We'll need to vary log2_size_addr among other things to
     efficiently pack small integers *)
  let field_address =
    assert (size_float = size_addr);
    array_indexing log2_size_addr block index_in_words dbg
  in
  Cop
    (Cload { memory_chunk; mutability; is_atomic = false }, [field_address], dbg)

let get_field_computed imm_or_ptr mutability ~block ~index dbg =
  let memory_chunk =
    match imm_or_ptr with
    | Lambda.Immediate -> Word_int
    | Lambda.Pointer -> Word_val
  in
  get_field_unboxed ~dbg memory_chunk mutability block ~index_in_words:index

let set_field_unboxed ~dbg memory_chunk block ~index_in_words newval =
  match memory_chunk with
  | Word_val ->
    Misc.fatal_error "Attempted to set a value via [setfield_unboxed]"
  | memory_chunk ->
    let size_in_bytes = memory_chunk_width_in_bytes memory_chunk in
    (* CR layouts v5.1: Properly support big-endian. *)
    if Arch.big_endian && size_in_bytes <> size_addr
    then
      Misc.fatal_error
        "Unboxed non-word-size fields are only supported on little-endian \
         architectures";
    (* CR layouts v5.1: We will need to vary log2_size_addr, among other things,
       when small fields are efficiently packed. *)
    let field_address =
      array_indexing log2_size_addr block index_in_words dbg
    in
    let newval = low_bits newval ~dbg ~bits:(8 * size_in_bytes) in
    return_unit dbg
      (Cop (Cstore (memory_chunk, Assignment), [field_address; newval], dbg))

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
    | Val_and_int
    | Any_int
    | Float
    | Vec128
    | Float32

  let of_machtype_component (component : machtype_component) =
    match component with
    | Val -> Val
    | Addr -> Addr
    | Int -> Any_int
    | Float -> Float
    | Vec128 -> Vec128
    | Float32 -> Float32
    | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"

  let to_machtype_component t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Val_and_int | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128
    | Float32 -> Float32

  let change_tagged_int_to_val t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Val_and_int -> Val
    | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128
    | Float32 -> Float32
end

module Extended_machtype = struct
  type t = Extended_machtype_component.t array

  let typ_val = [| Extended_machtype_component.Val |]

  let typ_tagged_int = [| Extended_machtype_component.Val_and_int |]

  let typ_any_int = [| Extended_machtype_component.Any_int |]

  let typ_float = [| Extended_machtype_component.Float |]

  let typ_float32 = [| Extended_machtype_component.Float32 |]

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
    | Punboxed_float Unboxed_float64 -> typ_float
    | Punboxed_float Unboxed_float32 -> typ_float32
    | Punboxed_vector Unboxed_vec128 -> typ_vec128
    | Punboxed_int _ ->
      (* Only 64-bit architectures, so this is always [typ_int] *)
      typ_any_int
    | Pvalue { raw_kind = Pintval; _ } -> typ_tagged_int
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
    | Float32 -> 'S'
    | Addr ->
      Misc.fatal_error "[Addr] is forbidden inside arity for generic functions"
    | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
  in
  String.of_seq (Seq.map char_of_component (Array.to_seq t))

let unique_arity_identifier (arity : Cmm.machtype list) =
  if List.for_all (function [| Val |] -> true | _ -> false) arity
  then Int.to_string (List.length arity)
  else String.concat "_" (List.map machtype_identifier arity)

let result_layout_suffix result =
  match result with [| Val |] -> "" | _ -> "_R" ^ machtype_identifier result

let send_function_name arity result (mode : Cmx_format.alloc_mode) =
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

(* CR layouts 5.1: When we pack int8/16/32s/float32s more efficiently, this code
   will need to change. *)
let memory_chunk_size_in_words_for_mixed_block = function
  | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
  | Thirtytwo_unsigned | Thirtytwo_signed ->
    (* small integers are currently stored using a whole word *)
    1
  | Single _ | Double ->
    (* Float32s are currently stored using a whole word *)
    if size_float <> size_addr
    then
      Misc.fatal_error
        "Unable to compile mixed blocks on a platform where a float is not the \
         same width as a value.";
    1
  | Word_int | Word_val -> 1
  | Onetwentyeight_unaligned | Onetwentyeight_aligned -> 2

let alloc_generic_set_fn block ofs newval memory_chunk dbg =
  let generic_case () =
    let addr = array_indexing log2_size_addr block ofs dbg in
    Cop (Cstore (memory_chunk, Initialization), [addr; newval], dbg)
  in
  match (memory_chunk : Cmm.memory_chunk) with
  | Word_val ->
    (* Values must go through "caml_initialize" *)
    addr_array_initialize block ofs newval dbg
  | Word_int -> generic_case ()
  (* Generic cases that may differ under big endian archs *)
  | Single _ | Double | Thirtytwo_unsigned | Thirtytwo_signed
  | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
    if Arch.big_endian
    then
      Misc.fatal_errorf
        "Fields with memory_chunk %s are not supported on big-endian \
         architectures"
        (Printcmm.chunk memory_chunk);
    generic_case ()
  (* Forbidden cases *)
  | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed ->
    Misc.fatal_errorf
      "Fields with memory_chunk %s are not supported in generic allocations"
      (Printcmm.chunk memory_chunk)

let make_alloc_generic ~block_kind ~mode ~alloc_block_kind dbg tag wordsize args
    args_memory_chunks =
  (* allocs of size 0 must be statically allocated else the Gc will bug *)
  assert (List.compare_length_with args 0 > 0);
  if Cmm.Alloc_mode.is_local mode || wordsize <= Config.max_young_wosize
  then
    let hdr =
      match (mode : Cmm.Alloc_mode.t) with
      | Local -> local_block_header ~block_kind tag wordsize
      | Heap -> block_header ~block_kind tag wordsize
    in
    Cop (Calloc (mode, alloc_block_kind), Cconst_natint (hdr, dbg) :: args, dbg)
  else
    let id = V.create_local "*alloc*" in
    let rec fill_fields idx args memory_chunks =
      match args, memory_chunks with
      | [], [] -> Cvar id
      | e1 :: el, m1 :: ml ->
        let ofs = memory_chunk_size_in_words_for_mixed_block m1 in
        Csequence
          ( alloc_generic_set_fn (Cvar id) (int_const dbg idx) e1 m1 dbg,
            fill_fields (idx + ofs) el ml )
      | _ ->
        Misc.fatal_errorf
          "To_cmm_helpers.make_alloc_generic: mismatched list sizes between \
           fields and memory chunks"
    in
    let caml_alloc_func, caml_alloc_args =
      match block_kind with
      | Regular_block -> "caml_alloc_shr_check_gc", [wordsize; tag]
      | Mixed_block { scannable_prefix } ->
        Mixed_block_support.assert_mixed_block_support ();
        "caml_alloc_mixed_shr_check_gc", [wordsize; tag; scannable_prefix]
    in
    Clet
      ( VP.create id,
        Cop
          ( Cextcall
              { func = caml_alloc_func;
                ty = typ_val;
                alloc = true;
                builtin = false;
                returns = true;
                effects = Arbitrary_effects;
                coeffects = Has_coeffects;
                ty_args = []
              },
            List.map (fun arg -> Cconst_int (arg, dbg)) caml_alloc_args,
            dbg ),
        fill_fields 0 args args_memory_chunks )

let make_alloc ~mode dbg ~tag args =
  make_alloc_generic ~block_kind:Regular_block ~mode dbg tag (List.length args)
    ~alloc_block_kind:Alloc_block_kind_other args
    (List.map (fun _ -> Word_val) args)

let make_float_alloc ~mode dbg ~tag args =
  make_alloc_generic ~block_kind:Regular_block ~mode dbg tag
    ~alloc_block_kind:Alloc_block_kind_float
    (List.length args * size_float / size_addr)
    args
    (List.map (fun _ -> Double) args)

let make_closure_alloc ~mode dbg ~tag args args_memory_chunks =
  let size =
    List.fold_left
      (fun acc memory_chunk ->
        acc + memory_chunk_size_in_words_for_mixed_block memory_chunk)
      0 args_memory_chunks
  in
  make_alloc_generic ~block_kind:Regular_block ~mode dbg tag size args
    ~alloc_block_kind:Alloc_block_kind_closure args_memory_chunks

let make_mixed_alloc ~mode dbg ~tag ~value_prefix_size args args_memory_chunks =
  let size =
    List.fold_left
      (fun ofs memory_chunk ->
        let ok () =
          ofs + memory_chunk_size_in_words_for_mixed_block memory_chunk
        in
        let error situation =
          Misc.fatal_errorf
            "Fields with memory chunk %s are not allowed in %s.@\n\
             value_prefix_size: %d@\n\
             args: @[<v>%a@]@\n\
             chunks: @[<v>%a@]@."
            (Printcmm.chunk memory_chunk)
            situation value_prefix_size
            (Format.pp_print_list Printcmm.expression)
            args
            (Format.pp_print_list Format.pp_print_string)
            (List.map Printcmm.chunk args_memory_chunks)
        in
        if ofs < value_prefix_size
        then
          (* regular scanned part of a block *)
          match memory_chunk with
          | Word_int | Word_val -> ok ()
          | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed ->
            error "mixed blocks"
          | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
          | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
            error "the value prefix of a mixed block"
        else
          (* flat suffix part of the block *)
          match memory_chunk with
          | Word_int | Thirtytwo_unsigned | Thirtytwo_signed | Double
          | Onetwentyeight_unaligned | Onetwentyeight_aligned | Single _ ->
            ok ()
          | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed ->
            error "mixed blocks"
          | Word_val -> error "the flat suffix of a mixed block")
      0 args_memory_chunks
  in
  make_alloc_generic
    ~block_kind:(Mixed_block { scannable_prefix = value_prefix_size })
    ~alloc_block_kind:Alloc_block_kind_other ~mode dbg tag size args
    args_memory_chunks

(* Record application and currying functions *)

let apply_function_name arity result (mode : Cmx_format.alloc_mode) =
  let res = result_layout_suffix result in
  let suff =
    match mode with
    | Cmx_format.Alloc_heap -> ""
    | Cmx_format.Alloc_local -> "L"
  in
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
  | Pbigarray_float16 -> 2
  | Pbigarray_float32 -> 4
  | Pbigarray_float32_t -> 4
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
  | Pbigarray_float16 -> Sixteen_unsigned
  | Pbigarray_float32 -> Single { reg = Float64 }
  | Pbigarray_float32_t -> Single { reg = Float32 }
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word_int
  | Pbigarray_caml_int -> Word_int
  | Pbigarray_native_int -> Word_int
  | Pbigarray_complex32 -> Single { reg = Float64 }
  | Pbigarray_complex64 -> Double

(* Boxed integers *)

let operations_boxed_int (bi : Primitive.boxed_integer) =
  let sym_name =
    match bi with
    | Boxed_nativeint -> caml_nativeint_ops
    | Boxed_int32 -> caml_int32_ops
    | Boxed_int64 -> caml_int64_ops
  in
  global_symbol sym_name

let alloc_header_boxed_int (bi : Primitive.boxed_integer) mode dbg =
  match bi with
  | Boxed_nativeint -> alloc_boxedintnat_header mode dbg
  | Boxed_int32 -> alloc_boxedint32_header mode dbg
  | Boxed_int64 -> alloc_boxedint64_header mode dbg

let box_int_gen dbg (bi : Primitive.boxed_integer) mode arg =
  let arg' =
    if bi = Primitive.Boxed_int32
    then
      if big_endian then lsl_const arg 32 dbg else sign_extend ~bits:32 arg ~dbg
    else arg
  in
  Cop
    ( Calloc (mode, Alloc_block_kind_boxed_int bi),
      [ alloc_header_boxed_int bi mode dbg;
        Cconst_symbol (operations_boxed_int bi, dbg);
        arg' ],
      dbg )

let alloc_matches_boxed_int bi ~hdr ~ops =
  match (bi : Primitive.boxed_integer), hdr, ops with
  | Boxed_nativeint, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedintnat_header
    || Nativeint.equal hdr boxedintnat_local_header)
    && String.equal sym.sym_name caml_nativeint_ops
  | Boxed_int32, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint32_header
    || Nativeint.equal hdr boxedint32_local_header)
    && String.equal sym.sym_name caml_int32_ops
  | Boxed_int64, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint64_header
    || Nativeint.equal hdr boxedint64_local_header)
    && String.equal sym.sym_name caml_int64_ops
  | (Boxed_nativeint | Boxed_int32 | Boxed_int64), _, _ -> false

let unbox_int dbg bi =
  let default arg =
    let memory_chunk =
      if bi = Primitive.Boxed_int32 then Thirtytwo_signed else Word_int
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
      when bi = Primitive.Boxed_int32 && big_endian
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend ~bits:32 contents ~dbg
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when bi = Primitive.Boxed_int32 && (not big_endian)
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend ~bits:32 contents ~dbg
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when alloc_matches_boxed_int bi ~hdr ~ops ->
      contents
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name, bi with
      | Some (Const_nativeint n), Primitive.Boxed_nativeint ->
        natint_const_untagged dbg n
      | Some (Const_int32 n), Primitive.Boxed_int32 ->
        natint_const_untagged dbg (Nativeint.of_int32 n)
      | Some (Const_int64 n), Primitive.Boxed_int64 ->
        natint_const_untagged dbg (Int64.to_nativeint n)
      | _ -> default cmm)
    | cmm -> default cmm)

let make_unsigned_int bi arg dbg =
  if bi = Primitive.Unboxed_int32 then zero_extend ~bits:32 arg ~dbg else arg

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

let unaligned_load_f32 ptr idx dbg =
  Cop (mk_load_mut (Single { reg = Float32 }), [add_int ptr idx dbg], dbg)

let unaligned_set_f32 ptr idx newval dbg =
  Cop
    ( Cstore (Single { reg = Float32 }, Assignment),
      [add_int ptr idx dbg; newval],
      dbg )

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

  type key = int option * int

  type context = int

  let make_key index expr =
    let continuation =
      match expr with Cexit (Lbl i, [], []) -> Some i | _ -> None
    in
    Some (continuation, index)

  let compare_key (cont, index) (cont', index') =
    match cont, cont' with
    | Some i, Some i' when i = i' -> 0
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
             Cmx_format.Alloc_local dbg)
        old_region)
  | (arity, args), Some (arity', args') -> (
    let body old_region =
      bind "result"
        (call_caml_apply [| Val |] arity mut clos args Rc_normal
           Cmx_format.Alloc_local dbg) (fun clos ->
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
    | None, Cmx_format.Alloc_heap when Config.stack_allocation ->
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
  let cconst_int i = Cconst_int (i, dbg) in
  let found_cont = Lambda.next_raise_count () in
  let result_label_index = V.create_local "*result_index*" in
  let result = V.create_local "*result*" in
  let found_expr =
    (* We need to store in the cache and return the offset, in bytes, of the
       label we found, compared to the first label (not the start of the array),
       and add one to make it scannable. So from the label index in the method
       array, we shift by [log2_size_addr] to get an offset in bytes, add one
       for tagging, and subtract [3 * size_addr] which is the offset of the
       first label. *)
    Clet
      ( VP.create result,
        Cop
          ( Caddi,
            [ lsl_const (Cvar result_label_index) log2_size_addr dbg;
              cconst_int (1 - (3 * size_addr)) ],
            dbg ),
        Csequence
          ( Cop (Cstore (Word_int, Assignment), [cache; Cvar result], dbg),
            Cvar result ) )
  in
  let loop_cont = Lambda.next_raise_count () in
  let li = V.create_local "*li*" in
  let hi = V.create_local "*hi*" in
  let mi = V.create_local "*mi*" in
  let check_cont = Lambda.next_raise_count () in
  let check_li = V.create_local "*check_li*" in
  let check_hi = V.create_local "*check_hi*" in
  let check_expr =
    (* Here we check whether the interval [li; hi] is a singleton, and exit the
       loop if so. *)
    Cifthenelse
      ( Cop (Ccmpi Cge, [Cvar check_li; Cvar check_hi], dbg),
        dbg,
        Cexit (Lbl found_cont, [Cvar check_li], []),
        dbg,
        Cexit (Lbl loop_cont, [Cvar check_li; Cvar check_hi], []),
        dbg,
        Any )
  in
  let dichotomy_expr =
    Clet
      ( VP.create mi,
        Cop
          ( Cor,
            [ Cop
                (Clsr, [Cop (Caddi, [Cvar li; Cvar hi], dbg); cconst_int 1], dbg);
              cconst_int 1 ],
            dbg ),
        Cifthenelse
          ( Cop
              ( Ccmpi Clt,
                [ tag;
                  Cop
                    ( mk_load_mut Word_int,
                      [ Cop
                          ( Cadda,
                            [meths; lsl_const (Cvar mi) log2_size_addr dbg],
                            dbg ) ],
                      dbg ) ],
                dbg ),
            dbg,
            (* tag < a.(mi) : interval is now [ li; mi - 2 ] *)
            Cexit
              ( Lbl check_cont,
                [Cvar li; Cop (Csubi, [Cvar mi; cconst_int 2], dbg)],
                [] ),
            dbg,
            (* tag >= a.(mi) : interval is now [ mi; hi ] *)
            Cexit (Lbl check_cont, [Cvar mi; Cvar hi], []),
            dbg,
            Any ) )
  in
  let loop_body =
    ccatch
      ( check_cont,
        [VP.create check_li, typ_int; VP.create check_hi, typ_int],
        dichotomy_expr,
        check_expr,
        dbg,
        Any,
        false )
  in
  let li_vp = VP.create li in
  let hi_vp = VP.create hi in
  ccatch
    ( found_cont,
      [VP.create result_label_index, typ_int],
      Ccatch
        ( Recursive,
          [loop_cont, [li_vp, typ_int; hi_vp, typ_int], loop_body, dbg, false],
          (* Start the first iteration of the loop *)
          Cexit
            ( Lbl loop_cont,
              [cconst_int 3; Cop (mk_load_mut Word_int, [meths], dbg)],
              [] ),
          Any ),
      found_expr,
      dbg,
      Any,
      false )

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

let apply_function_body arity result (mode : Cmx_format.alloc_mode) =
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
      | Cmx_format.Alloc_heap -> Some (V.create_local "region")
      | Cmx_format.Alloc_local -> None
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
      match (c : machtype_component) with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
      | Val | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Float32 ->
        (* Float32 slots still take up a full word *)
        cur + 1
      | Vec128 -> cur + ints_per_vec128)
    0 t

let machtype_non_scanned_size t =
  Array.fold_left
    (fun cur c ->
      match (c : machtype_component) with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
      | Val -> cur
      | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Float32 ->
        (* Float32 slots still take up a full word *)
        cur + 1
      | Vec128 -> cur + ints_per_vec128)
    0 t

let make_tuple l = match l with [e] -> e | _ -> Ctuple l

let tuple_field exp ~component_tys n dbg =
  Cop (Ctuple_field (n, component_tys), [exp], dbg)

let value_slot_given_machtype vs =
  let non_scanned, scanned =
    List.partition
      (fun (_, c) ->
        match (c : machtype_component) with
        | Int | Float | Float32 | Vec128 -> true
        | Val -> false
        | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
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
        match (c : machtype_component) with
        | Int ->
          (non_scanned_pos + 1, scanned_pos), load Word_int non_scanned_pos
        | Float ->
          ( (non_scanned_pos + ints_per_float, scanned_pos),
            load Double non_scanned_pos )
        | Float32 ->
          (* Float32 slots still take up a full word *)
          ( (non_scanned_pos + 1, scanned_pos),
            load (Single { reg = Float32 }) non_scanned_pos )
        | Vec128 ->
          (* Vectors stored in closures may not be 16-byte aligned. *)
          ( (non_scanned_pos + ints_per_vec128, scanned_pos),
            load Onetwentyeight_unaligned non_scanned_pos )
        | Val -> (non_scanned_pos, scanned_pos + 1), load Word_val scanned_pos
        | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
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
      let mode : Cmm.Alloc_mode.t =
        if num >= narity - nlocal then Local else Heap
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
              ( Calloc (mode, Alloc_block_kind_closure),
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

let raise_prim raise_kind ~extra_args arg dbg =
  if !Clflags.debug
  then Cop (Craise raise_kind, arg :: extra_args, dbg)
  else Cop (Craise Lambda.Raise_notrace, arg :: extra_args, dbg)

let negint arg dbg = Cop (Csubi, [Cconst_int (2, dbg); arg], dbg)

let addr_array_length arg dbg =
  let hdr = get_header_masked arg dbg in
  Cop (Cor, [addr_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)

(* CR-soon gyorsh: effects and coeffects for primitives are set conservatively
   to Arbitrary_effects and Has_coeffects, resp. Check if this can be improved
   (e.g., bswap). *)

let bbswap bi arg dbg =
  let bitwidth : Cmm.bswap_bitwidth =
    match (bi : Primitive.unboxed_integer) with
    | Unboxed_nativeint -> if size_int = 4 then Thirtytwo else Sixtyfour
    | Unboxed_int32 -> Thirtytwo
    | Unboxed_int64 -> Sixtyfour
  in
  let op = Cbswap { bitwidth } in
  if (bi = Primitive.Unboxed_int64 && size_int = 4)
     || not (Proc.operation_supported op)
  then
    let prim, tyarg =
      match (bi : Primitive.unboxed_integer) with
      | Unboxed_nativeint -> "nativeint", XInt
      | Unboxed_int32 -> "int32", XInt32
      | Unboxed_int64 -> "int64", XInt64
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

let div_int_caml arg1 arg2 dbg =
  let dividend_cannot_be_min_int =
    (* Since caml integers are tagged, we know that they when they're untagged,
       they can't be [Nativeint.min_int] *)
    true
  in
  tag_int
    (div_int ~dividend_cannot_be_min_int (untag_int arg1 dbg)
       (untag_int arg2 dbg) dbg)
    dbg

let mod_int_caml arg1 arg2 dbg =
  let dividend_cannot_be_min_int =
    (* Since caml integers are tagged, we know that they when they're untagged,
       they can't be [Nativeint.min_int] *)
    true
  in
  tag_int
    (mod_int ~dividend_cannot_be_min_int (untag_int arg1 dbg)
       (untag_int arg2 dbg) dbg)
    dbg

let and_int_caml arg1 arg2 dbg = and_int arg1 arg2 dbg

let or_int_caml arg1 arg2 dbg = or_int arg1 arg2 dbg

let xor_int_caml arg1 arg2 dbg =
  Cop
    ( Cor,
      [ xor_int (ignore_low_bit_int arg1) (ignore_low_bit_int arg2) dbg;
        Cconst_int (1, dbg) ],
      dbg )

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
  let n =
    (* This will sign extend. *)
    Nativeint.of_int32 n
  in
  Csymbol_address (global_symbol caml_int32_ops) :: Cint n :: cont

let emit_boxed_int64_constant_fields n cont =
  let lo = Int64.to_nativeint n in
  Csymbol_address (global_symbol caml_int64_ops) :: Cint lo :: cont

let emit_boxed_nativeint_constant_fields n cont =
  Csymbol_address (global_symbol caml_nativeint_ops) :: Cint n :: cont

let emit_float32_constant symb f cont =
  (* Here we are relying on the fact that the data section is zero initialized
     by just using [Csingle] and not worrying about the high 64 bits of the
     relevant field. *)
  emit_block symb boxedfloat32_header
    (Csymbol_address (global_symbol caml_float32_ops) :: Csingle f :: cont)

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
  let high = cconst_int (List.length namelist) in
  let body =
    let dbg = dbg () in
    let incr_i id = Cop (Caddi, [Cvar id; Cconst_int (1, dbg)], dbg) in
    let exit_if_last_iteration id =
      Cifthenelse
        ( Cop (Ccmpi Ceq, [Cvar id; high], dbg),
          dbg,
          Cexit (Lbl raise_num, [], []),
          dbg,
          Ctuple [],
          dbg,
          Any )
    in
    let cont = Lambda.next_raise_count () in
    let id = Backend_var.create_local "*id*" in
    ccatch
      ( raise_num,
        [],
        Ccatch
          ( Recursive,
            [ ( cont,
                [VP.create id, typ_int],
                Csequence
                  ( exit_if_last_iteration id,
                    Csequence (call (Cvar id), Cexit (Lbl cont, [incr_i id], []))
                  ),
                dbg,
                false ) ],
            Cexit (Lbl cont, [cconst_int 0], []),
            Any ),
        Ctuple [],
        dbg,
        Any,
        false )
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

let float32 ~dbg f = Cconst_float32 (f, dbg)

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
  | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cconst_vec128 _ | Clet _ | Cphantom_let _ | Ctuple _
  | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _
  | Ctrywith _ ->
    Clet (v, defining_expr, body)

let sequence x y =
  match x, y with
  | Ctuple [], _ -> y
  | _, Ctuple [] -> x
  | _, _ -> Csequence (x, y)

let ite ~dbg ~then_dbg ~then_ ~else_dbg ~else_ cond =
  Cifthenelse (cond, then_dbg, then_, else_dbg, else_, dbg, Any)

let trywith ~dbg ~body ~exn_var ~extra_args ~handler_cont ~handler () =
  Ctrywith (body, handler_cont, exn_var, extra_args, handler, dbg, Any)

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

let int64_as_float = unary (Creinterpret_cast Float_of_int64)

let float_as_int64 = unary (Creinterpret_cast Int64_of_float)

let int_of_float = unary (Cstatic_cast (Int_of_float Float64))

let float_of_int = unary (Cstatic_cast (Float_of_int Float64))

let int_of_float32 = unary (Cstatic_cast (Int_of_float Float32))

let float32_of_int = unary (Cstatic_cast (Float_of_int Float32))

let float32_of_float = unary (Cstatic_cast Float32_of_float)

let float_of_float32 = unary (Cstatic_cast Float_of_float32)

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

let float_abs = unary (Cabsf Float64)

let float_neg = unary (Cnegf Float64)

let float_add = binary (Caddf Float64)

let float_sub = binary (Csubf Float64)

let float_mul = binary (Cmulf Float64)

let float_div = binary (Cdivf Float64)

let float_eq = binary (Ccmpf (Float64, CFeq))

let float_neq = binary (Ccmpf (Float64, CFneq))

let float_lt = binary (Ccmpf (Float64, CFlt))

let float_le = binary (Ccmpf (Float64, CFle))

let float_gt = binary (Ccmpf (Float64, CFgt))

let float_ge = binary (Ccmpf (Float64, CFge))

let float32_abs = unary (Cabsf Float32)

let float32_neg = unary (Cnegf Float32)

let float32_add = binary (Caddf Float32)

let float32_sub = binary (Csubf Float32)

let float32_mul = binary (Cmulf Float32)

let float32_div = binary (Cdivf Float32)

let float32_eq = binary (Ccmpf (Float32, CFeq))

let float32_neq = binary (Ccmpf (Float32, CFneq))

let float32_lt = binary (Ccmpf (Float32, CFlt))

let float32_le = binary (Ccmpf (Float32, CFle))

let float32_gt = binary (Ccmpf (Float32, CFgt))

let float32_ge = binary (Ccmpf (Float32, CFge))

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
  | Pbigarray_float16 ->
    assert (match elt_chunk with Sixteen_unsigned -> true | _ -> false);
    float_of_float16 dbg (load ~dbg elt_chunk Mutable ~addr)
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
  | Pbigarray_float16 ->
    assert (match elt_chunk with Sixteen_unsigned -> true | _ -> false);
    let new_value = float16_of_float dbg new_value in
    return_unit dbg (store ~dbg elt_chunk Assignment ~addr ~new_value)
  | _ -> return_unit dbg (store ~dbg elt_chunk Assignment ~addr ~new_value)

(* Infix field address. Contrary to regular field addresses, these addresses are
   valid ocaml values, and can be live at gc points. *)

let infix_field_address ~dbg ptr n =
  if n = 0
  then ptr
  else Cmm.Cop (Cmm.Caddv, [ptr; int ~dbg (n * Arch.size_addr)], dbg)

(* Data items *)

let cint i = Cmm.Cint i

let cint32 i = Cmm.Cint32 (Nativeint.of_int32 i)

let cfloat32 f = Cmm.Csingle f

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
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cvar _ | Cconst_vec128 _ ->
    Some 0
  | Cop _ -> Some (cmm_arith_size0 e)
  | Clet _ | Cphantom_let _ | Ctuple _ | Csequence _ | Cifthenelse _ | Cswitch _
  | Ccatch _ | Cexit _ | Ctrywith _ ->
    None

(* Atomics *)

let atomic_load ~dbg (imm_or_ptr : Lambda.immediate_or_pointer) atomic =
  let memory_chunk =
    match imm_or_ptr with Immediate -> Word_int | Pointer -> Word_val
  in
  Cop (mk_load_atomic memory_chunk, [atomic], dbg)

let atomic_exchange_extcall ~dbg atomic ~new_value =
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

let atomic_exchange ~dbg (imm_or_ptr : Lambda.immediate_or_pointer) atomic
    ~new_value =
  match imm_or_ptr with
  | Immediate ->
    let op = Catomic { op = Exchange; size = Word } in
    if Proc.operation_supported op
    then Cop (op, [new_value; atomic], dbg)
    else atomic_exchange_extcall ~dbg atomic ~new_value
  | Pointer -> atomic_exchange_extcall ~dbg atomic ~new_value

let atomic_arith ~dbg ~op ~untag ~ext_name atomic i =
  let i = if untag then decr_int i dbg else i in
  let op = Catomic { op; size = Word } in
  if Proc.operation_supported op
  then (* input is a tagged integer *)
    Cop (op, [i; atomic], dbg)
  else
    Cop
      ( Cextcall
          { func = ext_name;
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

let atomic_fetch_and_add ~dbg atomic i =
  atomic_arith ~dbg ~untag:true ~op:Fetch_and_add
    ~ext_name:"caml_atomic_fetch_add" atomic i

let atomic_add ~dbg atomic i =
  atomic_arith ~dbg ~untag:true ~op:Add ~ext_name:"caml_atomic_add" atomic i
  |> return_unit dbg

let atomic_sub ~dbg atomic i =
  atomic_arith ~dbg ~untag:true ~op:Sub ~ext_name:"caml_atomic_sub" atomic i
  |> return_unit dbg

let atomic_land ~dbg atomic i =
  atomic_arith ~dbg ~untag:false ~op:Land ~ext_name:"caml_atomic_land" atomic i
  |> return_unit dbg

let atomic_lor ~dbg atomic i =
  atomic_arith ~dbg ~untag:false ~op:Lor ~ext_name:"caml_atomic_lor" atomic i
  |> return_unit dbg

let atomic_lxor ~dbg atomic i =
  atomic_arith ~dbg ~untag:true ~op:Lxor ~ext_name:"caml_atomic_lxor" atomic i
  |> return_unit dbg

let atomic_compare_and_set_extcall ~dbg atomic ~old_value ~new_value =
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

let atomic_compare_and_set ~dbg (imm_or_ptr : Lambda.immediate_or_pointer)
    atomic ~old_value ~new_value =
  match imm_or_ptr with
  | Immediate ->
    let op = Catomic { op = Compare_set; size = Word } in
    if Proc.operation_supported op
    then
      (* Use a bind to ensure [tag_int] gets optimised. *)
      bind "res"
        (Cop (op, [old_value; new_value; atomic], dbg))
        (fun a2 -> tag_int a2 dbg)
    else atomic_compare_and_set_extcall ~dbg atomic ~old_value ~new_value
  | Pointer -> atomic_compare_and_set_extcall ~dbg atomic ~old_value ~new_value

let atomic_compare_exchange_extcall ~dbg atomic ~old_value ~new_value =
  Cop
    ( Cextcall
        { func = "caml_atomic_compare_exchange";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_val;
          ty_args = [];
          alloc = false
        },
      [atomic; old_value; new_value],
      dbg )

let atomic_compare_exchange ~dbg (imm_or_ptr : Lambda.immediate_or_pointer)
    atomic ~old_value ~new_value =
  match imm_or_ptr with
  | Immediate ->
    let op = Catomic { op = Compare_exchange; size = Word } in
    if Proc.operation_supported op
    then Cop (op, [old_value; new_value; atomic], dbg)
    else atomic_compare_exchange_extcall ~dbg atomic ~old_value ~new_value
  | Pointer -> atomic_compare_exchange_extcall ~dbg atomic ~old_value ~new_value

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
              zero_extend ~bits:32 a ~dbg;
              Cop (Clsl, [b; Cconst_int (32, dbg)], dbg) ],
            dbg )
      in
      aux (i :: acc) r
  in
  aux [] unboxed_int32_list

let allocate_unboxed_int32_array ~elements (mode : Cmm.Alloc_mode.t) dbg =
  let num_elts, payload = make_unboxed_int32_array_payload dbg elements in
  let header =
    let size = 1 (* custom_ops field *) + List.length payload in
    match mode with
    | Heap -> custom_header ~size
    | Local -> custom_local_header ~size
  in
  let custom_ops =
    (* For odd-length unboxed int32 arrays there are 32 bits spare at the end of
       the block, which are never read. They are initialized to the sign
       extension of the last element. *)
    match num_elts with
    | Even -> custom_ops_unboxed_int32_even_array
    | Odd -> custom_ops_unboxed_int32_odd_array
  in
  Cop
    ( Calloc (mode, Alloc_block_kind_int32_u_array),
      Cconst_natint (header, dbg) :: custom_ops :: payload,
      dbg )

let make_unboxed_float32_array_payload dbg unboxed_float32_list =
  if Sys.big_endian
  then
    Misc.fatal_error "Big-endian platforms not yet supported for unboxed arrays";
  let rec aux acc = function
    | [] -> Even, List.rev acc
    | a :: [] -> Odd, List.rev (a :: acc)
    | a :: b :: r ->
      let i =
        Cop
          ( Cpackf32,
            [ Cop (Creinterpret_cast Float_of_float32, [a], dbg);
              Cop (Creinterpret_cast Float_of_float32, [b], dbg) ],
            dbg )
      in
      aux (i :: acc) r
  in
  aux [] unboxed_float32_list

let allocate_unboxed_float32_array ~elements (mode : Cmm.Alloc_mode.t) dbg =
  let num_elts, payload = make_unboxed_float32_array_payload dbg elements in
  let header =
    let size = 1 (* custom_ops field *) + List.length payload in
    match mode with
    | Heap -> custom_header ~size
    | Local -> custom_local_header ~size
  in
  let custom_ops =
    (* For odd-length unboxed float32 arrays there are 32 bits spare at the end
       of the block, which are never read. They are *not* initialized. *)
    match num_elts with
    | Even -> custom_ops_unboxed_float32_even_array
    | Odd -> custom_ops_unboxed_float32_odd_array
  in
  Cop
    ( Calloc (mode, Alloc_block_kind_float32_u_array),
      Cconst_natint (header, dbg) :: custom_ops :: payload,
      dbg )

let allocate_unboxed_int64_or_nativeint_array custom_ops ~elements
    (mode : Cmm.Alloc_mode.t) dbg =
  let header =
    let size = 1 (* custom_ops field *) + List.length elements in
    match mode with
    | Heap -> custom_header ~size
    | Local -> custom_local_header ~size
  in
  Cop
    ( Calloc (mode, Alloc_block_kind_int64_u_array),
      Cconst_natint (header, dbg) :: custom_ops :: elements,
      dbg )

let allocate_unboxed_int64_array =
  allocate_unboxed_int64_or_nativeint_array custom_ops_unboxed_int64_array

let allocate_unboxed_nativeint_array =
  allocate_unboxed_int64_or_nativeint_array custom_ops_unboxed_nativeint_array

let allocate_unboxed_vec128_array ~elements (mode : Cmm.Alloc_mode.t) dbg =
  let header =
    let size =
      1 (* custom_ops field *) + (ints_per_vec128 * List.length elements)
    in
    match mode with
    | Heap -> custom_header ~size
    | Local -> custom_local_header ~size
  in
  Cop
    ( Calloc (mode, Alloc_block_kind_vec128_u_array),
      Cconst_natint (header, dbg) :: custom_ops_unboxed_vec128_array :: elements,
      dbg )

(* Drop internal optional arguments from exported interface *)
let block_header x y = block_header x y

let dls_get ~dbg = Cop (Cdls_get, [], dbg)

let perform ~dbg eff =
  let cont =
    make_alloc dbg ~tag:Runtimetags.cont_tag
      [int_const dbg 0; int_const dbg 0]
      ~mode:Cmm.Alloc_mode.Heap
  in
  (* Rc_normal means "allow tailcalls". Preventing them here by using Rc_nontail
     improves backtraces of paused fibers. *)
  Cop
    ( Capply (typ_val, Rc_nontail),
      [Cconst_symbol (Cmm.global_symbol "caml_perform", dbg); eff; cont],
      dbg )

let run_stack ~dbg ~stack ~f ~arg =
  (* Rc_normal would be fine here, but this is unlikely to ever be a tail call
     (usages of this primitive shouldn't be generated in tail position), so we
     use Rc_nontail for clarity. *)
  Cop
    ( Capply (typ_val, Rc_nontail),
      [Cconst_symbol (Cmm.global_symbol "caml_runstack", dbg); stack; f; arg],
      dbg )

let resume ~dbg ~stack ~f ~arg ~last_fiber =
  (* Rc_normal is required here, because there are some uses of effects with
     repeated resumes, and these should consume O(1) stack space by tail-calling
     caml_resume. *)
  Cop
    ( Capply (typ_val, Rc_normal),
      [ Cconst_symbol (Cmm.global_symbol "caml_resume", dbg);
        stack;
        f;
        arg;
        last_fiber ],
      dbg )

let reperform ~dbg ~eff ~cont ~last_fiber =
  (* Rc_normal is required here, this is used in tail position and should tail
     call. *)
  Cop
    ( Capply (typ_val, Rc_normal),
      [ Cconst_symbol (Cmm.global_symbol "caml_reperform", dbg);
        eff;
        cont;
        last_fiber ],
      dbg )

let poll ~dbg = return_unit dbg (Cop (Cpoll, [], dbg))

module Scalar_type = struct
  module Float_width = struct
    type t = Cmm.float_width =
      | Float64
      | Float32

    let[@inline] static_cast ~dbg ~src ~dst exp =
      match src, dst with
      | Float64, Float64 -> exp
      | Float32, Float32 -> exp
      | Float32, Float64 -> float_of_float32 ~dbg exp
      | Float64, Float32 -> float32_of_float ~dbg exp
  end

  module Signedness = struct
    type t =
      | Signed
      | Unsigned

    let equal (x : t) (y : t) = x = y

    let print ppf t =
      match t with
      | Signed -> Format.pp_print_string ppf "signed"
      | Unsigned -> Format.pp_print_string ppf "unsigned"
  end

  module Bit_width_and_signedness : sig
    (** An integer with signedness [signedness t] that fits into a general-purpose
        register. It is canonically stored in twos-complement representation, in the lower
        [bits] bits of its container (whether that be memory or a register), and is sign-
        or zero-extended to fill the entire container. *)
    type t [@@immediate]

    val create_exn : bit_width:int -> signedness:Signedness.t -> t

    val bit_width : t -> int

    val signedness : t -> Signedness.t

    val equal : t -> t -> bool
  end = struct
    (* [signedness t] is stored in the low bit of [t], and [bit_width t] is
       stored in the remaining high bits of [t]. We use this encoding to fit [t]
       into an immediate value. This is worth trying since we expect to create
       one of these for ~every integer operation, so it should cut down on
       garbage *)
    type t = { bit_width_and_signedness : int } [@@unboxed]

    let[@inline] equal { bit_width_and_signedness = x }
        { bit_width_and_signedness = y } =
      Int.equal x y

    let[@inline] bit_width { bit_width_and_signedness } =
      bit_width_and_signedness lsr 1

    let[@inline] signedness { bit_width_and_signedness } =
      match bit_width_and_signedness land 1 with
      | 0 -> Signedness.Signed
      | 1 -> Signedness.Unsigned
      | _ -> assert false

    let[@inline] int_of_signedness : Signedness.t -> int = function
      | Signed -> 0
      | Unsigned -> 1

    let[@inline] create_exn ~bit_width ~signedness =
      assert (0 < bit_width && bit_width <= arch_bits);
      { bit_width_and_signedness =
          (bit_width lsl 1) + int_of_signedness signedness
      }
  end

  module Integral_type = struct
    include Bit_width_and_signedness

    let[@inline] with_signedness t ~signedness =
      create_exn ~bit_width:(bit_width t) ~signedness

    let[@inline] signed t = with_signedness t ~signedness:Signed

    let[@inline] unsigned t = with_signedness t ~signedness:Unsigned

    (** Determines whether [dst] can represent every value of [src], preserving sign *)
    let[@inline] can_cast_without_losing_information ~src ~dst =
      match signedness src, signedness dst with
      | Signed, Signed | Unsigned, Unsigned -> bit_width src <= bit_width dst
      | Unsigned, Signed -> bit_width src < bit_width dst
      | Signed, Unsigned -> false

    let[@inline] static_cast ~dbg ~src ~dst exp =
      if can_cast_without_losing_information ~src ~dst
      then
        (* Since [Bit_width_and_signedness] represents sign- or zero-extended
           expressions, this is a no-op *)
        exp
      else
        match signedness dst with
        | Signed -> sign_extend ~bits:(bit_width dst) exp ~dbg
        | Unsigned -> zero_extend ~bits:(bit_width dst) exp ~dbg

    let[@inline] conjugate ~outer ~inner ~dbg ~f x =
      x
      |> static_cast ~src:outer ~dst:inner ~dbg
      |> f
      |> static_cast ~src:inner ~dst:outer ~dbg
  end

  module Integer = struct
    include Integral_type

    let print ppf t =
      Format.fprintf ppf "%a int%d" Signedness.print (signedness t)
        (bit_width t)

    let nativeint = create_exn ~bit_width:arch_bits ~signedness:Signed
  end

  (** An {!Integer.t} but with the additional stipulation that its container must
      reserve its lowest bit to be 1. The [bit_width] field includes this bit. *)
  module Tagged_integer = struct
    include Integral_type

    let[@inline] create_exn ~bit_width_including_tag_bit:bit_width ~signedness =
      assert (bit_width > 1);
      create_exn ~bit_width ~signedness

    let immediate =
      create_exn ~bit_width_including_tag_bit:arch_bits ~signedness:Signed

    let[@inline] bit_width_including_tag_bit t = bit_width t

    let[@inline] bit_width_excluding_tag_bit t = bit_width t - 1

    let[@inline] untagged t =
      Integer.create_exn
        ~bit_width:(bit_width_excluding_tag_bit t)
        ~signedness:(signedness t)

    let[@inline] untag ~dbg t exp =
      match signedness t with
      | Signed -> asr_const exp 1 dbg
      | Unsigned -> lsr_const exp 1 dbg

    let print ppf t =
      Format.fprintf ppf "tagged %a int%d" Signedness.print (signedness t)
        (bit_width_excluding_tag_bit t)
  end

  module Integral = struct
    type t =
      | Untagged of Integer.t
      | Tagged of Tagged_integer.t

    let nativeint = Untagged Integer.nativeint

    let[@inline] untagged_or_identity = function
      | Untagged t -> t
      | Tagged t -> Tagged_integer.untagged t

    let signedness = function
      | Untagged t -> Integer.signedness t
      | Tagged t -> Tagged_integer.signedness t

    let with_signedness t ~signedness =
      match t with
      | Untagged t -> Untagged (Integer.with_signedness t ~signedness)
      | Tagged t -> Tagged (Tagged_integer.with_signedness t ~signedness)

    let[@inline] signed t = with_signedness t ~signedness:Signed

    let[@inline] unsigned t = with_signedness t ~signedness:Unsigned

    let[@inline] equal x y =
      match x, y with
      | Untagged x, Untagged y -> Integer.equal x y
      | Untagged _, _ -> false
      | Tagged x, Tagged y -> Tagged_integer.equal x y
      | Tagged _, _ -> false

    let print ppf t =
      match t with
      | Untagged untagged -> Integer.print ppf untagged
      | Tagged tagged -> Tagged_integer.print ppf tagged

    let[@inline] can_cast_without_losing_information ~src ~dst =
      Integer.can_cast_without_losing_information
        ~src:(untagged_or_identity src) ~dst:(untagged_or_identity dst)

    let static_cast ~dbg ~src ~dst exp =
      match src, dst with
      | Untagged src, Untagged dst -> Integer.static_cast ~dbg ~src ~dst exp
      | Tagged src, Tagged dst -> Tagged_integer.static_cast ~dbg ~src ~dst exp
      | Untagged src, Tagged dst ->
        tag_int
          (Integer.static_cast ~dbg ~src ~dst:(Tagged_integer.untagged dst) exp)
          dbg
      | Tagged src, Untagged dst ->
        Integer.static_cast ~dbg
          ~src:(Tagged_integer.untagged src)
          ~dst
          (Tagged_integer.untag ~dbg src exp)

    let[@inline] conjugate ~outer ~inner ~dbg ~f x =
      x
      |> static_cast ~src:outer ~dst:inner ~dbg
      |> f
      |> static_cast ~src:inner ~dst:outer ~dbg
  end

  type t =
    | Integral of Integral.t
    | Float of Float_width.t

  let static_cast ~dbg ~src ~dst exp =
    match src, dst with
    | Integral src, Integral dst -> Integral.static_cast ~dbg ~src ~dst exp
    | Float src, Float dst -> Float_width.static_cast ~dbg ~src ~dst exp
    | Integral src, Float dst ->
      let float_of_int_arg = Integral.nativeint in
      if not
           (Integral.can_cast_without_losing_information ~src
              ~dst:float_of_int_arg)
      then
        Misc.fatal_errorf "static_cast: casting %a to float is not implemented"
          Integral.print src
      else
        unary (Cstatic_cast (Float_of_int dst)) ~dbg
          (Integral.static_cast exp ~dbg ~src ~dst:float_of_int_arg)
    | Float src, Integral dst -> (
      match Integral.signedness dst with
      | Unsigned ->
        Misc.fatal_errorf
          "static_cast: casting floats to unsigned values is not implemented"
      | Signed ->
        (* we can truncate because casting from float -> int is unspecified when
           the rounded value doesn't fit in the integral type. We can't promote
           since nativeint is already the largest integral type supported
           here. *)
        let exp = unary (Cstatic_cast (Int_of_float src)) exp ~dbg in
        let src = Integral.nativeint in
        (* assert that nativeint is indeed the largest integer width *)
        assert (Integral.can_cast_without_losing_information ~src:dst ~dst:src);
        Integral.static_cast exp ~dbg ~src ~dst)

  let[@inline] conjugate ~outer ~inner ~dbg ~f x =
    x
    |> static_cast ~src:outer ~dst:inner ~dbg
    |> f
    |> static_cast ~src:inner ~dst:outer ~dbg

  module Untagged = struct
    type numeric = t

    type t =
      | Untagged of Integer.t
      | Float of float_width

    let to_numeric : t -> numeric = function
      | Untagged width -> Integral (Untagged width)
      | Float float -> Float float

    let[@inline] static_cast ~dbg ~src ~dst exp =
      static_cast ~dbg ~src:(to_numeric src) ~dst:(to_numeric dst) exp
  end
end
