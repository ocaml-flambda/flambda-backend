[@@@ocaml.warning "+a-40-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Arch

module Width_in_bits = struct
  type t =
    | W8
    | W16
    | W32
    | W64
    | W128
    | W256
    | W512

  let of_memory_chunk (c : Cmm.memory_chunk) =
    match c with
    | Byte_unsigned | Byte_signed -> W8
    | Sixteen_unsigned | Sixteen_signed -> W16
    | Thirtytwo_unsigned | Thirtytwo_signed | Single _ -> W32
    | Word_int | Word_val | Double -> W64
    | Onetwentyeight_unaligned | Onetwentyeight_aligned -> W128
    | Twofiftysix_unaligned | Twofiftysix_aligned -> W256
    | Fivetwelve_unaligned | Fivetwelve_aligned -> W512

  let of_atomic_bitwidth (b : Cmm.atomic_bitwidth) =
    match b with Thirtytwo -> W32 | Sixtyfour -> W64 | Word -> W64

  let to_int t =
    match t with
    | W512 -> 512
    | W256 -> 256
    | W128 -> 128
    | W64 -> 64
    | W32 -> 32
    | W16 -> 16
    | W8 -> 8

  let equal t1 t2 =
    match t1, t2 with
    | W512, W512 -> true
    | W256, W256 -> true
    | W128, W128 -> true
    | W64, W64 -> true
    | W32, W32 -> true
    | W16, W16 -> true
    | W8, W8 -> true
    | (W512 | W256 | W128 | W64 | W32 | W16 | W8), _ -> false

  let print ppf t = Format.fprintf ppf "%d" (to_int t)
end

module Memory_access = struct
  module Init_or_assign = struct
    type t =
      | Initialization
      | Assignment
  end

  type desc =
    | Alloc
    | Arbitrary
    | Read of
        { width_in_bits : Width_in_bits.t;
          addressing_mode : addressing_mode;
          is_mutable : bool;
          is_atomic : bool
        }
    | Write of
        { width_in_bits : Width_in_bits.t;
          addressing_mode : addressing_mode;
          init_or_assign : Init_or_assign.t
        }
    | Read_and_write of
        { width_in_bits : Width_in_bits.t;
          addressing_mode : addressing_mode;
          is_atomic : bool
        }

  type t =
    { desc : desc;
      first_memory_arg_index : int
    }

  let create ?(first_memory_arg_index = 0) desc =
    { desc; first_memory_arg_index }

  let desc t = t.desc

  let first_memory_arg_index t = t.first_memory_arg_index

  let alignment_in_bytes _t =
    (* CR-someday gyorsh: propagate alignment of base address (such as
       bigarray). Can be used to emit more efficient vector sequences, for
       example, arithmetic operations with memory arguments (not stack). *)
    Arch.size_int
end

module Vectorized_instruction = struct
  type register =
    | New_Vec128 of int
    | Argument of int
    | Result of int
    | Original of int

  type t =
    { operation : Operation.t;
      arguments : register array;
      results : register array
    }

  let print ppf t = Format.fprintf ppf "%a " Cfg.dump_basic (Cfg.Op t.operation)

  let make_default ~arg_count ~res_count operation : t =
    { operation;
      arguments = Array.init arg_count (fun i -> Argument i);
      results = Array.init res_count (fun i -> Result i)
    }
end

let vectorizable_machtypes (r1 : Reg.t) (r2 : Reg.t) =
  match r1.typ, r2.typ with
  | Addr, _ | _, Addr ->
    (* Register of type [Addr] can point into the middle of a heap block. It
       must not be live across a GC as the pointer can be invalidated if the GC
       moves the block. This information would be lost if we combined [Addr]
       with another non-scannable type into [Vec128]. To correctly vectorize
       [Addr], we could generalize [machtype], but for simplicity do not
       vectorize [Addr]. *)
    false
  | ( (Vec128 | Vec256 | Vec512 | Valx2),
      (Val | Int | Float | Float32 | Vec128 | Vec256 | Vec512 | Valx2) )
  | (Val | Int | Float | Float32), (Vec128 | Vec256 | Vec512 | Valx2) ->
    Misc.fatal_errorf "Unexpected vector machtype: %a %a" Printreg.reg r1
      Printreg.reg r2
  | Val, Val -> true
  | Val, (Int | Float | Float32) | (Int | Float | Float32), Val -> false
  | (Int | Float | Float32), (Int | Float | Float32) ->
    (* It is safe to mix Float32, Float, and Int for the purpose of GC, because
       they are not scannable. It may not be possible to vectorize the
       operation. *)
    true

let vectorize_machtypes (pack : Reg.t list) : Cmm.machtype_component =
  match pack with
  | [] -> assert false
  | hd :: tl -> (
    let can_vectorize = List.for_all (vectorizable_machtypes hd) tl in
    if not can_vectorize
    then
      Misc.fatal_errorf "register pack with incompatible mach types:"
        Printreg.reglist pack;
    match hd.typ, List.length pack with
    | Addr, _ -> Misc.fatal_errorf "Unexpected machtype for %a" Printreg.reg hd
    | Float, 2 | Float32, 4 -> Vec128
    | Int, _ ->
      (* [Int] may be used for int32, width should be correct by construction of
         [Group]. *)
      Vec128
    | Val, 2 -> Valx2
    | (Val | Float | Float32), n ->
      Misc.fatal_errorf "Unexpected pack size %d for %a" n Printreg.reglist pack
    | Vec128, _ | Vec256, _ | Vec512, _ | Valx2, _ ->
      Misc.fatal_errorf "Unexpected machtype for %a" Printreg.reg hd)
