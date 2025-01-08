open Arch

module Width_in_bits = struct
  type t =
    | W8
    | W16
    | W32
    | W64
    | W128

  let of_memory_chunk (c : Cmm.memory_chunk) =
    match c with
    | Byte_unsigned | Byte_signed -> W8
    | Sixteen_unsigned | Sixteen_signed -> W16
    | Thirtytwo_unsigned | Thirtytwo_signed | Single _ -> W32
    | Word_int | Word_val | Double -> W64
    | Onetwentyeight_unaligned | Onetwentyeight_aligned -> W128

  let of_atomic_bitwidth (b : Cmm.atomic_bitwidth) =
    match b with Thirtytwo -> W32 | Sixtyfour -> W64 | Word -> W64

  let to_int t =
    match t with W128 -> 128 | W64 -> 64 | W32 -> 32 | W16 -> 16 | W8 -> 8

  let equal t1 t2 =
    match t1, t2 with
    | W128, W128 -> true
    | W64, W64 -> true
    | W32, W32 -> true
    | W16, W16 -> true
    | W8, W8 -> true
    | (W128 | W64 | W32 | W16 | W8), _ -> false

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

  let alignment_in_bytes t =
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
  | (Vec128 | Valx2), (Val | Int | Addr | Float | Float32 | Vec128 | Valx2)
  | (Val | Int | Addr | Float | Float32), (Vec128 | Valx2) ->
    Misc.fatal_errorf "Unexpected vector machtype Vec128 or Valx2: %a %a"
      Printreg.reg r1 Printreg.reg r2
  | Val, Val -> true
  | Val, (Int | Addr | Float | Float32) | (Int | Addr | Float | Float32), Val ->
    false
  | (Int | Addr | Float | Float32), (Int | Addr | Float | Float32) ->
    (* It is safe to mix Float32, Float, and Int for the purpose of GC, because
       they are not scannable. It may not be possible to vectorize the
       operation. *)
    true
