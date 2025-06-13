[@@@ocaml.warning "+a-40-41-42"]

open Arch

module Width_in_bits : sig
  type t =
    | W8
    | W16
    | W32
    | W64
    | W128
    | W256
    | W512

  val of_memory_chunk : Cmm.memory_chunk -> t

  val of_atomic_bitwidth : Cmm.atomic_bitwidth -> t

  val to_int : t -> int

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

module Memory_access : sig
  module Init_or_assign : sig
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

  type t

  val create : ?first_memory_arg_index:int -> desc -> t

  val desc : t -> desc

  val first_memory_arg_index : t -> int

  (** Base address of memory access [t] is guaranteed to be aligned to
      at least [alignment_in_bytes t]. *)
  val alignment_in_bytes : t -> int
end

module Vectorized_instruction : sig
  (** Registers used in vectorized instructions of one scalar instruction
     group. *)
  type register =
    | New_Vec128 of int
        (** The n-th new temporary register used in the vectorized instructions *)
    | Argument of int
        (** Vector version of the n-th argument's register of the scalar
       instruction *)
    | Result of int
        (** Vector version of the n-th result's register of the scalar instruction *)
    | Original of int
        (** Keep the original instruction in the n-th argument/result (depending on whether
          it is used in the argument or result of the vectorized instructions) of the
          scalar instruction*)

  type t =
    { operation : Operation.t;
      arguments : register array;
      results : register array
    }

  val print : Format.formatter -> t -> unit

  val make_default : arg_count:int -> res_count:int -> Operation.t -> t
end

(** Given two registers of non-vector types, return true iff there exist a vector type
    that can contain both of them. Currently distinguishes between [Val] and other
    types. Mixing [Val] with non-Val in a vector is not yet supported. *)
val vectorizable_machtypes : Reg.t -> Reg.t -> bool

val vectorize_machtypes : Reg.t list -> Cmm.machtype_component
(* CR-someday gyorsh: [vectorizable_machtypes] should take a [Reg.t list]
   instead of a pair, to handle longer vectors, and to present a uniform
   interface with [vectorize_machtypes]. *)
