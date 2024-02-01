(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type valnum = int

(* Classification of operations *)

type op_class =
  | Op_pure           (* pure arithmetic, produce one or several result *)
  | Op_load of Mach.mutable_flag (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

(* We maintain sets of equations of the form
       valnums = operation(valnums)
   plus a mapping from registers to valnums (value numbers). *)

module type Operation = sig
  type t
end

module type S = sig

  type op

  type rhs = op * valnum array

  module Equations : sig
    module Rhs_map : Map.S with type key = rhs

    type 'a t =
      { mutable_load_equations : 'a Rhs_map.t;
        other_equations : 'a Rhs_map.t }
  end

  type numbering =
    { num_next: int;                      (* next fresh value number *)
      num_eqs: valnum array Equations.t;  (* mapping rhs -> valnums *)
      num_reg: valnum Reg.Map.t }         (* mapping register -> valnum *)

  val empty_numbering : numbering

  val valnum_regs : numbering -> Reg.t array -> numbering * valnum array

  val find_equation : op_class -> numbering -> rhs -> valnum array option

  val find_regs_containing : numbering -> valnum array -> Reg.t array option

  val set_known_regs : numbering -> Reg.t array -> valnum array -> numbering

  val set_move : numbering -> Reg.t -> Reg.t -> numbering

  val set_fresh_regs : numbering -> Reg.t array -> rhs -> op_class -> numbering

  val set_unknown_regs : numbering -> Reg.t array -> numbering

  val remove_mutable_load_numbering : numbering -> numbering

  val kill_addr_regs : numbering -> numbering

end

module Make (Op : Operation) : S with type op = Op.t
