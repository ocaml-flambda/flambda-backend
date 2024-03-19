(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module L = Linear
module V = Backend_var

module Key = struct
  type t = Reg_with_debug_info.t

  type key = t

  module Raw_set = Reg_with_debug_info.Set

  module Set = struct
    include Reg_availability_set

    let print ppf t = print ~print_reg:Printmach.reg ppf t
  end

  module Map = Map.Make (struct
    type t = Reg_with_debug_info.t

    let compare = Reg_with_debug_info.compare
  end)

  let print ppf t = Reg_with_debug_info.print ~print_reg:Printmach.reg ppf t

  let all_parents _ = []
end

module Vars = struct
  module RD = Reg_with_debug_info

  (* By the time this pass runs, register stamps are irrelevant; indeed, there
     may be multiple registers with different stamps assigned to the same
     location. As such, we quotient register sets by the equivalence relation
     that identifies two registers iff they have the same name and location. *)
  (* CR mshinwell: as part of rethinking the Reg_availability_set stuff, we
     should think about the above comment. Should we be using
     RD.Set_distinguishing_names_and_locations? *)
  module Key = Key
  module Index = V

  module Subrange_state : sig
    include Compute_ranges_intf.S_subrange_state

    val stack_offset : t -> int
  end = struct
    type t = { stack_offset : int }

    let create () = { stack_offset = Proc.initial_stack_offset }

    let advance_over_instruction t (insn : L.instruction) =
      let stack_offset =
        match insn.desc with
        | Lop (Istackoffset delta) -> t.stack_offset + delta
        | Lpushtrap _ -> t.stack_offset + Proc.trap_frame_size_in_bytes
        | Lpoptrap -> t.stack_offset - Proc.trap_frame_size_in_bytes
        | Lend | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Llabel _
        | Lbranch _ | Lcondbranch _ | Lcondbranch3 _ | Lswitch _ | Lentertrap
        | Lraise _ | Ladjust_stack_offset _ | Lstackcheck _ ->
          (* CR xclerc for xclerc: double check `Lstackcheck`. *)
          t.stack_offset
      in
      { stack_offset }

    let stack_offset t = t.stack_offset
  end

  module Subrange_info : sig
    include
      Compute_ranges_intf.S_subrange_info
        with type key := Key.t
        with type subrange_state := Subrange_state.t

    val reg : t -> Reg.t

    val offset : t -> Stack_reg_offset.t option
  end = struct
    type t =
      { reg : Reg.t;
        offset : Stack_reg_offset.t option
      }

    let print ppf { reg; offset } =
      Format.fprintf ppf "@[<hov 1>((reg %a)@ (offset %a))@]" Printmach.reg reg
        (Misc.Stdlib.Option.print Stack_reg_offset.print)
        offset

    let create reg subrange_state ~fun_contains_calls ~fun_num_stack_slots =
      let reg = RD.reg reg in
      let stack_offset = Subrange_state.stack_offset subrange_state in
      let offset =
        match reg.loc with
        | Stack loc ->
          let frame_size =
            Proc.frame_size ~stack_offset ~fun_contains_calls
              ~fun_num_stack_slots
          in
          let slot_offset =
            Proc.slot_offset loc
              ~stack_class:(Proc.stack_slot_class reg.typ)
              ~stack_offset ~fun_contains_calls ~fun_num_stack_slots
          in
          let offset : Stack_reg_offset.t =
            match slot_offset with
            | Bytes_relative_to_stack_pointer i ->
              Bytes_relative_to_cfa (frame_size - i)
            | Bytes_relative_to_domainstate_pointer i ->
              Bytes_relative_to_domainstate_pointer i
          in
          Some offset
        | Reg _ | Unknown -> None
      in
      { reg; offset }

    let reg t = t.reg

    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from the
       CFA, not from the stack pointer. *)

    let offset t = t.offset
  end

  module Range_info : sig
    include
      Compute_ranges_intf.S_range_info
        with type key := Key.t
        with type index := Index.t

    val provenance : t -> V.Provenance.t option

    val is_parameter : t -> Is_parameter.t
  end = struct
    type t =
      { provenance : V.Provenance.t option;
        is_parameter : Is_parameter.t
      }

    let print ppf { provenance; is_parameter } =
      Format.fprintf ppf "@[<hov 1>((provenance %a)@ (is_parameter %a))@]"
        (Misc.Stdlib.Option.print V.Provenance.print)
        provenance Is_parameter.print is_parameter

    let create _fundecl reg ~start_insn:_ =
      match RD.debug_info reg with
      | None -> None
      | Some debug_info ->
        let var = RD.Debug_info.holds_value_of debug_info in
        let provenance = RD.Debug_info.provenance debug_info in
        let is_parameter = RD.Debug_info.is_parameter debug_info in
        let t = { provenance; is_parameter } in
        Some (var, t)

    let provenance t = t.provenance

    let is_parameter t = t.is_parameter
  end

  (* CR mshinwell: update comment to explain what "subset inclusion" means
     here *)
  (* Important note: [Reg_availability_set.canonicalise] does not preserve
     subset inclusion. This means in particular that a canonicalised
     [available_across] set may not be a subset of the corresponding
     canonicalised [available_before]. [Compute_ranges] can cope with this. *)

  let availability_set_to_key_set (avail : Reg_availability_set.t) =
    Reg_availability_set.canonicalise avail

  let available_before (insn : L.instruction) =
    Option.map availability_set_to_key_set insn.available_before

  let available_across (insn : L.instruction) =
    match insn.available_across with
    | None -> available_before insn
    | Some across -> Some (availability_set_to_key_set across)
end

module Subrange_state = Vars.Subrange_state
module Subrange_info = Vars.Subrange_info
module Range_info = Vars.Range_info
include Compute_ranges.Make (Vars)
