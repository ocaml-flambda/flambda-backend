(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module L = Linear

(* CR mshinwell/xclerc/poechsel:

   1. maybe "set" isn't the correct algebraic structure for the Compute_ranges
   interface

   2. maybe a list-based structure could be used here instead (not for variable
   ranges) *)

module Inlined_frames = struct
  module Key = struct
    module T0 = struct
      type t = Debuginfo.t

      let print = Debuginfo.print_compact_extended

      let compare t1 t2 =
        (* It needs to be the case that [compare shorter longer] returns -1 (and
           vice-versa) when [shorter] is a prefix of [longer] whilst also having
           fewer frames. See dwarf_inlined_frames:create_up_to_root. *)
        let items1 = Debuginfo.to_items t1 in
        let items2 = Debuginfo.to_items t2 in
        let rec loop (items1 : Debuginfo.item list)
            (items2 : Debuginfo.item list) =
          match items1, items2 with
          | [], [] -> 0
          | [], _ :: _ -> -1
          | _ :: _, [] -> 1
          | i1 :: items1, i2 :: items2 ->
            (* CR mshinwell: think about the [None] cases some more *)
            let c = Option.compare String.compare i1.dinfo_uid i2.dinfo_uid in
            if c <> 0
            then c
            else
              let c =
                Option.compare String.compare i1.dinfo_function_symbol
                  i2.dinfo_function_symbol
              in
              if c <> 0 then c else loop items1 items2
        in
        loop items1 items2

      let equal t1 t2 = compare t1 t2 = 0

      let hash = Hashtbl.hash

      let output _ _ = Misc.fatal_error "Not implemented"
    end

    include T0
    include Identifiable.Make (T0)

    type key = t

    module Raw_set = Set

    module Set = struct
      type t =
        | Ok of Raw_set.t
        | Unreachable

      let of_list keys = Ok (Raw_set.of_list keys)

      let union t1 t2 =
        match t1, t2 with
        | Unreachable, _ | _, Unreachable -> Unreachable
        | Ok s1, Ok s2 -> Ok (Raw_set.union s1 s2)

      let inter t1 t2 =
        match t1, t2 with
        | Unreachable, t | t, Unreachable -> t
        | Ok s1, Ok s2 -> Ok (Raw_set.inter s1 s2)

      let diff t1 t2 =
        match t1, t2 with
        | Unreachable, _ -> Unreachable
        | _, Unreachable -> Ok Raw_set.empty
        | Ok s1, Ok s2 -> Ok (Raw_set.diff s1 s2)

      let fold f t init =
        match t with Unreachable -> init | Ok s -> Raw_set.fold f s init

      let print ppf t =
        match t with
        | Unreachable -> Format.pp_print_string ppf "Unreachable"
        | Ok s -> Raw_set.print ppf s
    end

    let parent t =
      match List.rev t with [] | [_] -> None | _ :: t -> Some (List.rev t)

    let all_parents _t = []
  end

  module Index = struct
    include Key
    module Set = Raw_set
  end

  module Subrange_state : Compute_ranges_intf.S_subrange_state = struct
    type t = unit

    let create () = ()

    let advance_over_instruction () _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t = struct
    type t = unit

    let create _var _subrange_state ~fun_contains_calls:_ ~fun_num_stack_slots:_
        =
      ()

    let print ppf () = Format.pp_print_string ppf "()"
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t = struct
    type t = unit

    let create _fundecl key ~start_insn:_ = Some (key, ())

    let print ppf () = Format.pp_print_string ppf "()"
  end

  let available_before (insn : L.instruction) =
    let get_parents (dbg : Debuginfo.item list) : Debuginfo.t list =
      match List.rev dbg with
      | [] | [_] -> []
      | _ :: parents ->
        let rec loop (t : Debuginfo.item list) =
          match t with
          | [] -> []
          | _ :: tl -> Debuginfo.of_items (List.rev t) :: loop tl
        in
        loop parents
    in
    let insn_dbg = Debuginfo.to_items insn.dbg in
    match insn_dbg with
    | [] -> None
    | _ :: _ ->
      Some (Key.Set.Ok (Key.Raw_set.of_list (insn.dbg :: get_parents insn_dbg)))

  let available_across insn =
    (* A single [Linear] instruction never spans inlined frames. *)
    available_before insn

  let must_restart_ranges_upon_any_change () = false
end

module Subrange_state = Inlined_frames.Subrange_state
module Subrange_info = Inlined_frames.Subrange_info
module Range_info = Inlined_frames.Range_info
include Compute_ranges.Make (Inlined_frames)
