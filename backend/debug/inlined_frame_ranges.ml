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

module Inlined_frames = struct
  module Key = struct
    module T0 = struct
      type t = Debuginfo.t

      let print = Debuginfo.print_compact

      let compare = Stdlib.compare

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
        | Unreachable, t -> Unreachable
        | t, Unreachable -> Ok Raw_set.empty
        | Ok s1, Ok s2 -> Ok (Raw_set.diff s1 s2)

      let fold f t init =
        match t with Unreachable -> init | Ok s -> Raw_set.fold f s init

      let print ppf t =
        match t with
        | Unreachable -> Format.pp_print_string ppf "Unreachable"
        | Ok s -> Raw_set.print ppf s
    end

    let parent t =
      (* Format.eprintf "K.parent: t has length %d\n%!" (List.length t); *)
      match List.rev t with [] | [_] -> None | _ :: t -> Some t

    let all_parents _t = []
    (* match List.rev t with | [] | [_] -> [] | _ :: parents -> let rec loop t =
       match t with [] -> [] | _ :: tl -> t :: loop tl in loop parents *)
  end

  (* XXX think about this Index thing *)
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
    let get_parents dbg =
      match List.rev dbg with
      | [] | [_] -> []
      | _ :: parents ->
        let rec loop t = match t with [] -> [] | _ :: tl -> t :: loop tl in
        loop parents
    in
    match insn.dbg with
    | [] -> None
    | _ :: _ ->
      Some (Key.Set.Ok (Key.Raw_set.of_list (insn.dbg :: get_parents insn.dbg)))

  let available_across insn =
    (* A single [Linear] instruction never spans inlined frames. *)
    available_before insn

  let must_restart_ranges_upon_any_change () = false
end

module Subrange_state = Inlined_frames.Subrange_state
module Subrange_info = Inlined_frames.Subrange_info
module Range_info = Inlined_frames.Range_info
include Compute_ranges.Make (Inlined_frames)
