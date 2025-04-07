[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils

let log_function = lazy (make_log_function ~label:"irc")

let indent () = (Lazy.force log_function).indent ()

let dedent () = (Lazy.force log_function).dedent ()

let reset_indentation () = (Lazy.force log_function).reset_indentation ()

let log : type a. ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ?no_eol fmt -> (Lazy.force log_function).log ?no_eol fmt

let instr_prefix (instr : Cfg.basic Cfg.instruction) =
  InstructionId.to_string_padded instr.id

let term_prefix (term : Cfg.terminator Cfg.instruction) =
  InstructionId.to_string_padded term.id

let log_body_and_terminator :
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun body terminator liveness ->
  make_log_body_and_terminator (Lazy.force log_function) ~instr_prefix
    ~term_prefix body terminator liveness

let log_cfg_with_infos : Cfg_with_infos.t -> unit =
 fun cfg_with_infos ->
  make_log_cfg_with_infos (Lazy.force log_function) ~instr_prefix ~term_prefix
    cfg_with_infos

module WorkList = struct
  type t =
    | Unknown_list
    | Precolored
    | Initial
    | Simplify
    | Freeze
    | Spill
    | Spilled
    | Coalesced
    | Colored
    | Select_stack

  let equal left right =
    match left, right with
    | Unknown_list, Unknown_list
    | Precolored, Precolored
    | Initial, Initial
    | Simplify, Simplify
    | Freeze, Freeze
    | Spill, Spill
    | Spilled, Spilled
    | Coalesced, Coalesced
    | Colored, Colored
    | Select_stack, Select_stack ->
      true
    | ( ( Unknown_list | Precolored | Initial | Simplify | Freeze | Spill
        | Spilled | Coalesced | Colored | Select_stack ),
        _ ) ->
      false

  let to_string = function
    | Unknown_list -> "unknown_list"
    | Precolored -> "precolored"
    | Initial -> "initial"
    | Simplify -> "simplify"
    | Freeze -> "freeze"
    | Spill -> "spill"
    | Spilled -> "spilled"
    | Coalesced -> "coalesced"
    | Colored -> "colored"
    | Select_stack -> "select_stack"
end

module Color = struct
  type t = int
end

module RegisterStamp = struct
  type t = int

  type pair = t * t

  let pair (x : t) (y : t) = if x <= y then x, y else y, x

  let fst = fst

  let snd = snd

  (* CR xclerc for xclerc: consider using a bit matrix *)

  module PS = Hashtbl.Make (struct
    type t = pair

    let equal (left : t) (right : t) : bool =
      Int.equal (fst left) (fst right) && Int.equal (snd left) (snd right)

    let hash ((x, y) : t) =
      (* CR xclerc for xclerc: review *)
      (x lsl 17) lxor y
  end)

  module PairSet = struct
    type t = unit PS.t

    let default_size = 256

    let make ~num_registers =
      let estimated_size = (num_registers * num_registers) asr 5 in
      PS.create
        (if estimated_size < default_size then default_size else estimated_size)

    let clear set = PS.clear set

    let mem set (x : pair) = PS.mem set x

    let add set (x : pair) = PS.replace set x ()

    let cardinal set = PS.length set

    let iter set ~f = PS.iter (fun key () -> f key) set
  end
end

module Degree = struct
  type t = int

  let infinite = max_int

  let to_string deg = if deg = max_int then "+inf" else string_of_int deg

  let to_float deg = if deg = max_int then Float.infinity else Float.of_int deg
end

let is_move_basic : Cfg.basic -> bool =
 fun desc ->
  match desc with
  | Op op -> (
    match op with
    | Move -> true
    (* CR mslater: reinterpret_cast, other than value<->int, can be true *)
    | Reinterpret_cast _ -> false
    | Static_cast _ -> false
    | Spill -> false
    | Reload -> false
    | Const_int _ -> false
    | Const_float32 _ -> false
    | Const_float _ -> false
    | Const_symbol _ -> false
    | Const_vec128 _ -> false
    | Stackoffset _ -> false
    | Load _ -> false
    | Store _ -> false
    | Intop _ -> false
    | Intop_imm _ -> false
    | Intop_atomic _ -> false
    | Floatop _ -> false
    | Csel _ -> false
    | Probe_is_enabled _ -> false
    | Opaque -> false
    | Begin_region -> false
    | End_region -> false
    | Specific _ -> false
    | Name_for_debugger _ -> false
    | Dls_get -> false
    | Poll -> false
    | Alloc _ -> false
    | Extcall _ -> false)
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ -> false

let is_move_instruction : Cfg.basic Cfg.instruction -> bool =
 fun instr -> is_move_basic instr.desc

let all_precolored_regs =
  Proc.init ();
  Proc.precolored_regs

let k reg = Proc.num_available_registers.(Proc.register_class reg)

module Spilling_heuristics = struct
  type t =
    | Set_choose
    | Flat_uses
    | Hierarchical_uses

  let default = Flat_uses

  let all = [Set_choose; Flat_uses; Hierarchical_uses]

  let to_string = function
    | Set_choose -> "set_choose"
    | Flat_uses -> "flat_uses"
    | Hierarchical_uses -> "hierarchical_uses"

  let value =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match find_param_value "IRC_SPILLING_HEURISTICS" with
      | None -> default
      | Some id -> (
        match String.lowercase_ascii id with
        | "set_choose" | "set-choose" -> Set_choose
        | "flat_uses" | "flat-uses" -> Flat_uses
        | "hierarchical_uses" | "hierarchical-uses" -> Hierarchical_uses
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end
