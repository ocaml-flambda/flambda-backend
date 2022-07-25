[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils

let irc_debug = false

let fatal_if_not_irc_debug () =
  if not irc_debug
  then fatal "IRC_VERBOSE is set but debugging mode is disabled"

let irc_verbose : bool =
  match Sys.getenv_opt "IRC_VERBOSE" with
  | Some "1" ->
    fatal_if_not_irc_debug ();
    true
  | Some _ | None -> false

let irc_invariants : bool =
  match Sys.getenv_opt "IRC_INVARIANTS" with
  | Some "1" ->
    fatal_if_not_irc_debug ();
    true
  | Some _ | None -> false

let make_indent n = String.make (2 * n) ' '

let log : type a. indent:int -> (a, Format.formatter, unit) format -> a =
  if irc_verbose
  then
    fun ~indent fmt ->
    Format.eprintf ("[irc] %s" ^^ fmt ^^ "\n%!") (make_indent indent)
  else fun ~indent:_ fmt -> Format.(ifprintf err_formatter) fmt

let log_body_and_terminator :
    indent:int ->
    Cfg.basic Cfg.instruction list ->
    Cfg.terminator Cfg.instruction ->
    unit =
 fun ~indent body term ->
  if irc_debug && irc_verbose
  then (
    List.iter body ~f:(fun instr ->
        Format.eprintf "[irc] %s" (make_indent indent);
        Cfg.dump_basic Format.err_formatter instr.Cfg.desc;
        Format.eprintf "\n%!");
    Format.eprintf "[irc] %s" (make_indent indent);
    Cfg.dump_terminator ~sep:", " Format.err_formatter term.Cfg.desc;
    Format.eprintf "\n%!")

module Color = struct
  type t = int
end

module RegisterStamp = struct
  type t = int

  (* CR xclerc for xclerc: consider using a bit matrix *)

  module PS = Hashtbl.Make (struct
    type nonrec t = t * t

    let equal (left : t) (right : t) : bool =
      Int.equal (fst left) (fst right) && Int.equal (snd left) (snd right)

    let hash ((x, y) : t) =
      (* CR xclerc for xclerc: review *)
      (x lsl 10) lor y
  end)

  module PairSet = struct
    type stamp = t

    type t = unit PS.t

    let make () = PS.create 256

    let clear set = PS.clear set

    (* CR xclerc for xclec: the caller is likely to call mem and add, so build
       the pair there *)

    let mem set (x : stamp) (y : stamp) =
      let v = if x <= y then x, y else y, x in
      PS.mem set v

    let add set (x : stamp) (y : stamp) =
      let v = if x <= y then x, y else y, x in
      PS.replace set v ()

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
    | Spill -> false
    | Reload -> false
    | Const_int _ -> false
    | Const_float _ -> false
    | Const_symbol _ -> false
    | Stackoffset _ -> false
    | Load _ -> false
    | Store _ -> false
    | Intop _ -> false
    | Intop_imm _ -> false
    | Negf -> false
    | Absf -> false
    | Addf -> false
    | Subf -> false
    | Mulf -> false
    | Divf -> false
    | Compf _ -> false
    | Floatofint -> false
    | Intoffloat -> false
    | Probe _ -> false
    | Probe_is_enabled _ -> false
    | Opaque -> false
    | Begin_region -> false
    | End_region -> false
    | Specific _ -> false
    | Name_for_debugger _ -> false)
  | Call _ | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> false

let is_move_instruction : Cfg.basic Cfg.instruction -> bool =
 fun instr -> is_move_basic instr.desc

let all_precolored_regs : Reg.t array = Proc.all_phys_regs

let k reg = Proc.num_available_registers.(Proc.register_class reg)

let update_register_locations : unit -> unit =
 fun () ->
  if irc_debug then log ~indent:0 "update_register_locations";
  List.iter (Reg.all_registers ()) ~f:(fun reg ->
      match reg.Reg.loc with
      | Reg _ -> ()
      | Stack _ -> ()
      | Unknown -> (
        match reg.Reg.irc_color with
        | None ->
          (* because of rewrites, the register may no longer be present *)
          ()
        | Some color ->
          if irc_debug
          then log ~indent:1 "updating %a to %d" Printmach.reg reg color;
          reg.Reg.loc <- Reg color))

module Split_mode = struct
  type t =
    | Off
    | Naive

  let all = [Off; Naive]

  let to_string = function Off -> "off" | Naive -> "naive"

  let env =
    let available_modes () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match Sys.getenv_opt "IRC_SPLIT" with
      | None ->
        fatal
          "the IRC_SPLIT environment variable is not set (possible values: %s)"
          (available_modes ())
      | Some id -> (
        match String.lowercase_ascii id with
        | "off" -> Off
        | "naive" -> Naive
        | _ ->
          fatal "unknown split mode %S (possible values: %s)" id
            (available_modes ())))
end

module Spilling_heuristics = struct
  type t =
    | Set_choose
    | Flat_uses
  (* CR xclerc for xclerc: | Hierarchical_uses *)

  let all =
    [Set_choose; Flat_uses (* CR xclerc for xclerc: Hierarchical_uses; *)]

  let to_string = function
    | Set_choose -> "set_choose"
    | Flat_uses -> "flat_uses"
  (* CR xclerc for xclerc: | Hierarchical_uses -> "hierarchical_uses" *)

  let env =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match Sys.getenv_opt "IRC_SPILLING_HEURISTICS" with
      | None ->
        fatal
          "the IRC_SPILLING_HEURISTICS environment variable is not set \
           (possible values: %s)"
          (available_heuristics ())
      | Some id -> (
        match String.lowercase_ascii id with
        | "set_choose" | "set-choose" -> Set_choose
        | "flat_uses" | "flat-uses" -> Flat_uses
        (* CR xclerc for xclerc: | "hierarchical_uses" | "hierarchical-uses" ->
           Hierarchical_uses *)
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end
