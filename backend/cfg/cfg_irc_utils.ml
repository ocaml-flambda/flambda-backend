[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let irc_debug = false

let fatal_if_not_irc_debug env_var =
  if not irc_debug then fatal "%s is set but debugging mode is disabled" env_var

let bool_of_env env_var =
  if Cfg_regalloc_utils.bool_of_env env_var
  then (
    fatal_if_not_irc_debug env_var;
    true)
  else false

let irc_verbose : bool = bool_of_env "IRC_VERBOSE"

let irc_invariants : bool = bool_of_env "IRC_INVARIANTS"

let make_indent n = String.make (2 * n) ' '

let log : type a. indent:int -> (a, Format.formatter, unit) format -> a =
  if irc_verbose
  then
    fun ~indent fmt ->
    Format.eprintf ("[irc] %s" ^^ fmt ^^ "\n%!") (make_indent indent)
  else fun ~indent:_ fmt -> Format.(ifprintf err_formatter) fmt

let log_instruction_prefix ~indent (instr : _ Cfg.instruction) : unit =
  Format.eprintf "[irc] %s #%04d " (make_indent indent) instr.id

let log_instruction_suffix (instr : _ Cfg.instruction) (liveness : liveness) :
    unit =
  let live =
    match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
    | None -> Reg.Set.empty
    | Some { before = _; across } -> across
  in
  Format.eprintf " arg: %a res: %a live: %a" Printmach.regs instr.arg
    Printmach.regs instr.res Printmach.regset live;
  Format.eprintf "\n%!"

let log_body_and_terminator :
    indent:int ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun ~indent body term liveness ->
  if irc_debug && irc_verbose
  then (
    DLL.iter body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
        log_instruction_prefix ~indent instr;
        Cfg.dump_basic Format.err_formatter instr.Cfg.desc;
        log_instruction_suffix instr liveness);
    log_instruction_prefix ~indent term;
    Cfg.dump_terminator ~sep:", " Format.err_formatter term.Cfg.desc;
    log_instruction_suffix term liveness)

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
    | Intop_atomic _ -> false
    | Negf -> false
    | Absf -> false
    | Addf -> false
    | Subf -> false
    | Mulf -> false
    | Divf -> false
    | Compf _ -> false
    | Csel _ -> false
    | Floatofint -> false
    | Intoffloat -> false
    | Valueofint -> false
    | Intofvalue -> false
    | Probe_is_enabled _ -> false
    | Opaque -> false
    | Begin_region -> false
    | End_region -> false
    | Specific _ -> false
    | Name_for_debugger _ -> false)
  | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> false

let is_move_instruction : Cfg.basic Cfg.instruction -> bool =
 fun instr -> is_move_basic instr.desc

let all_precolored_regs : Reg.t array =
  Proc.init ();
  let num_available_registers =
    Array.fold_left Proc.num_available_registers ~f:( + ) ~init:0
  in
  let res = Array.make num_available_registers Reg.dummy in
  let i = ref 0 in
  for reg_class = 0 to pred Proc.num_register_classes do
    let first_available_register = Proc.first_available_register.(reg_class) in
    let num_available_registers = Proc.num_available_registers.(reg_class) in
    for reg_idx = 0 to pred num_available_registers do
      res.(!i) <- Proc.phys_reg (first_available_register + reg_idx);
      incr i
    done
  done;
  res

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
    | Hierarchical_uses

  let all = [Set_choose; Flat_uses; Hierarchical_uses]

  let to_string = function
    | Set_choose -> "set_choose"
    | Flat_uses -> "flat_uses"
    | Hierarchical_uses -> "hierarchical_uses"

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
        | "hierarchical_uses" | "hierarchical-uses" -> Hierarchical_uses
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end
