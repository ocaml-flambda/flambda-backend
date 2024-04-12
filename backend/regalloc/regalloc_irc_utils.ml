[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

let irc_debug = false

let bool_of_param param_name =
  bool_of_param ~guard:(irc_debug, "irc_debug") param_name

let irc_verbose : bool Lazy.t = bool_of_param "IRC_VERBOSE"

let irc_invariants : bool Lazy.t = bool_of_param "IRC_INVARIANTS"

let log_function =
  lazy (make_log_function ~verbose:(Lazy.force irc_verbose) ~label:"irc")

let log :
    type a.
    indent:int -> ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ~indent ?no_eol fmt -> (Lazy.force log_function).log ~indent ?no_eol fmt

let instr_prefix (instr : Cfg.basic Cfg.instruction) =
  Printf.sprintf "#%04d" instr.id

let term_prefix (term : Cfg.terminator Cfg.instruction) =
  Printf.sprintf "#%04d" term.id

let log_body_and_terminator :
    indent:int ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun ~indent body terminator liveness ->
  make_log_body_and_terminator (Lazy.force log_function) ~instr_prefix
    ~term_prefix ~indent body terminator liveness

let log_cfg_with_infos : indent:int -> Cfg_with_infos.t -> unit =
 fun ~indent cfg_with_infos ->
  make_log_cfg_with_infos (Lazy.force log_function) ~instr_prefix ~term_prefix
    ~indent cfg_with_infos

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
    (* CR mslater: (SIMD) vectorcast / float64<->float64x2 cast may be true *)
    | Vectorcast _ -> false
    | Scalarcast _ -> false
    | Spill -> false
    | Reload -> false
    | Const_int _ -> false
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
    | Valueofint -> false
    | Intofvalue -> false
    | Probe_is_enabled _ -> false
    | Opaque -> false
    | Begin_region -> false
    | End_region -> false
    | Specific _ -> false
    | Name_for_debugger _ -> false
    | Dls_get -> false
    | Poll -> false
    | Alloc _ -> false)
  | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> false

let is_move_instruction : Cfg.basic Cfg.instruction -> bool =
 fun instr -> is_move_basic instr.desc

let all_precolored_regs =
  Proc.init ();
  Proc.precolored_regs

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

  let value =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match find_param_value "IRC_SPILLING_HEURISTICS" with
      | None ->
        fatal
          "the IRC_SPILLING_HEURISTICS parameter is not set (possible values: \
           %s)"
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

module ArraySet = struct
  module type S = sig
    type e

    type t

    val make : original_capacity:int -> t

    val clear : t -> unit

    val is_empty : t -> bool

    val choose_and_remove : t -> e option

    val add : t -> e -> unit

    val remove : t -> e -> unit

    val iter : t -> f:(e -> unit) -> unit

    val fold : t -> f:('a -> e -> 'a) -> init:'a -> 'a

    val to_list : t -> e list
  end

  module type OrderedTypeWithDummy = sig
    include Set.OrderedType

    val dummy : t
  end

  external unsafe_blit :
    src:'a array ->
    src_pos:int ->
    dst:'a array ->
    dst_pos:int ->
    len:int ->
    unit = "caml_array_blit"

  external unsafe_fill : 'a array -> pos:int -> len:int -> 'a -> unit
    = "caml_array_fill"

  module Make (T : OrderedTypeWithDummy) : S with type e = T.t = struct
    type e = T.t

    type t =
      { mutable array : e array;
        mutable length : int
      }

    let make ~original_capacity =
      let array = Array.make (max 1 original_capacity) T.dummy in
      let length = 0 in
      { array; length }

    let clear t =
      unsafe_fill t.array ~pos:0 ~len:t.length T.dummy;
      t.length <- 0

    let is_empty t = Int.equal t.length 0

    let index array length e =
      let low = ref 0 in
      let high = ref length in
      while !low < !high do
        let mid = (!low + !high) / 2 in
        if T.compare e (Array.unsafe_get array mid) > 0
        then low := succ mid
        else high := mid
      done;
      !low

    let new_length curr = if curr < 512 then 2 * curr else curr + 128

    let add t e =
      let idx = index t.array t.length e in
      if idx >= Array.length t.array
         || T.compare e (Array.unsafe_get t.array idx) <> 0
      then (
        if t.length = Array.length t.array
        then (
          (* reallocation *)
          let new_array =
            Array.make (new_length (Array.length t.array)) T.dummy
          in
          let len_before = idx in
          if len_before > 0
          then
            unsafe_blit ~src:t.array ~src_pos:0 ~dst:new_array ~dst_pos:0
              ~len:len_before;
          let len_after = t.length - idx in
          if len_after > 0
          then
            unsafe_blit ~src:t.array ~src_pos:idx ~dst:new_array
              ~dst_pos:(succ idx) ~len:len_after;
          Array.unsafe_set new_array idx e;
          t.array <- new_array;
          t.length <- succ t.length)
        else
          (* insertion *)
          let len = t.length - idx in
          if len > 0
          then
            unsafe_blit ~src:t.array ~src_pos:idx ~dst:t.array
              ~dst_pos:(succ idx) ~len;
          Array.unsafe_set t.array idx e;
          t.length <- succ t.length)

    let remove t e =
      let idx = index t.array t.length e in
      if idx < Array.length t.array
         && T.compare e (Array.unsafe_get t.array idx) = 0
      then (
        let len = t.length - idx - 1 in
        if len > 0
        then
          unsafe_blit ~src:t.array ~src_pos:(succ idx) ~dst:t.array ~dst_pos:idx
            ~len;
        t.length <- pred t.length;
        Array.unsafe_set t.array t.length T.dummy)

    let choose_and_remove t =
      if Int.equal t.length 0
      then None
      else
        let idx = pred t.length in
        t.length <- idx;
        let res = Some (Array.unsafe_get t.array idx) in
        Array.unsafe_set t.array idx T.dummy;
        res

    let iter t ~f =
      for i = 0 to pred t.length do
        f (Array.unsafe_get t.array i)
      done

    let fold t ~f ~init =
      let res = ref init in
      for i = 0 to pred t.length do
        res := f !res (Array.unsafe_get t.array i)
      done;
      !res

    let to_list t =
      let rec loop arr idx acc =
        if idx < 0
        then acc
        else loop arr (pred idx) (Array.unsafe_get arr idx :: acc)
      in
      loop t.array (pred t.length) []
  end
end
