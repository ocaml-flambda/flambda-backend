(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

let verbose = ref false

include Cfg_intf.S
module DLL = Flambda_backend_utils.Doubly_linked_list

type basic_instruction_list = basic instruction DLL.t

type basic_block =
  { start : Label.t;
    body : basic_instruction_list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
    mutable stack_offset : int;
    mutable exn : Label.t option;
    mutable can_raise : bool;
    mutable is_trap_handler : bool;
    mutable dead : bool;
    mutable cold : bool
  }

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Assume_zero_alloc of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check_zero_alloc of
      { strict : bool;
        loc : Location.t
      }

let rec of_cmm_codegen_option : Cmm.codegen_option list -> codegen_option list =
 fun cmm_options ->
  match cmm_options with
  | [] -> []
  | hd :: tl -> (
    match hd with
    | No_CSE -> No_CSE :: of_cmm_codegen_option tl
    | Reduce_code_size -> Reduce_code_size :: of_cmm_codegen_option tl
    | Assume_zero_alloc { strict; never_returns_normally; never_raises; loc } ->
      Assume_zero_alloc { strict; never_returns_normally; never_raises; loc }
      :: of_cmm_codegen_option tl
    | Check_zero_alloc { strict; loc } ->
      Check_zero_alloc { strict; loc } :: of_cmm_codegen_option tl
    | Use_linscan_regalloc -> of_cmm_codegen_option tl)

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    fun_args : Reg.t array;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
    entry_label : Label.t;
    fun_contains_calls : bool;
    (* CR-someday gyorsh: compute locally. *)
    fun_num_stack_slots : int array;
    fun_poll : Lambda.poll_attribute
  }

let create ~fun_name ~fun_args ~fun_codegen_options ~fun_dbg ~fun_contains_calls
    ~fun_num_stack_slots ~fun_poll =
  { fun_name;
    fun_args;
    fun_codegen_options;
    fun_dbg;
    entry_label = 1;
    (* CR gyorsh: We should use [Cmm.new_label ()] here, but validator tests
       currently rely on it to be initialized as above. *)
    blocks = Label.Tbl.create 31;
    fun_contains_calls;
    fun_num_stack_slots;
    fun_poll
  }

let mem_block t label = Label.Tbl.mem t.blocks label

let successor_labels_normal ti =
  match ti.desc with
  | Tailcall_self { destination } -> Label.Set.singleton destination
  | Switch labels -> Array.to_seq labels |> Label.Set.of_seq
  | Return | Raise _ | Tailcall_func _ -> Label.Set.empty
  | Call_no_return _ -> Label.Set.empty
  | Never -> Label.Set.empty
  | Always l -> Label.Set.singleton l
  | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
    Label.Set.singleton ifso |> Label.Set.add ifnot
  | Float_test { width = _; lt; gt; eq; uo } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
    |> Label.Set.add uo
  | Int_test { lt; gt; eq; imm = _; is_signed = _ } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
  | Call { op = _; label_after }
  | Prim { op = _; label_after }
  | Specific_can_raise { op = _; label_after } ->
    Label.Set.singleton label_after

let successor_labels ~normal ~exn block =
  match normal, exn with
  | false, false -> Label.Set.empty
  | true, false -> successor_labels_normal block.terminator
  | false, true -> (
    match block.exn with
    | None -> Label.Set.empty
    | Some label -> Label.Set.singleton label)
  | true, true -> (
    match block.exn with
    | None -> successor_labels_normal block.terminator
    | Some label ->
      Label.Set.add label (successor_labels_normal block.terminator))

let predecessor_labels block = Label.Set.elements block.predecessors

let replace_successor_labels t ~normal ~exn block ~f =
  (* Check that the new labels are in [t] *)
  let f src =
    let dst = f src in
    if not (mem_block t dst)
    then
      Misc.fatal_errorf
        "Cfg.replace_successor_labels: \nnew successor %d not found in the cfg"
        dst;
    dst
  in
  if exn then block.exn <- Option.map f block.exn;
  if normal
  then
    let desc =
      match block.terminator.desc with
      | Never -> Never
      | Always l -> Always (f l)
      | Parity_test { ifso; ifnot } ->
        Parity_test { ifso = f ifso; ifnot = f ifnot }
      | Truth_test { ifso; ifnot } ->
        Truth_test { ifso = f ifso; ifnot = f ifnot }
      | Int_test { lt; eq; gt; is_signed; imm } ->
        Int_test { lt = f lt; eq = f eq; gt = f gt; is_signed; imm }
      | Float_test { width; lt; eq; gt; uo } ->
        Float_test { width; lt = f lt; eq = f eq; gt = f gt; uo = f uo }
      | Switch labels -> Switch (Array.map f labels)
      | Tailcall_self { destination } ->
        Tailcall_self { destination = f destination }
      | Tailcall_func Indirect
      | Tailcall_func (Direct _)
      | Return | Raise _ | Call_no_return _ ->
        block.terminator.desc
      | Call { op; label_after } -> Call { op; label_after = f label_after }
      | Prim { op; label_after } -> Prim { op; label_after = f label_after }
      | Specific_can_raise { op; label_after } ->
        Specific_can_raise { op; label_after = f label_after }
    in
    block.terminator <- { block.terminator with desc }

let add_block_exn t block =
  if Label.Tbl.mem t.blocks block.start
  then
    Misc.fatal_errorf "Cfg.add_block_exn: block %d is already present"
      block.start;
  Label.Tbl.add t.blocks block.start block

let remove_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
    Misc.fatal_errorf "Cfg.remove_block_exn: block %d not found" label
  | _ -> Label.Tbl.remove t.blocks label

let remove_blocks t labels_to_remove =
  Label.Tbl.filter_map_inplace
    (fun l b -> if Label.Set.mem l labels_to_remove then None else Some b)
    t.blocks

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
    Misc.fatal_errorf "Cfg.get_block_exn: block %d not found" label
  | block -> block

let can_raise_interproc block = block.can_raise && Option.is_none block.exn

type 'a instr_mapper = { f : 'b. 'b instruction -> 'a } [@@unboxed]

let map_first_instruction (block : basic_block) (t : 'a instr_mapper) =
  match DLL.hd block.body with
  | None -> t.f block.terminator
  | Some first_instr -> t.f first_instr

let first_instruction_id (block : basic_block) : int =
  map_first_instruction block { f = (fun instr -> instr.id) }

let first_instruction_stack_offset (block : basic_block) : int =
  map_first_instruction block { f = (fun instr -> instr.stack_offset) }

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let iter_blocks t ~f = Label.Tbl.iter f t.blocks

let fold_blocks t ~f ~init = Label.Tbl.fold f t.blocks init

let fold_body_instructions t ~f ~init =
  let helper _ block acc = DLL.fold_left block.body ~f ~init:acc in
  fold_blocks t ~f:helper ~init

let register_predecessors_for_all_blocks (t : t) =
  Label.Tbl.iter
    (fun label block ->
      let targets = successor_labels ~normal:true ~exn:true block in
      Label.Set.iter
        (fun target ->
          let target_block = Label.Tbl.find t.blocks target in
          target_block.predecessors
            <- Label.Set.add label target_block.predecessors)
        targets)
    t.blocks

(* Printing for debug *)

(* The next 2 functions are copied almost as is from asmcomp/printmach.ml
   because there is no interface to call them. Eventually this won't be needed
   when we change cfg to have its own types rather than referring back to mach
   and cmm. *)
(* CR-someday gyorsh: implement desc printing, and args/res/dbg, etc, properly,
   with regs, use the dreaded Format. *)

let intcomp (comp : Mach.integer_comparison) =
  match comp with
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let intop_atomic (op : Cmm.atomic_op) =
  match op with Fetch_and_add -> " += " | Compare_and_swap -> " cas "

let intop (op : Mach.integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed : bool } -> " *h" ^ if signed then " " else "u "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Ipopcnt -> " pop "
  | Iclz _ -> " clz "
  | Ictz _ -> " ctz "
  | Icomp cmp -> intcomp cmp

let dump_op ppf = function
  | Move -> Format.fprintf ppf "mov"
  | Spill -> Format.fprintf ppf "spill"
  | Reload -> Format.fprintf ppf "reload"
  | Const_int n -> Format.fprintf ppf "const_int %nd" n
  | Const_float32 f ->
    Format.fprintf ppf "const_float32 %Fs" (Int32.float_of_bits f)
  | Const_float f -> Format.fprintf ppf "const_float %F" (Int64.float_of_bits f)
  | Const_symbol s -> Format.fprintf ppf "const_symbol %s" s.sym_name
  | Const_vec128 { high; low } ->
    Format.fprintf ppf "const vec128 %016Lx:%016Lx" high low
  | Stackoffset n -> Format.fprintf ppf "stackoffset %d" n
  | Load _ -> Format.fprintf ppf "load"
  | Store _ -> Format.fprintf ppf "store"
  | Intop op -> Format.fprintf ppf "intop %s" (intop op)
  | Intop_imm (op, n) -> Format.fprintf ppf "intop %s %d" (intop op) n
  | Intop_atomic { op; size = _; addr = _ } ->
    Format.fprintf ppf "intop atomic %s" (intop_atomic op)
  | Floatop (Float64, op) ->
    Format.fprintf ppf "floatop %a" Printmach.floatop op
  | Floatop (Float32, op) ->
    Format.fprintf ppf "float32op %a" Printmach.floatop op
  | Csel _ -> Format.fprintf ppf "csel"
  | Reinterpret_cast cast ->
    Format.fprintf ppf "%s" (Printcmm.reinterpret_cast cast)
  | Static_cast cast -> Format.fprintf ppf "%s" (Printcmm.static_cast cast)
  | Specific _ -> Format.fprintf ppf "specific"
  | Probe_is_enabled { name } -> Format.fprintf ppf "probe_is_enabled %s" name
  | Opaque -> Format.fprintf ppf "opaque"
  | Begin_region -> Format.fprintf ppf "beginregion"
  | End_region -> Format.fprintf ppf "endregion"
  | Name_for_debugger _ -> Format.fprintf ppf "name_for_debugger"
  | Dls_get -> Format.fprintf ppf "dls_get"
  | Poll -> Format.fprintf ppf "poll"
  | Alloc { bytes; dbginfo = _; mode = Alloc_heap } ->
    Format.fprintf ppf "alloc %i" bytes
  | Alloc { bytes; dbginfo = _; mode = Alloc_local } ->
    Format.fprintf ppf "alloc_local %i" bytes
  | Return_addr -> Format.fprintf ppf "return_addr"

let dump_basic ppf (basic : basic) =
  let open Format in
  match basic with
  | Op op -> dump_op ppf op
  | Reloadretaddr -> fprintf ppf "Reloadretaddr"
  | Pushtrap { lbl_handler } -> fprintf ppf "Pushtrap handler=%d" lbl_handler
  | Poptrap -> fprintf ppf "Poptrap"
  | Prologue -> fprintf ppf "Prologue"
  | Stack_check { max_frame_size_bytes } ->
    fprintf ppf "Stack_check size=%d" max_frame_size_bytes

let dump_terminator' ?(print_reg = Printmach.reg) ?(res = [||]) ?(args = [||])
    ?(specific_can_raise = fun ppf _ -> Format.fprintf ppf "specific_can_raise")
    ?(sep = "\n") ppf (terminator : terminator) =
  let first_arg =
    if Array.length args >= 1
    then Format.fprintf Format.str_formatter " %a" print_reg args.(0);
    Format.flush_str_formatter ()
  in
  let second_arg =
    if Array.length args >= 2
    then Format.fprintf Format.str_formatter " %a" print_reg args.(1);
    Format.flush_str_formatter ()
  in
  let print_args ppf args =
    if Array.length args = 0
    then ()
    else Format.fprintf ppf " %a" (Printmach.regs' ~print_reg) args
  in
  let print_res ppf =
    if Array.length res > 0
    then Format.fprintf ppf "%a := " (Printmach.regs' ~print_reg) res
  in
  let dump_mach_op ppf op = Printmach.operation' ~print_reg op args ppf [||] in
  let open Format in
  match terminator with
  | Never -> fprintf ppf "deadend"
  | Always l -> fprintf ppf "goto %d" l
  | Parity_test { ifso; ifnot } ->
    fprintf ppf "if even%s goto %d%selse goto %d" first_arg ifso sep ifnot
  | Truth_test { ifso; ifnot } ->
    fprintf ppf "if true%s goto %d%selse goto %d" first_arg ifso sep ifnot
  | Float_test { width = _; lt; eq; gt; uo } ->
    fprintf ppf "if%s <%s goto %d%s" first_arg second_arg lt sep;
    fprintf ppf "if%s =%s goto %d%s" first_arg second_arg eq sep;
    fprintf ppf "if%s >%s goto %d%s" first_arg second_arg gt sep;
    fprintf ppf "else goto %d" uo
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let cmp =
      Printf.sprintf " %s%s"
        (if is_signed then "s" else "u")
        (match imm with None -> second_arg | Some i -> " " ^ Int.to_string i)
    in
    fprintf ppf "if%s <%s goto %d%s" first_arg cmp lt sep;
    fprintf ppf "if%s =%s goto %d%s" first_arg cmp eq sep;
    fprintf ppf "if%s >%s goto %d" first_arg cmp gt
  | Switch labels ->
    fprintf ppf "switch%s%s" first_arg sep;
    let label_count = Array.length labels in
    if label_count >= 1
    then (
      for i = 0 to label_count - 2 do
        fprintf ppf "case %d: goto %d%s" i labels.(i) sep
      done;
      let i = label_count - 1 in
      fprintf ppf "case %d: goto %d" i labels.(i))
  | Call_no_return { func_symbol; _ } ->
    fprintf ppf "Call_no_return %s%a" func_symbol print_args args
  | Return -> fprintf ppf "Return%a" print_args args
  | Raise _ -> fprintf ppf "Raise%a" print_args args
  | Tailcall_self { destination } ->
    dump_mach_op ppf
      (Mach.Itailcall_imm
         { func =
             { sym_name = Printf.sprintf "self(%d)" destination;
               sym_global = Local
             }
         })
  | Tailcall_func call ->
    dump_mach_op ppf
      (match call with
      | Indirect -> Mach.Itailcall_ind
      | Direct func -> Mach.Itailcall_imm { func })
  | Call { op = call; label_after } ->
    Format.fprintf ppf "%t%a" print_res dump_mach_op
      (match call with
      | Indirect -> Mach.Icall_ind
      | Direct func -> Mach.Icall_imm { func });
    Format.fprintf ppf "%sgoto %d" sep label_after
  | Prim { op = prim; label_after } ->
    Format.fprintf ppf "%t%a" print_res dump_mach_op
      (match prim with
      | External { func_symbol = func; ty_res; ty_args; alloc; stack_ofs } ->
        Mach.Iextcall
          { func; ty_res; ty_args; returns = true; alloc; stack_ofs }
      | Probe { name; handler_code_sym; enabled_at_init } ->
        Mach.Iprobe { name; handler_code_sym; enabled_at_init });
    Format.fprintf ppf "%sgoto %d" sep label_after
  | Specific_can_raise { op; label_after } ->
    Format.fprintf ppf "%a" specific_can_raise op;
    Format.fprintf ppf "%sgoto %d" sep label_after

let dump_terminator ?sep ppf terminator = dump_terminator' ?sep ppf terminator

let print_basic' ?print_reg ppf (instruction : basic instruction) =
  let desc = Cfg_to_linear_desc.from_basic instruction.desc in
  let instruction =
    { Linear.desc;
      next = Linear.end_instr;
      arg = instruction.arg;
      res = instruction.res;
      dbg = Debuginfo.none;
      fdo = None;
      live = Reg.Set.empty;
      available_before = None;
      available_across = None
    }
  in
  Printlinear.instr' ?print_reg ppf instruction

let print_basic ppf i = print_basic' ppf i

let print_terminator' ?print_reg ppf (ti : terminator instruction) =
  dump_terminator' ?print_reg
    ~specific_can_raise:(fun ppf op ->
      (* Print this as basic instruction. *)
      print_basic' ?print_reg ppf { ti with desc = Op (Specific op) })
    ~res:ti.res ~args:ti.arg ~sep:"\n" ppf ti.desc

let print_terminator ppf ti = print_terminator' ppf ti

let print_instruction' ?print_reg ppf i =
  match i with
  | `Basic i -> print_basic' ?print_reg ppf i
  | `Terminator i -> print_terminator' ?print_reg ppf i

let print_instruction ppf i = print_instruction' ppf i

let can_raise_terminator (i : terminator) =
  match i with
  | Raise _ | Tailcall_func _ | Call_no_return _ | Call _
  | Prim { op = External _ | Probe _; label_after = _ } ->
    true
  | Specific_can_raise { op; _ } ->
    assert (Arch.operation_can_raise op);
    true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Tailcall_self _ ->
    false

(* CR gyorsh: [is_pure_terminator] is not the same as [can_raise_terminator]
   because of [Tailcal Self] which is not pure but marked as cannot raise at the
   moment, which we might want to reconsider later. *)
let is_pure_terminator desc =
  match (desc : terminator) with
  | Return | Raise _ | Call_no_return _ | Tailcall_func _ | Tailcall_self _
  | Call _ | Prim _ ->
    false
  | Specific_can_raise { op; _ } ->
    assert (Arch.operation_can_raise op);
    false
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ ->
    (* CR gyorsh: fix for memory operands *)
    true

let is_pure_operation : operation -> bool = function
  | Move -> true
  | Spill -> true
  | Reload -> true
  | Const_int _ -> true
  | Const_float32 _ -> true
  | Const_float _ -> true
  | Const_symbol _ -> true
  | Const_vec128 _ -> true
  | Stackoffset _ -> false
  | Load _ -> true
  | Store _ -> false
  | Intop _ -> true
  | Intop_imm _ -> true
  | Intop_atomic _ -> false
  | Floatop _ -> true
  | Csel _ -> true
  | Reinterpret_cast
      ( V128_of_v128 | Float32_of_float | Float32_of_int32 | Float_of_float32
      | Float_of_int64 | Int64_of_float | Int32_of_float32 ) ->
    true
  | Static_cast _ -> true
  (* Conservative to ensure valueofint/intofvalue are not eliminated before
     emit. *)
  | Reinterpret_cast (Value_of_int | Int_of_value) -> false
  | Probe_is_enabled _ -> true
  | Opaque -> false
  | Begin_region -> false
  | End_region -> false
  | Specific s ->
    assert (not (Arch.operation_can_raise s));
    Arch.operation_is_pure s
  | Name_for_debugger _ -> false
  | Dls_get -> true
  | Poll -> false
  | Alloc _ -> false
  | Return_addr -> false

let is_pure_basic : basic -> bool = function
  | Op op -> is_pure_operation op
  | Reloadretaddr ->
    (* This is a no-op on supported backends but on some others like "power" it
       wouldn't be. Saying it's not pure doesn't decrease the generated code
       quality and is future-proof.*)
    false
  | Pushtrap _ | Poptrap ->
    (* Those instructions modify the trap stack which actually modifies the
       stack pointer. *)
    false
  | Prologue ->
    (* [Prologue] grows the stack when entering a function and therefore
       modifies the stack pointer. [Prologue] can be considered pure if it's
       ensured that it wouldn't modify the stack pointer (e.g. there are no used
       local stack slots nor calls). *)
    false
  | Stack_check _ ->
    (* May reallocate the stack. *)
    false

let same_location (r1 : Reg.t) (r2 : Reg.t) =
  Reg.same_loc r1 r2
  &&
  match r1.loc with
  | Unknown -> Misc.fatal_errorf "Cfg got unknown register location."
  | Reg _ -> Proc.register_class r1 = Proc.register_class r2
  | Stack _ -> Proc.stack_slot_class r1.typ = Proc.stack_slot_class r2.typ

let is_noop_move instr =
  match instr.desc with
  | Op (Move | Spill | Reload) -> same_location instr.arg.(0) instr.res.(0)
  | Op (Csel _) -> (
    match instr.res.(0).loc with
    | Unknown -> false
    | Reg _ | Stack _ ->
      let len = Array.length instr.arg in
      let ifso = instr.arg.(len - 2) in
      let ifnot = instr.arg.(len - 1) in
      Reg.same_loc instr.res.(0) ifso && Reg.same_loc instr.res.(0) ifnot)
  | Op
      ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
      | Const_vec128 _ | Stackoffset _ | Load _ | Store _ | Intop _
      | Intop_imm _ | Intop_atomic _ | Floatop _ | Opaque | Reinterpret_cast _
      | Static_cast _ | Probe_is_enabled _ | Specific _ | Name_for_debugger _
      | Begin_region | End_region | Dls_get | Poll | Alloc _ | Return_addr )
  | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ ->
    false

let set_stack_offset (instr : _ instruction) stack_offset =
  if stack_offset < 0
  then
    Misc.fatal_errorf "Cfg.set_stack_offset: expected non-negative got %d"
      stack_offset;
  instr.stack_offset <- stack_offset

let string_of_irc_work_list = function
  | Unknown_list -> "unknown_list"
  | Coalesced -> "coalesced"
  | Constrained -> "constrained"
  | Frozen -> "frozen"
  | Work_list -> "work_list"
  | Active -> "active"

let make_instruction ~desc ?(arg = [||]) ?(res = [||]) ?(dbg = Debuginfo.none)
    ?(fdo = Fdo_info.none) ?(live = Reg.Set.empty) ~stack_offset ~id
    ?(irc_work_list = Unknown_list) ?(ls_order = 0) ?(available_before = None)
    ?(available_across = None) () =
  { desc;
    arg;
    res;
    dbg;
    fdo;
    live;
    stack_offset;
    id;
    irc_work_list;
    ls_order;
    available_before;
    available_across
  }
