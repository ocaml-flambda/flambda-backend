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

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
    trap_depth : int;
    mutable exns : Label.Set.t;
    mutable can_raise : bool;
    mutable is_trap_handler : bool;
    mutable dead : bool
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    fun_dbg : Debuginfo.t;
    entry_label : Label.t
  }

let create ~fun_name ~fun_dbg =
  { fun_name; fun_dbg; entry_label = 1; blocks = Label.Tbl.create 31 }

let mem_block t label = Label.Tbl.mem t.blocks label

let successor_labels_normal ti =
  match ti.desc with
  | Tailcall (Self { destination }) -> Label.Set.singleton destination
  | Switch labels -> Array.to_seq labels |> Label.Set.of_seq
  | Return | Raise _ | Tailcall (Func _) -> Label.Set.empty
  | Call_no_return _ -> Label.Set.empty
  | Never -> Label.Set.empty
  | Always l -> Label.Set.singleton l
  | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
    Label.Set.singleton ifso |> Label.Set.add ifnot
  | Float_test { lt; gt; eq; uo } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
    |> Label.Set.add uo
  | Int_test { lt; gt; eq; imm = _; is_signed = _ } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq

let successor_labels ~normal ~exn block =
  match normal, exn with
  | false, false -> Label.Set.empty
  | true, false -> successor_labels_normal block.terminator
  | false, true -> block.exns
  | true, true ->
    Label.Set.union block.exns (successor_labels_normal block.terminator)

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
  if exn then block.exns <- Label.Set.map f block.exns;
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
      | Float_test { lt; eq; gt; uo } ->
        Float_test { lt = f lt; eq = f eq; gt = f gt; uo = f uo }
      | Switch labels -> Switch (Array.map f labels)
      | Tailcall (Self { destination }) ->
        Tailcall (Self { destination = f destination })
      | Tailcall (Func Indirect)
      | Tailcall (Func (Direct _))
      | Return | Raise _ | Call_no_return _ ->
        block.terminator.desc
    in
    block.terminator <- { block.terminator with desc }

let remove_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
    Misc.fatal_errorf "Cfg.remove_block_exn: block %d not found" label
  | _ -> Label.Tbl.remove t.blocks label

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
    Misc.fatal_errorf "Cfg.get_block_exn: block %d not found" label
  | block -> block

let can_raise_interproc block = block.can_raise && block.trap_depth = 1

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let iter_blocks t ~f = Label.Tbl.iter f t.blocks

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

let intop (op : Mach.integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
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
  | Icheckbound -> assert false

let dump_op ppf = function
  | Move -> Format.fprintf ppf "mov"
  | Spill -> Format.fprintf ppf "spill"
  | Reload -> Format.fprintf ppf "reload"
  | Const_int n -> Format.fprintf ppf "const_int %nd" n
  | Const_float f -> Format.fprintf ppf "const_float %F" (Int64.float_of_bits f)
  | Const_symbol s -> Format.fprintf ppf "const_symbol %s" s
  | Stackoffset n -> Format.fprintf ppf "stackoffset %d" n
  | Load _ -> Format.fprintf ppf "load"
  | Store _ -> Format.fprintf ppf "store"
  | Intop op -> Format.fprintf ppf "intop %s" (intop op)
  | Intop_imm (op, n) -> Format.fprintf ppf "intop %s %d" (intop op) n
  | Negf -> Format.fprintf ppf "negf"
  | Absf -> Format.fprintf ppf "absf"
  | Addf -> Format.fprintf ppf "addf"
  | Subf -> Format.fprintf ppf "subf"
  | Mulf -> Format.fprintf ppf "mulf"
  | Divf -> Format.fprintf ppf "divf"
  | Compf _ -> Format.fprintf ppf "compf"
  | Floatofint -> Format.fprintf ppf "floattoint"
  | Intoffloat -> Format.fprintf ppf "intoffloat"
  | Specific _ -> Format.fprintf ppf "specific"
  | Probe { name; handler_code_sym } ->
    Format.fprintf ppf "probe %s %s" name handler_code_sym
  | Probe_is_enabled { name } -> Format.fprintf ppf "probe_is_enabled %s" name
  | Opaque -> Format.fprintf ppf "opaque"
  | Name_for_debugger _ -> Format.fprintf ppf "name_for_debugger"

let dump_call ppf = function
  | P prim_call -> (
    match prim_call with
    | External { func_symbol : string; _ } ->
      Format.fprintf ppf "external %s" func_symbol
    | Alloc { bytes : int; _ } -> Format.fprintf ppf "alloc %d" bytes
    | Checkbound _ -> Format.fprintf ppf "checkbound")
  | F func_call -> (
    match func_call with
    | Indirect -> Format.fprintf ppf "indirect"
    | Direct { func_symbol : string; _ } ->
      Format.fprintf ppf "direct %s" func_symbol)

let dump_basic ppf i =
  let open Format in
  fprintf ppf "%d: " i.id;
  match i.desc with
  | Op op -> dump_op ppf op
  | Call call ->
    fprintf ppf "Call ";
    dump_call ppf call
  | Reloadretaddr -> fprintf ppf "Reloadretaddr"
  | Pushtrap { lbl_handler } -> fprintf ppf "Pushtrap handler=%d" lbl_handler
  | Poptrap -> fprintf ppf "Poptrap"
  | Prologue -> fprintf ppf "Prologue"

let dump_terminator ppf ?(sep = "\n") ti =
  let open Format in
  fprintf ppf "%d: " ti.id;
  match ti.desc with
  | Never -> fprintf ppf "deadend%s" sep
  | Always l -> fprintf ppf "goto %d%s" l sep
  | Parity_test { ifso; ifnot } ->
    fprintf ppf "if even goto %d%sif odd goto %d%s" ifso sep ifnot sep
  | Truth_test { ifso; ifnot } ->
    fprintf ppf "if true goto %d%sif false goto %d%s" ifso sep ifnot sep
  | Float_test { lt; eq; gt; uo } ->
    fprintf ppf "if < goto %d%s" lt sep;
    fprintf ppf "if = goto %d%s" eq sep;
    fprintf ppf "if > goto %d%s" gt sep;
    fprintf ppf "if uo goto %d%s" uo sep
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let cmp =
      Printf.sprintf " %s%s"
        (if is_signed then "s" else "u")
        (match imm with None -> "" | Some i -> " " ^ Int.to_string i)
    in
    fprintf ppf "if <%s goto %d%s" cmp lt sep;
    fprintf ppf "if =%s goto %d%s" cmp eq sep;
    fprintf ppf "if >%s goto %d%s" cmp gt sep
  | Switch labels ->
    fprintf ppf "switch%s" sep;
    for i = 0 to Array.length labels - 1 do
      fprintf ppf "case %d: goto %d%s" i labels.(i) sep
    done
  | Call_no_return { func_symbol : string; _ } ->
    fprintf ppf "Call_no_return %s%s" func_symbol sep
  | Return -> fprintf ppf "Return%s" sep
  | Raise _ -> fprintf ppf "Raise%s" sep
  | Tailcall (Self _) -> fprintf ppf "Tailcall self%s" sep
  | Tailcall (Func _) -> fprintf ppf "Tailcall%s" sep

let can_raise_terminator (i : terminator) =
  match i with
  | Raise _ | Tailcall (Func _) | Call_no_return _ -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return
  | Tailcall (Self _) ->
    false

let print_basic oc i =
  Format.kasprintf (Printf.fprintf oc "%s") "%a" dump_basic i

let print_terminator oc ?sep ti =
  Format.kasprintf (Printf.fprintf oc "%s") "%a" (dump_terminator ?sep) ti
