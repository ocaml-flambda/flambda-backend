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

type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    mutable new_labels : Label.Set.t;
    preserve_orig_labels : bool
  }

let create cfg ~layout ~preserve_orig_labels ~new_labels =
  { cfg; layout; new_labels; preserve_orig_labels }

let cfg t = t.cfg

let layout t = t.layout

let preserve_orig_labels t = t.preserve_orig_labels

let new_labels t = t.new_labels

let set_layout t layout =
  let cur_layout = Label.Set.of_list t.layout in
  let new_layout = Label.Set.of_list layout in
  if not
       (Label.Set.equal cur_layout new_layout
       && Label.equal (List.hd layout) t.cfg.entry_label)
  then
    Misc.fatal_error
      "Cfg set_layout: new layout is not a permutation of the current layout, \
       or first label is not entry";
  t.layout <- layout

let remove_block t label =
  Cfg.remove_block_exn t.cfg label;
  t.layout <- List.filter (fun l -> not (Label.equal l label)) t.layout;
  t.new_labels <- Label.Set.remove label t.new_labels

let is_trap_handler t label =
  let block = Cfg.get_block_exn t.cfg label in
  block.is_trap_handler

(* Printing utilities for debug *)

let dump ppf t ~msg =
  let open Format in
  fprintf ppf "\ncfg for %s\n" msg;
  fprintf ppf "%s\n" t.cfg.fun_name;
  fprintf ppf "layout.length=%d\n" (List.length t.layout);
  fprintf ppf "blocks.length=%d\n" (Label.Tbl.length t.cfg.blocks);
  let print_block label =
    let block = Label.Tbl.find t.cfg.blocks label in
    fprintf ppf "\n%d:\n" label;
    List.iter (Cfg.dump_basic ppf) block.body;
    Cfg.dump_terminator ppf block.terminator;
    fprintf ppf "\npredecessors:";
    Label.Set.iter (fprintf ppf " %d") block.predecessors;
    fprintf ppf "\nsuccessors:";
    Label.Set.iter (fprintf ppf " %d")
      (Cfg.successor_labels ~normal:true ~exn:false block);
    fprintf ppf "\nexn-successors:";
    Label.Set.iter (fprintf ppf " %d")
      (Cfg.successor_labels ~normal:false ~exn:true block)
  in
  List.iter print_block t.layout

let print t oc msg = Printf.fprintf oc "%s" (Format.asprintf "%a" (dump ~msg) t)

let print_dot t ?(show_instr = true) ?(show_exn = true) ?annotate_block
    ?annotate_succ oc =
  Printf.fprintf oc "strict digraph \"%s\" {\n" t.cfg.fun_name;
  let annotate_block label =
    match annotate_block with
    | None -> ""
    | Some f -> Printf.sprintf "\n%s" (f label)
  in
  let annotate_succ l1 l2 =
    match annotate_succ with
    | None -> ""
    | Some f -> Printf.sprintf " label=\"%s\"" (f l1 l2)
  in
  let print_block_dot label (block : Cfg.basic_block) index =
    let name l = Printf.sprintf "\".L%d\"" l in
    let show_index = Option.value index ~default:(-1) in
    Printf.fprintf oc "\n%s [shape=box label=\".L%d:I%d:S%d%s%s" (name label)
      label show_index (List.length block.body)
      (if block.is_trap_handler then ":eh" else "")
      (annotate_block label);
    if show_instr
    then (
      (* CR-someday gyorsh: Printing instruction using Printlinear doesn't work
         because of special characters like { } that need to be escaped. Should
         use sexp to print or implement a special printer. *)
      Printf.fprintf oc "\npreds:";
      Label.Set.iter (Printf.fprintf oc " %d") block.predecessors;
      Printf.fprintf oc "\\l";
      List.iter
        (fun i ->
          Cfg.print_basic oc i;
          Printf.fprintf oc "\\l")
        block.body;
      Cfg.print_terminator oc ~sep:"\\l" block.terminator;
      Printf.fprintf oc "\\l");
    Printf.fprintf oc "\"]\n";
    Label.Set.iter
      (fun l ->
        Printf.fprintf oc "%s->%s[%s]\n" (name label) (name l)
          (annotate_succ label l))
      (Cfg.successor_labels ~normal:true ~exn:false block);
    if show_exn
    then (
      Label.Set.iter
        (fun l ->
          Printf.fprintf oc "%s->%s [style=dashed %s]\n" (name label) (name l)
            (annotate_succ label l))
        (Cfg.successor_labels ~normal:false ~exn:true block);
      if Cfg.can_raise_interproc block
      then
        Printf.fprintf oc "%s->%s [style=dashed]\n" (name label) "placeholder")
  in
  (* print all the blocks, even if they don't appear in the layout *)
  List.iteri
    (fun index label ->
      let block = Label.Tbl.find t.cfg.blocks label in
      print_block_dot label block (Some index))
    t.layout;
  if List.length t.layout < Label.Tbl.length t.cfg.blocks
  then
    Label.Tbl.iter
      (fun label block ->
        match List.find_opt (fun lbl -> Label.equal label lbl) t.layout with
        | None -> print_block_dot label block None
        | _ -> ())
      t.cfg.blocks;
  Printf.fprintf oc "}\n"

let save_as_dot t ?show_instr ?show_exn ?annotate_block ?annotate_succ msg =
  let filename =
    Printf.sprintf "%s%s%s.dot"
      (* some of all the special characters that confuse assemblers also confuse
         dot. get rid of them.*)
      (X86_proc.string_of_symbol "" t.cfg.fun_name)
      (if msg = "" then "" else ".")
      msg
  in
  if !Cfg.verbose then Printf.printf "Writing cfg for %s to %s\n" msg filename;
  let oc = open_out filename in
  Misc.try_finally
    (fun () ->
      print_dot t ?show_instr ?show_exn ?annotate_block ?annotate_succ oc)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun _exn -> Misc.remove_file filename)
