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

open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear

let attributes fun_name =
  [DAH.create_name fun_name; DAH.create_external ~is_visible_externally:true]

let abstract_instance_proto_die_symbol ~fun_symbol =
  Asm_symbol.create (Asm_symbol.to_raw_string fun_symbol ^ "_absinst")

let add_empty state ~compilation_unit_proto_die ~fun_symbol ~demangled_name =
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some compilation_unit_proto_die) ~tag:Subprogram
      ~attribute_values:
        [ DAH.create_name (Asm_symbol.encode fun_symbol);
          DAH.create_linkage_name ~linkage_name:demangled_name;
          DAH.create_external ~is_visible_externally:true ]
      ()
  in
  let abstract_instance_proto_die_symbol =
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Asm_symbol.Tbl.add
    (DS.function_abstract_instances state)
    fun_symbol
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let add_root state ~parent ~demangled_name fun_symbol ~location_attributes =
  let attributes =
    [ DAH.create_name (Asm_symbol.encode fun_symbol);
      DAH.create_linkage_name ~linkage_name:demangled_name;
      DAH.create_external ~is_visible_externally:true ]
    @ location_attributes
  in
  let attribute_values =
    attributes
    @ [ (* We assume every function might potentially be inlined (and possibly
           in the future), so we choose [DW_INL_inlined] as the most appropriate
           setting for [DW_AT_inline], even if it doesn't seem exactly correct.
           We must set something here to ensure that the subprogram is marked as
           an abstract instance root. *)
        (* CR mshinwell/xclerc: we could propagate "inline never" attributes to
           this point *)
        DAH.create_inline Inlined ]
  in
  let abstract_instance_proto_die_symbol =
    abstract_instance_proto_die_symbol ~fun_symbol
  in
  DS.Debug.log "add_root: fun_symbol=%a\n" Asm_symbol.print fun_symbol;
  let abstract_instance_proto_die =
    match
      Asm_symbol.Tbl.find (DS.function_abstract_instances state) fun_symbol
    with
    | proto_die, _symbol ->
      (* See below in [find] *)
      Proto_die.replace_all_attribute_values proto_die attribute_values
    | exception Not_found ->
      (* DWARF-5 specification section 3.3.8.1, page 82. *)
      Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values
        ()
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Asm_symbol.Tbl.add (* or replace *)
    (DS.function_abstract_instances state)
    fun_symbol
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

type decomposed_singleton_debuginfo =
  { demangled_name : string;
    fun_symbol : Asm_symbol.t;
    compilation_unit : Compilation_unit.t
  }

let decompose_singleton_debuginfo dbg =
  let orig_dbg = dbg in
  match Debuginfo.to_items dbg with
  | [({ dinfo_scopes; dinfo_function_symbol; _ } as item)] -> (
    let module S = Debuginfo.Scoped_location in
    let compilation_unit = S.compilation_unit dinfo_scopes in
    let dbg = Debuginfo.of_items [item] in
    let fun_symbol =
      match dinfo_function_symbol with
      | Some dinfo_function_symbol -> Asm_symbol.create dinfo_function_symbol
      | None ->
        Misc.fatal_errorf
          "No function symbol in Debuginfo.t: orig_dbg=%a dbg=%a"
          Debuginfo.print_compact_extended orig_dbg
          Debuginfo.print_compact_extended dbg
    in
    let demangled_name =
      Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
        dinfo_scopes
      |> Misc.remove_double_underscores
    in
    match compilation_unit with
    | Some compilation_unit -> { demangled_name; fun_symbol; compilation_unit }
    | None ->
      Misc.fatal_errorf "No compilation unit extracted from: %a"
        Debuginfo.print_compact_extended dbg)
  | [] -> Misc.fatal_error "Empty Debuginfo.t"
  | _ :: _ ->
    Misc.fatal_errorf "Non-singleton Debuginfo.t: %a"
      Debuginfo.print_compact_extended dbg

type find_result =
  | Ok of Asm_symbol.t
  | External_unit of
      { demangled_name : string;
        fun_symbol : Asm_symbol.t
      }

let find state ~compilation_unit_proto_die (dbg : Debuginfo.t) =
  let { demangled_name; fun_symbol; compilation_unit = dbg_comp_unit } =
    decompose_singleton_debuginfo dbg
  in
  DS.Debug.log "found comp unit %a\n%!" Compilation_unit.print dbg_comp_unit;
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if Compilation_unit.equal dbg_comp_unit this_comp_unit
  then (
    DS.Debug.log "looking in function_abstract_instances for %a\n%!"
      Asm_symbol.print fun_symbol;
    match
      Asm_symbol.Tbl.find (DS.function_abstract_instances state) fun_symbol
    with
    | existing_instance ->
      DS.Debug.log "...successfully found existing absint DIE\n%!";
      Ok (snd existing_instance)
    | exception Not_found ->
      (* Fabricate an empty abstract instance DIE to fill in later, just in case
         we haven't seen things in topological order. *)
      (* CR mshinwell: does that actually happen now? *)
      DS.Debug.log "...making empty absint DIE for %a\n" Asm_symbol.print
        fun_symbol;
      (* The empty abstract instances are parented to the compilation unit as
         they might be referenced by other DIEs in a completely different scope
         within the current unit. *)
      let _, die_symbol =
        add_empty state ~compilation_unit_proto_die ~fun_symbol ~demangled_name
      in
      Ok die_symbol)
  else
    (* abstract_instance_proto_die_symbol ~fun_symbol *)
    (* See the call site of this function *)
    External_unit { demangled_name; fun_symbol }
