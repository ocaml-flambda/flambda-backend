(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Flambda1_compilation_unit = Compilation_unit
module Flambda1_linkage_name = Linkage_name

module Compilation_unit = struct
  include Flambda2_compilenv_deps.Compilation_unit

  let of_flambda1_compilation_unit comp_unit =
    let ident = Flambda1_compilation_unit.get_persistent_ident comp_unit in
    let linkage_name =
      comp_unit |> Flambda1_compilation_unit.get_linkage_name
      |> Flambda1_linkage_name.to_string
      |> Flambda2_compilenv_deps.Linkage_name.create
    in
    create ident linkage_name
end

module Linkage_name = Flambda2_compilenv_deps.Linkage_name
module Symbol = Flambda2_compilenv_deps.Symbol

let symbol_for_module_block id =
  assert (Ident.global id);
  assert (not (Ident.is_predef id));
  let comp_unit =
    Compilenv.unit_for_global id
    |> Compilation_unit.of_flambda1_compilation_unit
  in
  Symbol.unsafe_create comp_unit
    (Linkage_name.create (Compilenv.symbol_for_global id))

let symbol_for_global' ?comp_unit id =
  if Ident.global id && not (Ident.is_predef id)
  then symbol_for_module_block id
  else
    let comp_unit =
      match comp_unit with
      | Some comp_unit -> comp_unit
      | None ->
        if Ident.is_predef id
        then Compilation_unit.predefined_exception ()
        else Compilation_unit.get_current_exn ()
    in
    Symbol.unsafe_create comp_unit
      (Linkage_name.create (Compilenv.symbol_for_global id))

let find_predef_exn name =
  let matches ident = String.equal (Ident.name ident) name in
  match List.find matches Predef.all_predef_exns with
  | exception Not_found -> Misc.fatal_errorf "Cannot find predef exn '%s'" name
  | ident -> ident

let division_by_zero =
  symbol_for_global'
    ~comp_unit:(Compilation_unit.predefined_exception ())
    Predef.ident_division_by_zero

let invalid_argument =
  symbol_for_global'
    ~comp_unit:(Compilation_unit.predefined_exception ())
    (find_predef_exn "Invalid_argument")

let all_predefined_exception_symbols =
  Predef.all_predef_exns
  |> List.map (fun ident ->
         symbol_for_global'
           ~comp_unit:(Compilation_unit.predefined_exception ())
           ident)
  |> Symbol.Set.of_list

let () =
  assert (Symbol.Set.mem division_by_zero all_predefined_exception_symbols);
  assert (Symbol.Set.mem invalid_argument all_predefined_exception_symbols)

let symbol_for_global' id : Symbol.t = symbol_for_global' id

let size_int = Arch.size_int

let big_endian = Arch.big_endian

let max_sensible_number_of_arguments = Proc.max_arguments_for_tailcalls - 1

let set_global_info info = Compilenv.flambda2_set_export_info info

let get_global_info comp_unit =
  (* The Flambda simplifier should have returned the typing information for the
     predefined exception compilation unit before getting here. *)
  assert (not (Compilation_unit.is_predefined_exception comp_unit));
  if Compilation_unit.is_external_symbols comp_unit
  then None
  else
    let id =
      (* CR mshinwell: Unsure how to construct this properly. Also see CR in
         Closure_conversion about the linkage names of module blocks *)
      Compilation_unit.get_persistent_ident comp_unit
    in
    match Compilenv.get_global_info' id with
    | None | Some (Flambda2 None) -> None
    | Some (Flambda2 (Some info)) -> Some info
    | Some (Clambda _) ->
      (* CR mshinwell: This should be a user error, not a fatal error. Same
         below. *)
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Closure middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit
    | Some (Flambda1 _) ->
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Flambda 1 middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit
