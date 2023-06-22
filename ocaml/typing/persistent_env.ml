(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Persistent structure descriptions *)

open Misc
open Cmi_format

module CU = Compilation_unit
module Impl = struct
  (* The implementation compilation unit for some import, if known *)
  type t =
    | Unknown_argument (* The import is a parameter module *)
    | Known of CU.t

  let and_crc_of_import_info info =
    match Import_info.Intf.crc info with
    | None -> None
    | Some crc ->
        match Import_info.Intf.impl info with
        | None -> Some (Unknown_argument, crc)
        | Some cu -> Some (Known cu, crc)
end
module Consistbl = Consistbl.Make (CU.Name) (Impl)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of CU.Name.t * CU.Name.t * filepath
  | Inconsistent_import of CU.Name.t * filepath * filepath
  | Need_recursive_types of CU.Name.t
  | Depend_on_unsafe_string_unit of CU.Name.t
  | Inconsistent_package_declaration of CU.t * filepath
  | Inconsistent_package_declaration_between_imports of
      filepath * CU.t * CU.t
  | Direct_reference_from_wrong_package of
      CU.t * filepath * CU.Prefix.t
  | Illegal_import_of_parameter of CU.Name.t * filepath
  | Not_compiled_as_parameter of CU.Name.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : CU.Name.t;
        parameter : Global.Name.t;
      }
  | Imported_module_has_no_such_parameter of
      { imported : CU.Name.t;
        valid_parameters : Global.Name.t list;
        parameter : Global.Name.t;
        value : Global.Name.t;
      }
  | Not_compiled_as_argument of CU.Name.t * filepath
  | Argument_type_mismatch of
      { value : Global.Name.t;
        filename : filepath;
        expected : Global.Name.t;
        actual : Global.Name.t;
      }
  | Inconsistent_global_name_resolution of {
      name: Global.Name.t;
      old_global : Global.t;
      new_global : Global.t;
      first_mentioned_by : Global.Name.t;
      now_mentioned_by : Global.Name.t;
    }

exception Error of error
let error err = raise (Error err)

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos_lazy }

  let load = ref (fun ~unit_name ->
      let unit_name = CU.Name.to_string unit_name in
      match Load_path.find_uncap (unit_name ^ ".cmi") with
      | filename -> Some { filename; cmi = read_cmi_lazy filename }
      | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

type binding =
  | Local of Ident.t (* Bound to a runtime parameter *)
  | Static of Compilation_unit.t (* Bound to a static constant *)

type global_name_info = {
  gn_global : Global.t;
  gn_mentioned_by : Global.Name.t; (* For error reporting *)
}

(* Data relating directly to a .cmi - does not depend on arguments *)
type import_info = {
  imp_is_param : bool;
  imp_filename : string;
}

type  pers_struct = {
  ps_global: Global.t;
  ps_arg_for: Global.Name.t option;
  ps_binding: binding;
  ps_import: import_info;
  (* CR lmaurer: Move [ps_crcs] and [ps_filename] into [import_info] *)
  ps_crcs : Import_info.Intf.t array;
  ps_filename : string;
}

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type 'a pers_struct_info =
  | Missing
  | Found of pers_struct * 'a

module Param_set = Global.Name.Set

type 'a t = {
  globals : (Global.Name.t, global_name_info) Hashtbl.t;
  persistent_structures : (Global.Name.t, 'a pers_struct_info) Hashtbl.t;
  imported_units: CU.Name.Set.t ref;
  imported_opaque_units: CU.Name.Set.t ref;
  imports : (CU.Name.t, import_info) Hashtbl.t;
  param_imports : CU.Name.Set.t ref;
  exported_params : Param_set.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  globals = Hashtbl.create 17;
  persistent_structures = Hashtbl.create 17;
  imported_units = ref CU.Name.Set.empty;
  imported_opaque_units = ref CU.Name.Set.empty;
  imports = Hashtbl.create 17;
  param_imports = ref CU.Name.Set.empty;
  exported_params = ref Param_set.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let [@warning "+missing-record-field-pattern"] {
    globals;
    persistent_structures;
    imported_units;
    imported_opaque_units;
    imports;
    param_imports;
    exported_params;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear globals;
  Hashtbl.clear persistent_structures;
  imported_units := CU.Name.Set.empty;
  imported_opaque_units := CU.Name.Set.empty;
  Hashtbl.clear imports;
  param_imports := CU.Name.Set.empty;
  exported_params := Param_set.empty;
  Consistbl.clear crc_units;
  can_load_cmis := Can_load_cmis;
  ()

let clear_missing {persistent_structures; _} =
  let missing_entries =
    Hashtbl.fold
      (fun name r acc -> if r = Missing then name :: acc else acc)
      persistent_structures []
  in
  List.iter (Hashtbl.remove persistent_structures) missing_entries

let add_import {imported_units; _} s =
  imported_units := CU.Name.Set.add s !imported_units

let register_import_as_opaque {imported_opaque_units; _} s =
  imported_opaque_units := CU.Name.Set.add s !imported_opaque_units

let find_import_info_in_cache {imports; _} import =
  match Hashtbl.find imports import with
  | exception Not_found -> None
  | imp -> Some imp

let find_info_in_cache {persistent_structures; _} name =
  match Hashtbl.find persistent_structures name with
  | exception Not_found -> None
  | Missing -> None
  | Found (ps, pm) -> Some (ps, pm)

let register_parameter_import ({param_imports; _} as penv) import =
  begin match find_import_info_in_cache penv import with
  | None ->
      (* Not loaded yet; if it's wrong, we'll get an error at load time *)
      ()
  | Some imp ->
      if not imp.imp_is_param then
        raise (Error (Not_compiled_as_parameter(import, imp.imp_filename)))
  end;
  param_imports := CU.Name.Set.add import !param_imports

let register_exported_parameter ({exported_params; _} as penv) modname =
  register_parameter_import penv (CU.Name.of_head_of_global_name modname);
  exported_params := Param_set.add modname !exported_params

let find_in_cache penv name =
  find_info_in_cache penv name |> Option.map (fun (_ps, pm) -> pm)

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc import_info =
    let name = Import_info.Intf.name import_info in
    match Impl.and_crc_of_import_info import_info with
    | None -> ()
    | Some (impl, crc) ->
        add_import penv name;
        Consistbl.check crc_units name impl crc source
  in Array.iter import_crc crcs

let check_consistency penv ps =
  try import_crcs penv ~source:ps.ps_filename ps.ps_crcs
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = source;
      original_source = auth;
      inconsistent_data = source_impl;
      original_data = auth_impl;
    } ->
    match source_impl, auth_impl with
    | Known source_unit, Known auth_unit
      when not (CU.equal source_unit auth_unit) ->
        error (Inconsistent_package_declaration_between_imports(
            ps.ps_filename, auth_unit, source_unit))
    | (Known _ | Unknown_argument), _ ->
      error (Inconsistent_import(name, auth, source))

let is_exported_parameter {exported_params; _} name =
  Param_set.mem name !exported_params

let is_registered_parameter_import {param_imports; _} import =
  CU.Name.Set.mem import !param_imports

let is_registered_parameter penv name =
  is_registered_parameter_import penv (CU.Name.of_head_of_global_name name)

let is_unexported_parameter penv name =
  is_registered_parameter penv name
  && not (is_exported_parameter penv name)

let can_load_cmis penv =
  !(penv.can_load_cmis)
let set_can_load_cmis penv setting =
  penv.can_load_cmis := setting

let without_cmis penv f x =
  let log = Lazy_backtrack.log () in
  let res =
    Misc.(protect_refs
            [R (penv.can_load_cmis, Cannot_load_cmis log)]
            (fun () -> f x))
  in
  Lazy_backtrack.backtrack log;
  res

let fold {persistent_structures; _} f x =
  Hashtbl.fold (fun name pso x -> match pso with
      | Missing -> x
      | Found (_, pm) -> f name pm x)
    persistent_structures x

(* Reading persistent structures from .cmi files *)

let remember_crc penv impl crc import filename =
  let {crc_units; _} = penv in
  Consistbl.set crc_units import impl crc filename;
  add_import penv import

let remember_global { globals; _ } global_name global ~mentioned_by =
  if Global.has_arguments global then
    match Hashtbl.find globals global_name with
    | exception Not_found ->
        Hashtbl.add globals global_name
          { gn_global = global; gn_mentioned_by = mentioned_by }
    | { gn_global = old_global; gn_mentioned_by = first_mentioned_by } ->
        if not (Global.equal old_global global) then
          error (Inconsistent_global_name_resolution {
              name = global_name;
              old_global;
              new_global = global;
              first_mentioned_by;
              now_mentioned_by = mentioned_by;
            })

let current_unit_is0 name ~allow_args =
  match CU.get_current () with
  | None -> false
  | Some current ->
      match CU.to_global_name current with
      | Some { head; args } ->
          (args = [] || allow_args)
          && CU.Name.equal name (head |> CU.Name.of_string)
      | None -> false

let current_unit_is name =
  current_unit_is0 name ~allow_args:false

let current_unit_is_instance_of name =
  current_unit_is0 name ~allow_args:true

let need_local_ident penv (global : Global.t) =
  (* There are three equivalent ways to phrase the question we're asking here:

     1. Is this either a parameter or an open import?
     2. Will the generated lambda code need a parameter to take this module's
          value?
     3. Is the value not statically bound?

     Crucially, all modules (besides the one being compiled or instantiated)
     must be either statically bound or toplevel parameters, since the actual
     functor calls that instantiate open modules happen elsewhere (so that they
     can happen exactly once). *)
  let name = global |> Global.to_name |> CU.Name.of_head_of_global_name in
  if is_registered_parameter_import penv name
  then
    (* Already a parameter *)
    true
  else if current_unit_is name
  then
    (* Not actually importing it in the sense of needing its value (we're
       building its value!) *)
    false
  else if current_unit_is_instance_of name
  then
    (* We're instantiating the module, so (here and only here!) we're accessing
       its actual functor, which is a compile-time constant *)
    (* CR lmaurer: Relying on [current_unit_is_instance_of] here feels hacky
       when only a pretty specific call sequence gets here. *)
    false
  else if Global.is_complete global
  then
    (* It's a compile-time constant *)
    false
  else
    (* Some argument is missing, or some argument's argument is missing, etc.,
       so it's not a compile-time constant *)
    true

let make_binding penv (global : Global.t) kind : binding =
  if need_local_ident penv global
  then Local (Ident.create_local_binding_for_global (Global.to_name global))
  else
    let unit_from_cmi =
      match (kind : Cmi_format.kind) with
      | Normal { cmi_impl } -> cmi_impl
      | Parameter ->
          Misc.fatal_errorf
            "Can't bind a parameter statically:@ %a"
            Global.print global
    in
    let unit =
      match global.visible_args with
      | [] ->
          (* Make sure the names are consistent up to the pack prefix *)
          assert (Global.Name.equal
                    (unit_from_cmi |> CU.to_global_name_without_prefix)
                    (global |> Global.to_name));
          unit_from_cmi
      | _ ->
          (* Make sure the unit isn't supposed to be packed *)
          assert (not (CU.is_packed unit_from_cmi));
          CU.of_global_name (global |> Global.to_name)
    in
    Static unit

type 'a sig_reader =
  Persistent_signature.t
  -> global:Global.t
  -> bound_global_names:(Global.Name.t * Global.t) array
  -> binding:binding
  -> 'a * (Global.Name.t * Global.t) array

(* CR lmaurer: This ordering of the recursive group is accidental and should
   probably at least be thought about *)

let rec global_of_global_name penv val_of_pers_sig ~check ~param name =
  match Hashtbl.find penv.globals name with
  | { gn_global; _ } -> gn_global
  | exception Not_found ->
      if param then
        register_parameter_import penv (CU.Name.of_head_of_global_name name);
      let (ps, _) = find_pers_struct penv val_of_pers_sig ~check name in
      ps.ps_global

and compute_global penv val_of_pers_sig modname ~params ~check =
  let arg_global_by_param_name =
    List.map
      (fun (name, value) ->
         name, global_of_global_name penv val_of_pers_sig ~check value ~param:false)
      modname.Global.Name.args
  in
  let param_globals =
    (* Assume for now that there aren't any interdependencies to elaborate
       here. *)
    List.map
      (fun name -> name, global_of_global_name penv val_of_pers_sig ~check name ~param:true)
      params
  in
  let subst : Global.subst = Global.Name.Map.of_list arg_global_by_param_name in
  if check && modname.Global.Name.args <> [] then begin
    (* Produce the expected type of each argument. This takes into account
       substitutions among the parameter types: if the parameters are T and
       To_string[T] and the arguments are [Int] and [Int_to_string], we want to
       check that [Int] has type [T] and that [Int_to_string] has type
       [To_string[T\Int]].

       This currently won't actually do anything because parameters aren't
       interdependent, but it's a tricky bit of dependent type theory that I'm
       happy to have written down now. *)
    let expected_type_by_param_name =
      List.map (fun (name, param) -> name, Global.subst_inside param subst)
        param_globals
    in
    let compare_by_param (param1, _) (param2, _) =
      Global.Name.compare param1 param2
    in
    Misc.Stdlib.List.merge_iter
      ~cmp:compare_by_param
      expected_type_by_param_name
      arg_global_by_param_name
      ~left_only:
        (fun (param, _) ->
           (* Parameter with no argument: fine so long as it's a parameter *)
           register_parameter_import penv
             (CU.Name.of_head_of_global_name param);
           let ps, _ = find_pers_struct penv val_of_pers_sig ~check param in
           (* Either [register_parameter_import] or [find_pers_struct] should
              have thrown if this were false *)
           assert ps.ps_import.imp_is_param)
      ~right_only:
        (fun (param, value) ->
            (* Argument with no parameter: not fine *)
            raise
              (Error (Imported_module_has_no_such_parameter {
                        imported = CU.Name.of_head_of_global_name modname;
                        valid_parameters = params;
                        parameter = param;
                        value = value |> Global.to_name;
                      })))
      ~both:
        (fun (_param_name, expected_type_global) (_arg_name, arg_value_global) ->
            let arg_value = arg_value_global |> Global.to_name in
            let ps, _ = find_pers_struct penv val_of_pers_sig ~check arg_value in
            let actual_type =
              match ps.ps_arg_for with
              | None ->
                  let import = CU.Name.of_head_of_global_name arg_value in
                  error (Not_compiled_as_argument (import, ps.ps_filename))
              | Some ty -> ty
            in
            let actual_type_global =
              global_of_global_name penv val_of_pers_sig actual_type ~check ~param:true
            in
            if not (Global.equal expected_type_global actual_type_global)
            then begin
              let expected_type = Global.to_name expected_type_global in
              if Global.Name.equal expected_type actual_type then
                (* This shouldn't happen, I don't think, but if it does, I'd rather
                  not output an "X != X" sort of error message *)
                Misc.fatal_errorf
                  "Mismatched argument type (despite same name):@ \
                  expected %a,@ got %a"
                  Global.print expected_type_global
                  Global.print actual_type_global
              else
                raise (Error (Argument_type_mismatch {
                    value = arg_value;
                    filename = ps.ps_filename;
                    expected = expected_type;
                    actual = actual_type;
                  }))
            end)
  end;
  (* Form the name without any arguments at all, then substitute in all the
     arguments. A bit roundabout but should be sound *)
  let global_without_args =
    Global.create modname.Global.Name.head [] ~hidden_args:param_globals
  in
  Global.subst global_without_args subst

and acknowledge_pers_struct
      penv ~check global_name pers_sig (val_of_pers_sig : _ sig_reader) =
  let { Persistent_signature.filename; cmi } = pers_sig in
  let found_name = cmi.cmi_name in
  let kind = cmi.cmi_kind in
  let bound_global_names = cmi.cmi_globals in
  let params = cmi.cmi_params in
  let arg_for = cmi.cmi_arg_for in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let modname = CU.Name.of_head_of_global_name global_name in
  if not (CU.Name.equal modname found_name) then
    error (Illegal_renaming(modname, found_name, filename));
  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(modname))
        | Unsafe_string ->
            if Config.safe_string then
              error (Depend_on_unsafe_string_unit(modname));
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    flags;
  begin match kind, CU.get_current () with
  | Normal { cmi_impl = imported_unit }, Some current_unit ->
      let access_allowed =
        CU.can_access_by_name imported_unit ~accessed_by:current_unit
      in
      if not access_allowed then
        let prefix = CU.for_pack_prefix current_unit in
        error (Direct_reference_from_wrong_package (imported_unit, filename, prefix));
  | _, _ -> ()
  end;
  let is_param =
    match kind with
    | Normal _ -> false
    | Parameter -> true
  in
  begin match is_param, is_registered_parameter_import penv modname with
  | true, false ->
      error (Illegal_import_of_parameter(modname, filename))
  | false, true ->
      error (Not_compiled_as_parameter(modname, filename))
  | true, true
  | false, false -> ()
  end;
  let global =
    compute_global penv val_of_pers_sig global_name ~params ~check:true
  in
  let binding = make_binding penv global kind in
  let {imports; persistent_structures; _} = penv in
  let import =
    match Hashtbl.find imports modname with
    | exception Not_found ->
        let import = { imp_is_param = is_param;
                       imp_filename = filename;
                     } in
        Hashtbl.add imports modname import;
        import
    | { imp_is_param; imp_filename } as import ->
        assert (Bool.equal imp_is_param is_param);
        assert (String.equal imp_filename filename);
        import
  in
  let ps = { ps_global = global;
             ps_arg_for = arg_for;
             ps_binding = binding;
             ps_import = import;
             ps_crcs = crcs;
             ps_filename = filename;
           } in
  if check then check_consistency penv ps;
  let pm, bound_global_names =
    val_of_pers_sig pers_sig ~global ~binding ~bound_global_names
  in
  Hashtbl.add imports modname import;
  Hashtbl.add persistent_structures global_name (Found (ps, pm));
  remember_global penv global_name global ~mentioned_by:global_name;
  Array.iter
    (fun (bound_name, bound_global) ->
       remember_global penv bound_name bound_global ~mentioned_by:global_name)
    bound_global_names;
  (ps, pm)

(* CR lmaurer: This doesn't need to be in the recursive group *)
and read_pers_struct penv val_of_pers_sig ~check modname filename =
  add_import penv (CU.Name.of_head_of_global_name modname);
  let cmi = read_cmi_lazy filename in
  let pers_sig = { Persistent_signature.filename; cmi } in
  acknowledge_pers_struct penv ~check modname pers_sig val_of_pers_sig

and find_pers_struct penv val_of_pers_sig ~check name =
  let {persistent_structures; _} = penv in
  if Global.Name.equal name Global.Name.predef_exn then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Found (ps, pm) -> (ps, pm)
  | Missing -> raise Not_found
  | exception Not_found ->
    match can_load_cmis penv with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis ->
        let unit_name = CU.Name.of_head_of_global_name name in
        (* If there are instance arguments, we can imagine trying to save some
           effort by caching the uninstantiated module and then just
           substituting rather than doing another load. This is unlikely to help
           much, however, since it will be rare even to run into _two_
           instantiations of the same module, much less enough to justify a
           more complex caching scheme. *)
        let psig =
          match !Persistent_signature.load ~unit_name with
          | Some psig -> psig
          | None ->
            Hashtbl.add persistent_structures name Missing;
            raise Not_found
        in
        add_import penv unit_name;
        acknowledge_pers_struct penv ~check name psig val_of_pers_sig

let describe_prefix ppf prefix =
  if CU.Prefix.is_empty prefix then
    Format.fprintf ppf "outside of any package"
  else
    Format.fprintf ppf "package %a" CU.Prefix.print prefix

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct penv f ~loc name =
  let name_as_string = CU.Name.to_string (CU.Name.of_head_of_global_name name) in
  try
    ignore (find_pers_struct penv f ~check:false name)
  with
  | Not_found ->
      let warn = Warnings.No_cmi_file(name_as_string, None) in
        Location.prerr_warning loc warn
  | Cmi_format.Error err ->
      let msg = Format.asprintf "%a" Cmi_format.report_error err in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn
  | Error err ->
      let msg =
        match err with
        | Illegal_renaming(name, ps_name, filename) ->
            Format.asprintf
              " %a@ contains the compiled interface for @ \
               %a when %a was expected"
              Location.print_filename filename
              CU.Name.print ps_name
              CU.Name.print name
        | Inconsistent_import _ -> assert false
        | Need_recursive_types name ->
            Format.asprintf
              "%a uses recursive types"
              CU.Name.print name
        | Depend_on_unsafe_string_unit name ->
            Format.asprintf "%a uses -unsafe-string"
              CU.Name.print name
        | Inconsistent_package_declaration _ -> assert false
        | Inconsistent_package_declaration_between_imports _ -> assert false
        | Direct_reference_from_wrong_package (unit, _filename, prefix) ->
            Format.asprintf "%a is inaccessible from %a"
              CU.print unit
              describe_prefix prefix
        (* The cmi is necessary, otherwise the functor cannot be
           generated. Moreover, aliases of functor arguments are forbidden. *)
        | Illegal_import_of_parameter _ -> assert false
        | Not_compiled_as_parameter _ -> assert false
        | Imported_module_has_unset_parameter _ -> assert false
        | Imported_module_has_no_such_parameter _ -> assert false
        | Not_compiled_as_argument _ -> assert false
        | Argument_type_mismatch _ -> assert false
        | Inconsistent_global_name_resolution _ -> assert false
      in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn

let read penv f modname filename =
  snd (read_pers_struct penv f ~check:true modname filename)

let find penv f name =
  snd (find_pers_struct penv f ~check:true name)

let check penv f ~loc name =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import penv (name |> CU.Name.of_head_of_global_name);
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct penv f ~loc name)
  end

(* CR mshinwell: delete this having moved to 4.14 build compilers *)
module Array = struct
  include Array

  (* From stdlib/array.ml *)
  let find_opt p a =
    let n = Array.length a in
    let rec loop i =
      if i = n then None
      else
        let x = Array.unsafe_get a i in
        if p x then Some x
        else loop (succ i)
    in
    loop 0
end

let crc_of_unit penv f name =
  match Consistbl.find penv.crc_units name with
  | Some (_impl, crc) -> crc
  | None ->
    let global_name = Global.Name.create (name |> CU.Name.to_string) [] in
    let (ps, _pm) =
      (* This is a bit wasteful for instances since we'll end up loading the file
        twice, but probably not enough to be a problem. *)
      find_pers_struct penv f ~check:true global_name
    in
    match Array.find_opt (Import_info.Intf.has_name ~name) ps.ps_crcs with
    | None -> assert false
    | Some import_info ->
      match Import_info.Intf.crc import_info with
      | None -> assert false
      | Some crc -> crc

let imports {imported_units;  crc_units; _} =
  let imports =
    Consistbl.extract (CU.Name.Set.elements !imported_units)
      crc_units
  in
  List.map (fun (cu_name, data) ->
      let cu, crc =
        match (data : (Impl.t * Digest.t) option) with
        | None -> None, None
        | Some (Unknown_argument, crc) -> None, Some crc
        | Some (Known cu, crc) -> Some cu, Some crc
      in
      Import_info.Intf.create cu_name cu ~crc)
    imports

let local_ident penv modname =
  match find_info_in_cache penv modname with
  | Some ({ ps_binding = Local local_ident; _ }, _) ->
      if is_unexported_parameter penv modname
      then
        (* This isn't a great situation - the value of [ps_binding] is a lie
           since the module is not in fact bound at all. Possibly we could add
           [No_binding] as a constructor to [binding] and use that as the
           binding for an unexported parameter, but only if we first verify that
           a parameter never becomes exported after [acknowledge_pers_struct]
           sees it. *)
        None
      else
        Some local_ident
  | Some ({ ps_binding = Static _; _ }, _)
  | None -> None

let locally_bound_imports ({persistent_structures; _} as penv) =
  persistent_structures
  |> Hashtbl.to_seq_keys
  |> Seq.filter_map
       (fun name -> local_ident penv name |> Option.map (fun id -> name, id))
  |> List.of_seq

let exported_parameters {exported_params; _} =
  Param_set.elements !exported_params

let looked_up {persistent_structures; _} modname =
  Hashtbl.mem persistent_structures modname

let is_imported_opaque {imported_opaque_units; _} s =
  CU.Name.Set.mem s !imported_opaque_units

let is_parameter_unit penv s =
  is_registered_parameter_import penv s

let implemented_parameter penv modname =
  match find_info_in_cache penv modname with
  | Some ({ ps_arg_for; _ }, _) -> ps_arg_for
  | None -> None

let make_cmi penv modname kind sign secondary_sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      (if !Clflags.unsafe_string then [Cmi_format.Unsafe_string] else []);
      [Alerts alerts];
    ]
  in
  let params =
    (* Needs to be consistent with [Translmod] *)
    exported_parameters penv
  in
  let arg_for =
    !Clflags.as_argument_for
    |> Option.map (fun name -> Global.Name.create name [])
  in
  let crcs = imports penv in
  let globals =
    Hashtbl.to_seq penv.globals
    |> Array.of_seq
    |> Array.map (fun (name, { gn_global; _ }) -> name, gn_global)
  in
  {
    cmi_name = modname;
    cmi_kind = kind;
    cmi_globals = globals;
    cmi_sign = sign;
    cmi_secondary_sign = secondary_sign;
    cmi_params = params;
    cmi_arg_for = arg_for;
    cmi_crcs = Array.of_list crcs;
    cmi_flags = flags
  }

let save_cmi penv psig =
  let { Persistent_signature.filename; cmi } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_kind = kind;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in consistbl so that imports() and crc_of_unit() will
         also return its crc *)
      let impl : Impl.t =
        match kind with
        | Normal { cmi_impl } -> Known cmi_impl
        | Parameter -> Unknown_argument
      in
      remember_crc penv impl crc modname filename
    )
    ~exceptionally:(fun () -> remove_file filename)

let report_error ppf =
  let open Format in
  function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for@ \
       %a when %a was expected"
      Location.print_filename filename
      CU.Name.print ps_name
      CU.Name.print modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %a@]"
      Location.print_filename source1 Location.print_filename source2
      CU.Name.print name
  | Need_recursive_types(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %a, which uses recursive types.@ %s@]"
        CU.Name.print import
        "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %a, compiled with -unsafe-string.@ %s@]"
        CU.Name.print import
        "This compiler has been configured in strict \
                           safe-string mode (-force-safe-string)"
  | Inconsistent_package_declaration(intf_package, intf_filename) ->
      fprintf ppf
        "@[<hov>The interface %a@ is compiled for package %s.@ %s@]"
        CU.print intf_package intf_filename
        "The compilation flag -for-pack with the same package is required"
  | Inconsistent_package_declaration_between_imports (filename, unit1, unit2) ->
      fprintf ppf
        "@[<hov>The file %s@ is imported both as %a@ and as %a.@]"
        filename
        CU.print unit1
        CU.print unit2
  | Direct_reference_from_wrong_package(unit, filename, prefix) ->
      fprintf ppf
        "@[<hov>Invalid reference to %a (in file %s) from %a.@ %s]"
        CU.print unit
        filename
        describe_prefix prefix
        "Can only access members of this library's package or a containing package"
  | Illegal_import_of_parameter(modname, filename) ->
      fprintf ppf
        "@[<hov>The file %a@ contains the interface of a parameter.@ \
         %a is not declared as a parameter for the current unit (-parameter %a).@]"
        Location.print_filename filename
        CU.Name.print modname
        CU.Name.print modname
  | Not_compiled_as_parameter(modname, filename) ->
      fprintf ppf
        "@[<hov>The module %a@ is specified as a parameter, but %a@ \
         was not compiled with -as-parameter.@]"
        CU.Name.print modname
        Location.print_filename filename
  | Imported_module_has_unset_parameter
        { imported = modname; parameter = param } ->
      fprintf ppf
        "@[<hov>The module %a@ has parameter %a.@ \
         %a is not declared as a parameter for the current unit (-parameter %a)@ \
         and therefore %a@ is not accessible.@]"
        CU.Name.print modname
        Global.Name.print param
        Global.Name.print param
        Global.Name.print param
        CU.Name.print modname
  | Imported_module_has_no_such_parameter
        { valid_parameters; imported = modname; parameter = param; value; } ->
      fprintf ppf
        "@[<hov>The module %a@ is given argument %a@ for parameter %a.@ "
        CU.Name.print modname
        Global.Name.print value
        Global.Name.print param;
      begin match valid_parameters with
      | [] ->
          fprintf ppf "%a has no parameters."
            CU.Name.print modname
      | _ ->
          let print_params =
            Format.pp_print_list ~pp_sep:Format.pp_print_space Global.Name.print
          in
          fprintf ppf "Valid parameters for %a:@ @[<hov>%a@]"
            CU.Name.print modname
            print_params valid_parameters
      end;
      fprintf ppf "@]"
  | Not_compiled_as_argument(modname, filename) ->
      fprintf ppf
        "@[<hov>The module %a@ is specified as an instance argument, but %a@ \
         was not compiled with -as-argument-for.@]"
        CU.Name.print modname
        Location.print_filename filename
  | Argument_type_mismatch { value; filename; expected; actual; } ->
      fprintf ppf
        "@[<hov>The module %a@ was expected to satisfy the parameter %a@ \
         but %a@ was compiled with -as-argument-for %a.@]"
        Global.Name.print value
        Global.Name.print expected
        Location.print_filename filename
        Global.Name.print actual
  | Inconsistent_global_name_resolution
      { name; old_global; new_global; first_mentioned_by; now_mentioned_by } ->
      fprintf ppf
        "@[<hov>The name %a@ was bound to %a@ by %a@ \
         but it is instead bound to %a@ by %a.@]"
        Global.Name.print name
        Global.print old_global
        Global.Name.print first_mentioned_by
        Global.print new_global
        Global.Name.print now_mentioned_by

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
