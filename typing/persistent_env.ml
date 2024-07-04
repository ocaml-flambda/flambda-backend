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
module Consistbl_data = Import_info.Intf.Nonalias.Kind
module Consistbl = Consistbl.Make (CU.Name) (Consistbl_data)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of CU.Name.t * CU.Name.t * filepath
  | Inconsistent_import of CU.Name.t * filepath * filepath
  | Need_recursive_types of CU.Name.t
  | Inconsistent_package_declaration_between_imports of
      filepath * CU.t * CU.t
  | Direct_reference_from_wrong_package of
      CU.t * filepath * CU.Prefix.t
  | Illegal_import_of_parameter of CU.Name.t * filepath
  | Not_compiled_as_parameter of CU.Name.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : CU.Name.t;
        parameter : CU.Name.t;
      }

exception Error of error
let error err = raise (Error err)

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos_lazy;
      visibility : Load_path.visibility }

  let load = ref (fun ~allow_hidden ~unit_name ->
    let unit_name = CU.Name.to_string unit_name in
    match Load_path.find_uncap_with_visibility (unit_name ^ ".cmi") with
    | filename, visibility when allow_hidden ->
      Some { filename; cmi = read_cmi_lazy filename; visibility}
    | filename, Visible ->
      Some { filename; cmi = read_cmi_lazy filename; visibility = Visible}
    | _, Hidden
    | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

(* Data relating directly to a .cmi *)
type import = {
  imp_is_param : bool;
  imp_params : Compilation_unit.Name.t list;
  imp_arg_for : Compilation_unit.Name.t option;
  imp_impl : CU.t option; (* None iff import is a parameter *)
  imp_sign : Subst.Lazy.signature;
  imp_filename : string;
  imp_visibility: Load_path.visibility;
  imp_crcs : Import_info.Intf.t array;
  imp_flags : Cmi_format.pers_flags list;
}

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type import_info =
  | Missing
  | Found of import

(* What a global identifier is actually bound to in Lambda code *)
type binding =
  | Runtime_parameter of Ident.t (* Bound to a runtime parameter *)
  | Constant of Compilation_unit.t (* Bound to a static constant *)

(* Data relating to an actual referenceable module, with a signature and a
   representation in memory. *)
type 'a pers_struct_info = {
  ps_import : import;
  ps_binding : binding;
  ps_val : 'a;
}

module Param_set = CU.Name.Set

(* If you add something here, _do not forget_ to add it to [clear]! *)
type 'a t = {
  imports : (CU.Name.t, import_info) Hashtbl.t;
  persistent_structures :
    (CU.Name.t, 'a pers_struct_info) Hashtbl.t;
  imported_units: CU.Name.Set.t ref;
  imported_opaque_units: CU.Name.Set.t ref;
  param_imports : Param_set.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  imports = Hashtbl.create 17;
  persistent_structures = Hashtbl.create 17;
  imported_units = ref CU.Name.Set.empty;
  imported_opaque_units = ref CU.Name.Set.empty;
  param_imports = ref Param_set.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let {
    imports;
    persistent_structures;
    imported_units;
    imported_opaque_units;
    param_imports;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear imports;
  Hashtbl.clear persistent_structures;
  imported_units := CU.Name.Set.empty;
  imported_opaque_units := CU.Name.Set.empty;
  param_imports := Param_set.empty;
  Consistbl.clear crc_units;
  can_load_cmis := Can_load_cmis;
  ()

let clear_missing {imports; _} =
  let missing_entries =
    Hashtbl.fold
      (fun name r acc -> if r = Missing then name :: acc else acc)
      imports []
  in
  List.iter (Hashtbl.remove imports) missing_entries

let add_import {imported_units; _} s =
  imported_units := CU.Name.Set.add s !imported_units

let register_import_as_opaque {imported_opaque_units; _} s =
  imported_opaque_units := CU.Name.Set.add s !imported_opaque_units

let find_import_info_in_cache {imports; _} import =
  match Hashtbl.find imports import with
  | exception Not_found -> None
  | Missing -> None
  | Found imp -> Some imp

let find_info_in_cache {persistent_structures; _} name =
  match Hashtbl.find persistent_structures name with
  | exception Not_found -> None
  | ps -> Some ps

let find_in_cache penv name =
  find_info_in_cache penv name |> Option.map (fun ps -> ps.ps_val)

let register_parameter ({param_imports; _} as penv) import =
  begin match find_import_info_in_cache penv import with
  | None ->
      (* Not loaded yet; if it's wrong, we'll get an error at load time *)
      ()
  | Some imp ->
      if not imp.imp_is_param then
        raise (Error (Not_compiled_as_parameter(import, imp.imp_filename)))
  end;
  param_imports := Param_set.add import !param_imports

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc import_info =
    let name = Import_info.Intf.name import_info in
    let info = Import_info.Intf.info import_info in
    match info with
    | None -> ()
    | Some (kind, crc) ->
        add_import penv name;
        Consistbl.check crc_units name kind crc source
  in Array.iter import_crc crcs

let check_consistency penv imp =
  try import_crcs penv ~source:imp.imp_filename imp.imp_crcs
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = source;
      original_source = auth;
      inconsistent_data = source_kind;
      original_data = auth_kind;
    } ->
    match source_kind, auth_kind with
    | Normal source_unit, Normal auth_unit
      when not (CU.equal source_unit auth_unit) ->
        error (Inconsistent_package_declaration_between_imports(
            imp.imp_filename, auth_unit, source_unit))
    | (Normal _ | Parameter), _ ->
      error (Inconsistent_import(name, auth, source))

let is_registered_parameter_import {param_imports; _} import =
  Param_set.mem import !param_imports

let is_parameter_import t import =
  match find_import_info_in_cache t import with
  | Some { imp_is_param; _ } -> imp_is_param
  | None -> Misc.fatal_errorf "is_parameter_import %a" CU.Name.print import

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
  Hashtbl.fold (fun name ps x -> f name ps.ps_val x)
    persistent_structures x

(* Reading persistent structures from .cmi files *)

let save_import penv crc modname impl flags filename =
  let {crc_units; _} = penv in
  List.iter
    (function
        | Rectypes -> ()
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    flags;
  Consistbl.check crc_units modname impl crc filename;
  add_import penv modname

(* Add an import to the hash table. Checks that we are allowed to access
   this .cmi. *)

let acknowledge_import penv ~check modname pers_sig =
  let { Persistent_signature.filename; cmi; visibility } = pers_sig in
  let found_name = cmi.cmi_name in
  let kind = cmi.cmi_kind in
  let params = cmi.cmi_params in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let sign =
    (* Freshen identifiers bound by signature *)
    Subst.Lazy.signature Make_local Subst.identity cmi.cmi_sign
  in
  if not (CU.Name.equal modname found_name) then
    error (Illegal_renaming(modname, found_name, filename));
  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(modname))
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
  let arg_for, impl =
    match kind with
    | Normal { cmi_arg_for; cmi_impl } -> cmi_arg_for, Some cmi_impl
    | Parameter -> None, None
  in
  let {imports; _} = penv in
  let import =
    { imp_is_param = is_param;
      imp_params = params;
      imp_arg_for = arg_for;
      imp_impl = impl;
      imp_sign = sign;
      imp_filename = filename;
      imp_visibility = visibility;
      imp_crcs = crcs;
      imp_flags = flags;
    }
  in
  if check then check_consistency penv import;
  Hashtbl.add imports modname (Found import);
  import

let read_import penv ~check modname filename =
  add_import penv modname;
  let cmi = read_cmi_lazy filename in
  let pers_sig = { Persistent_signature.filename; cmi; visibility = Visible } in
  acknowledge_import penv ~check modname pers_sig

let check_visibility ~allow_hidden imp =
  if not allow_hidden && imp.imp_visibility = Load_path.Hidden then raise Not_found

let find_import ~allow_hidden penv ~check modname =
  let {imports; _} = penv in
  if CU.Name.equal modname CU.Name.predef_exn then raise Not_found;
  match Hashtbl.find imports modname with
  | Found imp -> check_visibility ~allow_hidden imp; imp
  | Missing -> raise Not_found
  | exception Not_found ->
      match can_load_cmis penv with
      | Cannot_load_cmis _ -> raise Not_found
      | Can_load_cmis ->
          let psig =
            match !Persistent_signature.load ~allow_hidden ~unit_name:modname with
            | Some psig -> psig
            | None ->
                if allow_hidden then Hashtbl.add imports modname Missing;
                raise Not_found
          in
          add_import penv modname;
          acknowledge_import penv ~check modname psig

(* Enforce the subset rule: we can only refer to a module if that module's
   parameters are also our parameters. *)
let check_for_unset_parameters penv modname import =
  List.iter
    (fun param ->
       if not (is_registered_parameter_import penv param) then
         error (Imported_module_has_unset_parameter {
             imported = modname;
             parameter = param;
           }))
    import.imp_params

let make_binding _penv modname (import : import) : binding =
  match import with
  | { imp_impl = Some unit; imp_params = [] } -> Constant unit
  | { imp_impl = None } | { imp_params = _ :: _ } ->
      Runtime_parameter (Ident.create_local_binding_for_global (CU.Name.to_string modname))

type address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type 'a sig_reader =
  Subst.Lazy.signature
  -> Compilation_unit.Name.t
  -> Shape.Uid.t
  -> shape:Shape.t
  -> address:address
  -> flags:Cmi_format.pers_flags list
  -> 'a

(* Add a persistent structure to the hash table and bind it in the [Env].
   Checks that OCaml source is allowed to refer to this module. *)

let acknowledge_pers_struct penv modname import val_of_pers_sig =
  let {persistent_structures; _} = penv in
  let is_param = import.imp_is_param in
  let sign = import.imp_sign in
  let filename = import.imp_filename in
  let flags = import.imp_flags in
  check_for_unset_parameters penv modname import;
  begin match is_param, is_registered_parameter_import penv modname with
  | true, false ->
      error (Illegal_import_of_parameter(modname, filename))
  | false, true ->
      error (Not_compiled_as_parameter(modname, filename))
  | true, true
  | false, false -> ()
  end;
  let binding = make_binding penv modname import in
  let address : address =
    match binding with
    | Runtime_parameter id -> Alocal id
    | Constant unit -> Aunit unit
  in
  let uid =
    (* This is source-level information that depends only on the import, not the
       arguments. (TODO: Consider moving this bit into [acknowledge_import].) *)
    match import.imp_impl with
    | Some unit -> Shape.Uid.of_compilation_unit_id unit
    | None ->
        (* TODO: [Shape.Uid.of_compilation_unit_id] is actually the wrong type, since
           parameters should also have uids but they don't have .cmx files and thus
           they don't have [CU.t]s *)
        Shape.Uid.internal_not_actually_unique
  in
  let shape =
    match import.imp_impl, import.imp_params with
    | Some unit, [] -> Shape.for_persistent_unit (CU.full_path_as_string unit)
    | _, _ ->
        (* TODO Implement shapes for parameters and parameterised modules *)
        Shape.error ~uid "parameter or parameterised module"
  in
  let pm = val_of_pers_sig sign modname uid ~shape ~address ~flags in
  let ps =
    { ps_import = import;
      ps_binding = binding;
      ps_val = pm;
    }
  in
  Hashtbl.add persistent_structures modname ps;
  ps

let read_pers_struct penv val_of_pers_sig check modname filename ~add_binding =
  let import = read_import penv ~check modname filename in
  if add_binding then
    ignore
      (acknowledge_pers_struct penv modname import val_of_pers_sig
       : _ pers_struct_info);
  import.imp_sign

let find_pers_struct ~allow_hidden penv val_of_pers_sig check name =
  let {persistent_structures; _} = penv in
  match Hashtbl.find persistent_structures name with
  | ps -> check_visibility ~allow_hidden ps.ps_import; ps
  | exception Not_found ->
      let import = find_import ~allow_hidden penv ~check name in
      acknowledge_pers_struct penv name import val_of_pers_sig

let describe_prefix ppf prefix =
  if CU.Prefix.is_empty prefix then
    Format.fprintf ppf "outside of any package"
  else
    Format.fprintf ppf "package %a" CU.Prefix.print prefix

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~allow_hidden penv f ~loc name =
  let name_as_string = CU.Name.to_string name in
  try
    ignore (find_pers_struct ~allow_hidden penv f false name)
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
        | Inconsistent_package_declaration_between_imports _ -> assert false
        | Direct_reference_from_wrong_package (unit, _filename, prefix) ->
            Format.asprintf "%a is inaccessible from %a"
              CU.print unit
              describe_prefix prefix
        | Illegal_import_of_parameter _ -> assert false
        | Not_compiled_as_parameter _ -> assert false
        | Imported_module_has_unset_parameter _ -> assert false
      in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn

let read penv f modname filename ~add_binding =
  read_pers_struct penv f true modname filename ~add_binding

let find ~allow_hidden penv f name =
  (find_pers_struct ~allow_hidden penv f true name).ps_val

let check ~allow_hidden penv f ~loc name =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import penv name;
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct ~allow_hidden penv f ~loc name)
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

let crc_of_unit penv name =
  match Consistbl.find penv.crc_units name with
  | Some (_impl, crc) -> crc
  | None ->
    let import = find_import ~allow_hidden:true penv ~check:true name in
    match Array.find_opt (Import_info.Intf.has_name ~name) import.imp_crcs with
    | None -> assert false
    | Some import_info ->
      match Import_info.crc import_info with
      | None -> assert false
      | Some crc -> crc

let imports {imported_units; crc_units; _} =
  let imports =
    Consistbl.extract (CU.Name.Set.elements !imported_units)
      crc_units
  in
  List.map (fun (cu_name, spec) -> Import_info.Intf.create cu_name spec)
    imports

let local_ident penv modname =
  match find_info_in_cache penv modname with
  | Some { ps_binding = Runtime_parameter local_ident; _ } -> Some local_ident
  | Some { ps_binding = Constant _; _ }
  | None -> None

let runtime_parameters ({persistent_structures; _} as penv) =
  persistent_structures
  |> Hashtbl.to_seq_keys
  |> Seq.filter_map
       (fun name -> local_ident penv name |> Option.map (fun id -> name, id))
  |> List.of_seq

let parameters {param_imports; _} =
  Param_set.elements !param_imports

let looked_up {persistent_structures; _} modname =
  Hashtbl.mem persistent_structures modname

let is_imported {imported_units; _} s =
  CU.Name.Set.mem s !imported_units

let is_imported_opaque {imported_opaque_units; _} s =
  CU.Name.Set.mem s !imported_opaque_units

let implemented_parameter penv modname =
  match find_import_info_in_cache penv modname with
  | Some { imp_arg_for; _ } -> imp_arg_for
  | None -> None

let make_cmi penv modname kind sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      [Alerts alerts];
    ]
  in
  let params =
    (* Needs to be consistent with [Translmod] *)
    parameters penv
  in
  let crcs = imports penv in
  {
    cmi_name = modname;
    cmi_kind = kind;
    cmi_sign = sign;
    cmi_params = params;
    cmi_crcs = Array.of_list crcs;
    cmi_flags = flags
  }

let save_cmi penv psig =
  let { Persistent_signature.filename; cmi; _ } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_kind = kind;
        cmi_sign = _;
        cmi_crcs = _;
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in consistbl so that imports()
         will also return its crc *)
      let data : Import_info.Intf.Nonalias.Kind.t =
        match kind with
        | Normal { cmi_impl } -> Normal cmi_impl
        | Parameter -> Parameter
      in
      save_import penv crc modname data flags filename
    )
    ~exceptionally:(fun () -> remove_file filename)

(* TODO: These should really have locations in them where possible (adapting
   [Typemod]'s [Error] constructor is probably the easiest path) *)

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
  | Inconsistent_package_declaration_between_imports (filename, unit1, unit2) ->
      fprintf ppf
        "@[<hov>The file %s@ is imported both as %a@ and as %a.@]"
        filename
        CU.print unit1
        CU.print unit2
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
  | Direct_reference_from_wrong_package(unit, filename, prefix) ->
      fprintf ppf
        "@[<hov>Invalid reference to %a (in file %s) from %a.@ %s]"
        CU.print unit
        filename
        describe_prefix prefix
        "Can only access members of this library's package or a containing package"
  | Imported_module_has_unset_parameter
        { imported = modname; parameter = param } ->
      fprintf ppf
        "@[<hov>The module %a@ is not accessible because it takes %a@ \
         as a parameter and the current unit does not.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Pass `-parameter %a`@ to add %a@ as a parameter@ \
           of the current unit.@]@]"
        CU.Name.print modname
        CU.Name.print param
        CU.Name.print param
        CU.Name.print param

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
