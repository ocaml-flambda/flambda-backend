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
  | Illegal_import_of_parameter of Global_module.Name.t * filepath
  | Not_compiled_as_parameter of Global_module.Name.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : Global_module.Name.t;
        parameter : Global_module.Name.t;
      }
  | Imported_module_has_no_such_parameter of
      { imported : CU.Name.t;
        valid_parameters : Global_module.Name.t list;
        parameter : Global_module.Name.t;
        value : Global_module.Name.t;
      }
  | Not_compiled_as_argument of CU.Name.t * filepath
  | Argument_type_mismatch of
      { value : Global_module.Name.t;
        filename : filepath;
        expected : Global_module.Name.t;
        actual : Global_module.Name.t;
      }
  | Inconsistent_global_name_resolution of {
      name: Global_module.Name.t;
      old_global : Global_module.t;
      new_global : Global_module.t;
      first_mentioned_by : Global_module.Name.t;
      now_mentioned_by : Global_module.Name.t;
    }
  | Unbound_module_as_argument_value of
      { instance: Global_module.Name.t;
        value: Global_module.Name.t;
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

type global_name_info = {
  gn_global : Global_module.t;
  gn_mentioned_by : Global_module.Name.t; (* For error reporting *)
}

(* Data relating directly to a .cmi - does not depend on arguments *)
type import = {
  imp_is_param : bool;
  imp_params : Global_module.t list;
  imp_arg_for : Global_module.Name.t option;
  imp_impl : CU.t option; (* None iff import is a parameter *)
  imp_raw_sign : Signature_with_global_bindings.t;
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

(* Data relating to a global name (possibly with arguments) but not necessarily
   a value in scope. For example, if we've encountered a parameter module only
   by seeing it used as the name of an argument in a [Global_module.Name.t], we
   won't bind it or construct a [pers_struct] for it but it will have a
   [pers_name]. *)
type pers_name = {
  pn_import : import;
  pn_global : Global_module.t;
  pn_arg_for : Global_module.Name.t option;
    (* Currently always the same as [pn_import.imp_arg_for], since parameters
       don't have parameters *)
  pn_sign : Subst.Lazy.signature;
}

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

module Param_set = Global_module.Name.Set

(* If you add something here, _do not forget_ to add it to [clear]! *)
type 'a t = {
  globals : (Global_module.Name.t, global_name_info) Hashtbl.t;
  imports : (CU.Name.t, import_info) Hashtbl.t;
  persistent_names : (Global_module.Name.t, pers_name) Hashtbl.t;
  persistent_structures : (Global_module.Name.t, 'a pers_struct_info) Hashtbl.t;
  imported_units: CU.Name.Set.t ref;
  imported_opaque_units: CU.Name.Set.t ref;
  param_imports : Param_set.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  globals = Hashtbl.create 17;
  imports = Hashtbl.create 17;
  persistent_names = Hashtbl.create 17;
  persistent_structures = Hashtbl.create 17;
  imported_units = ref CU.Name.Set.empty;
  imported_opaque_units = ref CU.Name.Set.empty;
  param_imports = ref Param_set.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let {
    globals;
    imports;
    persistent_names;
    persistent_structures;
    imported_units;
    imported_opaque_units;
    param_imports;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear globals;
  Hashtbl.clear imports;
  Hashtbl.clear persistent_names;
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

let find_name_info_in_cache {persistent_names; _} name =
  match Hashtbl.find persistent_names name with
  | exception Not_found -> None
  | pn -> Some pn

let find_info_in_cache {persistent_structures; _} name =
  match Hashtbl.find persistent_structures name with
  | exception Not_found -> None
  | ps -> Some ps

let find_in_cache penv name =
  find_info_in_cache penv name |> Option.map (fun ps -> ps.ps_val)

let register_parameter ({param_imports; _} as penv) modname =
  let import = CU.Name.of_head_of_global_name modname in
  begin match find_import_info_in_cache penv import with
  | None ->
      (* Not loaded yet; if it's wrong, we'll get an error at load time *)
      ()
  | Some imp ->
      if not imp.imp_is_param then
        raise (Error (Not_compiled_as_parameter(modname, imp.imp_filename)))
  end;
  param_imports := Param_set.add modname !param_imports

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

let is_parameter_import t modname =
  let import = CU.Name.of_head_of_global_name modname in
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
  let sign = Signature_with_global_bindings.read_from_cmi cmi in
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
      imp_raw_sign = sign;
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

let remember_global { globals; _ } global ~mentioned_by =
  if Global_module.has_arguments global then
    let global_name = Global_module.to_name global in
    match Hashtbl.find globals global_name with
    | exception Not_found ->
        Hashtbl.add globals global_name
          { gn_global = global; gn_mentioned_by = mentioned_by }
    | { gn_global = old_global; gn_mentioned_by = first_mentioned_by } ->
        if not (Global_module.equal old_global global) then
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

(* Enforce the subset rule: we can only refer to a module if that module's
   parameters are also our parameters. *)
let check_for_unset_parameters penv global =
  (* A hidden argument specifies that the importing module should forward a
     parameter to the imported module. Therefore it's the hidden arguments that
     we need to check. *)
  List.iter
    (fun (arg_name, arg_value) ->
       (* The _value_ is what we care about - the name lives in the imported
          module's namespace, not ours *)
       ignore arg_name;
       let value_name = Global_module.to_name arg_value in
       if not (is_registered_parameter_import penv value_name) then
         error (Imported_module_has_unset_parameter {
             imported = Global_module.to_name global;
             parameter = value_name;
           }))
    global.Global_module.hidden_args;
  (* The visible arguments in [global] must also satisfy the subset rule.
     However, these will already have been checked since [compute_global] loads
     all argument names and values. *)
  ()

let rec global_of_global_name penv ~check name =
  match Hashtbl.find penv.globals name with
  | { gn_global; _ } -> gn_global
  | exception Not_found ->
      let pn = find_pers_name ~allow_hidden:true penv check name in
      pn.pn_global

and compute_global penv modname ~params check =
  let arg_global_by_param_name =
    List.map
      (fun (name, value) ->
         match global_of_global_name penv ~check value with
         | value -> name, value
         | exception Not_found ->
             error
               (Unbound_module_as_argument_value { instance = modname; value }))
      modname.Global_module.Name.args
  in
  let subst : Global_module.subst = Global_module.Name.Map.of_list arg_global_by_param_name in
  if check && modname.Global_module.Name.args <> [] then begin
    (* Produce the expected type of each argument. This takes into account
       substitutions among the parameter types: if the parameters are T and
       To_string[T] and the arguments are [Int] and [Int_to_string], we want to
       check that [Int] has type [T] and that [Int_to_string] has type
       [To_string[T\Int]].

       This currently won't actually do anything because parameters aren't
       interdependent, but it's a tricky bit of dependent type theory that I'm
       happy to have written down now. *)
    let expected_type_by_param_name =
      List.map
        (fun param -> Global_module.to_name param, Global_module.subst_inside param subst)
        params
    in
    let compare_by_param (param1, _) (param2, _) =
      Global_module.Name.compare param1 param2
    in
    Misc.Stdlib.List.merge_iter
      ~cmp:compare_by_param
      expected_type_by_param_name
      arg_global_by_param_name
      ~left_only:
        (fun _ ->
           (* Parameter with no argument: fine (subset rule will be checked by
              [check_for_unset_parameters] later) *)
           ())
      ~right_only:
        (fun (param, value) ->
            (* Argument with no parameter: not fine *)
            raise
              (Error (Imported_module_has_no_such_parameter {
                        imported = CU.Name.of_head_of_global_name modname;
                        valid_parameters = params |> List.map Global_module.to_name;
                        parameter = param;
                        value = value |> Global_module.to_name;
                      })))
      ~both:
        (fun (_param_name, expected_type_global) (_arg_name, arg_value_global) ->
            let arg_value = arg_value_global |> Global_module.to_name in
            let pn = find_pers_name ~allow_hidden:true penv check arg_value in
            let actual_type =
              match pn.pn_arg_for with
              | None ->
                  let import = CU.Name.of_head_of_global_name arg_value in
                  error (Not_compiled_as_argument
                           (import, pn.pn_import.imp_filename))
              | Some ty -> ty
            in
            let actual_type_global =
              global_of_global_name penv ~check actual_type
            in
            if not (Global_module.equal expected_type_global actual_type_global)
            then begin
              let expected_type = Global_module.to_name expected_type_global in
              if Global_module.Name.equal expected_type actual_type then
                (* This shouldn't happen, I don't think, but if it does, I'd rather
                  not output an "X != X" sort of error message *)
                Misc.fatal_errorf
                  "Mismatched argument type (despite same name):@ \
                  expected %a,@ got %a"
                  Global_module.print expected_type_global
                  Global_module.print actual_type_global
              else
                raise (Error (Argument_type_mismatch {
                    value = arg_value;
                    filename = pn.pn_import.imp_filename;
                    expected = expected_type;
                    actual = actual_type;
                  }))
            end)
  end;
  (* Form the name without any arguments at all, then substitute in all the
     arguments. A bit roundabout but should be sound *)
  let hidden_args =
    List.map (fun param -> Global_module.to_name param, param) params
  in
  let global_without_args =
    (* Won't raise an exception, since the hidden args are all different
       (since the params are different, or else we have bigger problems) *)
    Global_module.create_exn modname.Global_module.Name.head [] ~hidden_args
  in
  let global, _changed = Global_module.subst global_without_args subst in
  global

and acknowledge_pers_name penv check global_name import =
  let params = import.imp_params in
  let arg_for = import.imp_arg_for in
  let sign = import.imp_raw_sign in
  let global = compute_global penv global_name ~params check in
  check_for_unset_parameters penv global;
  let {persistent_names; _} = penv in
  let sign =
    (* Only need to substitute the visible args, since the hidden args only
       reflect substitutions already made by visible args *)
    Signature_with_global_bindings.subst sign
      global.Global_module.visible_args
  in
  Array.iter
    (fun bound_global ->
       remember_global penv bound_global ~mentioned_by:global_name)
    sign.bound_globals;
  let pn = { pn_import = import;
             pn_global = global;
             pn_arg_for = arg_for;
             pn_sign = sign.sign;
           } in
  if check then check_consistency penv import;
  Hashtbl.add persistent_names global_name pn;
  remember_global penv global ~mentioned_by:global_name;
  pn

and find_pers_name ~allow_hidden penv check name =
  let {persistent_names; _} = penv in
  match Hashtbl.find persistent_names name with
  | pn -> pn
  | exception Not_found ->
      let unit_name = CU.Name.of_head_of_global_name name in
      let import = find_import ~allow_hidden penv ~check unit_name in
      acknowledge_pers_name penv check name import

let read_pers_name penv check name filename =
  let unit_name = CU.Name.of_head_of_global_name name in
  let import = read_import penv ~check unit_name filename in
  acknowledge_pers_name penv check name import

let need_local_ident penv (global : Global_module.t) =
  (* There are three equivalent ways to phrase the question we're asking here:

     1. Is this either a parameter or an open import?
     2. Will the generated lambda code need a parameter to take this module's
          value?
     3. Is the value not statically bound?

     Crucially, all modules (besides the one being compiled or instantiated)
     must be either statically bound or toplevel parameters, since the actual
     functor calls that instantiate open modules happen elsewhere (so that they
     can happen exactly once). *)
  let global_name = global |> Global_module.to_name in
  let name = global_name |> CU.Name.of_head_of_global_name in
  if is_registered_parameter_import penv global_name
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
  else if Global_module.is_complete global
  then
    (* It's a compile-time constant *)
    false
  else
    (* Some argument is missing, or some argument's argument is missing, etc.,
       so it's not a compile-time constant *)
    true

let make_binding penv (global : Global_module.t) (impl : CU.t option) : binding =
  let name = Global_module.to_name global in
  if need_local_ident penv global
  then Runtime_parameter (Ident.create_local_binding_for_global name)
  else
    let unit_from_cmi =
      match impl with
      | Some unit -> unit
      | None ->
          Misc.fatal_errorf
            "Can't bind a parameter statically:@ %a"
            Global_module.print global
    in
    let unit =
      match global.visible_args with
      | [] ->
          (* Make sure the names are consistent up to the pack prefix *)
          assert (Global_module.Name.equal
                    (unit_from_cmi |> CU.to_global_name_without_prefix)
                    name);
          unit_from_cmi
      | _ ->
          (* Make sure the unit isn't supposed to be packed *)
          assert (not (CU.is_packed unit_from_cmi));
          CU.of_global_name name
    in
    Constant unit

type address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type 'a sig_reader =
  Subst.Lazy.signature
  -> Global_module.Name.t
  -> Shape.Uid.t
  -> shape:Shape.t
  -> address:address
  -> flags:Cmi_format.pers_flags list
  -> 'a

(* Add a persistent structure to the hash table and bind it in the [Env].
   Checks that OCaml source is allowed to refer to this module. *)

let acknowledge_pers_struct penv modname pers_name val_of_pers_sig =
  let {persistent_structures; _} = penv in
  let import = pers_name.pn_import in
  let global = pers_name.pn_global in
  let sign = pers_name.pn_sign in
  let is_param = import.imp_is_param in
  let impl = import.imp_impl in
  let filename = import.imp_filename in
  let flags = import.imp_flags in
  begin match is_param, is_registered_parameter_import penv modname with
  | true, false ->
      error (Illegal_import_of_parameter(modname, filename))
  | false, true ->
      error (Not_compiled_as_parameter(modname, filename))
  | true, true
  | false, false -> ()
  end;
  let binding = make_binding penv global impl in
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
  let pers_name = read_pers_name penv check modname filename in
  if add_binding then
    ignore
      (acknowledge_pers_struct penv modname pers_name val_of_pers_sig
       : _ pers_struct_info);
  pers_name.pn_sign

let find_pers_struct ~allow_hidden penv val_of_pers_sig check name =
  let {persistent_structures; _} = penv in
  match Hashtbl.find persistent_structures name with
  | ps -> check_visibility ~allow_hidden ps.ps_import; ps
  | exception Not_found ->
      let pers_name = find_pers_name ~allow_hidden penv check name in
      acknowledge_pers_struct penv name pers_name val_of_pers_sig

let describe_prefix ppf prefix =
  if CU.Prefix.is_empty prefix then
    Format.fprintf ppf "outside of any package"
  else
    Format.fprintf ppf "package %a" CU.Prefix.print prefix

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~allow_hidden penv f ~loc name =
  let name_as_string = CU.Name.to_string (CU.Name.of_head_of_global_name name) in
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
        (* The cmi is necessary, otherwise the functor cannot be
           generated. Moreover, aliases of functor arguments are forbidden. *)
        | Illegal_import_of_parameter _ -> assert false
        | Not_compiled_as_parameter _ -> assert false
        | Imported_module_has_unset_parameter _ -> assert false
        | Imported_module_has_no_such_parameter _ -> assert false
        | Not_compiled_as_argument _ -> assert false
        | Argument_type_mismatch _ -> assert false
        | Inconsistent_global_name_resolution _ -> assert false
        | Unbound_module_as_argument_value _ -> assert false
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
    add_import penv (name |> CU.Name.of_head_of_global_name);
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

let is_imported_opaque {imported_opaque_units; _} s =
  CU.Name.Set.mem s !imported_opaque_units

let implemented_parameter penv modname =
  match find_name_info_in_cache penv modname with
  | Some { pn_arg_for; _ } -> pn_arg_for
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
    |> List.map (global_of_global_name penv ~check:true)
  in
  (* Need to calculate [params] before these since [global_of_global_name] has
     side effects *)
  let crcs = imports penv in
  let globals =
    Hashtbl.to_seq_values penv.globals
    |> Array.of_seq
    |> Array.map (fun ({ gn_global; _ }) -> gn_global)
  in
  {
    cmi_name = modname;
    cmi_kind = kind;
    cmi_globals = globals;
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
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in consistbl so that imports() and crc_of_unit() will
         also return its crc *)
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
        Global_module.Name.print modname
        Global_module.Name.print modname
  | Not_compiled_as_parameter(modname, filename) ->
      fprintf ppf
        "@[<hov>The module %a@ is specified as a parameter, but %a@ \
         was not compiled with -as-parameter.@]"
        Global_module.Name.print modname
        Location.print_filename filename
  | Imported_module_has_unset_parameter
        { imported = modname; parameter = param } ->
      fprintf ppf
        "@[<hov>The module %a@ is not accessible because it takes %a@ \
         as a parameter and the current unit does not.@]@.\
         @[<hov>@{<hint>Hint@}: \
           @[<hov>Pass `-parameter %a`@ to add %a@ as a parameter@ \
           of the current unit.@]@]"
        Global_module.Name.print modname
        Global_module.Name.print param
        Global_module.Name.print param
        Global_module.Name.print param
  | Imported_module_has_no_such_parameter
        { valid_parameters; imported = modname; parameter = param; value; } ->
      fprintf ppf
        "@[<hov>The module %a@ is given argument %a@ for parameter %a.@ "
        CU.Name.print modname
        Global_module.Name.print value
        Global_module.Name.print param;
      begin match valid_parameters with
      | [] ->
          fprintf ppf "%a has no parameters."
            CU.Name.print modname
      | _ ->
          let print_params =
            Format.pp_print_list ~pp_sep:Format.pp_print_space
              Global_module.Name.print
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
        Global_module.Name.print value
        Global_module.Name.print expected
        Location.print_filename filename
        Global_module.Name.print actual
  | Inconsistent_global_name_resolution
      { name; old_global; new_global; first_mentioned_by; now_mentioned_by } ->
      fprintf ppf
        "@[<hov>The name %a@ was bound to %a@ by %a@ \
         but it is instead bound to %a@ by %a.@]"
        Global_module.Name.print name
        Global_module.print old_global
        Global_module.Name.print first_mentioned_by
        Global_module.print new_global
        Global_module.Name.print now_mentioned_by
  | Unbound_module_as_argument_value { instance; value } ->
      fprintf ppf
        "@[<hov>Unbound module %a@ in instance %a@]"
        Global_module.Name.print value
        Global_module.Name.print instance

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
