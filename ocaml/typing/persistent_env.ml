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
module Consistbl = Consistbl.Make (CU.Name) (CU)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of CU.Name.t * CU.Name.t * filepath
  | Inconsistent_import of CU.Name.t * filepath * filepath
  | Need_recursive_types of CU.t
  | Depend_on_unsafe_string_unit of CU.t
  | Inconsistent_package_declaration of CU.t * filepath
  | Inconsistent_package_declaration_between_imports of
      filepath * CU.t * CU.t
  | Direct_reference_from_wrong_package of
      CU.t * filepath * CU.Prefix.t
  | Illegal_import_of_parameter of CU.t * filepath
  | Not_compiled_as_parameter of CU.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : CU.t;
        parameter : CU.t;
      }
  | Imported_module_has_no_such_parameter of
      { imported : CU.t;
        valid_parameters : CU.t list;
        parameter : CU.t;
        value : CU.t;
      }
  | Not_compiled_as_argument of CU.t * filepath
  | Argument_type_mismatch of
      { value : CU.t;
        filename : filepath;
        expected : CU.t;
        actual : CU.t;
      }

exception Error of error
let error err = raise (Error err)

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos }

  let load = ref (fun ~unit_name ->
      let unit_name = CU.Name.to_string unit_name in
      match Load_path.find_uncap (unit_name ^ ".cmi") with
      | filename -> Some { filename; cmi = read_cmi filename }
      | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

type pers_struct = {
  ps_name: CU.t;
  ps_is_param: bool;
  ps_global: Global.t;
  ps_arg_for: CU.t option;
  ps_local_ident: Ident.t option;
  ps_crcs: Import_info.t array;
  ps_filename: string;
  ps_flags: pers_flags list;
}

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type 'a pers_struct_info =
  | Missing
  | Found of pers_struct * 'a

type param_info = {
  (* Whether this parameter is visible to other compilation units. Always true
     in normal compilation runs, since only special [-instantiate] operations
     bind parameters internally. *)
  pi_exported: bool;
}

module Param_map = CU.Map

(* CR lmaurer: This probably belongs in [compilation_unit.ml] *)
(* CR lmaurer: Actually, it's just a [Global.Name.t] *)
module Instance_name = struct
  type t = CU.Name.t * (CU.t * CU.t) list

  let to_cu (name, args) =
    CU.create_instance (CU.create CU.Prefix.empty name) args

  include Identifiable.Make (struct
      type nonrec t = t

      let compare_args (param1, value1) (param2, value2) =
        match CU.compare param1 param2 with
        | 0 -> CU.compare value1 value2
        | c -> c

      let compare (name1, args1) (name2, args2) =
        match CU.Name.compare name1 name2 with
        | 0 -> List.compare compare_args args1 args2
        | c -> c

      let equal t1 t2 = compare t1 t2 = 0

      let hash_arg (param, value) =
        Hashtbl.hash (CU.hash param, CU.hash value)

      let hash (name, args) =
        Hashtbl.hash (CU.Name.hash name, List.map hash_arg args)

      (* [print] and [output] don't really need to be fast, so we're happy to go
         through [CU.t] *)

      let print ppf t = CU.print ppf (t |> to_cu)

      let output ppf t = CU.output ppf (t |> to_cu)
    end)
end

type 'a t = {
  persistent_structures : (Instance_name.t, 'a pers_struct_info) Hashtbl.t;
  imported_units: CU.Name.Set.t ref;
  imported_opaque_units: CU.Name.Set.t ref;
  registered_params: param_info Param_map.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  persistent_structures = Hashtbl.create 17;
  imported_units = ref CU.Name.Set.empty;
  imported_opaque_units = ref CU.Name.Set.empty;
  registered_params = ref Param_map.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let {
    persistent_structures;
    imported_units;
    imported_opaque_units;
    registered_params;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear persistent_structures;
  imported_units := CU.Name.Set.empty;
  imported_opaque_units := CU.Name.Set.empty;
  registered_params := Param_map.empty;
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

let register_parameter {registered_params; _} modname ~exported =
  let update old_info =
    (* It should be exported if at least one call to [register_parameter] said
       to export it *)
    match old_info with
    | None -> Some { pi_exported = exported }
    | Some { pi_exported } -> Some { pi_exported = pi_exported || exported }
  in
  registered_params := Param_map.update modname update !registered_params

(* CR lmaurer: This should probably take [Instance_name.t] *)
let find_info_in_cache {persistent_structures; _} s ~instance_args =
  match Hashtbl.find persistent_structures (s, instance_args) with
  | exception Not_found -> None
  | Missing -> None
  | Found (ps, pm) -> Some (ps, pm)

let find_in_cache penv s ~instance_args =
  find_info_in_cache penv s ~instance_args |> Option.map (fun (_ps, pm) -> pm)

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc import_info =
    let name = Import_info.name import_info in
    let crco = Import_info.crc_with_unit import_info in
    match crco with
    | None -> ()
    | Some (unit, crc) ->
        add_import penv name;
        Consistbl.check crc_units name unit crc source
  in Array.iter import_crc crcs

let check_consistency penv ps =
  try import_crcs penv ~source:ps.ps_filename ps.ps_crcs
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = source;
      original_source = auth;
      inconsistent_data = source_unit;
      original_data = auth_unit;
    } ->
    if CU.equal source_unit auth_unit
    then error (Inconsistent_import(name, auth, source))
    else error (Inconsistent_package_declaration_between_imports(
        ps.ps_filename, auth_unit, source_unit))

let is_registered_parameter {registered_params; _} name =
  Param_map.mem name !registered_params

let is_unexported_parameter {registered_params; _} name =
  match Param_map.find_opt name !registered_params with
  | Some { pi_exported } -> not pi_exported
  | None -> false

(* Enforce the "subset rule": if A refers to B, then B's parameters must be
   included in A's parameters. Note that if B is being given instance arguments,
   those parameters are exempt, since X does not occur free in B{M/X}. *)
let check_parameters penv modname ~params ~instance_args =
  Misc.Stdlib.List.merge_iter params instance_args
    ~cmp:(fun param (arg_name, _arg_value) -> CU.compare param arg_name)
    ~left_only:
      (fun param ->
        (* No argument for this parameter: subset rule applies *)
        if not (is_registered_parameter penv param)
        then error (Imported_module_has_unset_parameter
                      { imported = modname; parameter = param }))
    ~right_only:
      (fun (name, value) ->
        (* No parameter for this argument: definitely bad *)
        error (Imported_module_has_no_such_parameter
                 { imported = modname;
                   valid_parameters = params;
                   parameter = name;
                   value;
                 }))
    ~both:(fun _ _ ->
        (* Nothing to check here (previous run did typechecking) *)
        ())

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
  Hashtbl.fold (fun (modname, instance_args) pso x -> match pso with
      | Missing -> x
      | Found (_, pm) -> f modname ~instance_args pm x)
    persistent_structures x

(* Reading persistent structures from .cmi files *)

let save_pers_struct penv crc ps pm =
  let {persistent_structures; crc_units; _} = penv in
  let modname = CU.name ps.ps_name in
  let instance_args = CU.instance_arguments ps.ps_name in
  Hashtbl.add persistent_structures (modname, instance_args) (Found (ps, pm));
  List.iter
    (function
        | Rectypes -> ()
        | Alerts _ -> ()
        | Unsafe_string -> ()
        | Opaque -> register_import_as_opaque penv modname)
    ps.ps_flags;
  Consistbl.set crc_units modname ps.ps_name crc ps.ps_filename;
  add_import penv modname

let rec subset_args ~superset ~subset =
  match superset, subset with
  | _, [] -> true
  | [], _::_ -> false
  | (param_sup, value_sup) :: superset', (param_sub, value_sub) :: subset' ->
      match CU.compare param_sup param_sub with
      | 0 ->
          CU.equal value_sup value_sub
          && subset_args ~superset:superset' ~subset:subset'
      | c when c < 0 -> subset_args ~superset:superset' ~subset
      | _ -> false

let current_unit_is_instance_of cu =
  match CU.get_current () with
  | None -> false
  | Some current ->
      CU.is_instance current
      && CU.Name.equal (CU.name cu) (CU.name current)
      && subset_args
           ~superset:(CU.instance_arguments current)
           ~subset:(CU.instance_arguments cu)

let need_local_ident psig ~instance_args =
  (* There are three equivalent ways to phrase the question we're asking here:

     1. Is this either a parameter or an open import?
     2. Will the generated lambda code need a parameter to take this module's
          value?
     3. Is the value not statically bound?

     Crucially, all modules (besides the one being compiled or instantiated)
     must be either statically bound or toplevel parameters, since the actual
     functor calls that instantiate open modules happen elsewhere (so that they
     can happen exactly once). *)
  let { Persistent_signature.cmi =
          { cmi_name = name;
            cmi_is_param = is_param;
            cmi_params = params; _ }; _ } =
    psig
  in
  match params, instance_args with
  | _, _ when is_param ->
    (* Already a parameter *)
    true
  | [], _ ->
    (* Not a parameter and also not parameterised *)
    false
  | _, _ when CU.is_current name ->
    (* Not actually importing it in the sense of needing its value (we're
       building its value!) *)
    false
  | _, _ when current_unit_is_instance_of name ->
    (* We're instantiating the module, so (here and only here!) we're accessing
       its actual functor, which is a compile-time constant *)
    (* CR lmaurer: Relying on [current_unit_is_instance_of] here feels hacky
       when only a pretty specific call sequence gets here. *)
    false
  | _::_, [] ->
    (* It needs arguments and we're not providing it any *)
    true
  | _, _ when List.compare_lengths params instance_args > 0 ->
    (* Partial application *)
    true
  | _, _ ->
    (* Fully applied *)
    (* CR lmaurer: Not quite right! Some of the arguments may have parameters in
       them, meaning they're not actually statically determined after all *)
    false

let make_local_ident_if_needed (psig : Persistent_signature.t) ~instance_args =
  if need_local_ident psig ~instance_args then
    Some (Ident.create_local (CU.full_path_as_string psig.cmi.cmi_name))
  else
    None

type 'a sig_reader =
  Persistent_signature.t -> instance_args:(CU.t * CU.t) list ->
  global:Global.t ->
  local_ident:Ident.t option -> 'a

(* CR lmaurer: Probably want to use [Global.Name.t] in [Compilation_unit.t] to
   be rid of all this back-and-forth. *)

let rec compilation_unit_of_global_name Global.Name.{ head; args } =
  let head = head |> CU.Name.of_string |> CU.create CU.Prefix.empty in
  let args = args |> List.map compilation_units_of_global_name_pair in
  CU.create_instance head args
and compilation_units_of_global_name_pair (param, value) =
  compilation_unit_of_global_name param,
  compilation_unit_of_global_name value

let rec compilation_unit_of_global Global.{ head; args; params = _ } =
  let head = head |> CU.Name.of_string |> CU.create CU.Prefix.empty in
  let args = args |> List.map compilation_units_of_global_pair in
  CU.create_instance head args
and compilation_units_of_global_pair (param, value) =
  compilation_unit_of_global_name param,
  compilation_unit_of_global value

let rec global_name_of_compilation_unit cu : Global.Name.t =
  if CU.is_packed cu then
    Misc.fatal_errorf "Can't make global of packed compilation unit@ %a"
      CU.print cu;
  let head = CU.name_as_string cu in
  let args =
    List.map
      (fun (param, value) ->
         global_name_of_compilation_unit param,
         global_name_of_compilation_unit value)
      (CU.instance_arguments cu)
  in
  { head; args }

let rec acknowledge_pers_struct
      penv ~check modname pers_sig (val_of_pers_sig : _ sig_reader)
      ~instance_args =
  let { Persistent_signature.filename; cmi } = pers_sig in
  let name = cmi.cmi_name in
  let is_param = cmi.cmi_is_param in
  let params = cmi.cmi_params in
  let arg_for = cmi.cmi_arg_for in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let found_name = CU.name name in
  if not (CU.Name.equal modname found_name) then
    error (Illegal_renaming(modname, found_name, filename));
  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(name))
        | Unsafe_string ->
            if Config.safe_string then
              error (Depend_on_unsafe_string_unit(name));
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    flags;
  begin match CU.get_current () with
  | Some current_unit ->
      let access_allowed =
        CU.can_access_by_name name ~accessed_by:current_unit
      in
      if not access_allowed then
        let prefix = CU.for_pack_prefix current_unit in
        error (Direct_reference_from_wrong_package (name, filename, prefix));
  | None -> ()
  end;
  begin match is_param, is_registered_parameter penv name with
  | true, false ->
      error (Illegal_import_of_parameter(name, filename))
  | false, true ->
      error (Not_compiled_as_parameter(name, filename))
  | true, true
  | false, false -> ()
  end;
  check_parameters penv name ~params ~instance_args;
  let local_ident = make_local_ident_if_needed pers_sig ~instance_args in
  let global =
    compute_global penv val_of_pers_sig name ~params ~instance_args ~check:true
  in
  let ps = { ps_name = name;
             ps_is_param = is_param;
             ps_global = global;
             ps_arg_for = arg_for;
             ps_local_ident = local_ident;
             ps_crcs = crcs;
             ps_filename = filename;
             ps_flags = flags;
           } in
  if check then check_consistency penv ps;
  let {persistent_structures; _} = penv in
  let pm = val_of_pers_sig pers_sig ~instance_args ~local_ident ~global in
  Hashtbl.add persistent_structures (modname, instance_args) (Found (ps, pm));
  (ps, pm)

(* CR-someday lmaurer: This can be moved out of the recursive knot *)
and read_pers_struct penv val_of_pers_sig ~check modname filename
      ~instance_args =
  add_import penv modname;
  let cmi = read_cmi filename in
  let pers_sig = { Persistent_signature.filename; cmi } in
  acknowledge_pers_struct penv ~check modname pers_sig val_of_pers_sig
    ~instance_args

and find_pers_struct penv val_of_pers_sig ~check name ~instance_args =
  let {persistent_structures; _} = penv in
  if CU.Name.equal name CU.Name.predef_exn then raise Not_found;
  match Hashtbl.find persistent_structures (name, instance_args) with
  | Found (ps, pm) -> (ps, pm)
  | Missing -> raise Not_found
  | exception Not_found ->
    match can_load_cmis penv with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis ->
        (* If there are instance arguments, we can imagine trying to save some
           effort by caching the uninstantiated module and then just
           substituting rather than doing another load. This is unlikely to help
           much, however, since it will be rare even to run into _two_
           instantiations of the same module, much less enough to justify a
           more complex caching scheme. *)
        let psig =
          match !Persistent_signature.load ~unit_name:name with
          | Some psig -> psig
          | None ->
            Hashtbl.add persistent_structures (name, instance_args)
              Missing;
            raise Not_found
        in
        add_import penv name;
        acknowledge_pers_struct penv ~check name psig val_of_pers_sig
          ~instance_args

(* CR-someday lmaurer: Should be storing [val_of_pers_sig] as part of the
   environment, since it's always the same (in fact, it would be very bad if it
   changed!) and it's getting awkward to take it as a separate argument
   everywhere. *)
and compute_global penv val_of_pers_sig modname ~params ~instance_args ~check =
  if CU.is_packed modname then
    (* CR lmaurer: Yuck. *)
    (* Just make something up; this won't be used anyway *)
    Global.{ head = CU.name_as_string modname; args = []; params = [] }
  else if CU.is_instance modname then
    (* This just came from a .cmi, which should never be an instance *)
    Misc.fatal_errorf "Expected non-instance modname, got %a" CU.print modname
  else
    let global_arg_of_param param =
      (* This feels like a hacky place to do this. Also, it occurs to me there's no
         check that this module _hasn't_ already been used as a _non_-parameter. *)
      register_parameter penv param ~exported:false;
      let param = global_name_of_compilation_unit param in
      let param_as_value = global_of_global_name penv val_of_pers_sig param in
      param, param_as_value
    in
    let global_of_arg (param, value) =
      global_name_of_compilation_unit param,
      global_of_compilation_unit penv val_of_pers_sig value
    in
    let modname_string = modname |> CU.name_as_string in
    let param_globals = params |> List.map global_arg_of_param in
    let global_without_args : Global.t =
      { head = modname_string; args = []; params = param_globals }
    in
    let instance_arg_globals = instance_args |> List.map global_of_arg in
    let subst : Global.subst = Global.Name.Map.of_list instance_arg_globals in
    if check then begin
      (* Produce the expected type of each argument. This takes into account
         substitutions among the parameter types: if the parameters are T and
         To_string[T] and the arguments are [Int] and [Int_to_string], we want to
         check that [Int] has type [T] and that [Int_to_string] has type
         [To_string[T\Int]]. *)
      let expected_types =
        List.map (fun (_param_name, param) -> Global.subst_inside param subst)
          param_globals
      in
      let param_unit_to_expected_type = List.combine params expected_types in
      let compare_by_param (param1, _) (param2, _) = CU.compare param1 param2 in
      Misc.Stdlib.List.merge_iter
        ~cmp:compare_by_param
        param_unit_to_expected_type
        instance_args
        ~left_only:
          (fun _ ->
             (* Parameter with no argument: fine *)
             ())
        ~right_only:
          (fun (param, value) ->
             (* Argument with no parameter: not fine *)
             raise
               (Error (Imported_module_has_no_such_parameter {
                         imported = modname;
                         valid_parameters = params;
                         parameter = param;
                         value;
                       })))
        ~both:
          (fun (_param_unit, expected_type) (_param, arg_unit) ->
             let name = CU.name arg_unit in
             let instance_args = CU.instance_arguments arg_unit in
             let ps, _pm =
               find_pers_struct penv val_of_pers_sig ~check:true name ~instance_args
             in
             let actual_type_unit =
               match ps.ps_arg_for with
               | None ->
                   raise (Error
                           (Not_compiled_as_argument (arg_unit, ps.ps_filename)))
               | Some param_type_unit ->
                   param_type_unit
             in
             let actual_type =
               global_of_compilation_unit penv val_of_pers_sig actual_type_unit
             in
             if not (Global.equal expected_type actual_type) then begin
               let expected_type_unit = compilation_unit_of_global expected_type in
               if CU.equal expected_type_unit actual_type_unit then
                 (* This shouldn't happen, I don't think, but if it does, I'd rather
                   not output an "X != X" sort of error message *)
                 Misc.fatal_errorf
                   "Mismatched argument type (despite same compilation unit):@ \
                   expected %a,@ got %a"
                   Global.print expected_type
                   Global.print actual_type
               else
                 raise (Error (Argument_type_mismatch {
                     value = arg_unit;
                     filename = ps.ps_filename;
                     expected = expected_type_unit;
                     actual = actual_type_unit;
                   }))
             end)
  end;
  Global.subst global_without_args subst

and global_of_global_name penv val_of_pers_sig
    (Global.Name.{ head; args }) : Global.t =
  let unit_name = head |> CU.Name.of_string in
  let instance_args = args |> List.map compilation_units_of_global_name_pair in
  let ps, _pm =
    find_pers_struct penv val_of_pers_sig ~check:true unit_name ~instance_args
  in
  ps.ps_global

and global_of_compilation_unit penv val_of_pers_sig cu =
  cu
  |> global_name_of_compilation_unit
  |> global_of_global_name penv val_of_pers_sig

let describe_prefix ppf prefix =
  if CU.Prefix.is_empty prefix then
    Format.fprintf ppf "outside of any package"
  else
    Format.fprintf ppf "package %a" CU.Prefix.print prefix

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct penv f ~loc name =
  let name_as_string = CU.Name.to_string name in
  try
    ignore (find_pers_struct penv f ~check:false name ~instance_args:[])
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
              CU.print name
        | Depend_on_unsafe_string_unit name ->
            Format.asprintf "%a uses -unsafe-string"
              CU.print name
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
      in
      let warn = Warnings.No_cmi_file(name_as_string, Some msg) in
        Location.prerr_warning loc warn

let read penv f modname ~instance_args filename =
  snd (read_pers_struct penv f ~check:true modname filename ~instance_args)

let find penv f name ~instance_args =
  snd (find_pers_struct penv f ~check:true name ~instance_args)

let check penv f ~loc name ~instance_args =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures (name, instance_args)) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import penv name;
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
  let (ps, _pm) =
    (* This is a bit wasteful for instances since we'll end up loading the file
       twice, but probably not enough to be a problem. *)
    find_pers_struct penv f ~check:true name ~instance_args:[]
  in
  match Array.find_opt (Import_info.has_name ~name) ps.ps_crcs with
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
  List.map (fun (cu_name, crc_with_unit) ->
      let instance_arguments =
        (* These are interface imports - no arguments *)
        []
      in
      Import_info.create cu_name ~crc_with_unit ~instance_arguments)
    imports

let locally_bound_imports ({persistent_structures; _} as penv) =
  persistent_structures
  |> Hashtbl.to_seq_keys
  |> Seq.filter_map
       (fun ((name, instance_args) as instance_name) ->
          let cu = instance_name |> Instance_name.to_cu in
          (* CR lmaurer: This should probably take [Instance_name.t]? *)
          if is_unexported_parameter penv cu then None
          else
            match find_info_in_cache penv name ~instance_args with
              | Some ({ ps_local_ident = Some local_ident; _ }, _) ->
                  Some (cu, local_ident)
              | Some ({ ps_local_ident = None; _ }, _)
              | None -> None)
  |> List.of_seq

let exported_parameters {registered_params; _} =
  Param_map.bindings !registered_params
  |> List.filter_map
       (fun (param, { pi_exported }) -> if pi_exported then Some param else None)

let looked_up {persistent_structures; _} modname ~instance_args =
  Hashtbl.mem persistent_structures (modname, instance_args)

let is_imported_opaque {imported_opaque_units; _} s =
  CU.Name.Set.mem s !imported_opaque_units

let is_parameter_unit {persistent_structures; _} s =
  match Hashtbl.find persistent_structures (s, []) with
  | exception Not_found -> false
  | Missing -> false
  | Found (ps, _) -> ps.ps_is_param

let find_info_in_cache_non_packed penv modname =
  if CU.is_packed modname then None
  else
    let name = CU.name modname in
    let instance_args = CU.instance_arguments modname in
    find_info_in_cache penv name ~instance_args

let local_ident penv modname =
  match find_info_in_cache_non_packed penv modname with
  | Some ({ ps_local_ident; _ }, _) -> ps_local_ident
  | None -> None

let implemented_parameter penv modname =
  match find_info_in_cache_non_packed penv modname with
  | Some ({ ps_arg_for; _ }, _) -> ps_arg_for
  | None -> None

let make_cmi penv modname sign secondary_sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      (if !Clflags.unsafe_string then [Cmi_format.Unsafe_string] else []);
      [Alerts alerts];
    ]
  in
  let is_param = !Clflags.as_parameter in
  let params =
    (* Needs to be consistent with [Translmod] *)
    exported_parameters penv
  in
  let arg_for =
    !Clflags.as_argument_for |> Option.map Compilation_unit.of_string
  in
  let crcs = imports penv in
  {
    cmi_name = modname;
    cmi_sign = sign;
    cmi_secondary_sign = secondary_sign;
    cmi_is_param = is_param;
    cmi_params = params;
    cmi_arg_for = arg_for;
    cmi_crcs = Array.of_list crcs;
    cmi_flags = flags
  }

let save_cmi penv val_of_pers_sig psig pm =
  let { Persistent_signature.filename; cmi } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_sign = _;
        cmi_is_param = is_param;
        cmi_params = params;
        cmi_arg_for = arg_for;
        cmi_crcs = imports;
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in persistent table so that imports()
         will also return its crc *)
      let local_ident =
        (* CR lmaurer: I don't love this. We're counting on the fact that the
           only module we're likely to save is the one we're compiling, which
           of course can't be locally bound, but that's a hidden invariant. *)
        None
      in
      let global : Global.t =
        compute_global penv val_of_pers_sig modname ~params ~instance_args:[]
          ~check:false
      in
      let ps =
        { ps_name = modname;
          ps_is_param = is_param;
          ps_global = global;
          ps_arg_for = arg_for;
          ps_local_ident = local_ident;
          ps_crcs =
            Array.append
              [| Import_info.create_normal cmi.cmi_name ~crc:(Some crc) |]
              imports;
          ps_filename = filename;
          ps_flags = flags;
        } in
      save_pers_struct penv crc ps pm
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
        CU.print import
        "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %a, compiled with -unsafe-string.@ %s@]"
        CU.print import
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
        "@[<hov>The file %a@ contains the an interface of a parameter.@ \
         %a is not declared as a parameter for the current unit (-parameter %a).@]"
        Location.print_filename filename
        Compilation_unit.print modname
        Compilation_unit.print modname
  | Not_compiled_as_parameter(modname, filename) ->
      fprintf ppf
        "@[<hov>The module %a@ is specified as a parameter, but %a@ \
         was not compiled with -as-parameter.@]"
        Compilation_unit.print modname
        Location.print_filename filename
  | Imported_module_has_unset_parameter
        { imported = modname; parameter = param } ->
      fprintf ppf
        "@[<hov>The module %a@ has parameter %a.@ \
         %a is not declared as a parameter for the current unit (-parameter %a)@ \
         and therefore %a@ is not accessible.@]"
        CU.print modname
        CU.print param
        CU.print param
        CU.print param
        CU.print modname
  | Imported_module_has_no_such_parameter
        { valid_parameters; imported = modname; parameter = param; value; } ->
      fprintf ppf
        "@[<hov>The module %a@ is given argument %a@ for parameter %a.@ "
        CU.print modname
        CU.print value
        CU.print param;
      begin match valid_parameters with
      | [] ->
          fprintf ppf "%a has no parameters."
            CU.print modname
      | _ ->
          let print_params =
            Format.pp_print_list ~pp_sep:Format.pp_print_space CU.print
          in
          fprintf ppf "Valid parameters for %a:@ @[<hov>%a@]"
            CU.print modname
            print_params valid_parameters
      end;
      fprintf ppf "@]"
  | Not_compiled_as_argument(modname, filename) ->
      fprintf ppf
        "@[<hov>The module %a@ is specified as an instance argument, but %a@ \
         was not compiled with -as-argument-for.@]"
        CU.print modname
        Location.print_filename filename
  | Argument_type_mismatch { value; filename; expected; actual; } ->
      fprintf ppf
        "@[<hov>The module %a@ was expected to satisfy the parameter %a@ \
         but %a@ was compiled with -as-argument-for %a.@]"
        CU.print value
        CU.print expected
        Location.print_filename filename
        CU.print actual

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
