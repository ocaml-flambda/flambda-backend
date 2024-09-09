(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Environment handling *)

open Cmi_format
open Misc
open Asttypes
open Longident
open Path
open Types

open Local_store

module String = Misc.Stdlib.String

let add_delayed_check_forward = ref (fun _ -> assert false)

type 'a usage_tbl = ('a -> unit) Types.Uid.Tbl.t
(** This table is used to track usage of value declarations.
    A declaration is identified by its uid.
    The callback attached to a declaration is called whenever the value (or
    type, or ...) is used explicitly (lookup_value, ...) or implicitly
    (inclusion test between signatures, cf Includemod.value_descriptions, ...).
*)

let value_declarations  : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16
let type_declarations   : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16
let module_declarations : unit usage_tbl ref = s_table Types.Uid.Tbl.create 16

type constructor_usage = Positive | Pattern | Exported_private | Exported
type constructor_usages =
  {
    mutable cu_positive: bool;
    mutable cu_pattern: bool;
    mutable cu_exported_private: bool;
  }
let add_constructor_usage cu usage =
  match usage with
  | Positive -> cu.cu_positive <- true
  | Pattern -> cu.cu_pattern <- true
  | Exported_private -> cu.cu_exported_private <- true
  | Exported ->
    cu.cu_positive <- true;
    cu.cu_pattern <- true;
    cu.cu_exported_private <- true

let constructor_usages () =
  {cu_positive = false; cu_pattern = false; cu_exported_private = false}

let constructor_usage_complaint ~rebind priv cu
  : Warnings.constructor_usage_warning option =
  match priv, rebind with
  | Asttypes.Private, _ | _, true ->
      if cu.cu_positive || cu.cu_pattern || cu.cu_exported_private then None
      else Some Unused
  | Asttypes.Public, false -> begin
      match cu.cu_positive, cu.cu_pattern, cu.cu_exported_private with
      | true, _, _ -> None
      | false, false, false -> Some Unused
      | false, true, _ -> Some Not_constructed
      | false, false, true -> Some Only_exported_private
    end

let used_constructors : constructor_usage usage_tbl ref =
  s_table Types.Uid.Tbl.create 16

type label_usage =
    Projection | Mutation | Construct | Exported_private | Exported
type label_usages =
    {
     mutable lu_projection: bool;
     mutable lu_mutation: bool;
     mutable lu_construct: bool;
    }
let add_label_usage lu usage =
  match usage with
  | Projection -> lu.lu_projection <- true;
  | Mutation -> lu.lu_mutation <- true
  | Construct -> lu.lu_construct <- true
  | Exported_private ->
    lu.lu_projection <- true
  | Exported ->
    lu.lu_projection <- true;
    lu.lu_mutation <- true;
    lu.lu_construct <- true

let is_mutating_label_usage = function
  | Mutation -> true
  | (Projection | Construct | Exported_private | Exported) -> false

let label_usages () =
  {lu_projection = false; lu_mutation = false; lu_construct = false}

let label_usage_complaint priv mut lu
  : Warnings.field_usage_warning option =
  match priv, mut with
  | Asttypes.Private, _ ->
      if lu.lu_projection then None
      else Some Unused
  | Asttypes.Public, Types.Immutable -> begin
      match lu.lu_projection, lu.lu_construct with
      | true, _ -> None
      | false, false -> Some Unused
      | false, true -> Some Not_read
    end
  | Asttypes.Public, Types.Mutable _ -> begin
      match lu.lu_projection, lu.lu_mutation, lu.lu_construct with
      | true, true, _ -> None
      | false, false, false -> Some Unused
      | false, _, _ -> Some Not_read
      | true, false, _ -> Some Not_mutated
    end

let used_labels : label_usage usage_tbl ref =
  s_table Types.Uid.Tbl.create 16

(** Map indexed by the name of module components. *)
module NameMap = String.Map

type value_unbound_reason =
  | Val_unbound_instance_variable
  | Val_unbound_self
  | Val_unbound_ancestor
  | Val_unbound_ghost_recursive of Location.t

type module_unbound_reason =
  | Mod_unbound_illegal_recursion

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description * Mode.Value.l
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_presence * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration Path.Map.t
  | Env_copy_types of summary
  | Env_persistent of summary * Ident.t
  | Env_value_unbound of summary * string * value_unbound_reason
  | Env_module_unbound of summary * string * module_unbound_reason

let map_summary f = function
    Env_empty -> Env_empty
  | Env_value (s, id, d, m) -> Env_value (f s, id, d, m)
  | Env_type (s, id, d) -> Env_type (f s, id, d)
  | Env_extension (s, id, d) -> Env_extension (f s, id, d)
  | Env_module (s, id, p, d) -> Env_module (f s, id, p, d)
  | Env_modtype (s, id, d) -> Env_modtype (f s, id, d)
  | Env_class (s, id, d) -> Env_class (f s, id, d)
  | Env_cltype (s, id, d) -> Env_cltype (f s, id, d)
  | Env_open (s, p) -> Env_open (f s, p)
  | Env_functor_arg (s, id) -> Env_functor_arg (f s, id)
  | Env_constraints (s, m) -> Env_constraints (f s, m)
  | Env_copy_types s -> Env_copy_types (f s)
  | Env_persistent (s, id) -> Env_persistent (f s, id)
  | Env_value_unbound (s, u, r) -> Env_value_unbound (f s, u, r)
  | Env_module_unbound (s, u, r) -> Env_module_unbound (f s, u, r)

type address = Persistent_env.address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

module TycompTbl =
  struct
    (** This module is used to store components of types (i.e. labels
        and constructors).  We keep a representation of each nested
        "open" and the set of local bindings between each of them. *)

    type 'a t = {
      current: 'a Ident.tbl;
      (** Local bindings since the last open. *)

      opened: 'a opened option;
      (** Symbolic representation of the last (innermost) open, if any. *)
    }

    and 'a opened = {
      components: ('a list) NameMap.t;
      (** Components from the opened module. We keep a list of
          bindings for each name, as in comp_labels and
          comp_constrs. *)

      root: Path.t;
      (** Only used to check removal of open *)

      using: (string -> ('a * 'a) option -> unit) option;
      (** A callback to be applied when a component is used from this
          "open".  This is used to detect unused "opens".  The
          arguments are used to detect shadowing. *)

      next: 'a t;
      (** The table before opening the module. *)
    }

    let empty = { current = Ident.empty; opened = None }

    let add id x tbl =
      {tbl with current = Ident.add id x tbl.current}

    let add_open slot wrap root components next =
      let using =
        match slot with
        | None -> None
        | Some f -> Some (fun s x -> f s (wrap x))
      in
      {
        current = Ident.empty;
        opened = Some {using; components; root; next};
      }

    let remove_last_open rt tbl =
      match tbl.opened with
      | Some {root; next; _} when Path.same rt root ->
          { next with current =
            Ident.fold_all Ident.add tbl.current next.current }
      | _ ->
          assert false

    let rec find_same id tbl =
      try Ident.find_same id tbl.current
      with Not_found as exn ->
        begin match tbl.opened with
        | Some {next; _} -> find_same id next
        | None -> raise exn
        end

    let nothing = fun () -> ()

    let mk_callback rest name desc using =
      match using with
      | None -> nothing
      | Some f ->
          (fun () ->
             match rest with
             | [] -> f name None
             | (hidden, _) :: _ -> f name (Some (desc, hidden)))

    let rec find_all ~mark name tbl =
      List.map (fun (_id, desc) -> desc, nothing)
        (Ident.find_all name tbl.current) @
      match tbl.opened with
      | None -> []
      | Some {using; next; components; root = _} ->
          let rest = find_all ~mark name next in
          let using = if mark then using else None in
          match NameMap.find name components with
          | exception Not_found -> rest
          | opened ->
              List.map
                (fun desc -> desc, mk_callback rest name desc using)
                opened
              @ rest

    let rec fold_name f tbl acc =
      let acc = Ident.fold_name (fun _id d -> f d) tbl.current acc in
      match tbl.opened with
      | Some {using = _; next; components; root = _} ->
          acc
          |> NameMap.fold
            (fun _name -> List.fold_right f)
            components
          |> fold_name f next
      | None ->
          acc

    let rec local_keys tbl acc =
      let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
      match tbl.opened with
      | Some o -> local_keys o.next acc
      | None -> acc

    let diff_keys is_local tbl1 tbl2 =
      let keys2 = local_keys tbl2 [] in
      List.filter
        (fun id ->
           is_local (find_same id tbl2) &&
           try ignore (find_same id tbl1); false
           with Not_found -> true)
        keys2

  end


type empty = |

type closure_context =
  | Tailcall_function
  | Tailcall_argument
  | Partial_application
  | Return

type escaping_context =
  | Letop
  | Probe
  | Class
  | Module
  | Lazy

type shared_context =
  | For_loop
  | While_loop
  | Letop
  | Closure
  | Comprehension
  | Class
  | Module
  | Probe
  | Lazy

type lock =
  | Escape_lock of escaping_context
  | Share_lock of shared_context
  | Closure_lock of closure_context option * Mode.Value.Comonadic.r
  | Region_lock
  | Exclave_lock
  | Unboxed_lock (* to prevent capture of terms with non-value types *)

type lock_item =
  | Value
  | Module
  | Class

module IdTbl =
  struct
    (** This module is used to store all kinds of components except
        (labels and constructors) in environments.  We keep a
        representation of each nested "open" and the set of local
        bindings between each of them. *)


    type ('lock, 'a, 'b) t = {
      current: 'a Ident.tbl;
      (** Local bindings since the last open or lock *)

      layer: ('lock, 'a, 'b) layer;
      (** Symbolic representation of the last (innermost) open, if any. *)
    }

    and ('lock, 'a, 'b) layer =
      | Open of {
          root: Path.t;
          (** The path of the opened module, to be prefixed in front of
              its local names to produce a valid path in the current
              environment. *)

          components: 'b NameMap.t;
          (** Components from the opened module. *)

          using: (string -> ('a * 'a) option -> unit) option;
          (** A callback to be applied when a component is used from this
              "open".  This is used to detect unused "opens".  The
              arguments are used to detect shadowing. *)

          next: ('lock, 'a, 'b) t;
          (** The table before opening the module. *)
        }

      | Map of {
          f: ('a -> 'a);
          next: ('lock, 'a, 'b) t;
        }

      | Lock of {
          lock: 'lock;
          next: ('lock, 'a, 'b) t;
        }

      | Nothing

    let empty = { current = Ident.empty; layer = Nothing }

    let add id x tbl =
      {tbl with current = Ident.add id x tbl.current}

    let remove id tbl =
      {tbl with current = Ident.remove id tbl.current}

    let add_open slot wrap root components next =
      let using =
        match slot with
        | None -> None
        | Some f -> Some (fun s x -> f s (wrap x))
      in
      {
        current = Ident.empty;
        layer = Open {using; root; components; next};
      }

    let remove_last_open rt tbl =
      match tbl.layer with
      | Open {root; next; _} when Path.same rt root ->
          { next with current =
            Ident.fold_all Ident.add tbl.current next.current }
      | _ ->
          assert false

    let add_lock lock next =
      { current = Ident.empty; layer = Lock {lock; next} }

    let map f next =
      {
        current = Ident.empty;
        layer = Map {f; next}
      }

    let rec find_same_without_locks id tbl =
      try Ident.find_same id tbl.current
      with Not_found as exn ->
        begin match tbl.layer with
        | Open {next; _} -> find_same_without_locks id next
        | Map {f; next} -> f (find_same_without_locks id next)
        | Lock {lock=_; next} -> find_same_without_locks id next
        | Nothing -> raise exn
        end

    let find_same id (tbl : (empty, _, _) t) =
      find_same_without_locks id tbl

    let rec find_same_and_locks id tbl macc =
      try
        let desc = Ident.find_same id tbl.current in
        desc, macc
      with Not_found as exn ->
        begin match tbl.layer with
        | Open {next; _} -> find_same_and_locks id next macc
        | Map {f; next} ->
          let desc, locks = find_same_and_locks id next macc in
          f desc, locks
        | Lock {lock; next} -> find_same_and_locks id next (lock :: macc)
        | Nothing -> raise exn
        end

    let find_same_and_locks id tbl = find_same_and_locks id tbl []

    let rec find_name_and_locks wrap ~mark name tbl macc : _ Result.t =
      try
        let (id, desc) = Ident.find_name name tbl.current in
        Ok (Pident id, macc, desc)
      with Not_found ->
        begin match tbl.layer with
        | Open {using; root; next; components} ->
            begin try
              let descr = wrap (NameMap.find name components) in
              let res = Pdot (root, name), macc, descr in
              if mark then begin match using with
              | None -> ()
              | Some f -> begin match
                  (find_name_and_locks wrap ~mark:false name next macc : _ Result.t)
                  with
                  | Error _ -> f name None
                  | Ok (_, _, descr') -> f name (Some (descr', descr))
                end
              end;
              Ok res
            with Not_found ->
              find_name_and_locks wrap ~mark name next macc
            end
        | Map {f; next} ->
            find_name_and_locks wrap ~mark name next macc
            |> Result.map (fun (p, macc, desc) -> p, macc, f desc)
        | Lock {lock; next} ->
            find_name_and_locks wrap ~mark name next (lock :: macc)
        | Nothing ->
            Error macc
        end

    let find_name_and_locks wrap ~mark name tbl =
      find_name_and_locks wrap ~mark name tbl []

    (** Find item by name whose accesses are not affected by locks, and thus
        shouldn't encounter any locks. *)
    let find_name wrap ~mark name tbl =
      match (find_name_and_locks wrap ~mark name tbl
        : (_ * empty list * _, empty list) Result.t) with
      | Ok (id, [], desc) -> id, desc
      | Ok (_, _ :: _, _) -> .
      | Error [] -> raise Not_found
      | Error (_ :: _) -> .

    let rec find_all wrap name tbl =
      List.map
        (fun (id, desc) -> Pident id, desc)
        (Ident.find_all name tbl.current) @
      match tbl.layer with
      | Nothing -> []
      | Open {root; using = _; next; components} ->
          begin try
            let desc = wrap (NameMap.find name components) in
            (Pdot (root, name), desc) :: find_all wrap name next
          with Not_found ->
            find_all wrap name next
          end
      | Map {f; next} ->
          List.map (fun (p, desc) -> (p, f desc))
            (find_all wrap name next)
      | Lock {lock=_;next} ->
          find_all wrap name next

    let rec find_all_idents name tbl () =
      let current =
        Ident.find_all_seq name tbl.current
        |> Seq.map (fun (id, _) -> Some id)
      in
      let next () =
        match tbl.layer with
        | Nothing -> Seq.Nil
        | Open { next; components; _ } ->
            if NameMap.mem name components then
              Seq.Cons(None, find_all_idents name next)
            else
              find_all_idents name next ()
        | Map {next; _ } -> find_all_idents name next ()
        | Lock {lock=_;next} ->
            find_all_idents name next ()
      in
      Seq.append current next ()

    let rec fold_name wrap f tbl acc =
      let acc =
        Ident.fold_name
          (fun id d -> f (Ident.name id) (Pident id, d))
          tbl.current acc
      in
      match tbl.layer with
      | Open {root; using = _; next; components} ->
          acc
          |> NameMap.fold
            (fun name desc -> f name (Pdot (root, name), wrap desc))
            components
          |> fold_name wrap f next
      | Nothing ->
          acc
      | Map {f=g; next} ->
          acc
          |> fold_name wrap
               (fun name (path, desc) -> f name (path, g desc))
               next
      | Lock {lock=_; next} ->
          fold_name wrap f next acc

    let rec local_keys tbl acc =
      let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
      match tbl.layer with
      | Open {next; _ } | Map {next; _} | Lock {next; _} -> local_keys next acc
      | Nothing -> acc


    let rec iter wrap f tbl =
      Ident.iter (fun id desc -> f id (Pident id, desc)) tbl.current;
      match tbl.layer with
      | Open {root; using = _; next; components} ->
          NameMap.iter
            (fun s x ->
               let root_scope = Path.scope root in
              f (Ident.create_scoped ~scope:root_scope s)
                (Pdot (root, s), wrap x))
            components;
          iter wrap f next
      | Map {f=g; next} ->
          iter wrap (fun id (path, desc) -> f id (path, g desc)) next
      | Lock {lock=_; next} ->
          iter wrap f next
      | Nothing -> ()

    let diff_keys tbl1 tbl2 =
      let keys2 = local_keys tbl2 [] in
      List.filter
        (fun id ->
           try ignore (find_same_without_locks id tbl1); false
           with Not_found -> true)
        keys2


  end

type type_descr_kind =
  (label_description, constructor_description) type_kind

type type_descriptions = type_descr_kind

let in_signature_flag = 0x01

type t = {
  values: (lock, value_entry, value_data) IdTbl.t;
  constrs: constructor_data TycompTbl.t;
  labels: label_data TycompTbl.t;
  types: (empty, type_data, type_data) IdTbl.t;
  modules: (lock, module_entry, module_data) IdTbl.t;
  modtypes: (empty, modtype_data, modtype_data) IdTbl.t;
  classes: (lock, class_data, class_data) IdTbl.t;
  cltypes: (empty, cltype_data, cltype_data) IdTbl.t;
  functor_args: unit Ident.tbl;
  summary: summary;
  local_constraints: type_declaration Path.Map.t;
  flags: int;
}

and module_components =
  {
    alerts: alerts;
    uid: Uid.t;
    comps:
      (components_maker,
       (module_components_repr, module_components_failure) result)
        Lazy_backtrack.t;
  }

and components_maker = {
  cm_env: t;
  cm_prefixing_subst: Subst.t;
  cm_path: Path.t;
  cm_addr: address_lazy;
  cm_mty: Subst.Lazy.module_type;
  cm_shape: Shape.t;
}

and module_components_repr =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and module_components_failure =
  | No_components_abstract
  | No_components_alias of Path.t

and structure_components = {
  mutable comp_values: value_data NameMap.t;
  mutable comp_constrs: constructor_data list NameMap.t;
  mutable comp_labels: label_data list NameMap.t;
  mutable comp_types: type_data NameMap.t;
  mutable comp_modules: module_data NameMap.t;
  mutable comp_modtypes: modtype_data NameMap.t;
  mutable comp_classes: class_data NameMap.t;
  mutable comp_cltypes: cltype_data NameMap.t;
}

and functor_components = {
  fcomp_arg: functor_parameter;
  (* Formal parameter and argument signature *)
  fcomp_res: module_type;               (* Result signature *)
  fcomp_shape: Shape.t;
  fcomp_cache: (Path.t, module_components) Hashtbl.t;  (* For memoization *)
  fcomp_subst_cache: (Path.t, module_type) Hashtbl.t
}

and address_unforced =
  | Projection of { parent : address_lazy; pos : int; }
  | ModAlias of { env : t; path : Path.t; }

and address_lazy = (address_unforced, address) Lazy_backtrack.t

and value_data =
  { vda_description : Subst.Lazy.value_description;
    vda_address : address_lazy;
    vda_mode : Mode.Value.l;
    vda_shape : Shape.t }

and value_entry =
  | Val_bound of value_data
  | Val_unbound of value_unbound_reason

and constructor_data =
  { cda_description : constructor_description;
    cda_address : address_lazy option;
    cda_shape: Shape.t; }

and label_data = label_description

and type_data =
  { tda_declaration : type_declaration;
    tda_descriptions : type_descriptions;
    tda_shape : Shape.t; }

and module_data =
  { mda_declaration : Subst.Lazy.module_declaration;
    mda_components : module_components;
    mda_address : address_lazy;
    mda_shape: Shape.t; }

and module_entry =
  | Mod_local of module_data
  | Mod_persistent
  | Mod_unbound of module_unbound_reason

and modtype_data =
  { mtda_declaration : Subst.Lazy.modtype_declaration;
    mtda_shape : Shape.t; }

and class_data =
  { clda_declaration : class_declaration;
    clda_address : address_lazy;
    clda_shape : Shape.t }

and cltype_data =
  { cltda_declaration : class_type_declaration;
    cltda_shape : Shape.t }

let mda_mode = Mode.Value.legacy |> Mode.Value.disallow_right

let clda_mode = Mode.Value.legacy |> Mode.Value.disallow_right

let cm_mode = Mode.Value.legacy |> Mode.Value.disallow_right

let empty_structure =
  Structure_comps {
    comp_values = NameMap.empty;
    comp_constrs = NameMap.empty;
    comp_labels = NameMap.empty;
    comp_types = NameMap.empty;
    comp_modules = NameMap.empty; comp_modtypes = NameMap.empty;
    comp_classes = NameMap.empty;
    comp_cltypes = NameMap.empty }

type unbound_value_hint =
  | No_hint
  | Missing_rec of Location.t

type lookup_error =
  | Unbound_value of Longident.t * unbound_value_hint
  | Unbound_type of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of Longident.t
  | Unbound_class of Longident.t
  | Unbound_modtype of Longident.t
  | Unbound_cltype of Longident.t
  | Unbound_instance_variable of string
  | Not_an_instance_variable of string
  | Masked_instance_variable of Longident.t
  | Masked_self_variable of Longident.t
  | Masked_ancestor_variable of Longident.t
  | Structure_used_as_functor of Longident.t
  | Abstract_used_as_functor of Longident.t
  | Functor_used_as_structure of Longident.t
  | Abstract_used_as_structure of Longident.t
  | Generative_used_as_applicative of Longident.t
  | Illegal_reference_to_recursive_module
  | Cannot_scrape_alias of Longident.t * Path.t
  | Local_value_escaping of lock_item * Longident.t * escaping_context
  | Once_value_used_in of lock_item * Longident.t * shared_context
  | Value_used_in_closure of lock_item * Longident.t *
      Mode.Value.Comonadic.error * closure_context option
  | Local_value_used_in_exclave of lock_item * Longident.t
  | Non_value_used_in_object of Longident.t * type_expr * Jkind.Violation.t

type error =
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string
  | Lookup_error of Location.t * t * lookup_error

exception Error of error

let error err = raise (Error err)

let lookup_error loc env err =
  error (Lookup_error(loc, env, err))

type actual_mode = {
  mode : Mode.Value.l;
  context : shared_context option
}

let mode_default mode = {
  mode;
  context = None
}

let same_type_declarations e1 e2 =
  e1.types == e2.types &&
  e1.modules == e2.modules &&
  e1.local_constraints == e2.local_constraints

let same_constr = ref (fun _ _ _ -> assert false)

let constrain_type_jkind = ref (fun _ _ _ -> assert false)

let check_well_formed_module = ref (fun _ -> assert false)

(* Helper to decide whether to report an identifier shadowing
   by some 'open'. For labels and constructors, we do not report
   if the two elements are from the same re-exported declaration.

   Later, one could also interpret some attributes on value and
   type declarations to silence the shadowing warnings. *)

let check_shadowing env = function
  | `Constructor (Some (cda1, cda2))
    when not (!same_constr env
                cda1.cda_description.cstr_res
                cda2.cda_description.cstr_res) ->
      Some "constructor"
  | `Label (Some (l1, l2))
    when not (!same_constr env l1.lbl_res l2.lbl_res) ->
      Some "label"
  | `Value (Some (Val_unbound _, _)) -> None
  | `Value (Some (_, _)) -> Some "value"
  | `Type (Some _) -> Some "type"
  | `Module (Some (Mod_unbound _, _)) -> None
  | `Module (Some _) | `Component (Some _) ->
      Some "module"
  | `Module_type (Some _) -> Some "module type"
  | `Class (Some _) -> Some "class"
  | `Class_type (Some _) -> Some "class type"
  | `Constructor _ | `Label _
  | `Value None | `Type None | `Module None | `Module_type None
  | `Class None | `Class_type None | `Component None ->
      None

let empty = {
  values = IdTbl.empty; constrs = TycompTbl.empty;
  labels = TycompTbl.empty; types = IdTbl.empty;
  modules = IdTbl.empty; modtypes = IdTbl.empty;
  classes = IdTbl.empty; cltypes = IdTbl.empty;
  summary = Env_empty; local_constraints = Path.Map.empty;
  flags = 0;
  functor_args = Ident.empty;
 }

let in_signature b env =
  let flags =
    if b then env.flags lor in_signature_flag
    else env.flags land (lnot in_signature_flag)
  in
  {env with flags}

let is_in_signature env = env.flags land in_signature_flag <> 0

let has_local_constraints env =
  not (Path.Map.is_empty env.local_constraints)

let is_ext cda =
  match cda.cda_description with
  | {cstr_tag = Extension _} -> true
  | _ -> false

let is_local_ext cda =
  match cda.cda_description with
  | {cstr_tag = Extension(p, _)} -> begin
      match p with
      | Pident _ -> true
      | Pdot _ | Papply _ | Pextra_ty _ -> false
  end
  | _ -> false

let diff env1 env2 =
  IdTbl.diff_keys env1.values env2.values @
  TycompTbl.diff_keys is_local_ext env1.constrs env2.constrs @
  IdTbl.diff_keys env1.modules env2.modules @
  IdTbl.diff_keys env1.classes env2.classes

(* Functions for use in "wrap" parameters in IdTbl *)
let wrap_identity x = x
let wrap_value vda = Val_bound vda
let wrap_module mda = Mod_local mda

(* Forward declarations *)

let components_of_module_maker' =
  ref ((fun _ -> assert false) :
          components_maker ->
            (module_components_repr, module_components_failure) result)

let components_of_functor_appl' =
  ref ((fun ~loc:_ ~f_path:_ ~f_comp:_ ~arg:_ _env -> assert false) :
          loc:Location.t -> f_path:Path.t -> f_comp:functor_components ->
            arg:Path.t -> t -> module_components)
let check_functor_application =
  (* to be filled by Includemod *)
  ref ((fun ~errors:_ ~loc:_
         ~lid_whole_app:_  ~f0_path:_ ~args:_
         ~arg_path:_ ~arg_mty:_ ~arg_mode:_ ~param_mty:_
         _env
         -> assert false) :
         errors:bool -> loc:Location.t ->
       lid_whole_app:Longident.t ->
       f0_path:Path.t -> args:(Path.t * Types.module_type * Mode.Value.l) list ->
       arg_path:Path.t -> arg_mty:module_type -> arg_mode:Mode.Value.l ->
       param_mty:module_type -> t -> unit)

let scrape_alias =
  (* to be filled with Mtype.scrape_alias *)
  ref ((fun _env _mty -> assert false) :
        t -> Subst.Lazy.module_type -> Subst.Lazy.module_type)

let md md_type =
  {md_type; md_attributes=[]; md_loc=Location.none
  ;md_uid = Uid.internal_not_actually_unique}

(** The caller is not interested in modes, and thus [val_modalities] is
invalidated. *)
let vda_description vda =
  let vda_description = vda.vda_description in
  {vda_description with val_modalities = Mode.Modality.Value.undefined}

let normalize_vda_mode vda =
  let vda_description = vda.vda_description in
  let modalities = vda_description.val_modalities in
  let vda_description =
    {vda_description with val_modalities = Mode.Modality.Value.id}
  in
  let vda_mode = Mode.Modality.Value.apply modalities vda.vda_mode in
  vda_description, vda_mode

(* Print addresses *)

let rec print_address ppf = function
  | Aunit cu -> Format.fprintf ppf "%s" (Compilation_unit.full_path_as_string cu)
  | Alocal id -> Format.fprintf ppf "%s" (Ident.name id)
  | Adot(a, pos) -> Format.fprintf ppf "%a.[%i]" print_address a pos

type address_head =
  | AHunit of Compilation_unit.t
  | AHlocal of Ident.t

let rec address_head = function
  | Aunit cu -> AHunit cu
  | Alocal id -> AHlocal id
  | Adot (a, _) -> address_head a

(* The name of the compilation unit currently compiled. *)
module Current_unit_name : sig
  val get : unit -> Compilation_unit.t option
  val set : Compilation_unit.t option -> unit
  val is : string -> bool
  val is_ident : Ident.t -> bool
  val is_path : Path.t -> bool
end = struct
  let get () =
    Compilation_unit.get_current ()
  let set comp_unit =
    Compilation_unit.set_current comp_unit
  let get_name () =
    Option.map Compilation_unit.name (get ())
  let is name =
    let current_name_string =
      Option.map Compilation_unit.Name.to_string (get_name ())
    in
    Option.equal String.equal current_name_string (Some name)
  let is_ident id =
    Ident.is_global id && is (Ident.name id)
  let is_path = function
  | Pident id -> is_ident id
  | Pdot _ | Papply _ | Pextra_ty _ -> false
end

let set_unit_name = Current_unit_name.set
let get_unit_name = Current_unit_name.get

let find_same_module id tbl =
  match IdTbl.find_same_without_locks id tbl with
  | x -> x
  | exception Not_found
    when Ident.is_global id && not (Current_unit_name.is_ident id) ->
      Mod_persistent

let find_name_module ~mark name tbl =
  match IdTbl.find_name_and_locks wrap_module ~mark name tbl with
  | Ok x -> x
  | Error locks when not (Current_unit_name.is name) ->
      let path = Pident(Ident.create_persistent name) in
      path, locks, Mod_persistent
  | _ ->
    raise Not_found

let add_persistent_structure id env =
  if not (Ident.is_global id) then invalid_arg "Env.add_persistent_structure";
  if Current_unit_name.is_ident id then env
  else begin
    let material =
      (* This addition only observably changes the environment if it shadows a
         non-persistent module already in the environment.
         (See PR#9345) *)
      match
        IdTbl.find_name_and_locks wrap_module ~mark:false (Ident.name id) env.modules
      with
      | Error _ | Ok (_, _, Mod_persistent) -> false
      | _ -> true
    in
    let summary =
      if material then Env_persistent (env.summary, id)
      else env.summary
    in
    let modules =
      (* With [-no-alias-deps], non-material additions should not
         affect the environment at all. We should only observe the
         existence of a cmi when accessing components of the module.
         (See #9991). *)
      if material || not !Clflags.transparent_modules then
        IdTbl.add id Mod_persistent env.modules
      else
        env.modules
    in
    { env with modules; summary }
  end

let components_of_module ~alerts ~uid env ps path addr mty shape =
  {
    alerts;
    uid;
    comps = Lazy_backtrack.create {
      cm_env = env;
      cm_prefixing_subst = ps;
      cm_path = path;
      cm_addr = addr;
      cm_mty = mty;
      cm_shape = shape;
    }
  }

let read_sign_of_cmi sign name uid ~shape ~address:addr ~flags =
  let id = Ident.create_persistent (Compilation_unit.Name.to_string name) in
  let path = Pident id in
  let alerts =
    List.fold_left (fun acc -> function Alerts s -> s | _ -> acc)
      Misc.Stdlib.String.Map.empty
      flags
  in
  let md =
    { Subst.Lazy.md_type = Mty_signature sign;
      md_loc = Location.none;
      md_attributes = [];
      md_uid = uid;
    }
  in
  let mda_address = Lazy_backtrack.create_forced addr in
  let mda_declaration = md in
  let mda_shape = shape in
  let mda_components =
    let mty = md.md_type in
    components_of_module ~alerts ~uid:md.md_uid
      empty Subst.identity
      path mda_address mty mda_shape
  in
  {
    mda_declaration;
    mda_components;
    mda_address;
    mda_shape;
  }

let persistent_env : module_data Persistent_env.t ref =
  s_table Persistent_env.empty ()

let without_cmis f x =
  Persistent_env.without_cmis !persistent_env f x

let imports () = Persistent_env.imports !persistent_env

let import_crcs ~source crcs =
  Persistent_env.import_crcs !persistent_env ~source crcs

let runtime_parameters () = Persistent_env.runtime_parameters !persistent_env

let parameters () = Persistent_env.parameters !persistent_env

let read_pers_mod modname cmi ~add_binding =
  Persistent_env.read !persistent_env read_sign_of_cmi modname cmi
    ~add_binding

let find_pers_mod name =
  Persistent_env.find !persistent_env read_sign_of_cmi name

let check_pers_mod ~loc name =
  Persistent_env.check !persistent_env read_sign_of_cmi ~loc name

let crc_of_unit name =
  Persistent_env.crc_of_unit !persistent_env name

let is_imported_opaque modname =
  Persistent_env.is_imported_opaque !persistent_env modname

let register_import_as_opaque modname =
  Persistent_env.register_import_as_opaque !persistent_env modname

let is_parameter_unit modname =
  Persistent_env.is_parameter_import !persistent_env modname

let implemented_parameter modname =
  Persistent_env.implemented_parameter !persistent_env modname

let reset_declaration_caches () =
  Types.Uid.Tbl.clear !value_declarations;
  Types.Uid.Tbl.clear !type_declarations;
  Types.Uid.Tbl.clear !module_declarations;
  Types.Uid.Tbl.clear !used_constructors;
  Types.Uid.Tbl.clear !used_labels;
  ()

let reset_cache ~preserve_persistent_env =
  Compilation_unit.set_current None;
  if not preserve_persistent_env then
    Persistent_env.clear !persistent_env;
  reset_declaration_caches ();
  ()

let reset_cache_toplevel () =
  Persistent_env.clear_missing !persistent_env;
  reset_declaration_caches ();
  ()

(* get_components *)

let get_components_res c =
  match Persistent_env.can_load_cmis !persistent_env with
  | Persistent_env.Can_load_cmis ->
    Lazy_backtrack.force !components_of_module_maker' c.comps
  | Persistent_env.Cannot_load_cmis log ->
    Lazy_backtrack.force_logged log !components_of_module_maker' c.comps

let get_components c =
  match get_components_res c with
  | Error _ -> empty_structure
  | Ok c -> c

(* Module type of functor application *)

let modtype_of_functor_appl fcomp p1 p2 =
  match fcomp.fcomp_res with
  | Mty_alias _ as mty -> mty
  | mty ->
      try
        Hashtbl.find fcomp.fcomp_subst_cache p2
      with Not_found ->
        let scope = Path.scope (Papply(p1, p2)) in
        let mty =
          let subst =
            match fcomp.fcomp_arg with
            | Unit
            | Named (None, _) -> Subst.identity
            | Named (Some param, _) -> Subst.add_module param p2 Subst.identity
          in
          Subst.modtype (Rescope scope) subst mty
        in
        Hashtbl.add fcomp.fcomp_subst_cache p2 mty;
        mty

let check_functor_appl
    ~errors ~loc ~lid_whole_app ~f0_path ~args
    ~f_comp
    ~arg_path ~arg_mty ~arg_mode ~param_mty
    env =
  if not (Hashtbl.mem f_comp.fcomp_cache arg_path) then
    !check_functor_application
      ~errors ~loc ~lid_whole_app ~f0_path ~args
      ~arg_path ~arg_mty ~arg_mode ~param_mty
      env

let modname_of_ident id = Ident.name id |> Compilation_unit.Name.of_string

(* Lookup by identifier *)

let find_ident_module id env =
  match find_same_module id env.modules with
  | Mod_local data -> data
  | Mod_unbound _ -> raise Not_found
  | Mod_persistent -> find_pers_mod ~allow_hidden:true (id |> modname_of_ident)

let rec find_module_components path env =
  match path with
  | Pident id -> (find_ident_module id env).mda_components
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      (NameMap.find s sc.comp_modules).mda_components
  | Papply(f_path, arg) ->
      let f_comp = find_functor_components f_path env in
      let loc = Location.(in_file !input_name) in
      !components_of_functor_appl' ~loc ~f_path ~f_comp ~arg env
  | Pextra_ty _ -> raise Not_found

and find_structure_components path env =
  match get_components (find_module_components path env) with
  | Structure_comps c -> c
  | Functor_comps _ -> raise Not_found

and find_functor_components path env =
  match get_components (find_module_components path env) with
  | Functor_comps f -> f
  | Structure_comps _ -> raise Not_found

let find_module path env =
  match path with
  | Pident id ->
      let data = find_ident_module id env in
      Subst.Lazy.force_module_decl data.mda_declaration
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      let data = NameMap.find s sc.comp_modules in
      Subst.Lazy.force_module_decl data.mda_declaration
  | Papply(p1, p2) ->
      let fc = find_functor_components p1 env in
      md (modtype_of_functor_appl fc p1 p2)
  | Pextra_ty _ -> raise Not_found

let find_module_lazy ~alias path env =
  match path with
  | Pident id ->
      let data = find_ident_module id env in
      data.mda_declaration
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      let data = NameMap.find s sc.comp_modules in
      data.mda_declaration
  | Papply(p1, p2) ->
      let fc = find_functor_components p1 env in
      let md =
        if alias then md (fc.fcomp_res)
        else md (modtype_of_functor_appl fc p1 p2)
      in
      Subst.Lazy.of_module_decl md
  | Pextra_ty _ -> raise Not_found

let find_value_full path env =
  match path with
  | Pident id -> begin
      match IdTbl.find_same_without_locks id env.values with
      | Val_bound data -> data
      | Val_unbound _ -> raise Not_found
    end
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_values
  | Papply _ | Pextra_ty _ -> raise Not_found

let find_extension_full path env =
  match path with
  | Pident id -> TycompTbl.find_same id env.constrs
  | Pdot(p, s) -> begin
      let comps = find_structure_components p env in
      let cstrs = NameMap.find s comps.comp_constrs in
      let exts = List.filter is_ext cstrs in
      match exts with
      | [cda] -> cda
      | _ -> raise Not_found
    end
  | Papply _ | Pextra_ty _ -> raise Not_found

let type_of_cstr path = function
  | {cstr_inlined = Some decl; _} ->
      let labels =
        List.map snd (Datarepr.labels_of_type path decl)
      in
      begin match decl.type_kind with
      | Type_record (_, repr) ->
        {
          tda_declaration = decl;
          tda_descriptions = Type_record (labels, repr);
          tda_shape = Shape.leaf decl.type_uid;
        }
      | _ -> assert false
      end
  | _ -> assert false

let rec find_type_data path env =
  match Path.Map.find path env.local_constraints with
  | decl ->
    {
      tda_declaration = decl;
      tda_descriptions = Type_abstract (Btype.type_origin decl);
      tda_shape = Shape.leaf decl.type_uid;
    }
  | exception Not_found -> begin
      match path with
      | Pident id -> IdTbl.find_same id env.types
      | Pdot(p, s) ->
          let sc = find_structure_components p env in
          NameMap.find s sc.comp_types
      | Papply _ -> raise Not_found
      | Pextra_ty (p, extra) -> begin
          match extra with
          | Pcstr_ty s ->
              let cstr = find_cstr p s env in
              type_of_cstr path cstr
          | Pext_ty ->
              let cda = find_extension_full p env in
              type_of_cstr path cda.cda_description
        end
    end
and find_cstr path name env =
  let tda = find_type_data path env in
  match tda.tda_descriptions with
  | Type_variant (cstrs, _) ->
      List.find (fun cstr -> cstr.cstr_name = name) cstrs
  | Type_record _ | Type_abstract _ | Type_open -> raise Not_found



let find_modtype_lazy path env =
  match path with
  | Pident id -> (IdTbl.find_same id env.modtypes).mtda_declaration
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      (NameMap.find s sc.comp_modtypes).mtda_declaration
  | Papply _ | Pextra_ty _ -> raise Not_found

let find_modtype path env =
  Subst.Lazy.force_modtype_decl (find_modtype_lazy path env)

let find_class_full path env =
  match path with
  | Pident id -> IdTbl.find_same_without_locks id env.classes
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      NameMap.find s sc.comp_classes
  | Papply _ | Pextra_ty _ -> raise Not_found

let find_cltype path env =
  match path with
  | Pident id -> (IdTbl.find_same id env.cltypes).cltda_declaration
  | Pdot(p, s) ->
      let sc = find_structure_components p env in
      (NameMap.find s sc.comp_cltypes).cltda_declaration
  | Papply _ | Pextra_ty _ -> raise Not_found

let find_value path env =
  find_value_full path env |> vda_description

let find_value_no_locks_exn id env =
  match IdTbl.find_same_and_locks id env.values with
  | Val_bound _, _ :: _ -> Misc.fatal_error "locks encountered"
  | Val_bound data, [] -> normalize_vda_mode data
  | Val_unbound _, _ -> raise Not_found

let find_class path env =
  (find_class_full path env).clda_declaration

let find_ident_constructor id env =
  (TycompTbl.find_same id env.constrs).cda_description

let find_ident_label id env =
  TycompTbl.find_same id env.labels

let find_type p env =
  (find_type_data p env).tda_declaration
let find_type_descrs p env =
  (find_type_data p env).tda_descriptions

let rec find_module_address path env =
  match path with
  | Pident id -> find_ident_module_address id env
  | Pdot(p, s) ->
      let c = find_structure_components p env in
      get_address (NameMap.find s c.comp_modules).mda_address
  | Papply _ | Pextra_ty _ -> raise Not_found

and find_ident_module_address id env =
  get_address (find_ident_module id env).mda_address

and force_address = function
  | Projection { parent; pos } -> Adot(get_address parent, pos)
  | ModAlias { env; path } -> find_module_address path env

and get_address a =
  Lazy_backtrack.force force_address a

let find_value_address path env =
  get_address (find_value_full path env).vda_address

let find_class_address path env =
  get_address (find_class_full path env).clda_address

let rec get_constrs_address = function
  | [] -> raise Not_found
  | cda :: rest ->
    match cda.cda_address with
    | None -> get_constrs_address rest
    | Some a -> get_address a

let find_constructor_address path env =
  match path with
  | Pident id -> begin
      let cda = TycompTbl.find_same id env.constrs in
      match cda.cda_address with
      | None -> raise Not_found
      | Some addr -> get_address addr
    end
  | Pdot(p, s) ->
      let c = find_structure_components p env in
      get_constrs_address (NameMap.find s c.comp_constrs)
  | Papply _ | Pextra_ty _ -> raise Not_found

let find_hash_type path env =
  match path with
  | Pident id ->
      let name = Ident.name id in
      let _, cltda =
        IdTbl.find_name wrap_identity ~mark:false name env.cltypes
      in
      cltda.cltda_declaration.clty_hash_type
  | Pdot(p, name) ->
      let c = find_structure_components p env in
      let cltda = NameMap.find name c.comp_cltypes in
      cltda.cltda_declaration.clty_hash_type
  | Papply _ | Pextra_ty _ -> raise Not_found

let probes = ref String.Set.empty
let reset_probes () = probes := String.Set.empty
let add_probe name = probes := String.Set.add name !probes
let has_probe name = String.Set.mem name !probes

let find_shape env (ns : Shape.Sig_component_kind.t) id =
  match ns with
  | Type ->
      (IdTbl.find_same id env.types).tda_shape
  | Constructor ->
      Shape.leaf ((TycompTbl.find_same id env.constrs).cda_description.cstr_uid)
  | Label ->
      Shape.leaf ((TycompTbl.find_same id env.labels).lbl_uid)
  | Extension_constructor ->
      (TycompTbl.find_same id env.constrs).cda_shape
  | Value ->
      begin match IdTbl.find_same_without_locks id env.values with
      | Val_bound x -> x.vda_shape
      | Val_unbound _ -> raise Not_found
      end
  | Module ->
      begin match IdTbl.find_same_without_locks id env.modules with
      | Mod_local { mda_shape; _ } -> mda_shape
      | Mod_persistent -> Shape.for_persistent_unit (Ident.name id)
      | Mod_unbound _ ->
          (* Only present temporarily while approximating the environment for
             recursive modules.
             [find_shape] is only ever called after the environment gets
             properly populated. *)
          assert false
      | exception Not_found
        when Ident.is_global id && not (Current_unit_name.is_ident id) ->
          Shape.for_persistent_unit (Ident.name id)
      end
  | Module_type ->
      (IdTbl.find_same id env.modtypes).mtda_shape
  | Class ->
      (IdTbl.find_same_without_locks id env.classes).clda_shape
  | Class_type ->
      (IdTbl.find_same id env.cltypes).cltda_shape

let shape_of_path ~namespace env =
  Shape.of_path ~namespace ~find_shape:(find_shape env)

let shape_or_leaf uid = function
  | None -> Shape.leaf uid
  | Some shape -> shape

let required_globals = s_ref []
let reset_required_globals () = required_globals := []
let get_required_globals () = !required_globals
let add_required_unit cu =
  if not (List.exists (Compilation_unit.equal cu) !required_globals)
  then required_globals := cu :: !required_globals
let add_required_ident id env =
  if not !Clflags.transparent_modules && Ident.is_global id then
    let address = find_ident_module_address id env in
    match address_head address with
    | AHlocal _ -> ()
    | AHunit cu -> add_required_unit cu
let add_required_global path env =
  add_required_ident (Path.head path) env

let rec normalize_module_path lax env = function
  | Pident id as path when lax && Ident.is_global id ->
      path (* fast path (avoids lookup) *)
  | Pdot (p, s) as path ->
      let p' = normalize_module_path lax env p in
      if p == p' then expand_module_path lax env path
      else expand_module_path lax env (Pdot(p', s))
  | Papply (p1, p2) as path ->
      let p1' = normalize_module_path lax env p1 in
      let p2' = normalize_module_path true env p2 in
      if p1 == p1' && p2 == p2' then expand_module_path lax env path
      else expand_module_path lax env (Papply(p1', p2'))
  | Pident _ as path ->
      expand_module_path lax env path
  | Pextra_ty _ -> assert false

and expand_module_path lax env path =
  try match find_module_lazy ~alias:true path env with
    {md_type=Mty_alias path1} ->
      let path' = normalize_module_path lax env path1 in
      if not (lax || !Clflags.transparent_modules) then begin
        let id = Path.head path in
        if Ident.is_global_or_predef id && not (Ident.same id (Path.head path'))
        then add_required_global (Pident id) env
      end;
      path'
  | _ -> path
  with Not_found when lax
  || (match path with Pident id -> not (Ident.is_global id) | _ -> true) ->
      path

let normalize_module_path oloc env path =
  try normalize_module_path (oloc = None) env path
  with Not_found ->
    match oloc with None -> assert false
    | Some loc ->
        error (Missing_module(loc, path,
                              normalize_module_path true env path))

let rec normalize_path_prefix oloc env path =
  match path with
  | Pdot(p, s) ->
      let p2 = normalize_module_path oloc env p in
      if p == p2 then path else Pdot(p2, s)
  | Pident _ ->
      path
  | Pextra_ty (p, extra) ->
      let p2 = normalize_path_prefix oloc env p in
      if p == p2 then path else Pextra_ty (p2, extra)
  | Papply _  ->
      assert false

let normalize_type_path = normalize_path_prefix

let normalize_value_path = normalize_path_prefix

let rec normalize_modtype_path env path =
  let path = normalize_path_prefix None env path in
  expand_modtype_path env path

and expand_modtype_path env path =
  match (find_modtype_lazy path env).mtd_type with
  | Some (Mty_ident path) -> normalize_modtype_path env path
  | _ | exception Not_found -> path

let find_module_lazy path env =
  find_module_lazy ~alias:false path env

(* Find the manifest type associated to a type when appropriate:
   - the type should be public or should have a private row,
   - the type should have an associated manifest type. *)
let find_type_expansion path env =
  let decl = find_type path env in
  match decl.type_manifest with
  | Some body when decl.type_private = Public
              || not (Btype.type_kind_is_abstract decl)
              || Btype.has_constr_row body ->
      (decl.type_params, body, decl.type_expansion_scope)
  (* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. *)
  | _ -> raise Not_found

(* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. *)
let find_type_expansion_opt path env =
  let decl = find_type path env in
  match decl.type_manifest with
  (* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. *)
  | Some body ->
      (decl.type_params, body, decl.type_expansion_scope)
  | _ -> raise Not_found

let find_modtype_expansion_lazy path env =
  match (find_modtype_lazy path env).mtd_type with
  | None -> raise Not_found
  | Some mty -> mty

let find_modtype_expansion path env =
  Subst.Lazy.force_modtype (find_modtype_expansion_lazy path env)

let rec is_functor_arg path env =
  match path with
    Pident id ->
      begin try Ident.find_same id env.functor_args; true
      with Not_found -> false
      end
  | Pdot (p, _) | Pextra_ty (p, _) -> is_functor_arg p env
  | Papply _ -> true

(* Copying types associated with values *)

let make_copy_of_types env0 =
  let memo = Hashtbl.create 16 in
  let copy t =
    try
      Hashtbl.find memo (get_id t)
    with Not_found ->
      let t2 = Subst.Lazy.substitute Subst.identity (Subst.Lazy.of_value t) in
      Hashtbl.add memo (get_id t) t2;
      t2
  in
  let f = function
    | Val_unbound _ as entry -> entry
    | Val_bound vda ->
        let desc = vda.vda_description in
        let t = copy (Subst.Lazy.force_type_expr desc.val_type) in
        let desc = { desc with val_type = t } in
        Val_bound { vda with vda_description = desc }
  in
  let values =
    IdTbl.map f env0.values
  in
  (fun env ->
     (*if env.values != env0.values then fatal_error "Env.make_copy_of_types";*)
     {env with values; summary = Env_copy_types env.summary}
  )

(* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) *)

type iter_cont = unit -> unit
let iter_env_cont = ref []

let rec scrape_alias_for_visit env mty =
  let open Subst.Lazy in
  match mty with
  | Mty_alias path -> begin
      match path with
      | Pident id
        when Ident.is_global id
          && not (Persistent_env.looked_up !persistent_env (id |> modname_of_ident)) ->
          false
      | path -> (* PR#6600: find_module may raise Not_found *)
          try
            scrape_alias_for_visit env (find_module_lazy path env).md_type
          with Not_found -> false
    end
  | _ -> true

let iter_env wrap proj1 proj2 f env () =
  IdTbl.iter wrap (fun id x -> f (Pident id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match Lazy_backtrack.get_arg mcomps.comps with
        | None -> true
        | Some { cm_mty; _ } ->
            scrape_alias_for_visit env cm_mty
      in
      if not visit then () else
      match get_components mcomps with
        Structure_comps comps ->
          NameMap.iter
            (fun s d -> f (Pdot (path, s)) (Pdot (path', s), d))
            (proj2 comps);
          NameMap.iter
            (fun s mda ->
              iter_components
                (Pdot (path, s)) (Pdot (path', s)) mda.mda_components)
            comps.comp_modules
      | Functor_comps _ -> ()
    in iter_env_cont := (path, cont) :: !iter_env_cont
  in
  IdTbl.iter wrap_module
    (fun id (path, entry) ->
       match entry with
       | Mod_unbound _ -> ()
       | Mod_local data ->
           iter_components (Pident id) path data.mda_components
       | Mod_persistent -> ())
    env.modules;
  Persistent_env.fold !persistent_env (fun name data () ->
    let id = Ident.create_persistent (Compilation_unit.Name.to_string name) in
    let path = Pident id in
    iter_components path path data.mda_components) ()

let run_iter_cont l =
  iter_env_cont := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !iter_env_cont in
  iter_env_cont := [];
  cont

let iter_types f =
  iter_env wrap_identity (fun env -> env.types) (fun sc -> sc.comp_types)
    (fun p1 (p2, tda) -> f p1 (p2, tda.tda_declaration))

let same_types env1 env2 =
  env1.types == env2.types && env1.modules == env2.modules

let used_persistent () =
  Persistent_env.fold !persistent_env
    (fun s _m r -> Compilation_unit.Name.Set.add s r)
    Compilation_unit.Name.Set.empty

let find_all_comps wrap proj s (p, mda) =
  match get_components mda.mda_components with
    Functor_comps _ -> []
  | Structure_comps comps ->
      try
        let c = NameMap.find s (proj comps) in
        [Pdot(p,s), wrap c]
      with Not_found -> []

let rec find_shadowed_comps path env =
  match path with
  | Pident id ->
      List.filter_map
        (fun (p, data) ->
           match data with
           | Mod_local x -> Some (p, x)
           | Mod_unbound _ | Mod_persistent -> None)
        (IdTbl.find_all wrap_module (Ident.name id) env.modules)
  | Pdot (p, s) ->
      let l = find_shadowed_comps p env in
      let l' =
        List.map
          (find_all_comps wrap_identity
             (fun comps -> comps.comp_modules) s) l
      in
      List.flatten l'
  | Papply _ | Pextra_ty _ -> []

let find_shadowed wrap proj1 proj2 path env =
  match path with
    Pident id ->
      IdTbl.find_all wrap (Ident.name id) (proj1 env)
  | Pdot (p, s) ->
      let l = find_shadowed_comps p env in
      let l' = List.map (find_all_comps wrap proj2 s) l in
      List.flatten l'
  | Papply _ | Pextra_ty _ -> []

let find_shadowed_types path env =
  List.map fst
    (find_shadowed wrap_identity
       (fun env -> env.types) (fun comps -> comps.comp_types) path env)

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let prefix_idents root prefixing_sub sg =
  let open Subst.Lazy in
  let rec prefix_idents root items_and_paths prefixing_sub =
    function
    | [] -> (List.rev items_and_paths, prefixing_sub)
    | Sig_value(id, _, _) as item :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((item, p) :: items_and_paths) prefixing_sub rem
    | Sig_type(id, td, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((Sig_type(id, td, rs, vis), p) :: items_and_paths)
        (Subst.add_type id p prefixing_sub)
        rem
    | Sig_typext(id, ec, es, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      (* we extend the substitution in case of an inlined record *)
      prefix_idents root
        ((Sig_typext(id, ec, es, vis), p) :: items_and_paths)
        (Subst.add_type id p prefixing_sub)
        rem
    | Sig_module(id, pres, md, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((Sig_module(id, pres, md, rs, vis), p) :: items_and_paths)
        (Subst.add_module id p prefixing_sub)
        rem
    | Sig_modtype(id, mtd, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((Sig_modtype(id, mtd, vis), p) :: items_and_paths)
        (Subst.add_modtype id (Mty_ident p) prefixing_sub)
        rem
    | Sig_class(id, cd, rs, vis) :: rem ->
      (* pretend this is a type, cf. PR#6650 *)
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((Sig_class(id, cd, rs, vis), p) :: items_and_paths)
        (Subst.add_type id p prefixing_sub)
        rem
    | Sig_class_type(id, ctd, rs, vis) :: rem ->
      let p = Pdot(root, Ident.name id) in
      prefix_idents root
        ((Sig_class_type(id, ctd, rs, vis), p) :: items_and_paths)
        (Subst.add_type id p prefixing_sub)
        rem
  in
  let sg = Subst.Lazy.force_signature_once sg in
  prefix_idents root [] prefixing_sub sg

(* Compute structure descriptions *)

let add_to_tbl id decl tbl =
  let decls = try NameMap.find id tbl with Not_found -> [] in
  NameMap.add id (decl :: decls) tbl

let primitive_address_error =
  Invalid_argument "Primitives don't have addresses"

let value_declaration_address (_ : t) id decl =
  match decl.Subst.Lazy.val_kind with
  | Val_prim _ -> Lazy_backtrack.create_failed primitive_address_error
  | _ -> Lazy_backtrack.create_forced (Alocal id)

let extension_declaration_address (_ : t) id (_ : extension_constructor) =
  Lazy_backtrack.create_forced (Alocal id)

let class_declaration_address (_ : t) id (_ : class_declaration) =
  Lazy_backtrack.create_forced (Alocal id)

let module_declaration_address env id presence md =
  match presence with
  | Mp_absent -> begin
      let open Subst.Lazy in
      match md.md_type with
      | Mty_alias path -> Lazy_backtrack.create (ModAlias {env; path})
      | _ -> assert false
    end
  | Mp_present ->
      Lazy_backtrack.create_forced (Alocal id)

let is_identchar c =
  (* This should be kept in sync with the [identchar_latin1] character class
     in [lexer.mll] *)
  match c with
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214'
  | '\216'..'\246' | '\248'..'\255' | '\'' | '0'..'9' ->
    true
  | _ ->
    false

let rec components_of_module_maker
          {cm_env; cm_prefixing_subst;
           cm_path; cm_addr; cm_mty; cm_shape} : _ result =
  match !scrape_alias cm_env cm_mty with
    Mty_signature sg ->
      let c =
        { comp_values = NameMap.empty;
          comp_constrs = NameMap.empty;
          comp_labels = NameMap.empty; comp_types = NameMap.empty;
          comp_modules = NameMap.empty; comp_modtypes = NameMap.empty;
          comp_classes = NameMap.empty; comp_cltypes = NameMap.empty }
      in
      let items_and_paths, sub =
        prefix_idents cm_path cm_prefixing_subst sg
      in
      let env = ref cm_env in
      let pos = ref 0 in
      let next_address () =
        let addr : address_unforced =
          Projection { parent = cm_addr; pos = !pos }
        in
        incr pos;
        Lazy_backtrack.create addr
      in
      List.iter (fun ((item : Subst.Lazy.signature_item), path) ->
        match item with
          Sig_value(id, decl, _) ->
            let decl' = Subst.Lazy.value_description sub decl in
            let addr =
              match decl.val_kind with
              | Val_prim _ -> Lazy_backtrack.create_failed primitive_address_error
              | _ -> next_address ()
            in
            let vda_shape = Shape.proj cm_shape (Shape.Item.value id) in
            let vda =
              { vda_description = decl'; vda_address = addr;
                vda_mode = cm_mode; vda_shape }
            in
            c.comp_values <- NameMap.add (Ident.name id) vda c.comp_values;
        | Sig_type(id, decl, _, _) ->
            let final_decl = Subst.type_declaration sub decl in
            Btype.set_static_row_name final_decl
              (Subst.type_path sub (Path.Pident id));
            let descrs =
              match decl.type_kind with
              | Type_variant (_,repr) ->
                  let cstrs = List.map snd
                    (Datarepr.constructors_of_type path final_decl
                        ~current_unit:(get_unit_name ()))
                  in
                  List.iter
                    (fun descr ->
                      let cda_shape = Shape.leaf descr.cstr_uid in
                      let cda = {
                        cda_description = descr;
                        cda_address = None;
                        cda_shape }
                      in
                      c.comp_constrs <-
                        add_to_tbl descr.cstr_name cda c.comp_constrs
                    ) cstrs;
                 Type_variant (cstrs, repr)
              | Type_record (_, repr) ->
                  let lbls = List.map snd
                    (Datarepr.labels_of_type path final_decl)
                  in
                  List.iter
                    (fun descr ->
                      c.comp_labels <-
                        add_to_tbl descr.lbl_name descr c.comp_labels)
                    lbls;
                  Type_record (lbls, repr)
              | Type_abstract r -> Type_abstract r
              | Type_open -> Type_open
            in
            let shape = Shape.proj cm_shape (Shape.Item.type_ id) in
            let tda =
              { tda_declaration = final_decl;
                tda_descriptions = descrs;
                tda_shape = shape; }
            in
            c.comp_types <- NameMap.add (Ident.name id) tda c.comp_types;
            env := store_type_infos ~tda_shape:shape id decl !env
        | Sig_typext(id, ext, _, _) ->
            let ext' = Subst.extension_constructor sub ext in
            let descr =
              Datarepr.extension_descr ~current_unit:(get_unit_name ()) path
                ext'
            in
            let addr = next_address () in
            let cda_shape =
              Shape.proj cm_shape (Shape.Item.extension_constructor id)
            in
            let cda =
              { cda_description = descr; cda_address = Some addr; cda_shape }
            in
            c.comp_constrs <- add_to_tbl (Ident.name id) cda c.comp_constrs
        | Sig_module(id, pres, md, _, _) ->
            let md' =
              (* The prefixed items get the same scope as [cm_path], which is
                 the prefix. *)
              Subst.Lazy.module_decl
                (Subst.Rescope (Path.scope cm_path)) sub md
            in
            let addr =
              match pres with
              | Mp_absent -> begin
                  match md.md_type with
                  | Mty_alias path ->
                      Lazy_backtrack.create (ModAlias {env = !env; path})
                  | _ -> assert false
                end
              | Mp_present -> next_address ()
            in
            let alerts =
              Builtin_attributes.alerts_of_attrs md.md_attributes
            in
            let shape = Shape.proj cm_shape (Shape.Item.module_ id) in
            let comps =
              components_of_module ~alerts ~uid:md.md_uid !env
                sub path addr md.md_type shape
            in
            let mda =
              { mda_declaration = md';
                mda_components = comps;
                mda_address = addr;
                mda_shape = shape; }
            in
            c.comp_modules <-
              NameMap.add (Ident.name id) mda c.comp_modules;
            env :=
              store_module ~update_summary:false ~check:None
                id addr pres md shape !env
        | Sig_modtype(id, decl, _) ->
            let final_decl =
              (* The prefixed items get the same scope as [cm_path], which is
                 the prefix. *)
              Subst.Lazy.modtype_decl (Rescope (Path.scope cm_path))
                sub decl
            in
            let shape = Shape.proj cm_shape (Shape.Item.module_type id) in
            let mtda =
              { mtda_declaration = final_decl;
                mtda_shape = shape; }
            in
            c.comp_modtypes <-
              NameMap.add (Ident.name id) mtda c.comp_modtypes;
            env := store_modtype ~update_summary:false id decl shape !env
        | Sig_class(id, decl, _, _) ->
            let decl' = Subst.class_declaration sub decl in
            let addr = next_address () in
            let shape = Shape.proj cm_shape (Shape.Item.class_ id) in
            let clda =
              { clda_declaration = decl';
                clda_address = addr;
                clda_shape = shape; }
            in
            c.comp_classes <- NameMap.add (Ident.name id) clda c.comp_classes
        | Sig_class_type(id, decl, _, _) ->
            let decl' = Subst.cltype_declaration sub decl in
            let shape = Shape.proj cm_shape (Shape.Item.class_type id) in
            let cltda = { cltda_declaration = decl'; cltda_shape = shape } in
            c.comp_cltypes <-
              NameMap.add (Ident.name id) cltda c.comp_cltypes)
        items_and_paths;
        Ok (Structure_comps c)
  | Mty_functor(arg, ty_res) ->
      let sub = cm_prefixing_subst in
      let scoping = Subst.Rescope (Path.scope cm_path) in
      let open Subst.Lazy in
        Ok (Functor_comps {
          (* fcomp_arg and fcomp_res must be prefixed eagerly, because
             they are interpreted in the outer environment *)
          fcomp_arg =
            (match arg with
            | Unit -> Unit
            | Named (param, ty_arg) ->
              Named (param, force_modtype (modtype scoping sub ty_arg)));
          fcomp_res = force_modtype (modtype scoping sub ty_res);
          fcomp_shape = cm_shape;
          fcomp_cache = Hashtbl.create 17;
          fcomp_subst_cache = Hashtbl.create 17 })
  | Mty_ident _ | Mty_strengthen _ -> Error No_components_abstract
  | Mty_alias p -> Error (No_components_alias p)

(* Insertion of bindings by identifier + path *)

and check_usage loc id uid warn tbl =
  if not loc.Location.loc_ghost &&
     Uid.for_actual_declaration uid &&
     Warnings.is_active (warn "")
  then begin
    let name = Ident.name id in
    if Types.Uid.Tbl.mem tbl uid then ()
    else let used = ref false in
    Types.Uid.Tbl.add tbl uid (fun () -> used := true);
    if not (name = "" || name.[0] = '_' || name.[0] = '#')
    then
      !add_delayed_check_forward
        (fun () -> if not !used then Location.prerr_warning loc (warn name))
  end;

and check_value_name name loc =
  (* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged by -pp or
     -ppx preprocessors. *)
  if String.length name > 0 && not (is_identchar name.[0]) then
    for i = 1 to String.length name - 1 do
      if name.[i] = '#' then
        error (Illegal_value_name(loc, name))
    done

and store_value ?check ~mode id addr decl shape env =
  let open Subst.Lazy in
  check_value_name (Ident.name id) decl.val_loc;
  Builtin_attributes.mark_alerts_used decl.val_attributes;
  Option.iter
    (fun f -> check_usage decl.val_loc id decl.val_uid f !value_declarations)
    check;
  let vda =
    { vda_description = decl;
      vda_address = addr;
      vda_mode = mode;
      vda_shape = shape }
  in
  { env with
    values = IdTbl.add id (Val_bound vda) env.values;
    summary =
      Env_value(env.summary, id, Subst.Lazy.force_value_description decl, mode) }

and store_constructor ~check type_decl type_id cstr_id cstr env =
  Builtin_attributes.warning_scope cstr.cstr_attributes (fun () ->
  if check && not type_decl.type_loc.Location.loc_ghost
     && Warnings.is_active (Warnings.Unused_constructor ("", Unused))
  then begin
    let ty_name = Ident.name type_id in
    let name = cstr.cstr_name in
    let loc = cstr.cstr_loc in
    let k = cstr.cstr_uid in
    let priv = type_decl.type_private in
    if not (Types.Uid.Tbl.mem !used_constructors k) then begin
      let used = constructor_usages () in
      Types.Uid.Tbl.add !used_constructors k
        (add_constructor_usage used);
      if not (ty_name = "" || ty_name.[0] = '_')
      then
        !add_delayed_check_forward
          (fun () ->
            Option.iter
              (fun complaint ->
                 if not (is_in_signature env) then
                   Location.prerr_warning loc
                     (Warnings.Unused_constructor(name, complaint)))
              (constructor_usage_complaint ~rebind:false priv used));
    end;
  end);
  Builtin_attributes.mark_alerts_used cstr.cstr_attributes;
  Builtin_attributes.mark_warn_on_literal_pattern_used cstr.cstr_attributes;
  let cda_shape = Shape.leaf cstr.cstr_uid in
  { env with
    constrs =
      TycompTbl.add cstr_id
        { cda_description = cstr; cda_address = None; cda_shape } env.constrs;
  }

and store_label ~check type_decl type_id lbl_id lbl env =
  Builtin_attributes.warning_scope lbl.lbl_attributes (fun () ->
  if check && not type_decl.type_loc.Location.loc_ghost
     && Warnings.is_active (Warnings.Unused_field ("", Unused))
  then begin
    let ty_name = Ident.name type_id in
    let priv = type_decl.type_private in
    let name = lbl.lbl_name in
    let loc = lbl.lbl_loc in
    let mut = lbl.lbl_mut in
    let k = lbl.lbl_uid in
    if not (Types.Uid.Tbl.mem !used_labels k) then
      let used = label_usages () in
      Types.Uid.Tbl.add !used_labels k
        (add_label_usage used);
      if not (ty_name = "" || ty_name.[0] = '_' || name.[0] = '_')
      then !add_delayed_check_forward
          (fun () ->
            Option.iter
              (fun complaint ->
                 if not (is_in_signature env) then
                   Location.prerr_warning
                     loc (Warnings.Unused_field(name, complaint)))
              (label_usage_complaint priv mut used))
  end);
  Builtin_attributes.mark_alerts_used lbl.lbl_attributes;
  begin match lbl.lbl_mut with
    | Mutable _ ->
      Builtin_attributes.mark_deprecated_mutable_used lbl.lbl_attributes;
    | Immutable -> ()
  end;
  { env with
    labels = TycompTbl.add lbl_id lbl env.labels;
  }

and store_type ~check id info shape env =
  let loc = info.type_loc in
  if check then
    check_usage loc id info.type_uid
      (fun s -> Warnings.Unused_type_declaration s)
      !type_declarations;
  let descrs, env =
    let path = Pident id in
    match info.type_kind with
    | Type_variant (_,repr) ->
        let constructors = Datarepr.constructors_of_type path info
                            ~current_unit:(get_unit_name ())
        in
        Type_variant (List.map snd constructors, repr),
        List.fold_left
          (fun env (cstr_id, cstr) ->
            store_constructor ~check info id cstr_id cstr env)
          env constructors
    | Type_record (_, repr) ->
        let labels = Datarepr.labels_of_type path info in
        Type_record (List.map snd labels, repr),
        List.fold_left
          (fun env (lbl_id, lbl) ->
            store_label ~check info id lbl_id lbl env)
          env labels
    | Type_abstract r -> Type_abstract r, env
    | Type_open -> Type_open, env
  in
  let tda =
    { tda_declaration = info;
      tda_descriptions = descrs;
      tda_shape = shape }
  in
  Builtin_attributes.mark_alerts_used info.type_attributes;
  { env with
    types = IdTbl.add id tda env.types;
    summary = Env_type(env.summary, id, info) }

and store_type_infos ~tda_shape id info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  let tda =
    {
      tda_declaration = info;
      tda_descriptions = Type_abstract (Btype.type_origin info);
      tda_shape
    }
  in
  { env with
    types = IdTbl.add id tda env.types;
    summary = Env_type(env.summary, id, info) }

and store_extension ~check ~rebind id addr ext shape env =
  let loc = ext.ext_loc in
  let cstr =
    Datarepr.extension_descr ~current_unit:(get_unit_name ()) (Pident id) ext
  in
  let cda =
    { cda_description = cstr;
      cda_address = Some addr;
      cda_shape = shape }
  in
  Builtin_attributes.mark_alerts_used ext.ext_attributes;
  Builtin_attributes.mark_warn_on_literal_pattern_used cstr.cstr_attributes;
  Builtin_attributes.warning_scope ext.ext_attributes (fun () ->
  if check && not loc.Location.loc_ghost &&
    Warnings.is_active (Warnings.Unused_extension ("", false, Unused))
  then begin
    let priv = ext.ext_private in
    let is_exception = Path.same ext.ext_type_path Predef.path_exn in
    let name = cstr.cstr_name in
    let k = cstr.cstr_uid in
    if not (Types.Uid.Tbl.mem !used_constructors k) then begin
      let used = constructor_usages () in
      Types.Uid.Tbl.add !used_constructors k
        (add_constructor_usage used);
      !add_delayed_check_forward
         (fun () ->
           Option.iter
             (fun complaint ->
                if not (is_in_signature env) then
                  Location.prerr_warning loc
                    (Warnings.Unused_extension
                       (name, is_exception, complaint)))
             (constructor_usage_complaint ~rebind priv used))
    end;
  end);
  { env with
    constrs = TycompTbl.add id cda env.constrs;
    summary = Env_extension(env.summary, id, ext) }

and store_module ?(update_summary=true) ~check
                 id addr presence md shape env =
  let open Subst.Lazy in
  let loc = md.md_loc in
  Option.iter
    (fun f -> check_usage loc id md.md_uid f !module_declarations) check;
  Builtin_attributes.mark_alerts_used md.md_attributes;
  let alerts = Builtin_attributes.alerts_of_attrs md.md_attributes in
  let comps =
    components_of_module ~alerts ~uid:md.md_uid
      env Subst.identity (Pident id) addr md.md_type shape
  in
  let mda =
    { mda_declaration = md;
      mda_components = comps;
      mda_address = addr;
      mda_shape = shape }
  in
  let summary =
    if not update_summary then env.summary
    else Env_module (env.summary, id, presence, force_module_decl md) in
  { env with
    modules = IdTbl.add id (Mod_local mda) env.modules;
    summary }

and store_modtype ?(update_summary=true) id info shape env =
  Builtin_attributes.mark_alerts_used info.Subst.Lazy.mtd_attributes;
  let mtda = { mtda_declaration = info; mtda_shape = shape } in
  let summary =
    if not update_summary then env.summary
    else Env_modtype (env.summary, id, Subst.Lazy.force_modtype_decl info) in
  { env with
    modtypes = IdTbl.add id mtda env.modtypes;
    summary }

and store_class id addr desc shape env =
  Builtin_attributes.mark_alerts_used desc.cty_attributes;
  let clda =
    { clda_declaration = desc;
      clda_address = addr;
      clda_shape = shape; }
  in
  { env with
    classes = IdTbl.add id clda env.classes;
    summary = Env_class(env.summary, id, desc) }

and store_cltype id desc shape env =
  Builtin_attributes.mark_alerts_used desc.clty_attributes;
  let cltda = { cltda_declaration = desc; cltda_shape = shape } in
  { env with
    cltypes = IdTbl.add id cltda env.cltypes;
    summary = Env_cltype(env.summary, id, desc) }

(* Compute the components of a functor application in a path. *)

let components_of_functor_appl ~loc ~f_path ~f_comp ~arg env =
  try
    let c = Hashtbl.find f_comp.fcomp_cache arg in
    c
  with Not_found ->
    let p = Papply(f_path, arg) in
    let sub =
      match f_comp.fcomp_arg with
      | Unit
      | Named (None, _) -> Subst.identity
      | Named (Some param, _) -> Subst.add_module param arg Subst.identity
    in
    (* we have to apply eagerly instead of passing sub to [components_of_module]
       because of the call to [check_well_formed_module]. *)
    let mty = Subst.modtype (Rescope (Path.scope p)) sub f_comp.fcomp_res in
    let addr = Lazy_backtrack.create_failed Not_found in
    !check_well_formed_module env loc
      ("the signature of " ^ Path.name p) mty;
    let shape_arg =
      shape_of_path ~namespace:Shape.Sig_component_kind.Module env arg
    in
    let shape = Shape.app f_comp.fcomp_shape ~arg:shape_arg in
    let comps =
      components_of_module ~alerts:Misc.Stdlib.String.Map.empty
        ~uid:Uid.internal_not_actually_unique
        (*???*)
        env Subst.identity p addr (Subst.Lazy.of_modtype mty) shape
    in
    Hashtbl.add f_comp.fcomp_cache arg comps;
    comps

(* Define forward functions *)

let _ =
  components_of_functor_appl' := components_of_functor_appl;
  components_of_module_maker' := components_of_module_maker

(* Insertion of bindings by identifier *)

let add_functor_arg id env =
  {env with
   functor_args = Ident.add id () env.functor_args;
   summary = Env_functor_arg (env.summary, id)}

let add_value_lazy ?check ?shape ~mode id desc env =
  let addr = value_declaration_address env id desc in
  let shape = shape_or_leaf desc.Subst.Lazy.val_uid shape in
  let mode = Mode.Value.disallow_right mode in
  store_value ?check ~mode id addr desc shape env

let add_type ~check ?shape id info env =
  let shape = shape_or_leaf info.type_uid shape in
  store_type ~check id info shape env

and add_extension ~check ?shape ~rebind id ext env =
  let addr = extension_declaration_address env id ext in
  let shape = shape_or_leaf ext.ext_uid shape in
  store_extension ~check ~rebind id addr ext shape env

and add_module_declaration_lazy
      ~update_summary ?(arg=false) ?shape ~check id presence md env =
  let check =
    if not check then
      None
    else if arg && is_in_signature env then
      Some (fun s -> Warnings.Unused_functor_parameter s)
    else
      Some (fun s -> Warnings.Unused_module s)
  in
  let addr = module_declaration_address env id presence md in
  let shape = shape_or_leaf md.Subst.Lazy.md_uid shape in
  let env =
    store_module ~update_summary ~check id addr presence md shape env
  in
  if arg then add_functor_arg id env else env

let add_module_declaration ?(arg=false) ?shape ~check id presence md env =
  add_module_declaration_lazy ~update_summary:true ~arg ?shape ~check id
    presence (Subst.Lazy.of_module_decl md) env

and add_modtype_lazy ~update_summary ?shape id info env =
  let shape = shape_or_leaf info.Subst.Lazy.mtd_uid shape in
  store_modtype ~update_summary id info shape env

let add_modtype ?shape id info env =
  add_modtype_lazy ~update_summary:true ?shape id
    (Subst.Lazy.of_modtype_decl info) env

and add_class ?shape id ty env =
  let addr = class_declaration_address env id ty in
  let shape = shape_or_leaf ty.cty_uid shape in
  store_class id addr ty shape env

and add_cltype ?shape id ty env =
  let shape = shape_or_leaf ty.clty_uid shape in
  store_cltype id ty shape env

let add_module_lazy ~update_summary id presence mty env =
  let md = Subst.Lazy.{md_type = mty;
                       md_attributes = [];
                       md_loc = Location.none;
                       md_uid = Uid.internal_not_actually_unique}
  in
  add_module_declaration_lazy ~update_summary ~check:false id presence md env

let add_module ?arg ?shape id presence mty env =
  add_module_declaration ~check:false ?arg ?shape id presence (md mty) env

let add_local_constraint path info env =
  { env with
    local_constraints = Path.Map.add path info env.local_constraints }

(* Insertion of bindings by name *)

let enter_value ?check ~mode name desc env =
  let id = Ident.create_local name in
  let desc = Subst.Lazy.of_value_description desc in
  let addr = value_declaration_address env id desc in
  let mode = Mode.Value.disallow_right mode in
  let env =
    store_value ?check ~mode id addr desc (Shape.leaf desc.val_uid)
      env
  in
  (id, env)

let enter_type ~scope name info env =
  let id = Ident.create_scoped ~scope name in
  let env = store_type ~check:true id info (Shape.leaf info.type_uid) env in
  (id, env)

let enter_extension ~scope ~rebind name ext env =
  let id = Ident.create_scoped ~scope name in
  let addr = extension_declaration_address env id ext in
  let shape = Shape.leaf ext.ext_uid in
  let env = store_extension ~check:true ~rebind id addr ext shape env in
  (id, env)

let enter_module_declaration ~scope ?arg ?shape s presence md env =
  let id = Ident.create_scoped ~scope s in
  (id, add_module_declaration ?arg ?shape ~check:true id presence md env)

let enter_modtype ~scope name mtd env =
  let id = Ident.create_scoped ~scope name in
  let shape = Shape.leaf mtd.mtd_uid in
  let env = store_modtype id (Subst.Lazy.of_modtype_decl mtd) shape env in
  (id, env)

let enter_class ~scope name desc env =
  let id = Ident.create_scoped ~scope name in
  let addr = class_declaration_address env id desc in
  let env = store_class id addr desc (Shape.leaf desc.cty_uid) env in
  (id, env)

let enter_cltype ~scope name desc env =
  let id = Ident.create_scoped ~scope name in
  let env = store_cltype id desc (Shape.leaf desc.clty_uid) env in
  (id, env)

let enter_module ~scope ?arg s presence mty env =
  enter_module_declaration ~scope ?arg s presence (md mty) env

let add_lock lock env =
  { env with
    values = IdTbl.add_lock lock env.values;
    modules = IdTbl.add_lock lock env.modules;
    classes = IdTbl.add_lock lock env.classes;
  }

let add_escape_lock escaping_context env =
  let lock = Escape_lock escaping_context in
  add_lock lock env

let add_share_lock shared_context env =
  let lock = Share_lock shared_context in
  add_lock lock env

let add_closure_lock ?closure_context comonadic env =
  let lock = Closure_lock
    (closure_context,
     Mode.Value.Comonadic.disallow_left comonadic)
  in
  add_lock lock env

let add_region_lock env = add_lock Region_lock env

let add_exclave_lock env = add_lock Exclave_lock env

let add_unboxed_lock env = add_lock Unboxed_lock env

(* Insertion of all components of a signature *)

let proj_shape map mod_shape item =
  match mod_shape with
  | None -> map, None
  | Some mod_shape ->
      let shape = Shape.proj mod_shape item in
      Shape.Map.add map item shape, Some shape

module Add_signature(T : Types.Wrapped)(M : sig
  val add_value: ?shape:Shape.t -> mode:(Mode.allowed * 'r0) Mode.Value.t -> Ident.t ->
    T.value_description  -> t -> t
  val add_module_declaration: ?arg:bool -> ?shape:Shape.t -> check:bool
    -> Ident.t -> module_presence -> T.module_declaration -> t -> t
  val add_modtype: ?shape:Shape.t -> Ident.t -> T.modtype_declaration -> t -> t
end) = struct
  open T

  let add_item map mod_shape comp env =
    match comp with
    | Sig_value(id, decl, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.value id) in
        map, M.add_value ?shape ~mode:Mode.Value.legacy id decl env
    | Sig_type(id, decl, _, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.type_ id) in
        map, add_type ~check:false ?shape id decl env
    | Sig_typext(id, ext, _, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.extension_constructor id) in
        map, add_extension ~check:false ?shape ~rebind:false id ext env
    | Sig_module(id, presence, md, _, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.module_ id) in
        map, M.add_module_declaration ~check:false ?shape id presence md env
    | Sig_modtype(id, decl, _)  ->
        let map, shape = proj_shape map mod_shape (Shape.Item.module_type id) in
        map, M.add_modtype ?shape id decl env
    | Sig_class(id, decl, _, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.class_ id) in
        map, add_class ?shape id decl env
    | Sig_class_type(id, decl, _, _) ->
        let map, shape = proj_shape map mod_shape (Shape.Item.class_type id) in
        map, add_cltype ?shape id decl env

  let rec add_signature map mod_shape sg env =
    match sg with
        [] -> map, env
    | comp :: rem ->
        let map, env = add_item map mod_shape comp env in
        add_signature map mod_shape rem env
end

let add_signature =
  let module M = Add_signature(Types)(struct
    let add_value ?shape ~mode id vd =
      add_value_lazy ?shape ~mode id (Subst.Lazy.of_value_description vd)
    let add_module_declaration = add_module_declaration
    let add_modtype = add_modtype
  end)
  in
  M.add_signature

let add_signature_lazy =
  let module M = Add_signature(Subst.Lazy)(struct
    let add_value ?shape ~mode = add_value_lazy ?check:None ?shape ~mode
    let add_module_declaration =
      add_module_declaration_lazy ~update_summary:true
    let add_modtype = add_modtype_lazy ~update_summary:true
  end)
  in
  M.add_signature

let enter_signature_and_shape ~scope ~parent_shape mod_shape sg env =
  let sg = Subst.signature (Rescope scope) Subst.identity sg in
  let shape, env = add_signature parent_shape mod_shape sg env in
  sg, shape, env

let enter_signature ?mod_shape ~scope sg env =
  let sg, _, env =
    enter_signature_and_shape ~scope ~parent_shape:Shape.Map.empty
      mod_shape sg env
  in
  sg, env

let enter_signature_and_shape ~scope ~parent_shape mod_shape sg env =
  enter_signature_and_shape ~scope ~parent_shape (Some mod_shape) sg env

let add_value_lazy = add_value_lazy ?shape:None
let add_value ?check ~mode id vd =
  add_value_lazy ?check ~mode id (Subst.Lazy.of_value_description vd)
let add_class = add_class ?shape:None
let add_cltype = add_cltype ?shape:None
let add_modtype_lazy = add_modtype_lazy ?shape:None
let add_modtype = add_modtype ?shape:None
let add_module_declaration_lazy ?(arg=false) =
  add_module_declaration_lazy ~arg ?shape:None ~check:false
let add_signature sg env =
  let _, env = add_signature Shape.Map.empty None sg env in
  env
let add_signature_lazy sg env =
  let _, env = add_signature_lazy Shape.Map.empty None sg env in
  env

(* Add "unbound" bindings *)

let enter_unbound_value name reason env =
  let id = Ident.create_local name in
  { env with
    values = IdTbl.add id (Val_unbound reason) env.values;
    summary = Env_value_unbound(env.summary, name, reason) }

let enter_unbound_module name reason env =
  let id = Ident.create_local name in
  { env with
    modules = IdTbl.add id (Mod_unbound reason) env.modules;
    summary = Env_module_unbound(env.summary, name, reason) }

(* Open a signature path *)

let add_components slot root env0 comps =
  let add_l w comps env0 =
    TycompTbl.add_open slot w root comps env0
  in
  let add w comps env0 = IdTbl.add_open slot w root comps env0 in
  let constrs =
    add_l (fun x -> `Constructor x) comps.comp_constrs env0.constrs
  in
  let labels =
    add_l (fun x -> `Label x) comps.comp_labels env0.labels
  in
  let values =
    add (fun x -> `Value x) comps.comp_values env0.values
  in
  let types =
    add (fun x -> `Type x) comps.comp_types env0.types
  in
  let modtypes =
    add (fun x -> `Module_type x) comps.comp_modtypes env0.modtypes
  in
  let classes =
    add (fun x -> `Class x) comps.comp_classes env0.classes
  in
  let cltypes =
    add (fun x -> `Class_type x) comps.comp_cltypes env0.cltypes
  in
  let modules =
    add (fun x -> `Module x) comps.comp_modules env0.modules
  in
  { env0 with
    summary = Env_open(env0.summary, root);
    constrs;
    labels;
    values;
    types;
    modtypes;
    classes;
    cltypes;
    modules;
  }

let open_signature slot root env0 : (_,_) result =
  match get_components_res (find_module_components root env0) with
  | Error _ -> Error `Not_found
  | exception Not_found -> Error `Not_found
  | Ok (Functor_comps _) -> Error `Functor
  | Ok (Structure_comps comps) ->
    Ok (add_components slot root env0 comps)

let remove_last_open root env0 =
  let rec filter_summary summary =
    match summary with
      Env_empty -> raise Exit
    | Env_open (s, p) ->
        if Path.same p root then s else raise Exit
    | Env_value _
    | Env_type _
    | Env_extension _
    | Env_module _
    | Env_modtype _
    | Env_class _
    | Env_cltype _
    | Env_functor_arg _
    | Env_constraints _
    | Env_persistent _
    | Env_copy_types _
    | Env_value_unbound _
    | Env_module_unbound _ ->
        map_summary filter_summary summary
  in
  match filter_summary env0.summary with
  | summary ->
      let rem_l tbl = TycompTbl.remove_last_open root tbl
      and rem tbl = IdTbl.remove_last_open root tbl in
      Some { env0 with
             summary;
             constrs = rem_l env0.constrs;
             labels = rem_l env0.labels;
             values = rem env0.values;
             types = rem env0.types;
             modtypes = rem env0.modtypes;
             classes = rem env0.classes;
             cltypes = rem env0.cltypes;
             modules = rem env0.modules; }
  | exception Exit ->
      None

(* Open a signature from a file *)

let open_pers_signature name env =
  match open_signature None (Pident(Ident.create_persistent name)) env with
  | (Ok _ | Error `Not_found as res) -> res
  | Error `Functor -> assert false
        (* a compilation unit cannot refer to a functor *)

let open_signature
    ?(used_slot = ref false)
    ?(loc = Location.none) ?(toplevel = false)
    ovf root env =
  let unused =
    match ovf with
    | Asttypes.Fresh -> Warnings.Unused_open (Path.name root)
    | Asttypes.Override -> Warnings.Unused_open_bang (Path.name root)
  in
  let warn_unused =
    Warnings.is_active unused
  and warn_shadow_id =
    Warnings.is_active (Warnings.Open_shadow_identifier ("", ""))
  and warn_shadow_lc =
    Warnings.is_active (Warnings.Open_shadow_label_constructor ("",""))
  in
  if not toplevel && not loc.Location.loc_ghost
     && (warn_unused || warn_shadow_id || warn_shadow_lc)
  then begin
    let used = used_slot in
    if warn_unused then
      !add_delayed_check_forward
        (fun () ->
           if not !used then begin
             used := true;
             Location.prerr_warning loc unused
           end
        );
    let shadowed = ref [] in
    let slot s b =
      begin match check_shadowing env b with
      | Some kind when
          ovf = Asttypes.Fresh && not (List.mem (kind, s) !shadowed) ->
          shadowed := (kind, s) :: !shadowed;
          let w =
            match kind with
            | "label" | "constructor" ->
                Warnings.Open_shadow_label_constructor (kind, s)
            | _ -> Warnings.Open_shadow_identifier (kind, s)
          in
          Location.prerr_warning loc w
      | _ -> ()
      end;
      used := true
    in
    open_signature (Some slot) root env
  end
  else open_signature None root env

(* Read a signature from a file *)
let read_signature modname cmi ~add_binding =
  let mty = read_pers_mod modname cmi ~add_binding in
  Subst.Lazy.force_signature mty

let register_parameter modname =
  Persistent_env.register_parameter !persistent_env modname


let unit_name_of_filename fn =
  match Filename.extension fn with
  | ".cmi" ->
      let modname = Unit_info.modname_from_source fn in
      if Unit_info.is_unit_name modname then Some modname
      else None
  | _ -> None

let persistent_structures_of_dir dir =
  Load_path.Dir.basenames dir
  |> List.to_seq
  |> Seq.filter_map unit_name_of_filename
  |> String.Set.of_seq

(* Save a signature to a file *)
let save_signature_with_transform cmi_transform ~alerts sg modname kind
      cmi_info =
  Btype.cleanup_abbrev ();
  Subst.reset_additional_action_type_id ();
  let sg = Subst.Lazy.of_signature sg
    |> Subst.Lazy.signature Make_local
        (Subst.with_additional_action Prepare_for_saving Subst.identity)
  in
  let cmi =
    Persistent_env.make_cmi !persistent_env modname kind sg alerts
    |> cmi_transform in
  let filename = Unit_info.Artifact.filename cmi_info in
  let pers_sig =
    Persistent_env.Persistent_signature.{ filename; cmi; visibility = Visible }
  in
  Persistent_env.save_cmi !persistent_env pers_sig;
  cmi

let save_signature ~alerts sg modname cu cmi =
  save_signature_with_transform (fun cmi -> cmi) ~alerts sg modname cu cmi

let save_signature_with_imports ~alerts sg modname cu cmi imports =
  let with_imports cmi = { cmi with cmi_crcs = imports } in
  save_signature_with_transform with_imports ~alerts sg modname cu cmi

(* Make the initial environment, without language extensions *)
let initial =
  Predef.build_initial_env
    (add_type ~check:false)
    (add_extension ~check:false ~rebind:false)
    empty

let add_language_extension_types env =
  let add ext lvl f env  =
    match Language_extension.is_at_least ext lvl with
    | true ->
      (* CR-someday poechsel: Pass a correct shape here *)
      f (add_type ?shape:None ~check:false) env
    | false -> env
  in
  lazy
    Language_extension.(env
    |> add SIMD () Predef.add_simd_extension_types
    |> add Small_numbers Stable Predef.add_small_number_extension_types
    |> add Layouts Alpha Predef.add_or_null)

(* Some predefined types are part of language extensions, and we don't want to
   make them available in the initial environment if those extensions are not
   turned on.  We can't do this at startup because command line flags haven't
   been parsed yet. So, we make the initial environment lazy.

   If language extensions are adjusted after [initial] is forced, these
   environments may be inaccurate.
*)
let initial = add_language_extension_types initial

(* Tracking usage *)

let mark_module_used uid =
  match Types.Uid.Tbl.find !module_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_modtype_used _uid = ()

let mark_value_used uid =
  match Types.Uid.Tbl.find !value_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_type_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_type_path_used env path =
  match find_type path env with
  | decl -> mark_type_used decl.type_uid
  | exception Not_found -> ()

let mark_constructor_used usage cd =
  match Types.Uid.Tbl.find !used_constructors cd.cd_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_extension_used usage ext =
  match Types.Uid.Tbl.find !used_constructors ext.ext_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_label_used usage ld =
  match Types.Uid.Tbl.find !used_labels ld.ld_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_constructor_description_used usage env cstr =
  let ty_path = Btype.cstr_type_path cstr in
  mark_type_path_used env ty_path;
  match Types.Uid.Tbl.find !used_constructors cstr.cstr_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_label_description_used usage env lbl =
  let ty_path =
    match get_desc lbl.lbl_res with
    | Tconstr(path, _, _) -> path
    | _ -> assert false
  in
  mark_type_path_used env ty_path;
  match Types.Uid.Tbl.find !used_labels lbl.lbl_uid with
  | mark -> mark usage
  | exception Not_found -> ()

let mark_class_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let mark_cltype_used uid =
  match Types.Uid.Tbl.find !type_declarations uid with
  | mark -> mark ()
  | exception Not_found -> ()

let set_value_used_callback vd callback =
  Types.Uid.Tbl.add !value_declarations vd.Subst.Lazy.val_uid callback

let set_type_used_callback td callback =
  if Uid.for_actual_declaration td.type_uid then
    let old =
      try Types.Uid.Tbl.find !type_declarations td.type_uid
      with Not_found -> ignore
    in
    Types.Uid.Tbl.replace !type_declarations td.type_uid
      (fun () -> callback old)

(* Lookup by name *)

let may_lookup_error report_errors loc env err =
  if report_errors then lookup_error loc env err
  else raise Not_found

let report_module_unbound ~errors ~loc env reason =
  match reason with
  | Mod_unbound_illegal_recursion ->
      (* see #5965 *)
    may_lookup_error errors loc env Illegal_reference_to_recursive_module

let report_value_unbound ~errors ~loc env reason lid =
  match reason with
  | Val_unbound_instance_variable ->
      may_lookup_error errors loc env (Masked_instance_variable lid)
  | Val_unbound_self ->
      may_lookup_error errors loc env (Masked_self_variable lid)
  | Val_unbound_ancestor ->
      may_lookup_error errors loc env (Masked_ancestor_variable lid)
  | Val_unbound_ghost_recursive rloc ->
      let show_hint =
        (* Only display the "missing rec" hint for non-ghost code *)
        not loc.Location.loc_ghost
        && not rloc.Location.loc_ghost
      in
      let hint =
        if show_hint then Missing_rec rloc else No_hint
      in
      may_lookup_error errors loc env (Unbound_value(lid, hint))

let use_module ~use ~loc path mda =
  if use then begin
    let comps = mda.mda_components in
    mark_module_used comps.uid;
    Misc.Stdlib.String.Map.iter
      (fun kind message ->
         let message = if message = "" then "" else "\n" ^ message in
         Location.alert ~kind loc
           (Printf.sprintf "module %s%s" (Path.name path) message)
      )
      comps.alerts
  end

let use_value ~use ~loc path vda =
  if use then begin
    let desc = vda.vda_description in
    mark_value_used desc.val_uid;
    Builtin_attributes.check_alerts loc desc.val_attributes
      (Path.name path)
  end

let use_type ~use ~loc path tda =
  if use then begin
    let decl = tda.tda_declaration in
    mark_type_used decl.type_uid;
    Builtin_attributes.check_alerts loc decl.type_attributes
      (Path.name path)
  end

let use_modtype ~use ~loc path desc =
  let open Subst.Lazy in
  if use then begin
    mark_modtype_used desc.mtd_uid;
    Builtin_attributes.check_alerts loc desc.mtd_attributes
      (Path.name path)
  end

let use_class ~use ~loc path clda =
  if use then begin
    let desc = clda.clda_declaration in
    mark_class_used desc.cty_uid;
    Builtin_attributes.check_alerts loc desc.cty_attributes
      (Path.name path)
  end

let use_cltype ~use ~loc path desc =
  if use then begin
    mark_cltype_used desc.clty_uid;
    Builtin_attributes.check_alerts loc desc.clty_attributes
      (Path.name path)
  end

let use_label ~use ~loc usage env lbl =
  if use then begin
    mark_label_description_used usage env lbl;
    Builtin_attributes.check_alerts loc lbl.lbl_attributes lbl.lbl_name;
    if is_mutating_label_usage usage then
      Builtin_attributes.check_deprecated_mutable loc lbl.lbl_attributes
        lbl.lbl_name
  end

let use_constructor_desc ~use ~loc usage env cstr =
  if use then begin
    mark_constructor_description_used usage env cstr;
    Builtin_attributes.check_alerts loc cstr.cstr_attributes cstr.cstr_name
  end

let use_constructor ~use ~loc usage env cda =
  use_constructor_desc ~use ~loc usage env cda.cda_description

type _ load =
  | Load : module_data load
  | Don't_load : unit load

let lookup_ident_module (type a) (load : a load) ~errors ~use ~loc s env =
  let path, locks, data =
    match find_name_module ~mark:use s env.modules with
    | res -> res
    | exception Not_found ->
        may_lookup_error errors loc env (Unbound_module (Lident s))
  in
  match data with
  | Mod_local mda -> begin
      use_module ~use ~loc path mda;
      match load with
      | Load -> path, locks, (mda : a)
      | Don't_load -> path, locks, (() : a)
    end
  | Mod_unbound reason ->
      report_module_unbound ~errors ~loc env reason
  | Mod_persistent -> begin
      let name = s |> Compilation_unit.Name.of_string in
      match load with
      | Don't_load ->
          check_pers_mod ~allow_hidden:false ~loc name;
          path, locks, (() : a)
      | Load -> begin
          match find_pers_mod ~allow_hidden:false name with
          | mda ->
              use_module ~use ~loc path mda;
              path, locks, (mda : a)
          | exception Not_found ->
              may_lookup_error errors loc env (Unbound_module (Lident s))
        end
    end

let escape_mode ~errors ~env ~loc ~item ~lid vmode escaping_context =
  begin match
  Mode.Regionality.submode
    (Mode.Value.proj (Comonadic Areality) vmode.mode)
    (Mode.Regionality.global)
  with
  | Ok () -> ()
  | Error _ ->
      may_lookup_error errors loc env
        (Local_value_escaping (item, lid, escaping_context))
  end;
  vmode

let share_mode ~errors ~env ~loc ~item ~lid vmode shared_context =
  match
    Mode.Linearity.submode
      (Mode.Value.proj (Comonadic Linearity) vmode.mode)
      Mode.Linearity.many
  with
  | Error _ ->
      may_lookup_error errors loc env
        (Once_value_used_in (item, lid, shared_context))
  | Ok () ->
    let mode =
      Mode.Value.join_with (Monadic Uniqueness) Mode.Uniqueness.Const.Shared
        vmode.mode
    in
    {mode; context = Some shared_context}

let closure_mode ~errors ~env ~loc ~item ~lid
  ({mode = {Mode.monadic; comonadic}; _} as vmode) closure_context comonadic0 =
  begin
    match
      Mode.Value.Comonadic.submode comonadic comonadic0
    with
    | Error e ->
        may_lookup_error errors loc env
          (Value_used_in_closure (item, lid, e, closure_context))
    | Ok () -> ()
  end;
  let monadic =
    Mode.Value.Monadic.join
      [ monadic;
        Mode.Value.comonadic_to_monadic comonadic0 ]
  in
  {vmode with mode = {monadic; comonadic}}

let exclave_mode ~errors ~env ~loc ~item ~lid vmode =
  match
  Mode.Regionality.submode
    (Mode.Value.proj (Comonadic Areality) vmode.mode)
    Mode.Regionality.regional
with
| Ok () ->
  let mode = vmode.mode |> Mode.value_to_alloc_r2l |> Mode.alloc_as_value in
  {vmode with mode}
| Error _ ->
    may_lookup_error errors loc env
      (Local_value_used_in_exclave (item, lid))

let region_mode vmode =
  let mode =
    vmode.mode |> Mode.value_to_alloc_r2l |> Mode.alloc_to_value_l2r
  in
  {vmode with mode}

let unboxed_type ~errors ~env ~loc ~lid ty =
  match ty with
  | None -> ()
  | Some ty ->
    (* The type is the type of a variable in the environment. It thus is likely generic. Despite
       the fact that instantiated variables work better in [constrain_type_jkind] (because they
       can be assigned more specific jkinds), we actually want to work on these generic types
       here. After all, it's the value in the environment that is getting captured by the object,
       not a specific instance of that variable. *)
    match !constrain_type_jkind env ty Jkind.Builtin.(value_or_null ~why:Captured_in_object) with
    | Ok () -> ()
    | Result.Error err ->
      may_lookup_error errors loc env (Non_value_used_in_object (lid, ty, err))

(** Takes the [mode] and [ty] of a value at definition site, walks through the
    list of locks and constrains [mode] and [ty]. Return the access mode of the
    value allowed by the locks.

    [ty] is optional as the function works on modules and classes as well, for
    which [ty] should be [None]. *)
let walk_locks ~errors ~loc ~env ~item ~lid mode ty locks =
  let vmode = { mode; context = None } in
  List.fold_left
    (fun vmode lock ->
      match lock with
      | Region_lock -> region_mode vmode
      | Escape_lock escaping_context ->
          escape_mode ~errors ~env ~loc ~item ~lid vmode escaping_context
      | Share_lock shared_context ->
          share_mode ~errors ~env ~loc ~item ~lid vmode shared_context
      | Closure_lock (closure_context, comonadic) ->
          closure_mode ~errors ~env ~loc ~item ~lid vmode closure_context comonadic
      | Exclave_lock ->
          exclave_mode ~errors ~env ~loc ~item ~lid vmode
      | Unboxed_lock ->
          unboxed_type ~errors ~env ~loc ~lid ty;
          vmode
    ) vmode locks

let lookup_ident_value ~errors ~use ~loc name env =
  match IdTbl.find_name_and_locks wrap_value ~mark:use name env.values with
  | Ok (path, locks, Val_bound vda) ->
      use_value ~use ~loc path vda;
      path, locks, vda
  | Ok (_, _, Val_unbound reason) ->
      report_value_unbound ~errors ~loc env reason (Lident name)
  | Error _ ->
      may_lookup_error errors loc env (Unbound_value (Lident name, No_hint))

let lookup_ident_type ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.types with
  | (path, data) as res ->
      use_type ~use ~loc path data;
      res
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_type (Lident s))

let lookup_ident_modtype ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.modtypes with
  | (path, data) ->
      use_modtype ~use ~loc path data.mtda_declaration;
      (path, data.mtda_declaration)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_modtype (Lident s))

let lookup_ident_class ~errors ~use ~loc s env =
  match IdTbl.find_name_and_locks wrap_identity ~mark:use s env.classes with
  | Ok (path, locks, clda) ->
      use_class ~use ~loc path clda;
      path, locks, clda.clda_declaration
  | Error _ ->
      may_lookup_error errors loc env (Unbound_class (Lident s))

let lookup_ident_cltype ~errors ~use ~loc s env =
  match IdTbl.find_name wrap_identity ~mark:use s env.cltypes with
  | path, cltda ->
      use_cltype ~use ~loc path cltda.cltda_declaration;
      path, cltda.cltda_declaration
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_cltype (Lident s))

let lookup_all_ident_labels ~errors ~use ~loc usage s env =
  match TycompTbl.find_all ~mark:use s env.labels with
  | [] -> may_lookup_error errors loc env (Unbound_label (Lident s))
  | lbls -> begin
      List.map
        (fun (lbl, use_fn) ->
           let use_fn () =
             use_label ~use ~loc usage env lbl;
             use_fn ()
           in
           (lbl, use_fn))
        lbls
    end

let lookup_all_ident_constructors ~errors ~use ~loc usage s env =
  match TycompTbl.find_all ~mark:use s env.constrs with
  | [] -> may_lookup_error errors loc env (Unbound_constructor (Lident s))
  | cstrs ->
      List.map
        (fun (cda, use_fn) ->
           let use_fn () =
             use_constructor ~use ~loc usage env cda;
             use_fn ()
           in
           (cda.cda_description, use_fn))
        cstrs

let rec lookup_module_components ~errors ~use ~loc lid env =
  match lid with
  | Lident s ->
      let path, locks, data = lookup_ident_module Load ~errors ~use ~loc s env in
      path, locks, data.mda_components
  | Ldot(l, s) ->
      let path, locks, data = lookup_dot_module ~errors ~use ~loc l s env in
      path, locks, data.mda_components
  | Lapply _ as lid ->
      let f_path, f_comp, arg = lookup_apply ~errors ~use ~loc lid env in
      let comps =
        !components_of_functor_appl' ~loc ~f_path ~f_comp ~arg env in
      Papply (f_path, arg), [], comps

and lookup_structure_components ~errors ~use ~loc lid env =
  let path, locks, comps = lookup_module_components ~errors ~use ~loc lid env in
  match get_components_res comps with
  | Ok (Structure_comps comps) -> path, locks, comps
  | Ok (Functor_comps _) ->
      may_lookup_error errors loc env (Functor_used_as_structure lid)
  | Error No_components_abstract ->
      may_lookup_error errors loc env (Abstract_used_as_structure lid)
  | Error (No_components_alias p) ->
      may_lookup_error errors loc env (Cannot_scrape_alias(lid, p))

and get_functor_components ~errors ~loc lid env comps =
  match get_components_res comps with
  | Ok (Functor_comps fcomps) -> begin
      match fcomps.fcomp_arg with
      | Unit -> (* PR#7611 *)
          may_lookup_error errors loc env (Generative_used_as_applicative lid)
      | Named (_, arg) -> fcomps, arg
    end
  | Ok (Structure_comps _) ->
      may_lookup_error errors loc env (Structure_used_as_functor lid)
  | Error No_components_abstract ->
      may_lookup_error errors loc env (Abstract_used_as_functor lid)
  | Error (No_components_alias p) ->
      may_lookup_error errors loc env (Cannot_scrape_alias(lid, p))

and lookup_all_args ~errors ~use ~loc lid0 env =
  let rec loop_lid_arg args = function
    | Lident _ | Ldot _ as f_lid ->
        (f_lid, args)
    | Lapply (f_lid, arg_lid) ->
        let arg_path, arg_md, arg_vmode =
          lookup_module ~errors ~use ~lock:false ~loc arg_lid env
        in
        loop_lid_arg ((f_lid,arg_path,arg_md.md_type,arg_vmode)::args) f_lid
  in
  loop_lid_arg [] lid0

and lookup_apply ~errors ~use ~loc lid0 env =
  let f0_lid, args0 = lookup_all_args ~errors ~use ~loc lid0 env in
  let args_for_errors = List.map (fun (_,p,mty,vmode) -> (p,mty,vmode.mode)) args0 in
  let f0_path, _, f0_comp =
    lookup_module_components ~errors ~use ~loc f0_lid env
  in
  let check_one_apply ~errors ~loc ~f_lid ~f_comp ~arg_path ~arg_mty ~arg_mode
    env =
    let f_comp, param_mty =
      get_functor_components ~errors ~loc f_lid env f_comp
    in
    check_functor_appl
      ~errors ~loc ~lid_whole_app:lid0
      ~f0_path ~args:args_for_errors ~f_comp
      ~arg_path ~arg_mty ~arg_mode:arg_mode.mode ~param_mty
      env;
    arg_path, f_comp
  in
  let rec check_apply ~path:f_path ~comp:f_comp = function
    | [] -> invalid_arg "Env.lookup_apply: empty argument list"
    | [ f_lid, arg_path, arg_mty, arg_mode ] ->
        let arg_path, comps =
          check_one_apply ~errors ~loc ~f_lid ~f_comp
            ~arg_path ~arg_mty ~arg_mode env
        in
        f_path, comps, arg_path
    | (f_lid, arg_path, arg_mty, arg_mode) :: args ->
        let arg_path, f_comp =
          check_one_apply ~errors ~loc ~f_lid ~f_comp
            ~arg_path ~arg_mty ~arg_mode env
        in
        let comp =
          !components_of_functor_appl' ~loc ~f_path ~f_comp ~arg:arg_path env
        in
        let path = Papply (f_path, arg_path) in
        check_apply ~path ~comp args
  in
  check_apply ~path:f0_path ~comp:f0_comp args0

and lookup_module ~errors ~use ~lock ~loc lid env =
  let path, locks, md =
    match lid with
    | Lident s ->
        let path, locks, data = lookup_ident_module Load ~errors ~use ~loc s env in
        let md = Subst.Lazy.force_module_decl data.mda_declaration in
        path, locks, md
    | Ldot(l, s) ->
        let path, locks, data = lookup_dot_module ~errors ~use ~loc l s env in
        let md = Subst.Lazy.force_module_decl data.mda_declaration in
        path, locks, md
    | Lapply _ as lid ->
        let path_f, comp_f, path_arg = lookup_apply ~errors ~use ~loc lid env in
        let md = md (modtype_of_functor_appl comp_f path_f path_arg) in
        Papply(path_f, path_arg), [], md
  in
  let vmode =
    if lock then
      walk_locks ~errors ~loc ~env ~item:Module ~lid mda_mode None locks
    else
      mode_default mda_mode
  in
  path, md, vmode

and lookup_dot_module ~errors ~use ~loc l s env =
  let p, locks, comps = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_modules with
  | mda ->
      let path = Pdot(p, s) in
      use_module ~use ~loc path mda;
      (path, locks, mda)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_module (Ldot(l, s)))

let lookup_dot_value ~errors ~use ~loc l s env =
  let (path, locks, comps) =
    lookup_structure_components ~errors ~use ~loc l env
  in
  match NameMap.find s comps.comp_values with
  | vda ->
      let path = Pdot(path, s) in
      use_value ~use ~loc path vda;
      (path, locks, vda)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_value (Ldot(l, s), No_hint))

let lookup_dot_type ~errors ~use ~loc l s env =
  let (p, _, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_types with
  | tda ->
      let path = Pdot(p, s) in
      use_type ~use ~loc path tda;
      (path, tda)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_type (Ldot(l, s)))

let lookup_dot_modtype ~errors ~use ~loc l s env =
  let (p, _, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_modtypes with
  | mta ->
      let path = Pdot(p, s) in
      use_modtype ~use ~loc path mta.mtda_declaration;
      (path, mta.mtda_declaration)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_modtype (Ldot(l, s)))

let lookup_dot_class ~errors ~use ~loc l s env =
  let (p, locks, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_classes with
  | clda ->
      let path = Pdot(p, s) in
      use_class ~use ~loc path clda;
      (path, locks, clda.clda_declaration)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_class (Ldot(l, s)))

let lookup_dot_cltype ~errors ~use ~loc l s env =
  let (p, _, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_cltypes with
  | cltda ->
      let path = Pdot(p, s) in
      use_cltype ~use ~loc path cltda.cltda_declaration;
      (path, cltda.cltda_declaration)
  | exception Not_found ->
      may_lookup_error errors loc env (Unbound_cltype (Ldot(l, s)))

let lookup_all_dot_labels ~errors ~use ~loc usage l s env =
  let (_, _, comps) = lookup_structure_components ~errors ~use ~loc l env in
  match NameMap.find s comps.comp_labels with
  | [] | exception Not_found ->
      may_lookup_error errors loc env (Unbound_label (Ldot(l, s)))
  | lbls ->
      List.map
        (fun lbl ->
           let use_fun () = use_label ~use ~loc usage env lbl in
           (lbl, use_fun))
        lbls

let lookup_all_dot_constructors ~errors ~use ~loc usage l s env =
  match l with
  | Longident.Lident "*predef*" ->
      (* Hack to support compilation of default arguments *)
      lookup_all_ident_constructors
        ~errors ~use ~loc usage s (Lazy.force initial)
  | _ ->
      let (_, _, comps) = lookup_structure_components ~errors ~use ~loc l env in
      match NameMap.find s comps.comp_constrs with
      | [] | exception Not_found ->
          may_lookup_error errors loc env (Unbound_constructor (Ldot(l, s)))
      | cstrs ->
          List.map
            (fun cda ->
               let use_fun () = use_constructor ~use ~loc usage env cda in
               (cda.cda_description, use_fun))
            cstrs

(* General forms of the lookup functions *)

let lookup_module_path ~errors ~use ~lock ~loc ~load lid env : Path.t * _ =
  let path, locks =
    match lid with
    | Lident s ->
        if !Clflags.transparent_modules && not load then
          let path, locks, _ =
            lookup_ident_module Don't_load ~errors ~use ~loc s env
          in
          path, locks
        else
          let path, locks, _ =
            lookup_ident_module Load ~errors ~use ~loc s env
          in
          path, locks
    | Ldot(l, s) ->
        let path, locks, _ = lookup_dot_module ~errors ~use ~loc l s env in
        path, locks
    | Lapply _ as lid ->
        let path_f, _comp_f, path_arg = lookup_apply ~errors ~use ~loc lid env in
        Papply(path_f, path_arg), []
  in
  let vmode =
    if lock then
      walk_locks ~errors ~loc ~env ~item:Module ~lid mda_mode None locks
    else
      mode_default mda_mode
  in
  path, vmode

let lookup_value_lazy ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_value ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_value ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_value ~errors ~use ~loc lid env =
  check_value_name (Longident.last lid) loc;
  let path, locks, vda =
    lookup_value_lazy ~errors ~use ~loc lid env
  in
  (* There can be locks between the definition and a use of a value. For
  example, if a function closes over a value, there will be Closure_lock between
  the value's definition and the value's use in the function. Walking the locks
  will constrain the function and the value's modes accrodingly.

  Here, we apply the modalities to acquire the mode of the value at the
  definition site, using which we walk the locks. That means the surrounding
  closure would be closing over the value instead of the module. The latter can
  be achieved by walking the locks before apply modalities.

  Our route provides better ergonomics, but is dangerous as it doesn't reflect
  the real runtime behaviour. With the current set-up, it is sound. *)
  let vd, mode = normalize_vda_mode vda in
  let vd = Subst.Lazy.force_value_description vd in
  let vmode =
    if use then
      walk_locks ~errors ~loc ~env ~item:Value ~lid mode (Some vd.val_type) locks
    else
      mode_default mode
  in
  path, vd, vmode

let lookup_type_full ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_type ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_type ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_type ~errors ~use ~loc lid env =
  let (path, tda) = lookup_type_full ~errors ~use ~loc lid env in
  path, tda.tda_declaration

let lookup_modtype_lazy ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_modtype ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_modtype ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_modtype ~errors ~use ~loc lid env =
  let (path, mt) = lookup_modtype_lazy ~errors ~use ~loc lid env in
  path, Subst.Lazy.force_modtype_decl mt

let lookup_class ~errors ~use ~loc lid env =
  let path, locks, cld =
    match lid with
    | Lident s -> lookup_ident_class ~errors ~use ~loc s env
    | Ldot(l, s) -> lookup_dot_class ~errors ~use ~loc l s env
    | Lapply _ -> assert false
  in
  let vmode =
    if use then
      walk_locks ~errors ~loc ~env ~item:Class ~lid clda_mode None locks
    else
      mode_default clda_mode
  in
  path, cld, vmode

let lookup_cltype ~errors ~use ~loc lid env =
  match lid with
  | Lident s -> lookup_ident_cltype ~errors ~use ~loc s env
  | Ldot(l, s) -> lookup_dot_cltype ~errors ~use ~loc l s env
  | Lapply _ -> assert false

let lookup_all_labels ~errors ~use ~loc usage lid env =
  match lid with
  | Lident s -> lookup_all_ident_labels ~errors ~use ~loc usage s env
  | Ldot(l, s) -> lookup_all_dot_labels ~errors ~use ~loc usage l s env
  | Lapply _ -> assert false

let lookup_label ~errors ~use ~loc usage lid env =
  match lookup_all_labels ~errors ~use ~loc usage lid env with
  | [] -> assert false
  | (desc, use) :: _ -> use (); desc

let lookup_all_labels_from_type ~use ~loc usage ty_path env =
  match find_type_descrs ty_path env with
  | exception Not_found -> []
  | Type_variant _ | Type_abstract _ | Type_open -> []
  | Type_record (lbls, _) ->
      List.map
        (fun lbl ->
           let use_fun () = use_label ~use ~loc usage env lbl in
           (lbl, use_fun))
        lbls

let lookup_all_constructors ~errors ~use ~loc usage lid env =
  match lid with
  | Lident s -> lookup_all_ident_constructors ~errors ~use ~loc usage s env
  | Ldot(l, s) -> lookup_all_dot_constructors ~errors ~use ~loc usage l s env
  | Lapply _ -> assert false

let lookup_constructor ~errors ~use ~loc usage lid env =
  match lookup_all_constructors ~errors ~use ~loc usage lid env with
  | [] -> assert false
  | (desc, use) :: _ -> use (); desc

let lookup_all_constructors_from_type ~use ~loc usage ty_path env =
  match find_type_descrs ty_path env with
  | exception Not_found -> []
  | Type_record _ | Type_abstract _ | Type_open -> []
  | Type_variant (cstrs, _) ->
      List.map
        (fun cstr ->
           let use_fun () =
             use_constructor_desc ~use ~loc usage env cstr
           in
           (cstr, use_fun))
        cstrs

(* Lookup functions that do not mark the item as used or
   warn if it has alerts, and raise [Not_found] rather
   than report errors *)

let find_module_by_name lid env =
  let loc = Location.(in_file !input_name) in
  let path, desc, _ =
    lookup_module ~errors:false ~use:false ~lock:false ~loc lid env
  in
  path, desc

let find_value_by_name lid env =
  let loc = Location.(in_file !input_name) in
  let path, desc, _ = lookup_value ~errors:false ~use:false ~loc lid env in
  path, desc

let find_type_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_type ~errors:false ~use:false ~loc lid env

let find_modtype_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_modtype ~errors:false ~use:false ~loc lid env

let find_class_by_name lid env =
  let loc = Location.(in_file !input_name) in
  let path, desc, _ = lookup_class ~errors:false ~use:false ~loc lid env in
  path, desc

let find_cltype_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_cltype ~errors:false ~use:false ~loc lid env

let find_constructor_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_constructor ~errors:false ~use:false ~loc Positive lid env

let find_label_by_name lid env =
  let loc = Location.(in_file !input_name) in
  lookup_label ~errors:false ~use:false ~loc Projection lid env

(* Stable name lookup for printing *)

let find_index_tbl ident tbl  =
  let lbs = IdTbl.find_all_idents (Ident.name ident) tbl in
  let find_ident (n,p) = match p with
    | Some id -> if Ident.same ident id then Some n else None
    | _ -> None
  in
  Seq.find_map find_ident @@ Seq.mapi (fun i x -> i,x) lbs

let find_value_index id env = find_index_tbl id env.values
let find_type_index id env = find_index_tbl id env.types
let find_module_index id env = find_index_tbl id env.modules
let find_modtype_index id env = find_index_tbl id env.modtypes
let find_class_index id env = find_index_tbl id env.classes
let find_cltype_index id env = find_index_tbl id env.cltypes

(* Ordinary lookup functions *)

let lookup_module_path ?(use=true) ?(lock=use) ~loc ~load lid env =
  let path, vmode =
    lookup_module_path ~errors:true ~use ~lock ~loc ~load lid env
  in
  path, vmode.mode

let lookup_module ?(use=true) ?(lock=use) ~loc lid env =
  let path, desc, vmode = lookup_module ~errors:true ~use ~lock ~loc lid env in
  path, desc, vmode.mode

let lookup_value ?(use=true) ~loc lid env =
  lookup_value ~errors:true ~use ~loc lid env

let lookup_type ?(use=true) ~loc lid env =
  lookup_type ~errors:true ~use ~loc lid env

let lookup_modtype ?(use=true) ~loc lid env =
  lookup_modtype ~errors:true ~use ~loc lid env

let lookup_modtype_path ?(use=true) ~loc lid env =
  fst (lookup_modtype_lazy ~errors:true ~use ~loc lid env)

let lookup_class ?(use=true) ~loc lid env =
  let path, desc, vmode = lookup_class ~errors:true ~use ~loc lid env in
  path, desc, vmode.mode

let lookup_cltype ?(use=true) ~loc lid env =
  lookup_cltype ~errors:true ~use ~loc lid env

let lookup_all_constructors ?(use=true) ~loc usage lid env =
  match lookup_all_constructors ~errors:true ~use ~loc usage lid env with
  | exception Error(Lookup_error(loc', env', err)) ->
      (Error(loc', env', err) : _ result)
  | cstrs -> Ok cstrs

let lookup_constructor ?(use=true) ~loc lid env =
  lookup_constructor ~errors:true ~use ~loc lid env

let lookup_all_constructors_from_type ?(use=true) ~loc usage ty_path env =
  lookup_all_constructors_from_type ~use ~loc usage ty_path env

let lookup_all_labels ?(use=true) ~loc usage lid env =
  match lookup_all_labels ~errors:true ~use ~loc usage lid env with
  | exception Error(Lookup_error(loc', env', err)) ->
      (Error(loc', env', err) : _ result)
  | lbls -> Ok lbls

let lookup_label ?(use=true) ~loc lid env =
  lookup_label ~errors:true ~use ~loc lid env

let lookup_all_labels_from_type ?(use=true) ~loc usage ty_path env =
  lookup_all_labels_from_type ~use ~loc usage ty_path env

let lookup_instance_variable ?(use=true) ~loc name env =
  match IdTbl.find_name_and_locks wrap_value ~mark:use name env.values with
  | Ok (path, _, Val_bound vda) -> begin
      let desc = vda.vda_description in
      match desc.val_kind with
      | Val_ivar(mut, cl_num) ->
          use_value ~use ~loc path vda;
          path, mut, cl_num, Subst.Lazy.force_type_expr desc.val_type
      | _ ->
          lookup_error loc env (Not_an_instance_variable name)
    end
  | Ok (_, _, Val_unbound Val_unbound_instance_variable) ->
      lookup_error loc env (Masked_instance_variable (Lident name))
  | Ok (_, _, Val_unbound Val_unbound_self) ->
      lookup_error loc env (Not_an_instance_variable name)
  | Ok (_, _, Val_unbound Val_unbound_ancestor) ->
      lookup_error loc env (Not_an_instance_variable name)
  | Ok (_, _, Val_unbound Val_unbound_ghost_recursive _) ->
      lookup_error loc env (Unbound_instance_variable name)
  | Error _ ->
      lookup_error loc env (Unbound_instance_variable name)

(* Checking if a name is bound *)

let bound_module name env =
  match IdTbl.find_name_and_locks wrap_module ~mark:false name env.modules with
  | Ok _ -> true
  | Error _ ->
      if Current_unit_name.is name then false
      else begin
        match
          find_pers_mod ~allow_hidden:false
            (name |> Compilation_unit.Name.of_string)
        with
        | _ -> true
        | exception Not_found -> false
      end

let bound wrap proj name env =
  match IdTbl.find_name_and_locks wrap ~mark:false name (proj env) with
  | Ok _ -> true
  | Error _ -> false

let bound_value name env =
  bound wrap_value (fun env -> env.values) name env

let bound_type name env =
  bound wrap_identity (fun env -> env.types) name env

let bound_modtype name env =
  bound wrap_identity (fun env -> env.modtypes) name env

let bound_class name env =
  bound wrap_identity (fun env -> env.classes) name env

let bound_cltype name env =
  bound wrap_identity (fun env -> env.cltypes) name env

(* Folding on environments *)

let find_all wrap proj1 proj2 f lid env acc =
  match lid with
  | None ->
      IdTbl.fold_name wrap
        (fun name (p, data) acc -> f name p data acc)
        (proj1 env) acc
  | Some l ->
      let p, _locks, desc =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun s data acc -> f s (Pdot (p, s)) (wrap data) acc)
            (proj2 c) acc
      | Functor_comps _ ->
          acc
      end

let find_all_simple_list proj1 proj2 f lid env acc =
  match lid with
  | None ->
      TycompTbl.fold_name
        (fun data acc -> f data acc)
        (proj1 env) acc
  | Some l ->
      let (_p, _locks, desc) =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun _s comps acc ->
               match comps with
               | [] -> acc
               | data :: _ -> f data acc)
            (proj2 c) acc
      | Functor_comps _ ->
          acc
      end

let fold_modules f lid env acc =
  match lid with
  | None ->
      IdTbl.fold_name wrap_module
        (fun name (p, entry) acc ->
           match entry with
           | Mod_unbound _ -> acc
           | Mod_local mda ->
               let md =
                 Subst.Lazy.force_module_decl mda.mda_declaration
               in
               f name p md acc
           | Mod_persistent ->
               let modname = name |> Compilation_unit.Name.of_string in
               match Persistent_env.find_in_cache !persistent_env modname with
               | None -> acc
               | Some mda ->
                   let md =
                     Subst.Lazy.force_module_decl mda.mda_declaration
                   in
                   f name p md acc)
        env.modules
        acc
  | Some l ->
      let p, _locks, desc =
        lookup_module_components
          ~errors:false ~use:false ~loc:Location.none l env
      in
      begin match get_components desc with
      | Structure_comps c ->
          NameMap.fold
            (fun s mda acc ->
               let md =
                 Subst.Lazy.force_module_decl mda.mda_declaration
               in
               f s (Pdot (p, s)) md acc)
            c.comp_modules
            acc
      | Functor_comps _ ->
          acc
      end

let fold_values f =
  find_all wrap_value (fun env -> env.values) (fun sc -> sc.comp_values)
    (fun k p ve acc ->
       match ve with
       | Val_unbound _ -> acc
       | Val_bound vda ->
          let vd, mode = normalize_vda_mode vda in
          f k p vd mode acc)
and fold_constructors f =
  find_all_simple_list (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
    (fun cda acc -> f cda.cda_description acc)
and fold_labels f =
  find_all_simple_list (fun env -> env.labels) (fun sc -> sc.comp_labels) f
and fold_types f =
  find_all wrap_identity
    (fun env -> env.types) (fun sc -> sc.comp_types)
    (fun k p tda acc -> f k p tda.tda_declaration acc)
and fold_modtypes f =
  let f l path data acc = f l path (Subst.Lazy.force_modtype_decl data) acc in
  find_all wrap_identity
    (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)
    (fun k p mta acc -> f k p mta.mtda_declaration acc)
and fold_classes f =
  find_all wrap_identity (fun env -> env.classes) (fun sc -> sc.comp_classes)
    (fun k p clda acc -> f k p clda.clda_declaration acc)
and fold_cltypes f =
  find_all wrap_identity
    (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes)
    (fun k p cltda acc -> f k p cltda.cltda_declaration acc)

let filter_non_loaded_persistent f env =
  let to_remove =
    IdTbl.fold_name wrap_module
      (fun name (_, entry) acc ->
         match entry with
         | Mod_local _ -> acc
         | Mod_unbound _ -> acc
         | Mod_persistent ->
             let modname = name |> Compilation_unit.Name.of_string in
             match Persistent_env.find_in_cache !persistent_env modname with
             | Some _ -> acc
             | None ->
                 if f (Ident.create_persistent name) then
                   acc
                 else
                   String.Set.add name acc)
      env.modules
      String.Set.empty
  in
  let remove_ids tbl ids =
    String.Set.fold
      (fun name tbl -> IdTbl.remove (Ident.create_persistent name) tbl)
      ids
      tbl
  in
  let rec filter_summary summary ids =
    if String.Set.is_empty ids then
      summary
    else
      match summary with
        Env_persistent (s, id) when String.Set.mem (Ident.name id) ids ->
          filter_summary s (String.Set.remove (Ident.name id) ids)
      | Env_empty
      | Env_value _
      | Env_type _
      | Env_extension _
      | Env_module _
      | Env_modtype _
      | Env_class _
      | Env_cltype _
      | Env_open _
      | Env_functor_arg _
      | Env_constraints _
      | Env_copy_types _
      | Env_persistent _
      | Env_value_unbound _
      | Env_module_unbound _ ->
          map_summary (fun s -> filter_summary s ids) summary
  in
  { env with
    modules = remove_ids env.modules to_remove;
    summary = filter_summary env.summary to_remove;
  }

(* Return the environment summary *)

let summary env =
  if Path.Map.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env = s_ref empty
let last_reduced_env = s_ref empty

let keep_only_summary env =
  if !last_env == env then !last_reduced_env
  else begin
    let new_env =
      {
       empty with
       summary = env.summary;
       local_constraints = env.local_constraints;
       flags = env.flags;
      }
    in
    last_env := env;
    last_reduced_env := new_env;
    new_env
  end


let env_of_only_summary env_from_summary env =
  let new_env = env_from_summary env.summary Subst.identity in
  { new_env with
    local_constraints = env.local_constraints;
    flags = env.flags;
  }

(* Error report *)

open Format

(* Forward declarations *)

let print_longident =
  ref ((fun _ _ -> assert false) : formatter -> Longident.t -> unit)

let print_path =
  ref ((fun _ _ -> assert false) : formatter -> Path.t -> unit)

let print_type_expr =
  ref ((fun _ _ -> assert false) : formatter -> Types.type_expr -> unit)

let spellcheck ppf extract env lid =
  let choices ~path name = Misc.spellcheck (extract path env) name in
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
       Misc.did_you_mean ppf (fun () -> choices ~path:None s)
    | Longident.Ldot (r, s) ->
       Misc.did_you_mean ppf (fun () -> choices ~path:(Some r) s)

let spellcheck_name ppf extract env name =
  Misc.did_you_mean ppf
    (fun () -> Misc.spellcheck (extract env) name)

let extract_values path env =
  fold_values (fun name _ _ _ acc -> name :: acc) path env []
let extract_types path env =
  fold_types (fun name _ _ acc -> name :: acc) path env []
let extract_modules path env =
  fold_modules (fun name _ _ acc -> name :: acc) path env []
let extract_constructors path env =
  fold_constructors (fun desc acc -> desc.cstr_name :: acc) path env []
let extract_labels path env =
  fold_labels (fun desc acc -> desc.lbl_name :: acc) path env []
let extract_classes path env =
  fold_classes (fun name _ _ acc -> name :: acc) path env []
let extract_modtypes path env =
  fold_modtypes (fun name _ _ acc -> name :: acc) path env []
let extract_cltypes path env =
  fold_cltypes (fun name _ _ acc -> name :: acc) path env []
let extract_instance_variables env =
  fold_values
    (fun name _ descr _ acc ->
       match descr.val_kind with
       | Val_ivar _ -> name :: acc
       | _ -> acc) None env []

let string_of_escaping_context : escaping_context -> string =
  function
  | Letop -> "a letop"
  | Probe -> "a probe"
  | Class -> "a class"
  | Module -> "a module"
  | Lazy -> "a lazy expression"

let string_of_shared_context : shared_context -> string =
  function
  | For_loop -> "a for loop"
  | While_loop -> "a while loop"
  | Letop -> "a letop"
  | Closure -> "a closure that is not once"
  | Comprehension -> "a comprehension"
  | Class -> "a class"
  | Module -> "a module"
  | Probe -> "a probe"
  | Lazy -> "a lazy expression"

let sharedness_hint ppf : shared_context -> _ = function
  | For_loop ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the for-loop.@]"
  | While_loop ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the while-loop.@]"
  | Comprehension ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the comprehension.@]"
  | Letop ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the let-op.@]"
  | Class ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined in a class.@]"
  | Closure ->
    Format.fprintf ppf
        "@[Hint: This identifier was defined outside of the current closure.@ \
          Either this closure has to be once, or the identifier can be used only@ \
          as shared.@]"
  | Module ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined in a module.@]"
  | Probe ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined outside of the probe.@]"
  | Lazy ->
    Format.fprintf ppf
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined outside of the lazy expression.@]"

let print_lock_item ppf (item, lid) =
  match item with
  | Module -> fprintf ppf "Modules are"
  | Class -> fprintf ppf "Classes are"
  | Value -> fprintf ppf "The value %a is"
      (Style.as_inline_code !print_longident) lid

module Style = Misc.Style

let report_lookup_error _loc env ppf = function
  | Unbound_value(lid, hint) -> begin
      fprintf ppf "Unbound value %a"
        (Style.as_inline_code !print_longident) lid;
      spellcheck ppf extract_values env lid;
      match hint with
      | No_hint -> ()
      | Missing_rec def_loc ->
          let (_, line, _) =
            Location.get_pos_info def_loc.Location.loc_start
          in
          fprintf ppf
            "@.@[@{<hint>Hint@}: If this is a recursive definition,@ \
             you should add the %a keyword on line %i@]"
            Style.inline_code "rec"
            line
    end
  | Unbound_type lid ->
      fprintf ppf "Unbound type constructor %a"
        (Style.as_inline_code !print_longident) lid;
      spellcheck ppf extract_types env lid;
  | Unbound_module lid -> begin
      fprintf ppf "Unbound module %a"
        (Style.as_inline_code !print_longident) lid;
       match find_modtype_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_modules env lid;
      | _ ->
         fprintf ppf
           "@.@[@{<hint>Hint@}: There is a module type named %a, %s@]"
           (Style.as_inline_code !print_longident) lid
           "but module types are not modules"
    end
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a"
        (Style.as_inline_code !print_longident) lid;
      spellcheck ppf extract_constructors env lid;
  | Unbound_label lid ->
      fprintf ppf "Unbound record field %a"
        (Style.as_inline_code !print_longident) lid;
      spellcheck ppf extract_labels env lid;
  | Unbound_class lid -> begin
      fprintf ppf "Unbound class %a"
        (Style.as_inline_code !print_longident) lid;
      match find_cltype_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_classes env lid;
      | _ ->
         fprintf ppf
           "@.@[@{<hint>Hint@}: There is a class type named %a, %s@]"
           (Style.as_inline_code !print_longident) lid
           "but classes are not class types"
    end
  | Unbound_modtype lid -> begin
      fprintf ppf "Unbound module type %a"
        (Style.as_inline_code !print_longident) lid;
      match find_module_by_name lid env with
      | exception Not_found -> spellcheck ppf extract_modtypes env lid;
      | _ ->
         fprintf ppf
           "@.@[@{<hint>Hint@}: There is a module named %a, %s@]"
           (Style.as_inline_code !print_longident) lid
           "but modules are not module types"
    end
  | Unbound_cltype lid ->
      fprintf ppf "Unbound class type %a"
        (Style.as_inline_code !print_longident) lid;
      spellcheck ppf extract_cltypes env lid
  | Unbound_instance_variable s ->
      fprintf ppf "Unbound instance variable %a" Style.inline_code s;
      spellcheck_name ppf extract_instance_variables env s;
  | Not_an_instance_variable s ->
      fprintf ppf "The value %a is not an instance variable"
        Style.inline_code s;
      spellcheck_name ppf extract_instance_variables env s;
  | Masked_instance_variable lid ->
      fprintf ppf
        "The instance variable %a@ \
         cannot be accessed from the definition of another instance variable"
        (Style.as_inline_code !print_longident) lid
  | Masked_self_variable lid ->
      fprintf ppf
        "The self variable %a@ \
         cannot be accessed from the definition of an instance variable"
        (Style.as_inline_code !print_longident) lid
  | Masked_ancestor_variable lid ->
      fprintf ppf
        "The ancestor variable %a@ \
         cannot be accessed from the definition of an instance variable"
       (Style.as_inline_code !print_longident) lid
  | Illegal_reference_to_recursive_module ->
     fprintf ppf "Illegal recursive module reference"
  | Structure_used_as_functor lid ->
      fprintf ppf "@[The module %a is a structure, it cannot be applied@]"
        (Style.as_inline_code !print_longident) lid
  | Abstract_used_as_functor lid ->
      fprintf ppf "@[The module %a is abstract, it cannot be applied@]"
        (Style.as_inline_code !print_longident) lid
  | Functor_used_as_structure lid ->
      fprintf ppf "@[The module %a is a functor, \
                   it cannot have any components@]"
        (Style.as_inline_code !print_longident) lid
  | Abstract_used_as_structure lid ->
      fprintf ppf "@[The module %a is abstract, \
                   it cannot have any components@]"
        (Style.as_inline_code !print_longident) lid
  | Generative_used_as_applicative lid ->
      fprintf ppf "@[The functor %a is generative,@ it@ cannot@ be@ \
                   applied@ in@ type@ expressions@]"
        (Style.as_inline_code !print_longident) lid
  | Cannot_scrape_alias(lid, p) ->
      let cause =
        if Current_unit_name.is_path p then "is the current compilation unit"
        else "is missing"
      in
      fprintf ppf
        "The module %a is an alias for module %a, which %s"
        (Style.as_inline_code !print_longident) lid
        (Style.as_inline_code !print_path) p cause
  | Local_value_escaping (item, lid, context) ->
      fprintf ppf
        "@[%a local, so cannot be used \
          inside %s.@]"
        print_lock_item (item, lid)
        (string_of_escaping_context context);
  | Once_value_used_in (item, lid, context) ->
      fprintf ppf
        "@[%a once, so cannot be used \
            inside %s@]"
        print_lock_item (item, lid)
        (string_of_shared_context context)
  | Value_used_in_closure (item, lid, error, context) ->
      let e0, e1 =
        match error with
        | Error (Areality, _) -> "local", "might escape"
        | Error (Linearity, _) -> "once", "is many"
        | Error (Portability, _) -> "nonportable", "is portable"
      in
      fprintf ppf
      "@[%a %s, so cannot be used \
            inside a closure that %s.@]"
      print_lock_item (item, lid)
      e0 e1;
      begin match error, context with
      | Error (Areality, _), Some Tailcall_argument ->
         fprintf ppf "@.@[Hint: The closure might escape because it \
                          is an argument to a tail call@]"
      | _ -> ()
      end
  | Local_value_used_in_exclave (item, lid) ->
      fprintf ppf "@[%a local, so it cannot be used \
                  inside an exclave_@]"
        print_lock_item (item, lid)
  | Non_value_used_in_object (lid, typ, err) ->
      fprintf ppf "@[%a must have a type of layout value because it is \
                   captured by an object.@ %a@]"
        (Style.as_inline_code !print_longident) lid
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> !print_type_expr ppf typ)) err

let report_error ppf = function
  | Missing_module(_, path1, path2) ->
      fprintf ppf "@[@[<hov>";
      if Path.same path1 path2 then
        fprintf ppf "Internal path@ %a@ is dangling."
          Style.inline_code (Path.name path1)
      else
        fprintf ppf "Internal path@ %a@ expands to@ %a@ which is dangling."
          Style.inline_code (Path.name path1)
          Style.inline_code (Path.name path2);
      fprintf ppf "@]@ @[%s@ %a@ %s.@]@]"
        "The compiled interface for module"
        Style.inline_code (Ident.name (Path.head path2))
        "was not found"
  | Illegal_value_name(_loc, name) ->
      fprintf ppf "%a is not a valid value identifier."
       Style.inline_code name
  | Lookup_error(loc, t, err) -> report_lookup_error loc t ppf err

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          let loc =
            match err with
            | Missing_module (loc, _, _)
            | Illegal_value_name (loc, _)
            | Lookup_error(loc, _, _) -> loc
          in
          let error_of_printer =
            if loc = Location.none
            then Location.error_of_printer_file
            else Location.error_of_printer ~loc ?sub:None
          in
          Some (error_of_printer report_error err)
      | _ ->
          None
    )
