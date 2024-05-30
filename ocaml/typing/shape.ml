(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let compare (x : t) y =
      match x, y with
      | Compilation_unit s1, Compilation_unit s2 -> String.compare s1 s2
      | Item c1, Item c2 ->
        let c = Int.compare c1.id c2.id in
        if c <> 0 then c else String.compare c1.comp_unit c2.comp_unit
      | Internal, Internal -> 0
      | Predef s1, Predef s2 -> String.compare s1 s2
      | Compilation_unit _, (Item _ | Internal | Predef _) -> -1
      | Item _, (Internal | Predef _) -> -1
      | Internal, Predef _ -> -1
      | (Item _ | Internal | Predef _), Compilation_unit _ -> 1
      | (Internal | Predef _), Item _ -> 1
      | Predef _, Internal -> 1

    let equal x y = compare x y = 0

    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      let comp_unit =
        match current_unit with
        | Some cu -> cu |> Compilation_unit.full_path_as_string
        | None -> ""
      in
      Item { comp_unit; id = !id }

  let of_compilation_unit_id id =
    Compilation_unit (id |> Compilation_unit.full_path_as_string)

  let of_global_name name =
    (* CR lmaurer: This will probably need to get more complicated in order to
       treat parameterized modules properly. *)
    Compilation_unit (Format.asprintf "%a" Global.Name.print name)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Constructor -> "constructor"
    | Label -> "label"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Constructor
    | Label
    | Module
    | Module_type
    | Class
    | Class_type ->
        true

  let rank = function
    | Value -> 0
    | Type -> 1
    | Module -> 2
    | Module_type -> 3
    | Extension_constructor -> 4
    | Class -> 5
    | Class_type -> 6
    | Constructor -> 7
    | Label -> 8

  let compare a b =
    let a = rank a in
    let b = rank b in
    Int.compare a b
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t

    let compare (sa, ka) (sb, kb) =
      let c = String.compare sa sb in
      if c <> 0 then c
      else (Sig_component_kind.compare ka kb)

    let name (name, _) = name
    let kind (_, kind) = kind

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let constr id = Ident.name id, Sig_component_kind.Constructor
    let label id = Ident.name id, Sig_component_kind.Label
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type

    let print fmt (name, ns) =
      Format.fprintf fmt "%S[%s]"
        name
        (Sig_component_kind.to_string ns)

    let hash x = Hashtbl.hash x
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
type t = { hash:int; uid: Uid.t option; desc: desc; approximated: bool }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Alias of t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string
  | Error of string

let rec equal_desc d1 d2 =
  if d1 == d2 then true else
  match d1, d2 with
  | Var v1, Var v2 -> Ident.equal v1 v2
  | Alias a1, Alias a2 -> equal a1 a2
  | Error s1, Error s2 -> String.equal s1 s2
  | Abs (v1, t1), Abs (v2, t2) ->
    if Ident.equal v1 v2 then equal t1 t2
    else false
  | App (v1, t1), App (v2, t2) ->
    if not (equal t1 t2) then false
    else equal v1 v2
  | Leaf, Leaf -> true
  | Struct t1, Struct t2 ->
    Item.Map.equal equal t1 t2
  | Proj (t1, i1), Proj (t2, i2) ->
    if Item.compare i1 i2 <> 0 then false
    else equal t1 t2
  | Comp_unit c1, Comp_unit c2 -> String.equal c1 c2
  | Var _, (Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Abs _, (Var _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Alias _ | Error _)
  | App _, (Var _ | Abs _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Struct _, (Var _ | Abs _ | App _ | Leaf | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Leaf, (Var _ | Abs _ | App _ | Struct _ | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Proj _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Comp_unit _ | Alias _ | Error _)
  | Comp_unit _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _ | Alias _ | Error _)
  | Alias _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Error _)
  | Error _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Alias _)
    -> false

and equal t1 t2 =
  if t1.hash <> t2.hash then false
  else if not (Bool.equal t1.approximated t2.approximated) then false
  else if not (Option.equal Uid.equal t1.uid t2.uid) then false
  else equal_desc t1.desc t2.desc

let print fmt t =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  let rec aux fmt { uid; desc; hash = _ } =
    match desc with
    | Var id ->
        Format.fprintf fmt "%s%a" (Ident.name id) print_uid_opt uid
    | Abs (id, t) ->
        let rec collect_idents = function
          | { uid = None; desc = Abs(id, t) } ->
            let (ids, body) = collect_idents t in
            id :: ids, body
          | body ->
            ([], body)
        in
        let (other_idents, body) = collect_idents t in
        let pp_idents fmt idents =
          let idents_names = List.map Ident.name idents in
          let pp_sep fmt () = Format.fprintf fmt ",@ " in
          Format.pp_print_list ~pp_sep Format.pp_print_string fmt idents_names
        in
        Format.fprintf fmt "Abs@[%a@,(@[%a,@ @[%a@]@])@]"
          print_uid_opt uid pp_idents (id :: other_idents) aux body
    | App (t1, t2) ->
        Format.fprintf fmt "@[%a(@,%a)%a@]" aux t1 aux t2
          print_uid_opt uid
    | Leaf ->
        Format.fprintf fmt "<%a>" (Format.pp_print_option Uid.print) uid
    | Proj (t, item) ->
        begin match uid with
        | None ->
            Format.fprintf fmt "@[%a@ .@ %a@]"
              aux t
              Item.print item
        | Some uid ->
            Format.fprintf fmt "@[(%a@ .@ %a)<%a>@]"
              aux t
              Item.print item
              Uid.print uid
        end
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun item t ->
              Format.fprintf fmt "@[<hv 2>%a ->@ %a;@]@,"
                Item.print item
                aux t
            )
        in
        if Item.Map.is_empty map then
          Format.fprintf fmt "@[<hv>{%a}@]" print_uid_opt uid
        else
          Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
    | Alias t ->
        Format.fprintf fmt "Alias@[(@[<v>%a@,%a@])@]" print_uid_opt uid aux t
    | Error s ->
        Format.fprintf fmt "Error %s" s
  in
  if t.approximated then
    Format.fprintf fmt "@[(approx)@ %a@]@;" aux t
  else
    Format.fprintf fmt "@[%a@]@;" aux t

let rec strip_head_aliases = function
  | { desc = Alias t; _ } -> strip_head_aliases t
  | t -> t

let hash_var = 1
let hash_abs = 2
let hash_struct = 3
let hash_leaf = 4
let hash_proj = 5
let hash_app = 6
let hash_comp_unit = 7
let hash_alias = 8
let hash_error = 9

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var;
         hash = Hashtbl.hash (hash_var, uid, var);
         approximated = false }

let for_unnamed_functor_param = Ident.create_local "()"

let var uid id =
  { uid = Some uid; desc = Var id;
    hash = Hashtbl.hash (hash_var, uid, id);
    approximated = false }

let abs ?uid var body =
  { uid; desc = Abs (var, body);
    hash = Hashtbl.hash (hash_abs, uid, body.hash);
    approximated = false }

let str ?uid map =
  let h = Item.Map.fold (fun key t acc ->
    Hashtbl.hash (acc, Item.hash key, t.hash)) map 0
  in
  { uid; desc = Struct map; hash = Hashtbl.hash (hash_struct, uid, h);
    approximated = false }

let alias ?uid t =
  { uid; desc = Alias t; hash = Hashtbl.hash (hash_alias, uid, t.hash); approximated = false}

let error ?uid s =
  { uid; desc = Error s; hash = Hashtbl.hash (hash_error, uid, s); approximated = false}

let leaf' uid =
  { uid; desc = Leaf; hash = Hashtbl.hash (hash_leaf, uid);
    approximated = false }

let leaf uid = leaf' (Some uid)

let approx t = { t with approximated = true}

let set_approximated ~approximated t = { t with approximated}

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf
        as a best effort *)
      approx t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> approx t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item);
        hash = Hashtbl.hash (hash_proj, t.hash, item); approximated = false }

let app ?uid f ~arg =
      { uid; desc = App (f, arg); hash = Hashtbl.hash (hash_app, f.hash, uid, arg.hash)
        ; approximated = false }

let comp_unit ?uid s =
      { uid; desc = Comp_unit s; hash = Hashtbl.hash (hash_comp_unit, uid, s);
        approximated = false }

let no_fuel_left ?uid s = { s with uid }

let decompose_abs t =
  match t.desc with
  | Abs (x, t) -> Some (x, t)
  | _ -> None

let dummy_mod = str Item.Map.empty

let of_path ~find_shape ~namespace =
  (* We need to handle the following cases:
    Path of constructor:
      M.t.C
    Path of label:
      M.t.lbl
    Path of label of inline record:
      M.t.C.lbl *)
  let rec aux : Sig_component_kind.t -> Path.t -> t = fun ns -> function
    | Pident id -> find_shape ns id
    | Pdot (path, name) ->
      let namespace :  Sig_component_kind.t =
        match (ns : Sig_component_kind.t) with
        | Constructor -> Type
        | Label -> Type
        | _ -> Module
      in
      proj (aux namespace path) (name, ns)
    | Papply (p1, p2) -> app (aux Module p1) ~arg:(aux Module p2)
    | Pextra_ty (path, extra) -> begin
        match extra with
          Pcstr_ty name -> proj (aux Type path) (name, Constructor)
        | Pext_ty -> aux Extension_constructor path
      end
  in
  aux namespace

let for_persistent_unit s =
  comp_unit ~uid:(Compilation_unit s) s

let leaf_for_unpack = leaf' None

let set_uid_if_none t uid =
  match t.uid with
  | None -> { t with uid = Some uid }
  | _ -> t

module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id shape = Item.Map.add (Item.type_ id) shape t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_constr t id shape = Item.Map.add (Item.constr id) shape t
  let add_constr_proj t id shape =
    let item = Item.constr id in
    Item.Map.add item (proj shape item) t

  let add_label t id uid = Item.Map.add (Item.label id) (leaf uid) t
  let add_label_proj t id shape =
    let item = Item.label id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id shape =
    Item.Map.add (Item.extension_constructor id) shape t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
