module Uid = Shape.Uid

module Type_shape = struct
  module Predef = struct
    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Unboxed_float

    let to_string = function
      | Array -> "array"
      | Bytes -> "bytes"
      | Char -> "char"
      | Extension_constructor -> "extension_constructor"
      | Float -> "float"
      | Floatarray -> "floatarray"
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Lazy_t -> "lazy_t"
      | Nativeint -> "nativeint"
      | String -> "string"
      | Unboxed_float -> "float#"

    let of_string = function
      | "array" -> Some Array
      | "bytes" -> Some Bytes
      | "char" -> Some Char
      | "extension_constructor" -> Some Extension_constructor
      | "float" -> Some Float
      | "float#" -> Some Unboxed_float
      | "floatarray" -> Some Floatarray
      | "int" -> Some Int
      | "int32" -> Some Int32
      | "int64" -> Some Int64
      | "lazy_t" -> Some Lazy_t
      | "nativeint" -> Some Nativeint
      | "string" -> Some String
      | _ -> None
  end

  type t =
    | Ts_constr of (Uid.t * Path.t) * t list
    | Ts_tuple of t list
    | Ts_var of string option
    | Ts_predef of Predef.t * t list
    | Ts_other

  let rec of_type_expr (expr : Types.type_expr) uid_of_path =
    let desc = Types.get_desc expr in
    let map_expr_list (exprs : Types.type_expr list) =
      List.map (fun expr -> of_type_expr expr uid_of_path) exprs
    in
    match desc with
    | Tconstr (path, constrs, _abbrev_memo) -> (
      match Predef.of_string (Path.name path) with
      | Some predef -> Ts_predef (predef, map_expr_list constrs)
      | None -> (
        match uid_of_path path with
        | Some uid -> Ts_constr ((uid, path), map_expr_list constrs)
        | None -> Ts_other))
    | Ttuple exprs -> Ts_tuple (map_expr_list exprs)
    | Tvar { name; _ } -> Ts_var name
    | Tpoly (type_expr, []) -> of_type_expr type_expr uid_of_path
    | _ -> Ts_other

  let rec print ppf = function
    | Ts_predef (predef, shapes) ->
      Format.fprintf ppf "%s (%a)" (Predef.to_string predef)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_constr ((uid, path), shapes) ->
      Format.fprintf ppf "Ts_constr uid=%a path=%a (%a)" Uid.print uid
        Path.print path
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_tuple shapes ->
      Format.fprintf ppf "Ts_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_var name ->
      Format.fprintf ppf "Ts_var (%a)"
        (fun ppf opt -> Format.pp_print_option Format.pp_print_string ppf opt)
        name
    | Ts_other -> Format.fprintf ppf "Ts_other"

  let rec replace_tvar t ~(pairs : (t * t) list) =
    match
      List.filter_map
        (fun (from, to_) ->
          match t = from with true -> Some to_ | false -> None)
        pairs
    with
    | new_type :: _ -> new_type
    | [] -> (
      match t with
      | Ts_constr (uid, shape_list) ->
        Ts_constr (uid, List.map (replace_tvar ~pairs) shape_list)
      | Ts_tuple shape_list ->
        Ts_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_var name -> Ts_var name
      | Ts_predef (predef, shape_list) -> Ts_predef (predef, shape_list)
      | Ts_other -> Ts_other)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare

    let print = print

    let hash = Hashtbl.hash

    let equal (x : t) y = x = y

    let output _oc _t = Misc.fatal_error "unimplemented"
  end)
end

module Type_decl_shape = struct
  type tds =
    | Tds_variant of
        { simple_constructors : string list;
          complex_constructors :
            (string * (string option * Type_shape.t) list) list
        }
    | Tds_record of (string * Type_shape.t) list
    | Tds_alias of Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.t list
    }

  let get_variant_constructors (cstr_args : Types.constructor_declaration)
      uid_of_path =
    match cstr_args.cd_args with
    | Cstr_tuple list ->
      List.map
        (fun (type_expr, _flag) ->
          None, Type_shape.of_type_expr type_expr uid_of_path)
        list
    | Cstr_record list ->
      List.map
        (fun (lbl : Types.label_declaration) ->
          ( Some (Ident.name lbl.ld_id),
            Type_shape.of_type_expr lbl.ld_type uid_of_path ))
        list

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    let length =
      match cstr_args.cd_args with
      | Cstr_tuple list -> List.length list
      | Cstr_record list -> List.length list
    in
    length == 0

  let of_type_declaration path (type_declaration : Types.type_declaration)
      uid_of_path =
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias (Type_shape.of_type_expr type_expr uid_of_path)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, _variant_repr) ->
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun (cstr : Types.constructor_declaration) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right (name, get_variant_constructors cstr uid_of_path))
              cstr_list
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_record (lbl_list, record_repr) -> (
          match record_repr with
          | Record_boxed _ ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   ( Ident.name lbl.ld_id,
                     Type_shape.of_type_expr lbl.ld_type uid_of_path ))
                 lbl_list)
          | Record_float ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   Ident.name lbl.ld_id, Type_shape.Ts_predef (Unboxed_float, []))
                 lbl_list)
          | Record_inlined _ | Record_unboxed | Record_ufloat -> Tds_other)
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other)
    in
    let type_params =
      List.map
        (fun type_expr -> Type_shape.of_type_expr type_expr uid_of_path)
        type_declaration.type_params
    in
    { path; definition; type_params }

  let print_one_constructor ppf (name, type_shape) =
    match name with
    | Some name ->
      Format.fprintf ppf "%a=%a" Format.pp_print_string name Type_shape.print
        type_shape
    | None -> Format.fprintf ppf "%a" Type_shape.print type_shape

  let print_complex_constructor ppf (name, constructors) =
    Format.fprintf ppf "(%a: %a)" Format.pp_print_string name
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_one_constructor)
      constructors

  let print_field ppf (name, shape) =
    Format.fprintf ppf "(%a: %a)" Format.pp_print_string name Type_shape.print
      shape

  let print_tds ppf = function
    | Tds_variant { simple_constructors; complex_constructors } ->
      Format.fprintf ppf
        "Tds_variant simple_constructors=%a complex_constructors=%a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        simple_constructors
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           print_complex_constructor)
        complex_constructors
    | Tds_record field_list ->
      Format.fprintf ppf "Tds_record fields=%a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_field)
        field_list
    | Tds_alias type_shape ->
      Format.fprintf ppf "Tds_alias %a" Type_shape.print type_shape
    | Tds_other -> Format.fprintf ppf "Tds_other"

  let print ppf t =
    Format.fprintf ppf "path=%a, definition=(%a)" Path.print t.path print_tds
      t.definition

  let map_snd f list = List.map (fun (fst, snd) -> fst, f snd) list

  let replace_tvar (t : t) (shapes : Type_shape.t list) =
    let debug = false in
    if debug
    then
      Format.eprintf "replacing tvar %a; %a; %a\n%!" print t
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_shape.print)
        shapes
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_shape.print)
        t.type_params;
    match List.length t.type_params == List.length shapes with
    | true ->
      let replace_tvar =
        Type_shape.replace_tvar ~pairs:(List.combine t.type_params shapes)
      in
      let ret =
        { type_params = [];
          path = t.path;
          definition =
            (match t.definition with
            | Tds_variant { simple_constructors; complex_constructors } ->
              Tds_variant
                { simple_constructors;
                  complex_constructors =
                    map_snd (map_snd replace_tvar) complex_constructors
                }
            | Tds_record field_list ->
              Tds_record (map_snd replace_tvar field_list)
            | Tds_alias type_shape -> Tds_alias (replace_tvar type_shape)
            | Tds_other -> Tds_other)
        }
      in
      ret
    | false ->
      (* CR tnowak: investigate *)
      { type_params = []; path = t.path; definition = Tds_other }
end

let (all_type_decls : Type_decl_shape.t Uid.Tbl.t) = Uid.Tbl.create 42

let (all_type_shapes : Type_shape.t Uid.Tbl.t) = Uid.Tbl.create 42

let add_to_type_decls path (type_decl : Types.type_declaration) uid_of_path =
  let uid = type_decl.type_uid in
  let type_decl_shape =
    Type_decl_shape.of_type_declaration path type_decl uid_of_path
  in
  Uid.Tbl.add all_type_decls uid type_decl_shape

let add_to_type_shapes var_uid type_expr uid_of_path =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid type_shape

let tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " * " strings ^ ")"

let shapes_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd ^ " "
  | _ :: _ :: _ -> "(" ^ String.concat ", " strings ^ ") "

(* CR tnowak: this is copy-pasted from typedecl.ml *)
let rec split_type_path_at_compilation_unit (path : Path.t) =
  match path with
  | Pident _ | Papply _ -> None, path
  | Pdot (Pident i, s) ->
    if Ident.is_global i
    then Some (Ident.name i), Path.Pident (Ident.create_local s)
    else None, path
  | Pdot (path, s) ->
    let comp_unit, path = split_type_path_at_compilation_unit path in
    comp_unit, Path.Pdot (path, s)

let debug = false

let find_in_type_decls (type_uid : Uid.t) (type_path : Path.t)
    ~(load_decls_from_cms : string -> Type_decl_shape.t Shape.Uid.Tbl.t) =
  if debug
  then Format.eprintf "trying to find type_uid = %a\n" Uid.print type_uid;
  if debug then Format.eprintf "splitting %a\n" Path.print type_path;
  let compilation_unit_type_decls =
    match split_type_path_at_compilation_unit type_path with
    | Some compilation_unit, _ -> (
      if debug
      then
        Format.eprintf "got compilation unit %a\n" Format.pp_print_string
          compilation_unit;
      (* CR tnowak: change the [String.lowercase_ascii] to a proper function. *)
      let filename = compilation_unit |> String.uncapitalize_ascii in
      match Load_path.find_uncap (filename ^ ".cms") with
      | exception Not_found ->
        if debug
        then
          Format.eprintf "not found filename %a" Format.pp_print_string filename;
        None
      | fn ->
        let type_decls = load_decls_from_cms fn in
        Some type_decls)
    | None, _ ->
      if debug then Format.eprintf "same unit\n";
      Some all_type_decls
  in
  Option.bind compilation_unit_type_decls (fun tbl ->
      Uid.Tbl.find_opt tbl type_uid)

let rec type_name (type_shape : Type_shape.t)
    ~(load_decls_from_cms : string -> Type_decl_shape.t Shape.Uid.Tbl.t) =
  match type_shape with
  | Ts_predef (predef, shapes) ->
    shapes_to_string (List.map (type_name ~load_decls_from_cms) shapes)
    ^ Type_shape.Predef.to_string predef
  | Ts_other ->
    if debug then Format.eprintf "unknown0\n";
    "unknown"
  | Ts_tuple shapes ->
    tuple_to_string (List.map (type_name ~load_decls_from_cms) shapes)
  | Ts_var name -> "'" ^ Option.value name ~default:"?"
  | Ts_constr ((type_uid, type_path), shapes) -> (
    match find_in_type_decls type_uid type_path ~load_decls_from_cms with
    | None ->
      if debug then Format.eprintf "unknown2\n";
      "unknown"
    | Some { definition = Tds_other; _ } ->
      if debug then Format.eprintf "unknown1\n";
      "unknown"
    | Some type_decl_shape ->
      let type_decl_shape =
        Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      let args =
        shapes_to_string (List.map (type_name ~load_decls_from_cms) shapes)
      in
      let name = Path.name type_decl_shape.path in
      args ^ name)

let rec attach_head (path : Path.t) (new_head : Ident.t) =
  match path with
  | Pident ident -> Path.Pdot (Pident new_head, Ident.name ident)
  | Pdot (l, r) -> Path.Pdot (attach_head l new_head, r)
  | Papply (l, r) -> Path.Papply (attach_head l new_head, attach_head r new_head)

let attach_compilation_unit_to_path (path : Path.t)
    (compilation_unit : Compilation_unit.t) =
  match split_type_path_at_compilation_unit path with
  | None, _ ->
    attach_head path
      (Compilation_unit.to_global_ident_for_bytecode compilation_unit)
  | Some _, _ -> path

let map_snd f list = List.map (fun (a, b) -> a, f b) list

let attach_compilation_unit_to_paths (type_decl : Type_decl_shape.t)
    ~(compilation_unit : Compilation_unit.t) =
  let attach_to_shape = function
    | Type_shape.Ts_constr ((uid, path), ts) ->
      Type_shape.Ts_constr
        ((uid, attach_compilation_unit_to_path path compilation_unit), ts)
    | _ as x -> x
  in
  { Type_decl_shape.path =
      attach_compilation_unit_to_path type_decl.path compilation_unit;
    type_params = List.map attach_to_shape type_decl.type_params;
    definition =
      (match type_decl.definition with
      | Tds_variant { simple_constructors; complex_constructors } ->
        Tds_variant
          { simple_constructors;
            complex_constructors =
              map_snd (map_snd attach_to_shape) complex_constructors
          }
      | Tds_record list -> Tds_record (map_snd attach_to_shape list)
      | Tds_alias shape -> Tds_alias (attach_to_shape shape)
      | Tds_other -> Tds_other)
  }
