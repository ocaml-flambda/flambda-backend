open Asttypes
open Lambda
open Misc
open Typedtree
open Types
open Debuginfo.Scoped_location
open Longident
module Var_env = Map.Make (String)

type fv_env = lambda Var_env.t

type var_env =
  { env_vals : fv_env;
    env_tys : fv_env;
    env_mod : fv_env
  }

let rec print_path = function
  | Path.Pident id -> Ident.name id
  | Path.Pdot (p, s) -> print_path p ^ "." ^ s
  | Path.Papply (p1, p2) -> print_path p1 ^ "(" ^ print_path p2 ^ ")"
  | Path.Pextra_ty (p, _) -> print_path p ^ "[extra]"

let with_new_value var_env name val_ =
  { var_env with env_vals = Var_env.add name val_ var_env.env_vals }

let with_new_type var_env name ty =
  { var_env with env_vals = Var_env.add name ty var_env.env_tys }

let with_new_module var_env name mod_ =
  { var_env with env_mod = Var_env.add name mod_ var_env.env_mod }

let with_new_values var_env name_vals =
  List.fold_left
    (fun var_env (name, val_) -> with_new_value var_env name val_)
    var_env name_vals

let with_new_modules var_env name_mods =
  List.fold_left
    (fun var_env (name, mod_) -> with_new_module var_env name mod_)
    var_env name_mods

let with_new_idents_values var_env idents =
  List.fold_left
    (fun var_env id -> with_new_value var_env (Ident.name id) (Lvar id))
    var_env idents

let with_new_idents_types var_env idents =
  List.fold_left
    (fun var_env id -> with_new_type var_env (Ident.name id) (Lvar id))
    var_env idents

let with_new_idents_modules var_env idents =
  List.fold_left
    (fun var_env id -> with_new_module var_env (Ident.name id) (Lvar id))
    var_env idents

let new_env =
  { env_vals = Var_env.empty; env_tys = Var_env.empty; env_mod = Var_env.empty }

let camlinternalQuote =
  lazy
    (match
       Env.open_pers_signature "CamlinternalQuote" (Lazy.force Env.initial)
     with
    | exception Not_found -> fatal_error "Module CamlinternalQuote unavailable."
    | path, env -> path, env)

let rec get_arity = function
  | Tarrow (_, _, ty, _) -> 1 + get_arity (get_desc ty)
  | _ -> 0

let combinator modname field arity =
  lazy
    (let _, env = Lazy.force camlinternalQuote in
     let lid =
       match unflatten (String.split_on_char '.' modname) with
       | None -> Lident field
       | Some lid -> Ldot (lid, field)
     in
     match Env.find_value_by_name lid env with
     | p, value_desc ->
       let ty_ar = get_arity (get_desc value_desc.val_type) in
       if ty_ar = arity
       then transl_value_path Loc_unknown env p
       else
         fatal_error
           ("Primitive CamlinternalQuote." ^ modname ^ "." ^ field
          ^ " expects arity " ^ string_of_int arity ^ ", but it is "
          ^ string_of_int ty_ar ^ ".")
     | exception Not_found ->
       fatal_error
         ("Primitive CamlinternalQuote." ^ modname ^ "." ^ field ^ " not found."))

module Loc = struct
  let unknown = combinator "Loc" "unknown" 0

  let known = combinator "Loc" "known" 5
end

module Name = struct
  let mk = combinator "Name" "mk" 1
end

module Constant = struct
  let int = combinator "Constant" "int" 1

  let char = combinator "Constant" "char" 1

  let string = combinator "Constant" "string" 2

  let float = combinator "Constant" "float" 1

  let float32 = combinator "Constant" "float32" 1

  let int32 = combinator "Constant" "int32" 1

  let int64 = combinator "Constant" "int64" 1

  let nativeint = combinator "Constant" "nativeint" 1

  let unboxed_float = combinator "Constant" "unboxed_float" 1

  let unboxed_float32 = combinator "Constant" "unboxed_float32" 1

  let unboxed_int32 = combinator "Constant" "unboxed_int32" 1

  let unboxed_int64 = combinator "Constant" "unboxed_int64" 1

  let unboxed_nativeint = combinator "Constant" "unboxed_nativeint" 1
end

module Label = struct
  module Nonoptional = struct
    let no_label = combinator "Label.Nonoptional" "no_label" 0

    let labelled = combinator "Label.Nonoptional" "labelled" 1
  end

  let no_label = combinator "Label" "no_label" 0

  let labelled = combinator "Label" "labelled" 1

  let optional = combinator "Label" "optional" 1
end

module Identifier = struct
  module Module = struct
    let compilation_unit = combinator "Identifier.Module" "compilation_unit" 1

    let dot = combinator "Identifier.Module" "dot" 2

    let var = combinator "Identifier.Module" "var" 2
  end

  module Value = struct
    let dot = combinator "Identifier.Value" "dot" 2

    let var = combinator "Identifier.Value" "var" 2
  end

  module Type = struct
    let dot = combinator "Identifier.Type" "dot" 2

    let var = combinator "Identifier.Type" "var" 2

    let int = combinator "Identifier.Type" "int" 0

    let char = combinator "Identifier.Type" "char" 0

    let string = combinator "Identifier.Type" "string" 0

    let bytes = combinator "Identifier.Type" "bytes" 0

    let float = combinator "Identifier.Type" "float" 0

    let float32 = combinator "Identifier.Type" "float32" 0

    let bool = combinator "Identifier.Type" "bool" 0

    let unit = combinator "Identifier.Type" "unit" 0

    let exn = combinator "Identifier.Type" "exn" 0

    let array = combinator "Identifier.Type" "array" 0

    let iarray = combinator "Identifier.Type" "iarray" 0

    let list = combinator "Identifier.Type" "list" 0

    let option = combinator "Identifier.Type" "option" 0

    let nativeint = combinator "Identifier.Type" "nativeint" 0

    let int32 = combinator "Identifier.Type" "int32" 0

    let int64 = combinator "Identifier.Type" "int64" 0

    let lazy_t = combinator "Identifier.Type" "lazy_t" 0

    let extension_constructor =
      combinator "Identifier.Type" "extension_constructor" 0

    let floatarray = combinator "Identifier.Type" "floatarray" 0

    let lexing_position = combinator "Identifier.Type" "lexing_position" 0

    let code = combinator "Identifier.Type" "code" 0

    let unboxed_float = combinator "Identifier.Type" "unboxed_float" 0

    let unboxed_nativeint = combinator "Identifier.Type" "unboxed_nativeint" 0

    let unboxed_int32 = combinator "Identifier.Type" "unboxed_int32" 0

    let unboxed_int64 = combinator "Identifier.Type" "unboxed_int64" 0

    let int8x16 = combinator "Identifier.Type" "int8x16" 0

    let int16x8 = combinator "Identifier.Type" "int16x8" 0

    let int32x4 = combinator "Identifier.Type" "int32x4" 0

    let int64x2 = combinator "Identifier.Type" "int64x2" 0

    let float32x4 = combinator "Identifier.Type" "float32x4" 0

    let float64x2 = combinator "Identifier.Type" "float64x2" 0
  end

  module Module_type = struct
    let dot = combinator "Identifier.Module_type" "dot" 2
  end

  module Constructor = struct
    let dot = combinator "Identifier.Constructor" "dot" 2

    let false_ = combinator "Identifier.Constructor" "false_" 0

    let true_ = combinator "Identifier.Constructor" "true_" 0

    let void = combinator "Identifier.Constructor" "void" 0

    let nil = combinator "Identifier.Constructor" "nil" 0

    let cons = combinator "Identifier.Constructor" "cons" 0

    let none = combinator "Identifier.Constructor" "none" 0

    let some = combinator "Identifier.Constructor" "some" 0

    let match_failure = combinator "Identifier.Constructor" "match_failure" 0

    let out_of_memory = combinator "Identifier.Constructor" "out_of_memory" 0

    let invalid_argument =
      combinator "Identifier.Constructor" "invalid_argument" 0

    let failure = combinator "Identifier.Constructor" "failure" 0

    let not_found = combinator "Identifier.Constructor" "not_found" 0

    let sys_error = combinator "Identifier.Constructor" "sys_error" 0

    let end_of_file = combinator "Identifier.Constructor" "end_of_file" 0

    let division_by_zero =
      combinator "Identifier.Constructor" "division_by_zero" 0

    let stack_overflow = combinator "Identifier.Constructor" "stack_overflow" 0

    let sys_blocked_io = combinator "Identifier.Constructor" "sys_blocked_io" 0

    let assert_failure = combinator "Identifier.Constructor" "assert_failure" 0

    let undefined_recursive_module =
      combinator "Identifier.Constructor" "undefined_recursive_module" 0
  end

  module Field = struct
    let dot = combinator "Identifier.Field" "dot" 2
  end
end

module Variant_type = struct
  module Variant_form = struct
    let fixed = combinator "Variant_type.Variant_form" "fixed" 0

    let open_ = combinator "Variant_type.Variant_form" "open_" 0

    let closed = combinator "Variant_type.Variant_form" "closed" 1
  end

  module Row_field = struct
    let inherit_ = combinator "Variant_type.Row_field" "inherit_row_field" 1

    let tag = combinator "Variant_type.Row_field" "tag_row_field" 3
  end

  let of_row_fields_list = combinator "Variant_type" "of_row_fields_list" 2
end

module Module_type = struct
  let of_string = combinator "Module_type" "of_string" 1

  let ident = combinator "Module_type" "ident" 1
end

module Fragment = struct
  let name = combinator "Fragment" "name" 1

  let dot = combinator "Fragment" "dot" 2
end

module Type = struct
  let var = combinator "Type" "var" 1

  let arrow = combinator "Type" "arrow" 3

  let tuple = combinator "Type" "tuple" 1

  let unboxed_tuple = combinator "Type" "unboxed_tuple" 1

  let constr = combinator "Type" "constr" 2

  let alias = combinator "Type" "alias" 2

  let variant = combinator "Type" "variant" 1

  let poly = combinator "Type" "poly" 3

  let package = combinator "Type" "package" 2

  let object_ = combinator "Type" "object_" 2

  let class_ = combinator "Type" "class_" 2

  let call_pos = combinator "Type" "call_pos" 0
end

module Variant = struct
  let of_string = combinator "Variant" "of_string" 1
end

module Constructor = struct
  let ident = combinator "Constructor" "ident" 1

  let of_string = combinator "Constructor" "of_string" 1
end

module Field = struct
  let ident = combinator "Field" "ident" 1

  let of_string = combinator "Field" "of_string" 1
end

module Method = struct
  let of_string = combinator "Method" "of_string" 1
end

module Pat = struct
  let any = combinator "Pat" "any" 0

  let var = combinator "Pat" "var" 1

  let alias = combinator "Pat" "alias" 2

  let constant = combinator "Pat" "constant" 1

  let tuple = combinator "Pat" "tuple" 1

  let unboxed_tuple = combinator "Pat" "unboxed_tuple" 1

  let construct = combinator "Pat" "construct" 2

  let variant = combinator "Pat" "variant" 2

  let record = combinator "Pat" "record" 2

  let unboxed_record = combinator "Pat" "unboxed_record" 2

  let array = combinator "Pat" "array" 1

  let or_ = combinator "Pat" "or_" 1

  let lazy_ = combinator "Pat" "lazy_" 1

  let unpack = combinator "Pat" "unpack" 1

  let exception_ = combinator "Pat" "exception_" 1

  let constraint_ = combinator "Pat" "constraint_" 2
end

module Case = struct
  let nonbinding = combinator "Case" "nonbinding" 3

  let simple = combinator "Case" "simple" 3

  let pattern = combinator "Case" "pattern" 4

  let guarded = combinator "Case" "guarded" 4

  let refutation = combinator "Case" "refutation" 4
end

module Function = struct
  let body = combinator "Function" "body" 2

  let cases = combinator "Function" "cases" 2

  let param = combinator "Function" "param" 5

  let newtype = combinator "Function" "newtype" 3
end

module Module = struct
  let ident = combinator "Module" "ident" 1

  let apply = combinator "Module" "apply" 2

  let apply_unit = combinator "Module" "apply_unit" 1
end

module Comprehension = struct
  let body = combinator "Comprehension" "body" 1

  let when_clause = combinator "Comprehension" "when_clause" 2

  let for_range = combinator "Comprehension" "for_range" 6

  let for_in = combinator "Comprehension" "for_in" 4
end

module Type_constraint = struct
  let constraint_ = combinator "Type_constraint" "constraint_" 1

  let coercion = combinator "Type_constraint" "coercion" 2
end

module Exp = struct
  let ident = combinator "Exp" "ident" 1

  let constant = combinator "Exp" "constant" 1

  let let_rec_simple = combinator "Exp" "let_rec_simple" 3

  let let_ = combinator "Exp" "let_" 5

  let function_ = combinator "Exp" "function_" 1

  let apply = combinator "Exp" "apply" 2

  let match_ = combinator "Exp" "match_" 2

  let try_ = combinator "Exp" "try_" 2

  let tuple = combinator "Exp" "tuple" 1

  let construct = combinator "Exp" "construct" 2

  let variant = combinator "Exp" "variant" 2

  let record = combinator "Exp" "record" 2

  let field = combinator "Exp" "field" 2

  let setfield = combinator "Exp" "setfield" 3

  let array = combinator "Exp" "array" 1

  let ifthenelse = combinator "Exp" "ifthenelse" 3

  let sequence = combinator "Exp" "sequence" 2

  let while_ = combinator "Exp" "while_" 2

  let for_simple = combinator "Exp" "for_simple" 6

  let unboxed_tuple = combinator "Exp" "unboxed_tuple" 1

  let unboxed_record_product = combinator "Exp" "unboxed_record_product" 2

  let unboxed_field = combinator "Exp" "unboxed_field" 2

  let pack = combinator "Exp" "pack" 1

  let unreachable = combinator "Exp" "unreachable" 0

  let src_pos = combinator "Exp" "src_pos" 0

  let exclave = combinator "Exp" "exclave" 1

  let extension_constructor = combinator "Exp" "extension_constructor" 1

  let list_comprehension = combinator "Exp" "list_comprehension" 1

  let array_comprehension = combinator "Exp" "array_comprehension" 1

  let let_exception = combinator "Exp" "let_exception" 2

  let let_op = combinator "Exp" "let_op" 3

  let new_ = combinator "Exp" "new_" 1

  let send = combinator "Exp" "send" 2

  let assert_ = combinator "Exp" "assert_" 1

  let stack = combinator "Exp" "stack" 1

  let lazy_ = combinator "Exp" "lazy_" 1

  let letmodule_nonbinding = combinator "Exp" "letmodule_nonbinding" 3

  let letmodule = combinator "Exp" "letmodule" 4

  let constraint_ = combinator "Exp" "constraint_" 2

  let quote = combinator "Exp" "quote" 1

  let antiquote = combinator "Exp" "antiquote" 1

  let splice = combinator "Exp" "splice" 1
end

module Code = struct
  let to_exp = combinator "Code" "to_exp" 1

  let of_exp = combinator "Code" "of_exp" 2

  let of_exp_with_type_vars = combinator "Code" "of_exp_with_type_vars" 3
end

let use comb = Lazy.force comb

let apply loc comb args =
  let comb = Lazy.force comb in
  Lambda.Lapply
    { ap_func = comb;
      ap_args = args;
      ap_probe = None;
      ap_loc = of_location ~scopes:empty_scopes loc;
      ap_result_layout = Ptop;
      ap_region_close = Rc_normal;
      ap_mode = alloc_heap;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inlined;
      ap_specialised = Default_specialise
    }

let string loc s = Lconst (Const_base (Const_string (s, loc, None)))

let true_ = Lconst (Const_base (Const_int 1))

let false_ = Lconst (Const_base (Const_int 0))

let quote_bool b = if b then true_ else false_

let none = Lconst (Const_base (Const_int 0))

let some x =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x], Loc_unknown)

let option opt = match opt with None -> none | Some x -> some x

let string_option loc s = option (Option.map (string loc) s)

let nil = Lconst (Const_base (Const_int 0))

let cons hd tl =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [hd; tl], Loc_unknown)

let hd l = Lprim (Pfield (0, Immediate, Reads_vary), [l], Loc_unknown)

let tl l = Lprim (Pfield (1, Immediate, Reads_vary), [l], Loc_unknown)

let rec mk_list list =
  match list with [] -> nil | hd :: tl -> cons hd (mk_list tl)

let pair (x, y) =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y], Loc_unknown)

let triple (x, y, z) =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y; z], Loc_unknown)

let func ids body =
  let param_from_name name =
    { name;
      layout = Ptop;
      attributes = { unbox_param = false };
      mode = alloc_heap
    }
  in
  lfunction
    ~kind:(Curried { nlocal = List.length ids })
    ~params:(List.map param_from_name ids)
    ~return:Ptop ~attr:default_function_attribute ~body ~loc:Loc_unknown
    ~mode:alloc_heap ~ret_mode:alloc_heap ~region:false

let bind id def body = Llet (Strict, Ptop, id, def, body)

let quote_constant loc (const : Typedtree.constant) =
  match const with
  | Const_int x -> apply loc Constant.int [Lconst (Const_base (Const_int x))]
  | Const_char x -> apply loc Constant.char [Lconst (Const_base (Const_char x))]
  | Const_string (x, loc, lopt) ->
    let comp_opt = string_option loc lopt in
    apply loc Constant.string
      [Lconst (Const_base (Const_string (x, loc, None))); comp_opt]
  | Const_float x ->
    apply loc Constant.float [Lconst (Const_base (Const_string (x, loc, None)))]
  | Const_float32 x ->
    apply loc Constant.float32
      [Lconst (Const_base (Const_string (x, loc, None)))]
  | Const_int32 x ->
    apply loc Constant.int32 [Lconst (Const_base (Const_int32 x))]
  | Const_int64 x ->
    apply loc Constant.int64 [Lconst (Const_base (Const_int64 x))]
  | Const_nativeint x ->
    apply loc Constant.nativeint [Lconst (Const_base (Const_nativeint x))]
  | Const_unboxed_float x ->
    apply loc Constant.unboxed_float
      [Lconst (Const_base (Const_string (x, loc, None)))]
  | Const_unboxed_float32 x ->
    apply loc Constant.unboxed_float32
      [Lconst (Const_base (Const_string (x, loc, None)))]
  | Const_unboxed_int32 x ->
    apply loc Constant.unboxed_int32
      [Lconst (Const_base (Const_unboxed_int32 x))]
  | Const_unboxed_int64 x ->
    apply loc Constant.unboxed_int64
      [Lconst (Const_base (Const_unboxed_int64 x))]
  | Const_unboxed_nativeint x ->
    apply loc Constant.unboxed_nativeint
      [Lconst (Const_base (Const_unboxed_nativeint x))]

let quote_loc (loc : Location.t) =
  if loc = Location.none
  then use Loc.unknown
  else
    apply loc Loc.known
      [ Lconst (Const_base (Const_string (loc.loc_start.pos_fname, loc, None)));
        Lconst (Const_base (Const_int loc.loc_start.pos_lnum));
        Lconst (Const_base (Const_int loc.loc_start.pos_cnum));
        Lconst (Const_base (Const_int loc.loc_end.pos_lnum));
        Lconst (Const_base (Const_int loc.loc_end.pos_cnum)) ]

let quote_name (str : string loc) =
  apply str.loc Name.mk [string str.loc str.txt]

let quote_method loc (meth : Typedtree.meth) =
  let name =
    match meth with
    | Tmeth_name name -> name
    | Tmeth_val id -> Ident.name id
    | Tmeth_ancestor (id, path) -> Path.name path ^ "#" ^ Ident.name id
  in
  apply loc Method.of_string [string loc name]

let quote_arg_label loc = function
  | Labelled s -> apply loc Label.labelled [string loc s]
  | Optional s -> apply loc Label.optional [string loc s]
  | Nolabel -> Lazy.force Label.no_label
  | _ ->
    fatal_error
      "No support for any types of labels other than Labelled, Nolabel and \
       Optional"

let rec module_for_path var_env loc = function
  | Path.Pident id -> (
      match Var_env.find_opt (Ident.name id) var_env.env_mod with
      | Some m -> apply loc Identifier.Module.var [m; quote_loc loc]
      | None ->
        if Ident.is_global id
        then
          apply loc Identifier.Module.compilation_unit [string loc (Ident.name id)]
        else raise Exit)
  | Path.Pdot (p, s) ->
    apply loc Identifier.Module.dot [module_for_path var_env loc p; string loc s]
  | _ -> raise Exit

let module_type_for_path var_env loc = function
  | Path.Pident id -> apply loc Module_type.of_string [string loc (Ident.name id)]
  | Path.Pdot (p, s) ->
    apply loc Module_type.ident
      [apply loc Identifier.Module_type.dot [module_for_path var_env loc p; string loc s]]
  | _ -> raise Exit

let type_for_path var_env loc = function
  | Path.Pident id -> (
    match Var_env.find_opt (Ident.name id) var_env.env_tys with
    | Some t -> apply loc Identifier.Type.var [t; quote_loc loc]
    | None ->
      if Ident.is_global id
      then
        match Ident.name id with
        | "int" -> Lazy.force Identifier.Type.int
        | "char" -> Lazy.force Identifier.Type.char
        | "string" -> Lazy.force Identifier.Type.string
        | "bytes" -> Lazy.force Identifier.Type.bytes
        | "float" -> Lazy.force Identifier.Type.float
        | "float32" -> Lazy.force Identifier.Type.float32
        | "bool" -> Lazy.force Identifier.Type.bool
        | "unit" -> Lazy.force Identifier.Type.unit
        | "exn" -> Lazy.force Identifier.Type.exn
        | "array" -> Lazy.force Identifier.Type.array
        | "iarray" -> Lazy.force Identifier.Type.iarray
        | "list" -> Lazy.force Identifier.Type.list
        | "option" -> Lazy.force Identifier.Type.option
        | "nativeint" -> Lazy.force Identifier.Type.nativeint
        | "int32" -> Lazy.force Identifier.Type.int32
        | "int64" -> Lazy.force Identifier.Type.int64
        | "lazy_t" -> Lazy.force Identifier.Type.lazy_t
        | "extension_constructor" ->
          Lazy.force Identifier.Type.extension_constructor
        | "floatarray" -> Lazy.force Identifier.Type.floatarray
        | "lexing_position" -> Lazy.force Identifier.Type.lexing_position
        | "code" -> Lazy.force Identifier.Type.code
        | "unboxed_float" -> Lazy.force Identifier.Type.unboxed_float
        | "unboxed_nativeint" -> Lazy.force Identifier.Type.unboxed_nativeint
        | "unboxed_int32" -> Lazy.force Identifier.Type.unboxed_int32
        | "unboxed_int64" -> Lazy.force Identifier.Type.unboxed_int64
        | "int8x16" -> Lazy.force Identifier.Type.int8x16
        | "int16x8" -> Lazy.force Identifier.Type.int16x8
        | "int32x4" -> Lazy.force Identifier.Type.int32x4
        | "int64x2" -> Lazy.force Identifier.Type.int64x2
        | "float32x4" -> Lazy.force Identifier.Type.float32x4
        | "float62x2" -> Lazy.force Identifier.Type.float64x2
        | name -> fatal_error ("Unknown type: " ^ name)
      else raise Exit)
  | Path.Pdot (p, s) ->
    apply loc Identifier.Type.dot [module_for_path var_env loc p; string loc s]
  | _ -> raise Exit

let value_for_path var_env loc = function
  | Path.Pident id ->
    if Ident.is_global id
    then
      apply loc Identifier.Value.dot
        [ apply loc Identifier.Module.compilation_unit [string loc ""];
          string loc (Ident.name id) ]
    else raise Exit
  | Path.Pdot (p, s) ->
    apply loc Identifier.Value.dot [module_for_path var_env loc p; string loc s]
  | _ -> raise Exit

let value_for_path_opt var_env loc p =
  match value_for_path var_env loc p with
  | res -> Some res
  | exception Exit -> None

let quote_value_ident_path var_env loc path =
  match value_for_path_opt var_env loc path with
  | Some ident_val -> ident_val
  | None -> (
      match path with
      | Pident id ->
        if Var_env.mem (Ident.name id) var_env.env_vals
        then apply loc Identifier.Value.var [Lvar id; quote_loc loc]
        else fatal_error ("Cannot quote free variable " ^ Ident.name id)
      | _ -> fatal_error ("No global path for identifier " ^ print_path path))

let quote_value_ident_path_as_exp var_env loc path =
  apply loc Exp.ident [quote_value_ident_path var_env loc path]


let type_path env ty =
  let desc =
    Types.get_desc (Ctype.expand_head_opt env (Ctype.correct_levels ty))
  in
  match desc with Tconstr (p, _, _) -> Some p | _ -> None

let create_list_param_binding idents body =
  let fun_body, t_opt =
    List.fold_right
      (fun id (body, t_opt) ->
        let new_t = Ident.create_local "t" in
        let let_t =
          match t_opt with
          | None -> body
          | Some t -> bind t (tl (Lvar new_t)) body
        in
        bind id (hd (Lvar new_t)) let_t, Some new_t)
      idents (body, None)
  in
  let list_arg =
    match t_opt with None -> Ident.create_local "t" | Some t -> t
  in
  func [list_arg] fun_body

let quote_record_field env var_env loc lbl_desc =
  match type_path env lbl_desc.lbl_res with
  | None -> fatal_error "No global path for record field"
  | Some (Path.Pident _) ->
    apply loc Field.of_string [string loc lbl_desc.lbl_name]
  | Some (Path.Pdot (p, _)) ->
    apply loc Field.ident
      [apply loc Identifier.Field.dot
         [module_for_path var_env loc p; string loc lbl_desc.lbl_name]]
  | _ -> fatal_error "Unsupported constructor type detected."

let quote_constructor env var_env loc constr =
  match type_path env constr.cstr_res with
  | None -> fatal_error "No global path for constructor"
  | Some (Path.Pident _) -> (
    match constr.cstr_name with
    | "false" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.false_]
    | "true" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.true_]
    | "()" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.void]
    | "[]" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.nil]
    | "::" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.cons]
    | "None" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.none]
    | "Some" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.some]
    | "Match_failure" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.match_failure]
    | "Out_of_memory" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.out_of_memory]
    | "Invalid_argument" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.invalid_argument]
    | "Failure" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.failure]
    | "Not_found" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.not_found]
    | "Sys_error" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.sys_error]
    | "End_of_file" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.end_of_file]
    | "Division_by_zero" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.division_by_zero]
    | "Stack_overflow" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.stack_overflow]
    | "Sys_blocked_io" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.sys_blocked_io]
    | "Assert_failure" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.assert_failure]
    | "Undefined_recursive_module" -> apply loc Constructor.ident [Lazy.force Identifier.Constructor.undefined_recursive_module]
    | name -> apply loc Constructor.of_string [string loc name])
  | Some (Path.Pdot (p, _)) ->
    apply loc Constructor.ident
      [apply loc Identifier.Constructor.dot
         [module_for_path var_env loc p; string loc constr.cstr_name]]
  | _ -> fatal_error "Unsupported constructor type detected."

let quote_ext_constructor var_env loc = function
  | Path.Pident id ->
    apply loc Constructor.of_string [string loc (Ident.name id)]
  | Path.Pdot (p, s) ->
    apply loc Constructor.ident
      [apply loc Identifier.Constructor.dot
         [module_for_path var_env loc p; string loc s]]
  | _ -> fatal_error "Unsupported constructor type detected."

let rec quote_fragment_of_lid loc = function
  | Lident id -> apply loc Fragment.name [string loc id]
  | Ldot (p, s) ->
    apply loc Fragment.dot [quote_fragment_of_lid loc p; string loc s]
  | _ -> fatal_error "Unsupported fragment type detected."

let rec quote_fragment loc = function
  | Path.Pident id -> apply loc Fragment.name [string loc (Ident.name id)]
  | Path.Pdot (p, s) ->
    apply loc Fragment.dot [quote_fragment loc p; string loc s]
  | _ -> fatal_error "Unsupported fragment type detected."

let quote_variant loc name = apply loc Variant.of_string [string loc name]

let quote_nonopt loc (lbl : string option) =
  match lbl with
  | None -> Lazy.force Label.Nonoptional.no_label
  | Some s -> apply loc Label.Nonoptional.labelled [string loc s]

let is_module pat =
  List.mem Tpat_unpack (List.map (fun (extra, _, _) -> extra) pat.pat_extra)

let rec with_new_idents_pat pat var_env =
  match pat.pat_desc with
  | Tpat_any -> var_env
  | Tpat_var (id, _, _, _) ->
    if is_module pat then
      with_new_module var_env (Ident.name id) (Lambda.Lvar id)
    else
      with_new_value var_env (Ident.name id) (Lambda.Lvar id)
  | Tpat_alias (pat, id, _, _, _) ->
    let with_alias = with_new_value var_env (Ident.name id) (Lambda.Lvar id) in
    with_new_idents_pat pat with_alias
  | Tpat_constant const -> var_env
  | Tpat_tuple args ->
    List.fold_left
      (fun var_env (_, pat) -> with_new_idents_pat pat var_env)
      var_env args
  | Tpat_construct (_, _, args, _) ->
    List.fold_left
      (fun var_env pat -> with_new_idents_pat pat var_env)
      var_env args
  | Tpat_variant (_, argo, _) -> (
      match argo with
      | None -> var_env
      | Some pat -> with_new_idents_pat pat var_env)
  | Tpat_record (lbl_pats, _) ->
    List.fold_left
      (fun var_env (_, _, pat) -> with_new_idents_pat pat var_env)
      var_env lbl_pats
  | Tpat_array (_, _, pats) ->
    List.fold_left
      (fun var_env pat -> with_new_idents_pat pat var_env)
      var_env pats
  | Tpat_or (pat1, pat2, _) ->
    with_new_idents_pat pat1 (with_new_idents_pat pat2 var_env)
  | Tpat_unboxed_tuple args ->
    List.fold_left
      (fun var_env (_, pat, _) -> with_new_idents_pat pat var_env)
      var_env args
  | Tpat_record_unboxed_product (lbl_pats, _) ->
    List.fold_left
      (fun var_env (_, _, pat) -> with_new_idents_pat pat var_env)
      var_env lbl_pats
  | Tpat_lazy pat -> with_new_idents_pat pat var_env

let with_new_param var_env fp =
  let pat_of_param =
    match fp.fp_kind with
    | Tparam_pat pat -> pat
    | Tparam_optional_default (pat, _, _) -> pat
  in
  with_new_idents_pat pat_of_param var_env

type case_binding =
  | Non_binding of lambda * lambda
  | Simple of lambda * lambda
  | Pattern of lambda * lambda * lambda
  | Guarded of lambda * lambda * lambda
  | Refutation of lambda * lambda * lambda

let rec quote_module_path loc = function
  | Path.Pident s ->
    apply loc Identifier.Module.compilation_unit [string loc (Ident.name s)]
  | Path.Pdot (p, s) ->
    apply loc Identifier.Module.dot [quote_module_path loc p; string loc s]
  | _ -> fatal_error "No support for Papply in quoting modules"

let rec quote_computation_pattern var_env p =
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_value pat -> quote_value_pattern var_env (pat :> value general_pattern)
  | Tpat_exception pat ->
    apply loc Pat.exception_ [quote_value_pattern var_env pat]
  | Tpat_or (pat1, pat2, _) ->
    let pat1 = quote_computation_pattern var_env pat1 in
    let pat2 = quote_computation_pattern var_env pat2 in
    apply loc Pat.exception_ [pat1; pat2]

and quote_pat_extra var_env loc pat extra =
  let (extra, _, _) = extra in
  match extra with
  | Tpat_constraint ty ->
    apply loc Pat.constraint_ [pat; quote_core_type var_env ty]
  | Tpat_unpack ->
    apply loc Pat.unpack [pat]
  | Tpat_type _ -> pat (* TODO: consider adding support for #tconst *)
  | Tpat_open _ -> fatal_error "No support for open patterns."

and quote_value_pattern var_env p =
  let env = p.pat_env in
  let loc = p.pat_loc in
  let pat_quoted =
    match p.pat_desc with
    | Tpat_any -> Lazy.force Pat.any
    | Tpat_var (id, _, _, _) -> apply loc Pat.var [Lvar id]
    | Tpat_alias (pat, id, _, _, _) ->
      let pat = quote_value_pattern var_env pat in
      apply loc Pat.alias [pat; Lvar id]
    | Tpat_constant const ->
      let const = quote_constant loc const in
      apply loc Pat.constant [const]
    | Tpat_tuple pats ->
      let pats =
        List.map
          (fun (lbl, p) ->
             pair (quote_nonopt loc lbl, quote_value_pattern var_env p))
          pats
      in
      apply loc Pat.tuple [mk_list pats]
    | Tpat_construct (lid, constr, args, _) ->
      let constr = quote_constructor env var_env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
          let args = List.map (quote_value_pattern var_env) args in
          let with_labels =
            List.map
              (fun a -> pair (Lazy.force Label.Nonoptional.no_label, a))
              args
          in
          let as_tuple = apply loc Pat.tuple [mk_list with_labels] in
          Some as_tuple
      in
      apply loc Pat.construct [constr; option args]
    | Tpat_variant (variant, argo, _) ->
      let variant = quote_variant loc variant in
      let argo = Option.map (quote_value_pattern var_env) argo in
      apply loc Pat.variant [variant; option argo]
    | Tpat_record (lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl_desc, pat) ->
             let lbl =
               quote_record_field env var_env Asttypes.(lid.loc) lbl_desc
             in
             let pat = quote_value_pattern var_env pat in
             pair (lbl, pat))
          lbl_pats
      in
      let closed =
        match closed with Asttypes.Closed -> true_ | Asttypes.Open -> false_
      in
      apply loc Pat.record [mk_list lbl_pats; closed]
    | Tpat_array (_, _, pats) ->
      let pats = List.map (quote_value_pattern var_env) pats in
      apply loc Pat.array [mk_list pats]
    | Tpat_or (pat1, pat2, _) ->
      let pat1 = quote_value_pattern var_env pat1 in
      let pat2 = quote_value_pattern var_env pat2 in
      apply loc Pat.or_ [pat1; pat2]
    | Tpat_unboxed_tuple pats ->
      let pats =
        List.map
          (fun (lbl, p, _) ->
             pair (quote_nonopt loc lbl, quote_value_pattern var_env p))
          pats
      in
      apply loc Pat.unboxed_tuple [mk_list pats]
    | Tpat_record_unboxed_product (lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl_desc, pat) ->
             let lbl =
               quote_record_field env var_env Asttypes.(lid.loc) lbl_desc
             in
             let pat = quote_value_pattern var_env pat in
             pair (lbl, pat))
          lbl_pats
      in
      let closed =
        match closed with Asttypes.Closed -> true_ | Asttypes.Open -> false_
      in
      apply loc Pat.unboxed_record [mk_list lbl_pats; closed]
    | Tpat_lazy pat ->
      let pat = quote_value_pattern var_env pat in
      apply loc Pat.lazy_ [pat]
  in
  List.fold_right
    (fun extra p -> quote_pat_extra var_env loc p extra)
    p.pat_extra
    pat_quoted

and quote_core_type var_env ty =
  let loc = ty.ctyp_loc in
  match ty.ctyp_desc with
  | Ttyp_var (id, _) ->
    let id = Option.map (fun s -> Var_env.find s var_env.env_tys) id in
    apply loc Type.var [option id]
  | Ttyp_arrow (arg_lab, ty1, ty2) ->
    let lab = quote_arg_label loc arg_lab
    and ty1 = quote_core_type var_env ty1
    and ty2 = quote_core_type var_env ty2 in
    apply loc Type.arrow [lab; ty1; ty2]
  | Ttyp_tuple ts ->
    let tups =
      List.map
        (fun (s_opt, ty) ->
          pair (quote_nonopt loc s_opt, quote_core_type var_env ty))
        ts
    in
    apply loc Type.tuple [mk_list tups]
  | Ttyp_unboxed_tuple ts ->
    let tups =
      List.map
        (fun (s_opt, ty) ->
          pair (quote_nonopt loc s_opt, quote_core_type var_env ty))
        ts
    in
    apply loc Type.unboxed_tuple [mk_list tups]
  | Ttyp_constr (path, _, tys) ->
    let ident = type_for_path var_env loc path
    and tys = List.map (quote_core_type var_env) tys in
    apply loc Type.constr [ident; mk_list tys]
  | Ttyp_object (fields, flag) -> fatal_error "Still not implemented."
  | Ttyp_class (path, lident, tys) -> fatal_error "Still not implemented."
  | Ttyp_alias (ty, alias_opt, _) -> fatal_error "Still not implemented."
  | Ttyp_variant (row_fields, closed_flag, labels) ->
    let row_fields =
      List.map
        (fun rf ->
          match rf.rf_desc with
          | Tinherit ty ->
            apply rf.rf_loc Variant_type.Row_field.inherit_
              [quote_core_type var_env ty]
          | Ttag (tag, b, tys) ->
            let variant =
              apply tag.loc Variant.of_string [string tag.loc tag.txt]
            in
            apply rf.rf_loc Variant_type.Row_field.tag
              [ variant;
                quote_bool b;
                mk_list (List.map (quote_core_type var_env) tys) ])
        row_fields
    and variant_form =
      match closed_flag, labels with
      | Open, None -> Lazy.force Variant_type.Variant_form.open_
      | Closed, None -> Lazy.force Variant_type.Variant_form.fixed
      | _, Some labs ->
        apply loc Variant_type.Variant_form.closed
          [mk_list (List.map (string loc) labs)]
    in
    apply loc Variant_type.of_row_fields_list [mk_list row_fields; variant_form]
  | Ttyp_poly (tvs, ty) ->
    let names =
      List.map (fun (name, _) -> apply loc Name.mk [string loc name]) tvs
    and ids = List.map (fun (name, _) -> Ident.create_local name) tvs in
    let new_var_env = with_new_idents_types var_env ids in
    let body = create_list_param_binding ids (quote_core_type new_var_env ty) in
    apply loc Type.poly [quote_loc loc; mk_list names; body]
  | Ttyp_package package ->
    let {pack_path; pack_fields; pack_type; pack_txt} = package in
    let mod_type = module_type_for_path var_env loc pack_path
    and with_types =
      List.map
        (fun (lid, ty) ->
           pair (quote_fragment_of_lid Asttypes.(lid.loc) lid.txt, quote_core_type var_env ty))
        pack_fields
    in
    apply loc Type.package [mod_type; mk_list with_types]
  | Ttyp_open (path, lident, ty) -> fatal_error "Still not implemented."
  | Ttyp_call_pos -> fatal_error "Still not implemented."

let rec case_binding var_env transl stage case =
  let pat = case.c_lhs in
  match case.c_guard with
  | None -> (
    let binding_with_computation_pat () =
      match pat_bound_idents pat with
      | [] ->
        let pat = quote_computation_pattern var_env pat in
        let exp = quote_expression var_env transl stage case.c_rhs in
        Non_binding (pat, exp)
      | ids -> (
        let names =
          List.map (fun id -> string pat.pat_loc (Ident.name id)) ids
        in
        let new_var_env = with_new_idents_values var_env ids in
        let pat = quote_computation_pattern var_env pat
        and exp = quote_expression new_var_env transl stage case.c_rhs
        and pat_id = Ident.create_local "pattern"
        and exp_id = Ident.create_local "expression" in
        let body =
          bind pat_id pat (bind exp_id exp (pair (Lvar pat_id, Lvar exp_id)))
        in
        match case.c_rhs.exp_desc with
        | Texp_unreachable ->
          Refutation
            ( mk_list names,
              mk_list [],
              create_list_param_binding ids (create_list_param_binding [] body)
            )
        | _ ->
          Pattern
            ( mk_list names,
              mk_list [],
              create_list_param_binding ids (create_list_param_binding [] body)
            ))
    in
    match pat.pat_desc with
    | Tpat_value pat -> (
      match (pat :> value general_pattern).pat_desc with
      | Tpat_var (id, name, _, _) ->
        let new_var_env = with_new_idents_values var_env [id] in
        let name = quote_name name
        and exp = quote_expression new_var_env transl stage case.c_rhs in
        Simple (name, func [id] exp)
      | _ -> binding_with_computation_pat ())
    | _ -> binding_with_computation_pat ())
  | Some guard ->
    let ids = pat_bound_idents case.c_lhs in
    let names = List.map (fun id -> string pat.pat_loc (Ident.name id)) ids in
    let pat = quote_computation_pattern var_env case.c_lhs in
    let new_var_env = with_new_idents_values var_env ids in
    let exp = quote_expression new_var_env transl stage case.c_rhs in
    let guard = quote_expression new_var_env transl stage guard in
    let pat_id = Ident.create_local "pattern"
    and guard_id = Ident.create_local "guard"
    and exp_id = Ident.create_local "expression" in
    let body =
      bind pat_id pat
        (bind guard_id guard
           (bind exp_id exp (triple (Lvar pat_id, Lvar guard_id, Lvar exp_id))))
    in
    Guarded
      ( mk_list names,
        mk_list [],
        create_list_param_binding ids (create_list_param_binding [] body) )

and case_value_pattern_binding var_env transl stage case =
  case_binding var_env transl stage
    { case with c_lhs = as_computation_pattern case.c_lhs }

and quote_case_binding loc cb =
  match cb with
  | Non_binding (pat, exp) -> apply loc Case.nonbinding [quote_loc loc; pat; exp]
  | Simple (name, body) -> apply loc Case.simple [quote_loc loc; name; body]
  | Pattern (names_vals, names_mods, body) ->
    apply loc Case.pattern [quote_loc loc; names_vals; names_mods; body]
  | Guarded (names_vals, names_mods, body) ->
    apply loc Case.guarded [quote_loc loc; names_vals; names_mods; body]
  | Refutation (names_vals, names_mods, body) ->
    apply loc Case.refutation [quote_loc loc; names_vals; names_mods; body]

and quote_case var_env transl stage loc case =
  quote_case_binding loc (case_binding var_env transl stage case)

and quote_value_pattern_case var_env transl stage loc case =
  quote_case_binding loc (case_value_pattern_binding var_env transl stage case)

and quote_newtype loc ident sloc rest =
  apply loc Function.newtype [quote_loc loc; quote_name sloc; func [ident] rest]

and fun_param_binding var_env transl stage loc param frest =
  let with_newtypes =
    List.fold_right
      (fun (ident, sloc, _, _) rest -> quote_newtype loc ident sloc rest)
      param.fp_newtypes frest
  in
  let pat, opt_exp =
    match param.fp_kind with
    | Tparam_pat pat -> pat, None
    | Tparam_optional_default (pat, exp, _) ->
      pat, Some (quote_expression var_env transl stage exp)
  in
  let idents = pat_bound_idents pat in
  let names =
    List.map (fun s -> apply loc Name.mk [string loc (Ident.name s)]) idents
  in
  let fun_rem =
    create_list_param_binding idents
      (pair (quote_value_pattern var_env pat, with_newtypes))
  in
  apply loc Function.param
    [ quote_arg_label loc param.fp_arg_label;
      option opt_exp;
      quote_loc loc;
      mk_list names;
      fun_rem ]

and quote_function var_env transl stage loc fn =
  match fn with
  | Texp_function fn ->
    let body_env = List.fold_left with_new_param var_env fn.params in
    let fn_body =
      match fn.body with
      | Tfunction_body exp ->
        apply loc Function.body
          [quote_expression body_env transl stage exp; none]
      | Tfunction_cases cases ->
        apply loc Function.cases
          [ mk_list
              (List.map
                 (fun fc ->
                   quote_case_binding fc.c_lhs.pat_loc
                     (case_value_pattern_binding
                        (with_new_idents_pat fc.c_lhs body_env)
                        transl stage fc))
                 cases.fc_cases);
            none ]
    in
    List.fold_right
      (fun_param_binding body_env transl stage loc)
      fn.params fn_body
  | _ -> fatal_error "Unexpected usage of quote_function."

and quote_module_exp var_env transl stage loc mod_exp =
  match mod_exp.mod_desc with
  | Tmod_ident (path, _) ->
    let m = quote_module_path loc path in
    apply loc Module.ident [m]
  | Tmod_apply (funct, arg, _) ->
    let transl_funct = quote_module_exp var_env transl stage loc funct in
    let transl_arg = quote_module_exp var_env transl stage loc arg in
    apply loc Module.apply [transl_funct; transl_arg]
  | Tmod_apply_unit funct ->
    let transl_funct = quote_module_exp var_env transl stage loc funct in
    apply loc Module.apply_unit [transl_funct]
  | Tmod_constraint (mod_exp, _, _, _) ->
    quote_module_exp var_env transl stage loc mod_exp
  | Tmod_structure _ | Tmod_functor _ ->
    fatal_error "Cannot quote struct..end blocks"
  | Tmod_unpack _ -> fatal_error "No support for unpacking first-class modules"

and quote_comprehension var_env transl stage loc { comp_body; comp_clauses } =
  let add_clause body = function
    | Texp_comp_when exp ->
      let exp = quote_expression var_env transl stage exp in
      apply loc Comprehension.when_clause [body; exp]
    | Texp_comp_for clause_bindings ->
      List.fold_left
        (fun body clb ->
          match clb.comp_cb_iterator with
          | Texp_comp_range rcd ->
            let start = quote_expression var_env transl stage rcd.start
            and stop = quote_expression var_env transl stage rcd.stop
            and direction =
              match rcd.direction with Upto -> true | Downto -> false
            in
            apply loc Comprehension.for_range
              [ quote_loc loc;
                quote_name (mkloc (Ident.name rcd.ident) loc);
                start;
                stop;
                quote_bool direction ]
          | Texp_comp_in { pattern; sequence } ->
            let exp = quote_expression var_env transl stage sequence in
            apply loc Comprehension.for_in
              [quote_loc loc; quote_value_pattern var_env pattern; exp])
        body clause_bindings
  in
  let body =
    apply loc Comprehension.body
      [quote_expression var_env transl stage comp_body]
  in
  List.fold_left (fun body clause -> add_clause body clause) body comp_clauses

and quote_expression_extra var_env _ _ extra lambda =
  let extra, loc, _ = extra in
  match extra with
  | Texp_newtype (id, sloc, _, _) -> quote_newtype loc id sloc lambda
  | Texp_constraint ty ->
    let constr_ =
      apply loc Type_constraint.constraint_ [quote_core_type var_env ty]
    in
    apply loc Exp.constraint_ [lambda; constr_]
  | Texp_coerce (ty_opt, ty) ->
    let coerce =
      apply loc Type_constraint.coercion
        [ option (Option.map (quote_core_type var_env) ty_opt);
          quote_core_type var_env ty ]
    in
    apply loc Exp.constraint_ [lambda; coerce]
  | Texp_stack -> apply loc Exp.stack [lambda]
  | Texp_poly ty_opt -> fatal_error "No support for Texp_poly yet"
  | Texp_mode alloc_opt -> fatal_error "No support for modes yet"

and update_env_with_extra var_env extra =
  let extra, loc, _ = extra in
  match extra with
  | Texp_newtype (id, _, _, _) -> with_new_type var_env (Ident.name id) (Lvar id)
  | Texp_constraint _ | Texp_coerce _ | Texp_stack _ -> var_env
  | Texp_poly ty_opt -> fatal_error "No support for Texp_poly yet"
  | Texp_mode _ -> fatal_error "No support for modes yet"

and quote_expression var_env transl stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  let var_env =
    List.fold_left
      (fun var_env extra -> update_env_with_extra var_env extra)
      var_env
      e.exp_extra
  in
  let body =
    match e.exp_desc with
    | Texp_ident (path, _, _, _, _) -> quote_value_ident_path_as_exp var_env loc path
    | Texp_constant const ->
      let const = quote_constant loc const in
      apply loc Exp.constant [const]
    | Texp_let (rec_flag, vbs, exp) -> (
      match rec_flag with
      | Recursive ->
        let names_defs =
          List.map
            (fun vb ->
              match vb.vb_pat.pat_desc with
              | Tpat_var (ident, _, _, _) -> ident, vb.vb_expr
              | _ -> assert false)
            vbs
        in
        let idents, defs = List.split names_defs in
        let names_lam =
          List.map
            (fun s -> apply loc Name.mk [string loc (Ident.name s)])
            idents
        and body_env = with_new_idents_values var_env idents in
        let defs_lam = List.map (quote_expression body_env transl stage) defs in
        let frest =
          create_list_param_binding idents
            (pair
               (mk_list defs_lam, quote_expression body_env transl stage exp))
        in
        apply loc Exp.let_rec_simple [quote_loc loc; mk_list names_lam; frest]
      | Nonrecursive ->
        let body_env, val_l, mod_l, pats, defs =
          List.fold_left
            (fun (body_env, val_l, mod_loc, pats, defs) vb ->
              let pat = vb.vb_pat in
              let idents = pat_bound_idents pat in
              let def = quote_expression var_env transl stage vb.vb_expr
              and body_env = with_new_idents_values body_env idents in
              body_env, idents @ val_l, [], pat :: pats, def :: defs)
            (var_env, [], [], [], []) (List.rev vbs)
        in
        let def_pat =
          apply loc Pat.tuple
            [ mk_list
                (List.map
                   (fun pat ->
                     pair
                       ( Lazy.force Label.Nonoptional.no_label,
                         quote_value_pattern var_env pat ))
                   pats) ]
        in
        let names_lam =
          List.map
            (fun s -> apply loc Name.mk [string loc (Ident.name s)])
            val_l
        and frest =
          create_list_param_binding val_l
            (create_list_param_binding []
               (pair (def_pat, quote_expression body_env transl stage exp)))
        in
        apply loc Exp.let_
          [quote_loc loc; mk_list names_lam; nil; mk_list defs; frest])
    | Texp_function fun_spec ->
      let fn =
        quote_function var_env transl stage loc (Texp_function fun_spec)
      in
      apply loc Exp.function_ [fn]
    | Texp_apply (fn, args, _, _, _) ->
      let fn = quote_expression var_env transl stage fn in
      let args =
        List.filter
          (fun (_, exp) -> match exp with Omitted _ -> false | _ -> true)
          args
      in
      let args =
        List.map
          (fun (lbl, exp) ->
            match exp with
            | Omitted _ -> assert false
            | Arg (exp, _) ->
              let lbl = quote_arg_label loc lbl in
              let exp = quote_expression var_env transl stage exp in
              pair (lbl, exp))
          args
      in
      apply loc Exp.apply [fn; mk_list args]
    | Texp_match (exp, _, cases, _) ->
      let exp = quote_expression var_env transl stage exp in
      let cases = List.map (quote_case var_env transl stage loc) cases in
      apply loc Exp.match_ [exp; mk_list cases]
    | Texp_try (exp, cases) ->
      let exp = quote_expression var_env transl stage exp
      and cases =
        List.map (quote_value_pattern_case var_env transl stage loc) cases
      in
      apply loc Exp.try_ [exp; mk_list cases]
    | Texp_tuple (exps, _) ->
      let exps =
        List.map
          (fun (lab, exp) ->
            pair
              (string_option loc lab, quote_expression var_env transl stage exp))
          exps
      in
      apply loc Exp.tuple [mk_list exps]
    | Texp_construct (lid, constr, args, _) ->
      let constr = quote_constructor env var_env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | [arg] -> Some (quote_expression var_env transl stage arg)
        | _ :: _ ->
          let args = List.map (quote_expression var_env transl stage) args in
          let with_labels =
            List.map
              (fun a -> pair (Lazy.force Label.Nonoptional.no_label, a))
              args
          in
          let as_tuple = apply loc Exp.tuple [mk_list with_labels] in
          Some as_tuple
      in
      apply loc Exp.construct [constr; option args]
    | Texp_variant (variant, argo) ->
      let variant = quote_variant loc variant
      and argo =
        Option.map
          (fun (arg, _) -> quote_expression var_env transl stage arg)
          argo
      in
      apply loc Exp.variant [variant; option argo]
    | Texp_record record ->
      let lbl_exps =
        Array.map
          (fun (lbl, def) ->
            let lbl = quote_record_field env var_env loc lbl in
            let exp =
              match def with
              | Overridden (_, exp) -> quote_expression var_env transl stage exp
              | Kept _ ->
                fatal_error "No support for record update syntax in quotations"
            in
            pair (lbl, exp))
          record.fields
      in
      let base =
        Option.map
          (fun (e, _) -> quote_expression var_env transl stage e)
          record.extended_expression
      in
      apply loc Exp.record [mk_list (Array.to_list lbl_exps); option base]
    | Texp_field (rcd, lid, lbl, _, _) ->
      let rcd = quote_expression var_env transl stage rcd in
      let lbl = quote_record_field env var_env lid.loc lbl in
      apply loc Exp.field [rcd; lbl]
    | Texp_setfield (rcd, _, lid, lbl, exp) ->
      let rcd = quote_expression var_env transl stage rcd in
      let lbl = quote_record_field env var_env lid.loc lbl in
      let exp = quote_expression var_env transl stage exp in
      apply loc Exp.setfield [rcd; lbl; exp]
    | Texp_array (_, _, exps, _) ->
      let exps = List.map (quote_expression var_env transl stage) exps in
      apply loc Exp.array [mk_list exps]
    | Texp_ifthenelse (cond, then_, else_) ->
      let cond = quote_expression var_env transl stage cond in
      let then_ = quote_expression var_env transl stage then_ in
      let else_ = Option.map (quote_expression var_env transl stage) else_ in
      apply loc Exp.ifthenelse [cond; then_; option else_]
    | Texp_sequence (exp1, _, exp2) ->
      let exp1 = quote_expression var_env transl stage exp1 in
      let exp2 = quote_expression var_env transl stage exp2 in
      apply loc Exp.sequence [exp1; exp2]
    | Texp_while wh ->
      let cond = quote_expression var_env transl stage wh.wh_cond in
      let body = quote_expression var_env transl stage wh.wh_body in
      apply loc Exp.while_ [cond; body]
    | Texp_for floop ->
      let low = quote_expression var_env transl stage floop.for_from
      and high = quote_expression var_env transl stage floop.for_to
      and dir =
        match floop.for_dir with
        | Asttypes.Upto -> true_
        | Asttypes.Downto -> false_
      and body_env =
        with_new_value var_env (Ident.name floop.for_id) (Lvar floop.for_id)
      and name = quote_name (mkloc (Ident.name floop.for_id) loc) in
      let body = quote_expression body_env transl stage floop.for_body in
      apply loc Exp.for_simple
        [quote_loc loc; name; low; high; dir; func [floop.for_id] body]
    | Texp_send (obj, meth, _) ->
      let obj = quote_expression var_env transl stage obj in
      let meth = quote_method loc meth in
      apply loc Exp.send [obj; meth]
    | Texp_open (open_decl, exp) ->
      fatal_error "No support for opening modules yet."
    | Texp_letmodule (ident, _, _, mod_exp, body) -> (
      let mod_exp = quote_module_exp var_env transl stage loc mod_exp in
      match ident with
      | None ->
        apply loc Exp.letmodule_nonbinding
          [mod_exp; quote_expression var_env transl stage body]
      | Some ident ->
        let name = quote_name (mkloc (Ident.name ident) loc)
        and body_env =
          with_new_module var_env (Ident.name ident) (Lvar ident)
        in
        let body = quote_expression body_env transl stage body in
        apply loc Exp.letmodule [quote_loc loc; name; mod_exp; func [ident] body]
      )
    | Texp_assert (exp, _) ->
      let exp = quote_expression var_env transl stage exp in
      apply loc Exp.assert_ [exp]
    | Texp_lazy exp ->
      let exp = quote_expression var_env transl stage exp in
      apply loc Exp.lazy_ [exp]
    | Texp_quotation exp ->
      let exp = quote_expression var_env transl (stage + 1) exp in
      apply loc Exp.quote [exp]
    | Texp_antiquotation exp ->
      if stage > 0 then
        let exp = quote_expression var_env transl stage exp in
        apply loc Exp.antiquote [exp]
      else
        apply loc Exp.splice [transl exp]
    | Texp_new (path, _, _, _) ->
      apply loc Exp.new_ [quote_value_ident_path var_env loc path]
    | Texp_pack m ->
      apply loc Exp.pack [quote_module_exp var_env transl stage loc m]
    | Texp_unreachable -> Lazy.force Exp.unreachable
    | Texp_src_pos -> Lazy.force Exp.src_pos
    | Texp_exclave e ->
      apply loc Exp.exclave [quote_expression var_env transl stage e]
    | Texp_extension_constructor (lid, path) ->
      apply loc Exp.extension_constructor
        [quote_loc lid.loc; quote_ext_constructor var_env loc path]
    | Texp_unboxed_tuple ts ->
      let tups =
        List.map
          (fun (lab_opt, exp, _) ->
            pair
              ( quote_nonopt loc lab_opt,
                quote_expression var_env transl stage exp ))
          ts
      in
      apply loc Exp.unboxed_tuple [mk_list tups]
    | Texp_record_unboxed_product record ->
      let lbl_exps =
        Array.map
          (fun (lbl, def) ->
            let lbl = quote_record_field env var_env loc lbl in
            let exp =
              match def with
              | Overridden (_, exp) -> quote_expression var_env transl stage exp
              | Kept _ ->
                fatal_error "No support for record update syntax in quotations."
            in
            pair (lbl, exp))
          record.fields
      in
      let base =
        Option.map
          (fun (e, _) -> quote_expression var_env transl stage e)
          record.extended_expression
      in
      apply loc Exp.unboxed_record_product
        [mk_list (Array.to_list lbl_exps); option base]
    | Texp_unboxed_field (rcd, _, lid, lbl, _) ->
      let rcd = quote_expression var_env transl stage rcd in
      let lbl = quote_record_field env var_env lid.loc lbl in
      apply loc Exp.unboxed_field [rcd; lbl]
    | Texp_letexception (ext_const, exp) ->
      let exp = quote_expression var_env transl stage exp in
      apply loc Exp.let_exception [quote_name ext_const.ext_name; exp]
    | Texp_letop rcd ->
      let let_l =
        quote_value_ident_path var_env rcd.let_.bop_loc rcd.let_.bop_op_path
      and ands_l =
        List.map
          (fun bop ->
            quote_value_ident_path var_env bop.bop_loc bop.bop_op_path)
          rcd.ands
      and defs =
        quote_expression var_env transl stage rcd.let_.bop_exp
        :: List.map
             (fun d -> quote_expression var_env transl stage d.bop_exp)
             rcd.ands
      and body = quote_value_pattern_case var_env transl stage loc rcd.body in
      apply loc Exp.let_op [mk_list (let_l :: ands_l); mk_list defs; body]
    | Texp_list_comprehension compr ->
      apply loc Exp.list_comprehension
        [quote_comprehension var_env transl stage loc compr]
    | Texp_array_comprehension (_, _, compr) ->
      apply loc Exp.array_comprehension
        [quote_comprehension var_env transl stage loc compr]
    | Texp_overwrite _ -> fatal_error "Not implemented yet"
    | Texp_hole _ -> fatal_error "No support for typed holes inside quotations."
    | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ ->
      fatal_error "Should not encounter OOP syntax in quotes."
    | Texp_object _ -> fatal_error "Cannot quote object construction."
    | Texp_probe _ | Texp_probe_is_enabled _ ->
      fatal_error "Cannot quote probing constructs."
  in
  List.fold_right (quote_expression_extra var_env transl stage) e.exp_extra body

let transl_quote transl exp loc =
  let expr = quote_expression new_env transl 0 exp in
  apply loc Code.of_exp [expr; quote_loc loc]
