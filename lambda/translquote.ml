open Asttypes
open Lambda
open Misc
open Typedtree
open Types
open Debuginfo.Scoped_location
open Longident

let camlinternalQuote =
  lazy
    (match
       Env.open_pers_signature "CamlinternalQuote" (Lazy.force Env.initial)
     with
    | exception Not_found -> fatal_error "Module CamlinternalQuote unavailable."
    | path, env -> path, env)

let combinator modname field =
  lazy
    (let _, env = Lazy.force camlinternalQuote in
     let lid =
       match unflatten (String.split_on_char '.' modname) with
       | None -> Lident field
       | Some lid -> Ldot (lid, field)
     in
     match Env.find_value_by_name lid env with
     | p, _ -> transl_value_path Loc_unknown env p
     | exception Not_found ->
       fatal_error
         ("Primitive CamlinternalQuote." ^ modname ^ "." ^ field ^ " not found."))

module Loc = struct
  let unknown = combinator "Loc" "unknown"

  let known = combinator "Loc" "known"
end

module Name = struct
  let mk = combinator "Name" "mk"
end

module Constant = struct
  let int = combinator "Constant" "int"

  let char = combinator "Constant" "char"

  let string = combinator "Constant" "string"

  let float = combinator "Constant" "float"

  let int32 = combinator "Constant" "int32"

  let int64 = combinator "Constant" "int64"

  let nativeint = combinator "Constant" "nativeint"
end

module Label = struct
  module Nonoptional = struct
    let no_label = combinator "Label.Nonoptional" "no_label"

    let labelled = combinator "Label.Nonoptional" "labelled"
  end

  let no_label = combinator "Label" "no_label"

  let labelled = combinator "Label" "labelled"

  let optional = combinator "Label" "optional"
end

module Identifier = struct
  module Module = struct
    let compilation_unit = combinator "Identifier.Module" "compilation_unit"

    let dot = combinator "Identifier.Module" "dot"
  end

  module Value = struct
    let dot = combinator "Identifier.Value" "dot"

    let var = combinator "Identifier.Value" "var"
  end

  module Constructor = struct
    let dot = combinator "Identifier.Constructor" "dot"

    let false_ = combinator "Identifier.Constructor" "false_"

    let true_ = combinator "Identifier.Constructor" "true_"

    let void = combinator "Identifier.Constructor" "void"

    let nil = combinator "Identifier.Constructor" "nil"

    let cons = combinator "Identifier.Constructor" "cons"

    let none = combinator "Identifier.Constructor" "none"

    let some = combinator "Identifier.Constructor" "some"
  end

  module Field = struct
    let dot = combinator "Identifier.Field" "dot"
  end
end

module Variant = struct
  let of_string = combinator "Variant" "of_string"
end

module Method = struct
  let of_string = combinator "Method" "of_string"
end

module Pat = struct
  let any = combinator "Pat" "any"

  let var = combinator "Pat" "var"

  let alias = combinator "Pat" "alias"

  let constant = combinator "Pat" "constant"

  let tuple = combinator "Pat" "tuple"

  let construct = combinator "Pat" "construct"

  let variant = combinator "Pat" "variant"

  let record = combinator "Pat" "record"

  let array = combinator "Pat" "array"

  let or_ = combinator "Pat" "or_"

  let lazy_ = combinator "Pat" "lazy_"

  let exception_ = combinator "Pat" "exception_"
end

module Case = struct
  let nonbinding = combinator "Case" "nonbinding"

  let simple = combinator "Case" "simple"

  let pattern = combinator "Case" "pattern"

  let guarded = combinator "Case" "guarded"

  let refutation = combinator "Case" "refutation"
end

module Function = struct
  let body = combinator "Function" "body"

  let cases = combinator "Function" "cases"

  let param = combinator "Function" "param"

  let newtype = combinator "Function" "newtype"
end

module Exp = struct
  let ident = combinator "Exp" "ident"

  let constant = combinator "Exp" "constant"

  let let_rec_simple = combinator "Exp" "let_rec_simple"

  let let_ = combinator "Exp" "let_"

  let function_ = combinator "Exp" "function_"

  let apply = combinator "Exp" "apply"

  let match_ = combinator "Exp" "match_"

  let try_ = combinator "Exp" "try_"

  let tuple = combinator "Exp" "tuple"

  let construct = combinator "Exp" "construct"

  let variant = combinator "Exp" "variant"

  let record = combinator "Exp" "record"

  let field = combinator "Exp" "field"

  let setfield = combinator "Exp" "setfield"

  let array = combinator "Exp" "array"

  let ifthenelse = combinator "Exp" "ifthenelse"

  let sequence = combinator "Exp" "sequence"

  let while_ = combinator "Exp" "while_"

  let for_simple = combinator "Exp" "for_simple"

  let send = combinator "Exp" "send"

  let assert_ = combinator "Exp" "assert_"

  let lazy_ = combinator "Exp" "lazy_"

  let open_ = combinator "Exp" "open_"

  let quote = combinator "Exp" "quote"

  let antiquote = combinator "Exp" "antiquote"
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
    apply loc Constant.float [Lconst (Const_base (Const_float x))]
  | Const_int32 x ->
    apply loc Constant.int32 [Lconst (Const_base (Const_int32 x))]
  | Const_int64 x ->
    apply loc Constant.int64 [Lconst (Const_base (Const_int64 x))]
  | Const_nativeint x ->
    apply loc Constant.nativeint [Lconst (Const_base (Const_nativeint x))]
  | _ -> fatal_error "Unsupported constant type detected."

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

let rec module_for_path loc = function
  | Path.Pident id ->
    if Ident.is_global id
    then
      apply loc Identifier.Module.compilation_unit [string loc (Ident.name id)]
    else raise Exit
  | Path.Pdot (p, s) ->
    apply loc Identifier.Module.dot [module_for_path loc p; string loc s]
  | _ -> raise Exit

let value_for_path loc = function
  | Path.Pident id ->
    if Ident.is_global id
    then
      apply loc Identifier.Value.dot
        [ apply loc Identifier.Module.compilation_unit [string loc ""];
          string loc (Ident.name id) ]
    else raise Exit
  | Path.Pdot (p, s) ->
    apply loc Identifier.Value.dot [module_for_path loc p; string loc s]
  | _ -> raise Exit

let value_for_path_opt loc p =
  match value_for_path loc p with res -> Some res | exception Exit -> None

let type_path env ty =
  let desc =
    Types.get_desc (Ctype.expand_head_opt env (Ctype.correct_levels ty))
  in
  match desc with Tconstr (p, _, _) -> Some p | _ -> None

let quote_record_field env loc lbl_desc =
  match type_path env lbl_desc.lbl_res with
  | None -> fatal_error "No global path for record field"
  | Some (Path.Pident _) ->
    apply loc Identifier.Field.dot
      [ apply loc Identifier.Module.compilation_unit [string loc ""];
        string loc lbl_desc.lbl_name ]
  | Some (Path.Pdot (p, _)) ->
    apply loc Identifier.Field.dot
      [module_for_path loc p; string loc lbl_desc.lbl_name]
  | _ -> fatal_error "Unsupported constructor type detected."

let quote_constructor env loc constr =
  match type_path env constr.cstr_res with
  | None -> fatal_error "No global path for constructor"
  | Some (Path.Pident _) -> (
    match constr.cstr_name with
    | "false" -> Lazy.force Identifier.Constructor.false_
    | "true" -> Lazy.force Identifier.Constructor.true_
    | "()" -> Lazy.force Identifier.Constructor.void
    | "[]" -> Lazy.force Identifier.Constructor.nil
    | "::" -> Lazy.force Identifier.Constructor.cons
    | "None" -> Lazy.force Identifier.Constructor.none
    | "Some" -> Lazy.force Identifier.Constructor.some
    | _ ->
      apply loc Identifier.Constructor.dot
        [ apply loc Identifier.Module.compilation_unit [string loc ""];
          string loc constr.cstr_name ])
  | Some (Path.Pdot (p, _)) ->
    apply loc Identifier.Constructor.dot
      [module_for_path loc p; string loc constr.cstr_name]
  | _ -> fatal_error "Unsupported constructor type detected."

let quote_variant loc name = apply loc Variant.of_string [string loc name]

(* let field_for_path loc = function
 *   | Path.Pident id ->
 *     if Ident.is_global id then
 *       apply loc Identifier.Field.dot
 *         [apply loc Identifier.Module.compilation_unit [string loc ""];
 *          string loc (Ident.name id)]
 *     else raise Exit
 *   | Path.Pdot (p, s) -> apply loc Identifier.Field.dot [module_for_path loc p; string loc s]
 *   | _ -> raise Exit *)

(* let field_for_path_opt loc p =
 *   match field_for_path loc p with
 *   | res -> Some res
 *   | exception Exit -> None *)

let quote_nonopt loc (lbl : string option) =
  match lbl with
  | None -> Lazy.force Label.Nonoptional.no_label
  | Some s -> apply loc Label.Nonoptional.labelled [string loc s]

let rec quote_value_pattern p =
  let env = p.pat_env in
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> Lazy.force Pat.any
  | Tpat_var (id, _, _, _) -> apply loc Pat.var [Lvar id]
  | Tpat_alias (pat, id, _, _, _) ->
    let pat = quote_value_pattern pat in
    apply loc Pat.alias [pat; Lvar id]
  | Tpat_constant const ->
    let const = quote_constant loc const in
    apply loc Pat.constant [const]
  | Tpat_tuple pats ->
    let pats =
      List.map
        (fun (lbl, p) -> pair (quote_nonopt loc lbl, quote_value_pattern p))
        pats
    in
    apply loc Pat.tuple [mk_list pats]
  | Tpat_construct (lid, constr, args, _) ->
    let constr = quote_constructor env lid.loc constr in
    let args =
      match args with
      | [] -> None
      | _ :: _ ->
        let args = List.map quote_value_pattern args in
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
    let argo = Option.map quote_value_pattern argo in
    apply loc Pat.variant [variant; option argo]
  | Tpat_record (lbl_pats, closed) ->
    let lbl_pats =
      List.map
        (fun (lid, lbl_desc, pat) ->
          let lbl = quote_record_field env Asttypes.(lid.loc) lbl_desc in
          let pat = quote_value_pattern pat in
          pair (lbl, pat))
        lbl_pats
    in
    let closed =
      match closed with Asttypes.Closed -> true_ | Asttypes.Open -> false_
    in
    apply loc Pat.record [mk_list lbl_pats; closed]
  | Tpat_array (_, _, pats) ->
    let pats = List.map quote_value_pattern pats in
    apply loc Pat.array [mk_list pats]
  | Tpat_or (pat1, pat2, _) ->
    let pat1 = quote_value_pattern pat1 in
    let pat2 = quote_value_pattern pat2 in
    apply loc Pat.or_ [pat1; pat2]
  | Tpat_lazy pat ->
    let pat = quote_value_pattern pat in
    apply loc Pat.lazy_ [pat]
  | _ -> fatal_error "Unsupported pattern type (unboxed stuff)"

let rec quote_lid_module loc = function
  | Lident s -> apply loc Identifier.Module.compilation_unit [string loc s]
  | Ldot (lid, s) ->
    apply loc Identifier.Module.dot [quote_lid_module loc lid; string loc s]
  | _ -> fatal_error "No support for Lapply in quoting modules"

let rec quote_computation_pattern p =
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_value pat -> quote_value_pattern (pat :> value general_pattern)
  | Tpat_exception pat -> apply loc Pat.exception_ [quote_value_pattern pat]
  | Tpat_or (pat1, pat2, _) ->
    let pat1 = quote_computation_pattern pat1 in
    let pat2 = quote_computation_pattern pat2 in
    apply loc Pat.exception_ [pat1; pat2]

type case_binding =
  | Non_binding of lambda * lambda
  | Simple of lambda * lambda
  | Pattern of lambda * lambda * lambda
  | Guarded of lambda * lambda * lambda
  | Refutation of lambda * lambda * lambda

let rec case_binding transl stage case =
  let pat = case.c_lhs in
  match case.c_guard with
  | None -> (
    let binding_with_computation_pat () =
      match pat_bound_idents pat with
      | [] ->
        let exp = quote_expression transl stage case.c_rhs in
        let pat = quote_computation_pattern pat in
        Non_binding (pat, exp)
      | ids -> (
        let names =
          List.map (fun id -> string pat.pat_loc (Ident.name id)) ids
        in
        let pat = quote_computation_pattern pat in
        let exp = quote_expression transl stage case.c_rhs in
        let pat_id = Ident.create_local "pattern" in
        let exp_id = Ident.create_local "expression" in
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
        let name = quote_name name in
        let body = quote_expression transl stage case.c_rhs in
        Simple (name, func [id] body)
      | _ -> binding_with_computation_pat ())
    | _ -> binding_with_computation_pat ())
  | Some guard ->
    let ids = pat_bound_idents case.c_lhs in
    let names = List.map (fun id -> string pat.pat_loc (Ident.name id)) ids in
    let pat = quote_computation_pattern case.c_lhs in
    let guard = quote_expression transl stage guard in
    let exp = quote_expression transl stage case.c_rhs in
    let pat_id = Ident.create_local "pattern" in
    let guard_id = Ident.create_local "guard" in
    let exp_id = Ident.create_local "expression" in
    let body =
      bind pat_id pat
        (bind guard_id guard
           (bind exp_id exp (triple (Lvar pat_id, Lvar guard_id, Lvar exp_id))))
    in
    Guarded
      ( mk_list names,
        mk_list [],
        create_list_param_binding ids (create_list_param_binding [] body) )

and case_value_pattern_binding transl stage case =
  case_binding transl stage
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

and quote_case transl stage loc case =
  quote_case_binding loc (case_binding transl stage case)

and quote_value_pattern_case transl stage loc case =
  quote_case_binding loc (case_value_pattern_binding transl stage case)

and quote_newtype loc ident sloc rest =
  apply loc Function.newtype [quote_loc loc; quote_name sloc; func [ident] rest]

and create_list_param_binding idents body =
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

and fun_param_binding transl stage loc param frest =
  let with_newtypes =
    List.fold_right
      (fun (ident, sloc, _, _) rest -> quote_newtype loc ident sloc rest)
      param.fp_newtypes frest
  in
  let pat, opt_exp =
    match param.fp_kind with
    | Tparam_pat pat -> pat, None
    | Tparam_optional_default (pat, exp, _) ->
      pat, Some (quote_expression transl stage exp)
  in
  let idents = pat_bound_idents pat in
  let names =
    List.map (fun s -> apply loc Name.mk [string loc (Ident.name s)]) idents
  in
  let fun_rem =
    create_list_param_binding idents
      (pair (quote_value_pattern pat, with_newtypes))
  in
  apply loc Function.param
    [ quote_arg_label loc param.fp_arg_label;
      option opt_exp;
      quote_loc loc;
      mk_list names;
      fun_rem ]

and quote_function transl stage loc fn =
  match fn with
  | Texp_function fn ->
    let fn_body =
      match fn.body with
      | Tfunction_body exp ->
        apply loc Function.body [quote_expression transl stage exp]
      | Tfunction_cases cases ->
        apply loc Function.cases
          [ mk_list
              (List.map
                 (fun fc ->
                   quote_case_binding fc.c_lhs.pat_loc
                     (case_value_pattern_binding transl stage fc))
                 cases.fc_cases) ]
    in
    List.fold_right (fun_param_binding transl stage loc) fn.params fn_body
  | _ -> fatal_error "Unexpected usage of quote_function."

and quote_expression_extra _ _ extra lambda =
  let extra, loc, _ = extra in
  match extra with
  | Texp_newtype (id, sloc, _, _) -> quote_newtype loc id sloc lambda
  | _ ->
    failwith "Not implemented yet" (* TODO: type constraints and the rest *)

and quote_expression transl stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  let body =
    match e.exp_desc with
    | Texp_ident (path, _, _, _, _) -> (
      match value_for_path_opt loc path with
      | Some ident_val -> apply loc Exp.ident [ident_val]
      | None -> (
        match path with
        | Pident id ->
          (* TODO: properly check stage *)
          let v = apply loc Identifier.Value.var [Lvar id; quote_loc loc] in
          apply loc Exp.ident [v]
        | _ -> fatal_error "No global path for identifier"))
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
        in
        let defs_lam = List.map (quote_expression transl stage) defs in
        let frest =
          create_list_param_binding idents
            (pair (mk_list defs_lam, quote_expression transl stage exp))
        in
        apply loc Exp.let_rec_simple [quote_loc loc; mk_list names_lam; frest]
      | Nonrecursive ->
        List.fold_right
          (fun vb lexp ->
            let pat = vb.vb_pat in
            let idents = pat_bound_idents pat in
            let names_lam =
              List.map
                (fun s -> apply loc Name.mk [string loc (Ident.name s)])
                idents
            in
            let def = quote_expression transl stage vb.vb_expr in
            let frest =
              create_list_param_binding idents
                (pair (quote_value_pattern pat, lexp))
            in
            apply loc Exp.let_ [quote_loc loc; mk_list names_lam; def; frest])
          vbs
          (quote_expression transl stage exp))
    | Texp_function fun_spec ->
      let fn = quote_function transl stage loc (Texp_function fun_spec) in
      apply loc Exp.function_ [fn]
    | Texp_apply (fn, args, _, _, _) ->
      let fn = quote_expression transl stage fn in
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
              let exp = quote_expression transl stage exp in
              pair (lbl, exp))
          args
      in
      apply loc Exp.apply [fn; mk_list args]
    | Texp_match (exp, _, cases, _) ->
      let exp = quote_expression transl stage exp in
      let cases = List.map (quote_case transl stage loc) cases in
      apply loc Exp.match_ [exp; mk_list cases]
    | Texp_try (exp, cases) ->
      let exp = quote_expression transl stage exp in
      let cases = List.map (quote_value_pattern_case transl stage loc) cases in
      apply loc Exp.try_ [exp; mk_list cases]
    | Texp_tuple (exps, _) ->
      let exps =
        List.map
          (fun (lab, exp) ->
            pair (string_option loc lab, quote_expression transl stage exp))
          exps
      in
      apply loc Exp.tuple [mk_list exps]
    | Texp_construct (lid, constr, args, _) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
          let args = List.map (quote_expression transl stage) args in
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
      let variant = quote_variant loc variant in
      let argo =
        Option.map (fun (arg, _) -> quote_expression transl stage arg) argo
      in
      apply loc Exp.variant [variant; option argo]
    | Texp_record record ->
      let lbl_exps =
        Array.map
          (fun (lbl, def) ->
            let lbl = quote_record_field env loc lbl in
            let exp =
              match def with
              | Overridden (_, exp) -> quote_expression transl stage exp
              | Kept _ ->
                fatal_error "No support for record update syntax in quotations"
            in
            pair (lbl, exp))
          record.fields
      in
      let base =
        Option.map
          (fun (e, _) -> quote_expression transl stage e)
          record.extended_expression
      in
      apply loc Exp.record [mk_list (Array.to_list lbl_exps); option base]
    | Texp_field (rcd, lid, lbl, _, _) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_record_field env lid.loc lbl in
      apply loc Exp.field [rcd; lbl]
    | Texp_setfield (rcd, _, lid, lbl, exp) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_record_field env lid.loc lbl in
      let exp = quote_expression transl stage exp in
      apply loc Exp.setfield [rcd; lbl; exp]
    | Texp_array (_, _, exps, _) ->
      let exps = List.map (quote_expression transl stage) exps in
      apply loc Exp.array [mk_list exps]
    | Texp_ifthenelse (cond, then_, else_) ->
      let cond = quote_expression transl stage cond in
      let then_ = quote_expression transl stage then_ in
      let else_ = Option.map (quote_expression transl stage) else_ in
      apply loc Exp.ifthenelse [cond; then_; option else_]
    | Texp_sequence (exp1, _, exp2) ->
      let exp1 = quote_expression transl stage exp1 in
      let exp2 = quote_expression transl stage exp2 in
      apply loc Exp.sequence [exp1; exp2]
    | Texp_while wh ->
      let cond = quote_expression transl stage wh.wh_cond in
      let body = quote_expression transl stage wh.wh_body in
      apply loc Exp.while_ [cond; body]
    | Texp_for floop ->
      let low = quote_expression transl stage floop.for_from in
      let high = quote_expression transl stage floop.for_to in
      let dir =
        match floop.for_dir with
        | Asttypes.Upto -> true_
        | Asttypes.Downto -> false_
      in
      let name = mkloc (Ident.name floop.for_id) loc in
      let name = quote_name name in
      let body = quote_expression transl stage floop.for_body in
      apply loc Exp.for_simple
        [quote_loc loc; name; low; high; dir; func [floop.for_id] body]
    | Texp_send (obj, meth, _) ->
      let obj = quote_expression transl stage obj in
      let meth = quote_method loc meth in
      apply loc Exp.send [obj; meth]
    | Texp_open (open_decl, exp) -> (
      match open_decl.open_expr.mod_desc with
      | Tmod_ident (_, lid) ->
        let override =
          match open_decl.open_override with Override -> true | Fresh -> false
        in
        let exp = quote_expression transl stage exp in
        let lid = quote_lid_module lid.loc lid.txt in
        apply loc Exp.open_ [quote_bool override; lid; exp]
      | _ -> fatal_error "Unsupported module open syntax" (* TODO *))
    | Texp_assert (exp, _) ->
      let exp = quote_expression transl stage exp in
      apply loc Exp.assert_ [exp]
    | Texp_lazy exp ->
      let exp = quote_expression transl stage exp in
      apply loc Exp.lazy_ [exp]
    | Texp_quotation exp ->
      let exp = quote_expression transl (stage + 1) exp in
      apply loc Exp.quote [exp]
    | Texp_antiquotation exp ->
      if stage > 0
      then
        let exp = quote_expression transl (stage + 1) exp in
        apply loc Exp.antiquote [exp]
      else transl exp
    | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _
    | Texp_letmodule _ | Texp_object _ | Texp_pack _ | Texp_unreachable
    | Texp_src_pos | Texp_unboxed_tuple _ | Texp_record_unboxed_product _
    | Texp_unboxed_field _ | Texp_list_comprehension _
    | Texp_array_comprehension _ | Texp_letexception _ | Texp_letop _
    | Texp_extension_constructor _ | Texp_probe _ | Texp_probe_is_enabled _
    | Texp_exclave _ | Texp_overwrite _ | Texp_hole _ ->
      fatal_error "Expression cannot be quoted"
  in
  List.fold_right (quote_expression_extra transl stage) e.exp_extra body

let transl_quote transl exp = quote_expression transl 0 exp
