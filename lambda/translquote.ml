open Misc
open Asttypes
open Types
open Typedtree
open Lambda

let camlinternalQuote =
  lazy
    (match Env.open_pers_signature
             "CamlinternalQuote" Env.initial_safe_string with
     | exception Not_found ->
         fatal_error "Module CamlinternalQuote unavailable."
     | env -> ident, env)

let combinator modname field =
  lazy
    (let (ident, env) = Lazy.force camlinternalQuote in
     let lid = Longident.Ldot(Longident.Lident modname, field) in
     match Env.lookup_value lid env with
     | (Path.Pdot(Path.Pdot(Pident ident, _, pos1), _, pos2), _) ->
         Lprim(Pfield pos2,
               [Lprim(Pfield pos1,
                     [Lprim(Pgetglobal ident, [])])])
     | _ ->
         fatal_error
           "Primitive CamlinternalQuote."^modname^"."^field^" not found."
     | exception Not_found ->
        fatal_error
          "Primitive CamlinternalQuote."^modname^"."^field^" not found.")

module Loc = struct
  let none = combinator "Loc" "none"
  let unmarshal = combinator "Loc" "unmarshal"
end

module Name = struct
  let mk = combinator "Name" "mk"
  let unmarshal = combinator "Name" "unmarshal"
end

module Var : sig
  let name = combinator "Var" "name"
end

module Constant : sig
  let unmarshal = combinator "Constant" "unmarshal"
end

module Ident : sig
  let unmarshal = combinator "Ident" "unmarshal"
end

module Label : sig
  let none = combinator "Label" "none"
  let of_string = combinator "Label" "of_string"
end

module Variant : sig
  let of_string = combinator "Variant" "of_string"
end

module Method : sig
  let of_string = combinator "Method" "of_string"
end

module Pat : sig
  let any = combinator "Pat" "any"
  let var = combinator "Pat" "var"
  let alias = combinator "Pat" "alias"
  let constant = combinator "Pat" "constant"
  let interval = combinator "Pat" "interval"
  let tuple = combinator "Pat" "tuple"
  let construct = combinator "Pat" "construct"
  let variant = combinator "Pat" "variant"
  let record = combinator "Pat" "record"
  let array = combinator "Pat" "array"
  let or_ = combinator "Pat" "or"
  let type_ = combinator "Pat" "type"
  let lazy_ = combinator "Pat" "lazy"
  let exception_ = combinator "Pat" "exception"
end

module rec Case : sig
  let nonbinding = combinator "Case" "nonbinding"
  let simple = combinator "Case" "simple"
  let pattern = combinator "Case" "pattern"
  let guarded = combinator "Case" "guarded"
end

and Exp : sig
  let var = combinator "Exp" "var"
  let ident = combinator "Exp" "ident"
  let constant = combinator "Exp" "constant"
  let let_simple = combinator "Exp" "let_simple"
  let let_rec_simple = combinator "Exp" "let_rec_simple"
  let let_ = combinator "Exp" "let_"
  let fun_nonbinding = combinator "Exp" "fun_nonbinding"
  let fun_simple = combinator "Exp" "fun_simple"
  let fun_ = combinator "Exp" "fun_"
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
  let for_nonbinding = combinator "Exp" "for_nonbinding"
  let for_simple = combinator "Exp" "for_simple"
  let send = combinator "Exp" "send"
  let assert_ = combinator "Exp" "assert_"
  let lazy_ = combinator "Exp" "lazy_"
  let open_ = combinator "Exp" "open_"
  let quote = combinator "Exp" "quote"
  let escape = combinator "Exp" "escape"
end

let use comb =
  Lazy.force comb

let apply loc comb args =
  let comb = Lazy.force comb in
    Lapply (comb, args, loc)

let string s =
  Lconst (Const_base (Const_string(s, None)))

let marshal x =
  let s = Marshal.to_string x in
    string s

let true_ = Lconst(Const_pointer 1)

let false_ = Lconst(Const_pointer 0)

let none =  Lconst(Const_pointer 0)

let some x = Lprim(Pmakeblock(0, Immutable), [x])

let option opt =
  match opt with
  | None -> none
  | Some x -> some x

let nil = Lconst(Const_pointer 0)

let cons hd tl = Lprim(Pmakeblock(0, Immutable), [hd; tl])

let rec list list =
  match list with
  | [] -> nil
  | hd :: tl -> cons hd (list tl)

let pair (x, y) =
  Lprim(Pmakeblock(0, Immutable), [x; y])

let triple (x, y, z) =
  Lprim(Pmakeblock(0, Immutable), [x; y; z])

let func ids body =
  Lfunction(Curried, ids, body)

let bind id def body =
  Llet(Strict, id, def, body)

let quote_loc (loc : Location.t) =
  if loc = Location.none then use Loc.none
  else apply Location.none Loc.unmarshal [marshal loc]

let quote_constant loc (const : Asttypes.constant) =
  apply loc Const.unmarshal [marshal const]

let quote_name loc (str : string loc) =
  apply loc Name.unmarshal [marshal str]

let quote_variant loc (variant : label) =
  apply loc Variant.of_string [string variant]

let quote_method loc (meth : Typedtree.meth) =
  let name =
    match meth with
    | Tmeth_name name -> name
    | Tmeth_val id -> Ident.name id
  in
  apply loc Method.of_string [string name]

let quote_label loc lbl =
  if lbl = "" then use Label.none
  else apply loc Label.of_string [string lbl]

let lid_of_path p =
  let rec loop = function
    | Pident id ->
      if Ident.global id then Lid (Ident.name id)
      else raise Exit
    | Pdot(p, s, _) -> Ldot(loop p, s)
    | Papply _ -> raise Exit
  in
    match loop p with
    | lid -> Some lid
    | exception Exit -> None

let lid_of_type_path env ty =
  let desc =
    (Ctype.repr (Ctype.expand_head_opt env (Ctype.correct_levels ty))).desc
  in
  match desc with
  | Tconstr(p, _, _) -> lid_of_path p
  | _ -> None

let quote_variant_constructor env loc constr =
  let lid =
    match lid_of_type_path env constr.cstr_res with
    | None -> fatal_error "No global path for variant constructor"
    | Some (Lident _) -> Lident constr.cstr_name
    | Some (Ldot(lid, _)) -> Ldot(lid, constr.cstr_name)
  in
  apply loc Ident.unmarshall [marshall lid]

let quote_record_label env loc lbl =
  let lid =
    match lid_of_type_path env lbl.lbl_res with
    | None -> fatal_error "No global path for record label"
    | Some (Lident _) -> Lident constr.lbl_name
    | Some (Ldot(lid, _)) -> Ldot(lid, constr.lbl_name)
  in
  apply loc Ident.unmarshall [marshall lid]

let rec quote_pattern p =
  let env = p.pat_env in
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> apply loc Pat.any [quote_loc loc]
  | Tpat_var(id, _) -> apply loc Pat.var [quote_loc loc; Lvar id]
  | Tpat_alias(pat, id, _) ->
      let pat = quote_pattern pat in
      apply loc Pat.alias [quote_loc loc; pat; Lvar id]
  | Tpat_constant const ->
      let const = quote_constant loc const in
      apply loc Pat.const [quote_loc loc; const]
  | Tpat_tuple pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.tuple [quote_loc loc; list pats]
  | Tpat_construct(lid, constr, args) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
            let args = List.map quote_pattern args in
            Some (apply loc Pat.tuple [quote_loc loc; list args])
      in
      apply loc Pat.construct [quote_loc loc; constr; option args]
  | Tpat_variant(variant, argo, _) ->
      let variant = quote_variant loc variant in
      let argo = Utils.may_map quote_pattern argo in
      apply loc Pat.variant [quote_loc loc; variant; option argo]
  | Tpat_record(lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl, pat) ->
            let lbl = quote_record_label env lid.loc lbl in
            let pat = quote_pattern pat in
            pair (lbl, pat))
          lbl_pats
      in
      let closed =
        match closed with
        | Asttypes.Closed -> true_
        | Asttypes.Open -> false_
      in
      apply loc Pat.record [quote_loc loc; list lbl_pats; closed]
  | Tpat_array pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.array [quote_loc loc; list pats]
  | Tpat_or(pat1, pat2, _) ->
      let pat1 = quote_pattern pat1 in
      let pat2 = quote_pattern pat2 in
      apply loc Pat.or_ [quote_loc loc; pat1; pat2]
  | Tpat_lazy pat ->
      let pat = quote_pattern pat in
      apply loc Pat.lazy_ [quote_loc loc; pat]

type case_binding =
  | Non_binding of lambda * lambda
  | Simple of lambda * lambda
  | Pattern of lambda * lambda
  | Guarded of lambda * lambda

let rec case_binding exn transl stage cases =
  match cases.c_guard with
  | None -> begin
      match case.c_lhs, exn with
      | Tpat_var(id, name), false ->
          let name = quote_name name.loc name in
          let body = quote_expression transl stage case.c_rhs in
          Simple(name, func [id] body)
      | pat, _ ->
          match pat_bound_idents pat with
          | [] ->
              let pat = quote_pattern pat in
              let pat =
                if exn then
                  apply pat.pat_loc Pat.exception_ [quote_loc loc; pat]
                else
                  pat
              in
              let exp = quote_expression transl stage case.c_rhs in
              Non_binding(pat, exp)
          | id_names ->
              let ids = List.map fst id_names in
              let names =
                List.map (fun (_, name) -> quote_name name.loc name) id_names
              in
              let pat = quote_pattern pat in
              let pat =
                if exn then
                  apply pat.pat_loc Pat.exception_ [quote_loc loc; pat]
                else
                  pat
              in
              let exp = quote_expression transl stage case.c_rhs in
              let pat_id = Ident.create "pattern" in
              let exp_id = Ident.create "expression" in
              let body =
                bind pat_id pat
                  (bind exp_id exp
                    (pair (Lvar pat_id, Lvar exp_id)))
              in
              Pattern(list names, func ids body)
    end
  | Some guard ->
      let id_names = pat_bound_idents case.c_lhs in
      let ids = List.map fst id_names in
      let names =
        List.map (fun (_, name) -> quote_name name.loc name) id_names
      in
      let pat = quote_pattern case.c_lhs in
      let pat =
        if exn then apply pat.pat_loc Pat.exception_ [quote_loc loc; pat]
        else pat
      in
      let guard = quote_expression transl stage guard in
      let exp = quote_expression transl stage case.c_rhs in
      let pat_id = Ident.create "pattern" in
      let guard_id = Ident.create "guard" in
      let exp_id = Ident.create "expression" in
      let body =
        bind pat_id pat
          (bind guard_id guard
            (bind exp_id exp
              (triple (Lvar pat_id, Lvar guard_id, Lvar exp_id)))
      in
      Guarded(list names, func ids body)

and quote_case_binding loc cb =
  match cb with
  | Non_binding(pat, exp) ->
      apply loc Case.nonbinding [quote_loc loc; pat; exp]
  | Simple(name, body) ->
      apply loc Case.simple [quote_loc loc; name; body]
  | Pattern(names, body) ->
      apply loc Case.pattern [quote_loc loc; names; body]
  | Guarded(names, body) ->
      apply loc Case.guarded [quote_loc loc; names; body]

and quote_case exn transl stage loc case =
  quote_case_binding loc (case_binding exn transl stage case)

and quote_expression_extra transl stage extra lambda =
  match extra with
  | _ -> failwith "Not implemented"

and quote_expression transl stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  let body =
    match e.exp_desc with
    | Texp_ident(path, _, _) -> begin
        match lid_of_path path with
        | Some lid ->
            let lid = apply loc Ident.unmarshall [marshall lid] in
            apply loc Exp.ident [quote_loc loc; lid]
        | None ->
            match path with
            | Pident id ->
                (* TODO: properly check stage *)
                apply loc Exp.var [quote_loc loc; Lvar id]
            | Pdot _ | Papply _ ->
                fatal_error "No global path for identifier"
      end
    | Texp_constant const ->
        let const = quote_constant loc const in
        apply loc Exp.constant [quote_loc loc; const]
    | Texp_let of rec_flag * value_binding list * expression
    | Texp_function(label, cases, _) -> begin
        let cbs = List.map (case_binding transl stage) cases in
        match cbs with
        | [Non_binding(pat, exp)] ->
            let label = quote_label label in
            apply loc Exp.fun_nonbinding [quote_loc loc; label; pat; exp]
        | [Simple(name, body)] ->
            let label = quote_label label in
            apply loc Exp.fun_simple [quote_loc loc; name; label; body]
        | [Pattern(names, body)] ->
            let label = quote_label label in
            apply loc Exp.fun_ [quote_loc loc; names; label; body]
        | cases ->
            let cases = List.map (quote_case_binding loc) cases in
            apply loc Exp.function_ [quote_loc loc; list cases]
      end
    | Texp_apply(fn, args) ->
        let fn = quote_expression transl stage fn in
        let args = List.filter (fun (_, exp, _) -> exp <> None) args in
        let args =
          List.map
            (fun (lbl, exp, _) ->
              match exp with
              | None -> assert false
              | Some exp ->
                  let lbl = quote_label loc lbl in
                  let exp = quote_expression transl stage exp in
                  pair (lbl, exp))
            args
        in
        apply loc Exp.apply [quote_loc loc; fn; list args]
    | Texp_match(exp, cases, exn_cases, _) ->
        let exp = quote_expression transl stage exp in
        let cases = List.map (quote_case false transl stage loc) cases in
        let exn_cases = List.map (quote_case true transl stage loc) exn_cases in
        apply loc Exp.match_ [quote_loc loc; exp; list (cases @ exn_cases)]
    | Texp_try(exp, cases) ->
        let exp = quote_expression transl stage exp in
        let cases = List.map (quote_case false transl stage loc) cases in
        apply loc Exp.try_ [quote_loc loc; exp; list cases]
    | Texp_tuple exps ->
        let exps = List.map (quote_expression transl stage) exps in
        apply loc Exp.tuple [quote_loc loc; list exps]
    | Texp_construct(lid, constr, args) ->
        let constr = quote_constructor env lid.loc constr in
        let args =
          match args with
          | [] -> None
          | _ :: _ ->
              let args = List.map (quote_expression transl stage) args in
              Some (apply loc Pat.tuple [quote_loc loc; list args])
        in
        apply loc Exp.construct [quote_loc loc; constr; option args]
    | Texp_variant(variant, argo) ->
        let variant = quote_variant loc variant in
        let argo = Utils.may_map (quote_expression transl stage) argo in
        apply loc Exp.variant [quote_loc loc; variant; option argo]
    | Texp_record(lbl_exps, base) ->
        let lbl_exps =
          List.map
            (fun (lid, lbl, exp) ->
              let lbl = quote_record_label env lid.loc lbl in
              let exp = quote_expression transl stage exp in
              pair (lbl, exp))
            lbl_exps
        in
        let base = Misc.may_map quote_expression base in
        apply loc Exp.record [quote_loc loc; list lbl_exps; option base]
    | Texp_field(rcd, lid, lbl) ->
        let rcd = quote_expression transl stage rcd in
        let lbl = quote_record_label env lid.loc lbl in
        apply loc Exp.field [quote_loc loc; rcd; lbl]
    | Texp_setfield(rcd, lid, lbl, exp) ->
        let rcd = quote_expression transl stage rcd in
        let lbl = quote_record_label env lid.loc lbl in
        let exp = quote_expression transl stage exp in
        apply loc Exp.setfield [quote_loc loc; rcd; lbl; exp]
    | Texp_array exps ->
        let exps = List.map (quote_expression transl stage) exps in
        apply loc Exp.array [quote_loc loc; list exps]
    | Texp_ifthenelse(cond, then_, else_) ->
        let cond = quote_expression transl stage cond in
        let then_ = quote_expression transl stage then_ in
        let else_ = Utils.may_map (quote_expression transl stage) else_ in
        apply loc Exp.ifthenelse [quote_loc loc; cond; then_; option else_]
    | Texp_sequence(exp1, exp2) ->
        let exp1 = quote_expression transl stage exp1 in
        let exp2 = quote_expression transl stage exp2 in
        apply loc Exp.sequence [quote_loc loc; exp1; exp2]
    | Texp_while(cond, body) ->
        let cond = quote_expression transl stage cond in
        let body = quote_expression transl stage body in
        apply loc Exp.while_ [quote_loc loc; cond; body]
    | Texp_for(_, pat, low, high, dir, body) -> begin
        let low = quote_expression transl stage low in
        let high = quote_expression transl stage high in
        let dir =
          match dir with
          | Asttype.Upto -> true_
          | Astype.Downto -> false_
        in
        match pat with
        | Tpat_var(id, name) ->
            let name = quote_name name.loc name in
            let body = quote_expression transl stage case.c_rhs in
            apply loc Exp.for_simple
              [quote_loc loc; name; low; high; dir; func [id] body]
        | pat ->
            let pat = quote_pattern pat in
            let body = quote_expression transl stage body in
            apply loc Exp.for_nonbinding
              [quote_loc loc; pat; low; high; dir; body]
      end
    | Texp_send(obj, meth, _) ->
        let obj = quote_expression transl stage obj in
        let meth = quote_method meth in
        apply loc Exp.send [quote_loc loc; obj; meth]
    | Texp_assert exp ->
        let exp = quote_expression transl stage exp in
        apply loc Exp.assert_ [quote_loc loc; exp]
    | Texp_lazy exp ->
        let exp = quote_expression transl stage exp in
        apply loc Exp.lazy_ [quote_loc loc; exp]
    | Texp_quote exp ->
        let exp = quote_expression (stage + 1) exp in
        apply loc Exp.quote_ [quote_loc loc; exp]
    | Texp_escape exp ->
        if stage > 0 then begin
            let exp = quote_expression (stage + 1) exp in
            apply loc Exp.escape_ [quote_loc loc; exp]
          end else transl exp
    | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _
      | Texp_letmodule _ | Texp_object _ | Texp_pack _ ->
        fatal_error "Expression cannot be quoted"
  in
  List.fold_right
    (quote_expression_extra transl stage)
    e.exp_extra body

let transl_quote =
