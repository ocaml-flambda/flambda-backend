(* Dummy-expr generate dummy expression without using %opaque *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Dummy
open Env

let core_type_from_type_expr etyp =
  {
    ctyp_desc = Ttyp_any;
    ctyp_type = etyp;
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let contain_type p1 etyp =
  let contain = ref false in
  let mapper =
    {
      Tast_mapper.default with
      typ =
        (fun mapper t ->
          match get_desc t.ctyp_type with
          | Tconstr (p2, _, _) ->
              contain := !contain || Path.same p1 p2;
              mapper.typ mapper t
          | _ -> mapper.typ mapper t);
    }
  in
  let _ = mapper.typ mapper (core_type_from_type_expr etyp) in
  !contain

let arity cd =
  match cd.cd_args with
  | Cstr_tuple etyp_list -> List.length etyp_list
  | Cstr_record lab_list -> List.length lab_list

let rec arity_min l min =
  match l with
  | cd :: q -> if arity cd < arity min then arity_min q cd else arity_min q min
  | [] -> min

let is_recursive p cons_decl =
  match cons_decl.cd_args with
  | Cstr_tuple etyp_list -> List.exists (contain_type p) etyp_list
  | Cstr_record lab_list ->
      List.exists (fun lab -> contain_type p lab.ld_type) lab_list

let find_simpler_cons p cstr_list =
  let non_rec_cons = List.filter (is_recursive p) cstr_list in
  arity_min non_rec_cons (List.hd non_rec_cons)

let rec find_value_of_typ t s =
  match s with
  | Env_empty -> failwith "no value of wanted type"
  | Env_value (s, id, vd) -> (
      match get_desc vd.val_type with
      | Tvar (Some s2) when t = s2 -> (id, vd)
      | _ -> find_value_of_typ t s)
  | Env_type (s, _, _)
  | Env_extension (s, _, _)
  | Env_module (s, _, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_open (s, _)
  | Env_functor_arg (s, _)
  | Env_constraints (s, _)
  | Env_copy_types s
  | Env_persistent (s, _)
  | Env_value_unbound (s, _, _)
  | Env_module_unbound (s, _, _) ->
      find_value_of_typ t s

let rec find_value_of_typ_none s =
  match s with
  | Env_empty -> failwith "no value of wanted type"
  | Env_value (s, id, vd) -> (
      match get_desc vd.val_type with
      | Tvar None -> (id, vd)
      | _ -> find_value_of_typ_none s)
  | Env_type (s, _, _)
  | Env_extension (s, _, _)
  | Env_module (s, _, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_open (s, _)
  | Env_functor_arg (s, _)
  | Env_constraints (s, _)
  | Env_copy_types s
  | Env_persistent (s, _)
  | Env_value_unbound (s, _, _)
  | Env_module_unbound (s, _, _) ->
      find_value_of_typ_none s

let is_known id =
  match name id with "int" | "char" | "string" | "unit" -> true | _ -> false

let rec generate_dummy_expr env etyp =
  exp_desc_to_exp
    (match get_desc etyp with
    | Tvar (Some vtyp) ->
        let id, vd = find_value_of_typ vtyp (summary env) in
        Texp_ident
          (Pident id, { txt = Lident (Ident.name id); loc = Location.none }, vd)
    | Tvar None ->
        let id, vd = find_value_of_typ_none (summary env) in
        Texp_ident
          (Pident id, { txt = Lident (Ident.name id); loc = Location.none }, vd)
    | Ttuple etyp_list ->
        Texp_tuple (List.map (generate_dummy_expr env) etyp_list)
    | Tarrow (arg_label, etyp1, etyp2, _) ->
        let param = Ident.create_local "x" in
        let vd = { dummy_value_description with val_type = etyp1 } in
        let nenv = add_value param vd env in
        Texp_function
          {
            arg_label;
            param;
            partial = Partial;
            cases =
              [
                {
                  c_lhs =
                    {
                      any_pat with
                      pat_desc =
                        Tpat_var (param, { loc = Location.none; txt = "x" });
                    };
                  c_guard = None;
                  c_rhs = generate_dummy_expr nenv etyp2;
                };
              ];
          }
    | Tconstr (Pident id, Unapplied, _) when is_known id -> (
        match name id with
        | "int" -> Texp_constant (Const_int 0)
        | "char" -> Texp_constant (Const_char '0')
        | "string" -> Texp_constant (Const_string ("", Location.none, None))
        | "unit" -> Texp_tuple []
        | _ -> assert false)
    | Tconstr (path, te_list, _) -> (
        let te_list = AppArgs.to_list te_list in
        let td = find_type path env in
        match td.type_kind with
        | Type_variant (cstr_list, _) ->
            let cons = find_simpler_cons path cstr_list in
            let cons_descr = find_ident_constructor cons.cd_id env in
            Texp_construct
              ( { txt = Lident (Ident.name cons.cd_id); loc = Location.none },
                cons_descr,
                List.map (generate_dummy_expr env) te_list )
        | _ -> failwith "todo")
    | Tobject _ -> assert false
    | Tfield _ -> assert false
    | Tnil -> failwith "tnil"
    | Tsubst _ -> assert false
    | Tvariant _ -> assert false
    | Tpoly _ -> assert false
    | Tpackage _ -> assert false
    | Tunivar _ -> assert false
    | Tlink etyp -> (generate_dummy_expr env etyp).exp_desc)
