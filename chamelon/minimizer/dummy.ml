(* Dummy expressions *)

open Typedtree
open Types
open Ident
open Compat

(* Dummy - utils *)
let dummy_type_expr = newty2 ~level:0 (mkTvar (Some "a"))

let dummy_core_type =
  {
    ctyp_desc = mkTtyp_any;
    ctyp_type = dummy_type_expr;
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let a_typ =
  {
    ctyp_desc = mkTtyp_var "a";
    ctyp_type = newty2 ~level:0 (mkTvar (Some "a"));
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let unit_typ =
  {
    ctyp_desc =
      Ttyp_constr
        ( Path.Pident (create_scoped ~scope:0 "unit"),
          { txt = Lident "unit"; loc = Location.none },
          [] );
    ctyp_type = newty2 ~level:0 (Ttuple []);
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let a_to_unit =
  {
    ctyp_desc = Ttyp_arrow (Nolabel, a_typ, unit_typ);
    ctyp_type =
      newty2 ~level:0
        (mkTarrow (Nolabel, a_typ.ctyp_type, unit_typ.ctyp_type, commu_ok));
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let unit_to_a =
  {
    ctyp_desc = Ttyp_arrow (Nolabel, unit_typ, a_typ);
    ctyp_type =
      newty2 ~level:0
        (mkTarrow (Nolabel, unit_typ.ctyp_type, a_typ.ctyp_type, commu_ok));
    ctyp_env = Env.empty;
    ctyp_loc = Location.none;
    ctyp_attributes = [];
  }

let dummy_core_typet : Parsetree.core_type =
  {
    ptyp_desc = Ptyp_any;
    ptyp_loc_stack = [];
    ptyp_loc = Location.none;
    ptyp_attributes = [];
  }

let dummy_value_description =
  mk_value_description ~val_type:dummy_type_expr ~val_kind:Val_reg
    ~val_attributes:[]

let exp_desc_to_exp ed =
  {
    exp_desc = ed;
    exp_loc = Location.none;
    exp_extra = [];
    exp_type = dummy_type_expr;
    exp_env = Env.empty;
    exp_attributes = [];
  }

let mk_attribute txt : attribute =
  {
    attr_name = { txt; loc = Location.none };
    attr_payload = PStr [];
    attr_loc = Location.none;
  }

let inline_never : attribute = mk_attribute "inline never"
let inline_always : attribute = mk_attribute "inline always"
let local_always : attribute = mk_attribute "local always"
let local_never : attribute = mk_attribute "local never"
let false_description = mk_constructor_description "false"

let false_expr =
  mkTexp_construct
    ({ txt = Lident "false"; loc = Location.none }, false_description, [])

let any_pat =
  {
    pat_desc = Tpat_any;
    pat_loc = Location.none;
    pat_extra = [];
    pat_type = dummy_type_expr;
    pat_env = Env.empty;
    pat_attributes = [];
  }

let dummy1_str_it_desc =
  Tstr_value
    ( Nonrecursive,
      [
        mk_value_binding
          ~vb_pat:
            {
              pat_desc =
                mkTpat_var
                  ( create_scoped ~scope:0 "__dummy1__",
                    { txt = "__dummy1__"; loc = Location.none } );
              pat_loc = Location.none;
              pat_extra = [];
              pat_type = dummy_type_expr;
              pat_env = Env.empty;
              pat_attributes = [];
            }
          ~vb_expr:
            (exp_desc_to_exp
               (mkTexp_function
                  (Function_compat.cases_view_to_function
                     {
                       arg_label = Nolabel;
                       param = create_local "()";
                       partial = Total;
                       optional_default = None;
                       cases_view_identifier =
                         Param texp_function_param_identifier_defaults;
                       cases =
                         [
                           {
                             c_lhs = any_pat;
                             c_guard = None;
                             c_rhs =
                               exp_desc_to_exp
                                 (mkTexp_assert
                                    (exp_desc_to_exp false_expr)
                                    Location.none);
                           };
                         ];
                     })))
          ~vb_attributes:[ inline_never ];
      ] )

let dummy1_def =
  {
    str_desc = dummy1_str_it_desc;
    str_loc = Location.none;
    str_env = Env.empty;
  }

let dummy1_desc =
  mkTexp_ident
    ( Pident (create_scoped ~scope:0 "__dummy1__"),
      { txt = Lident "__dummy1__"; loc = Location.none },
      dummy_value_description )

let dummy1 =
  {
    exp_desc = dummy1_desc;
    exp_loc = Location.none;
    exp_extra = [];
    exp_type = dummy_type_expr;
    exp_env = Env.empty;
    exp_attributes = [];
  }

(* external __dummy2__ : _ = "%opaque"*)

let dummy2_vd =
  {
    val_id = create_scoped ~scope:0 "__dummy2__";
    val_name = { txt = "__dummy2__"; loc = Location.none };
    val_desc = unit_to_a;
    val_prim = [ "%opaque" ];
    val_val = dummy_value_description;
    val_loc = Location.none;
    val_attributes = [];
  }

let dummy2_str_it_desc = Tstr_primitive dummy2_vd

let dummy2_def =
  {
    str_desc = dummy2_str_it_desc;
    str_loc = Location.none;
    str_env = Env.empty;
  }

let dummy2_desc =
  mkTexp_ident
    ( Pident dummy2_vd.val_id,
      { txt = Lident "__dummy2__"; loc = Location.none },
      dummy_value_description )

let dummy2 = exp_desc_to_exp dummy2_desc

let apply_dummy1 =
  exp_desc_to_exp
    (mkTexp_apply
       (dummy1, [ (Nolabel, mkExpArg (exp_desc_to_exp (mkTexp_tuple []))) ]))

let apply_dummy2 =
  exp_desc_to_exp
    (mkTexp_apply
       (dummy2, [ (Nolabel, mkExpArg (exp_desc_to_exp (mkTexp_tuple []))) ]))

(* external __ignore__ : _ = "%ignore"*)

let ignore_vd =
  {
    val_id = create_scoped ~scope:0 "__ignore__";
    val_name = { txt = "__ignore__"; loc = Location.none };
    val_desc = a_to_unit;
    val_prim = [ "%ignore" ];
    val_val = dummy_value_description;
    val_loc = Location.none;
    val_attributes = [];
  }

let ignore_str_it_desc = Tstr_primitive ignore_vd

let ignore_def =
  {
    str_desc = ignore_str_it_desc;
    str_loc = Location.none;
    str_env = Env.empty;
  }

let ignore_desc =
  mkTexp_ident
    ( Pident ignore_vd.val_id,
      { txt = Lident "__ignore__"; loc = Location.none },
      dummy_value_description )

let ignore = exp_desc_to_exp ignore_desc

(* empty cases*)

let empty_value_case =
  {
    c_lhs =
      {
        pat_desc = Tpat_any;
        pat_loc = Location.none;
        pat_extra = [];
        pat_type = dummy_type_expr;
        pat_env = Env.empty;
        pat_attributes = [];
      };
    c_guard = None;
    c_rhs =
      {
        exp_desc = apply_dummy2.exp_desc;
        exp_loc = Location.none;
        exp_extra = [];
        exp_type = dummy_type_expr;
        exp_env = Env.empty;
        exp_attributes = [];
      };
  }

let empty_computation_case =
  {
    c_lhs =
      as_computation_pattern
        {
          pat_desc = Tpat_any;
          pat_loc = Location.none;
          pat_extra = [];
          pat_type = dummy_type_expr;
          pat_env = Env.empty;
          pat_attributes = [];
        };
    c_guard = None;
    c_rhs =
      {
        exp_desc = apply_dummy2.exp_desc;
        exp_loc = Location.none;
        exp_extra = [];
        exp_type = dummy_type_expr;
        exp_env = Env.empty;
        exp_attributes = [];
      };
  }
