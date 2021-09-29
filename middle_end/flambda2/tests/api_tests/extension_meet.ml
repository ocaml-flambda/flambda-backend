module T = Flambda_type
module TE = Flambda_type.Typing_env
module TEE = Flambda_type.Typing_env_extension

let test_recursive_meet () =
  let env =
    TE.create
      ~resolver:(fun _ -> None)
      ~get_imported_names:(fun () -> Name.Set.empty)
  in
  let var_x = Variable.create "x" in
  let var_y = Variable.create "y" in
  let var_z = Variable.create "z" in
  let var_v = Variable.create "v" in
  let n_x = Name.var var_x in
  let n_y = Name.var var_y in
  let n_z = Name.var var_z in
  let n_v = Name.var var_v in
  let nb_x = Bound_name.create n_x Name_mode.normal in
  let nb_y = Bound_name.create n_y Name_mode.normal in
  let nb_z = Bound_name.create n_z Name_mode.normal in
  let nb_v = Bound_name.create n_v Name_mode.normal in
  let env = TE.add_definition env nb_x Flambda_kind.value in
  let env = TE.add_definition env nb_y Flambda_kind.value in
  let env = TE.add_definition env nb_z Flambda_kind.value in
  let env = TE.add_definition env nb_v Flambda_kind.value in
  let alias name = T.alias_type_of Flambda_kind.value (Simple.name name) in
  let mk_block_type name =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias name]
  in
  let env = TE.add_equation env n_x (mk_block_type n_y) in
  let env = TE.add_equation env n_y (mk_block_type n_z) in
  let env = TE.add_equation env n_z (mk_block_type n_x) in
  let ty1 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_v; alias n_v]
  in
  let ty2 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_x; alias n_y]
  in
  Format.eprintf "Environment: %a@." TE.print env;
  match T.meet env ty1 ty2 with
  | Ok (ty, ext) ->
    Format.eprintf "Result type: %a@.Extension:@ %a@." T.print ty TEE.print ext
  | Bottom -> Format.eprintf "Bottom@."

let test_bottom_detection () =
  let env =
    TE.create
      ~resolver:(fun _ -> None)
      ~get_imported_names:(fun () -> Name.Set.empty)
  in
  let var_x = Variable.create "x" in
  let n_x = Name.var var_x in
  let nb_x = Bound_name.create n_x Name_mode.normal in
  let env = TE.add_definition env nb_x Flambda_kind.value in
  let alias name = T.alias_type_of Flambda_kind.value (Simple.name name) in
  let const n =
    T.alias_type_of Flambda_kind.value
      (Simple.const
         (Reg_width_things.Const.const_int (Targetint_31_63.Imm.of_int n)))
  in
  let ty1 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_x; alias n_x]
  in
  let ty2 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[const 0; const 1]
  in
  Format.eprintf "Environment: %a@." TE.print env;
  match T.meet env ty1 ty2 with
  | Ok (ty, ext) ->
    Format.eprintf "Result type: %a@.Extension:@ %a@." T.print ty TEE.print ext
  | Bottom -> Format.eprintf "Bottom@."

let test_bottom_recursive () =
  let env =
    TE.create
      ~resolver:(fun _ -> None)
      ~get_imported_names:(fun () -> Name.Set.empty)
  in
  let var_x = Variable.create "x" in
  let n_x = Name.var var_x in
  let nb_x = Bound_name.create n_x Name_mode.normal in
  let env = TE.add_definition env nb_x Flambda_kind.value in
  let alias name = T.alias_type_of Flambda_kind.value (Simple.name name) in
  let const n =
    T.alias_type_of Flambda_kind.value
      (Simple.const
         (Reg_width_things.Const.const_int (Targetint_31_63.Imm.of_int n)))
  in
  let ty_x =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[T.unknown Flambda_kind.value; alias n_x]
  in
  let env = TE.add_equation env n_x ty_x in
  let ty_cell2 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[const 1; T.unknown Flambda_kind.value]
  in
  let ty_cell1 =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[const 0; ty_cell2]
  in
  Format.eprintf "Environment: %a@." TE.print env;
  match T.meet env (alias n_x) ty_cell1 with
  | Ok (ty, ext) ->
    Format.eprintf "Result type: %a@.Extension:@ %a@." T.print ty TEE.print ext
  | Bottom ->
    let[@inline never] [@local never] breakpoint () = () in
    breakpoint ();
    Format.eprintf "Bottom@."

let test_double_recursion () =
  let env =
    TE.create
      ~resolver:(fun _ -> None)
      ~get_imported_names:(fun () -> Name.Set.empty)
  in
  let var_x = Variable.create "x" in
  let var_y = Variable.create "y" in
  let var_z = Variable.create "z" in
  let n_x = Name.var var_x in
  let n_y = Name.var var_y in
  let n_z = Name.var var_z in
  let nb_x = Bound_name.create n_x Name_mode.normal in
  let nb_y = Bound_name.create n_y Name_mode.normal in
  let nb_z = Bound_name.create n_z Name_mode.normal in
  let env = TE.add_definition env nb_x Flambda_kind.value in
  let env = TE.add_definition env nb_y Flambda_kind.value in
  let env = TE.add_definition env nb_z Flambda_kind.value in
  let alias name = T.alias_type_of Flambda_kind.value (Simple.name name) in
  let ty_x =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_x; alias n_y; alias n_z]
  in
  let ty_y =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_y; alias n_z; alias n_x]
  in
  let ty_z =
    T.immutable_block ~is_unique:false Tag.zero ~field_kind:Flambda_kind.value
      ~fields:[alias n_z; alias n_x; alias n_y]
  in
  let env = TE.add_equation env n_x ty_x in
  let env = TE.add_equation env n_y ty_y in
  let env = TE.add_equation env n_z ty_z in
  Format.eprintf "Environment: %a@." TE.print env;
  match T.meet env (alias n_x) (alias n_y) with
  | Ok (ty, ext) ->
    Format.eprintf "Result type: %a@.Extension:@ %a@." T.print ty TEE.print ext
  | Bottom -> Format.eprintf "Bottom@."

let _ =
  let comp_unit =
    let id = Ident.create_persistent "Test" in
    let linkage_name = Linkage_name.create "camlTest" in
    Compilation_unit.create id linkage_name
  in
  Compilation_unit.set_current comp_unit;
  test_double_recursion ()
