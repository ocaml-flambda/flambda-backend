open Flambda2_bound_identifiers
open Flambda2_identifiers
open Flambda2_kinds
open Flambda2_nominal
open Flambda2_numbers
open Flambda2_term_basics
module K = Flambda_kind
module T = Flambda2_types
module TE = T.Typing_env
module TEE = T.Typing_env_extension

let create_env () =
  let resolver _ = None in
  let get_imported_names () = Name.Set.empty in
  TE.create ~resolver ~get_imported_names

let test_meet_chains_two_vars () =
  let env = create_env () in
  let var1 = Variable.create "var1" in
  let var1' = Bound_var.create var1 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var1') K.value in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
         Alloc_mode.For_types.heap ~fields:[T.any_tagged_immediate])
  in
  let var2 = Variable.create "var2" in
  let var2' = Bound_var.create var2 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var2') K.value in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let symbol =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string "my_symbol")
  in
  let env = TE.add_definition env (Bound_name.create_symbol symbol) K.value in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var2 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!" Variable.print var2 T.print
    new_type_for_var2;
  match T.meet env first_type_for_var2 new_type_for_var2 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var2) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let test_meet_chains_three_vars () =
  let env = create_env () in
  let var1 = Variable.create "var1" in
  let var1' = Bound_var.create var1 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var1') K.value in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
         Alloc_mode.For_types.heap ~fields:[T.any_tagged_immediate])
  in
  let var2 = Variable.create "var2" in
  let var2' = Bound_var.create var2 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var2') K.value in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let var3 = Variable.create "var3" in
  let var3' = Bound_var.create var3 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var3') K.value in
  let first_type_for_var3 = T.alias_type_of K.value (Simple.var var2) in
  let env = TE.add_equation env (Name.var var3) first_type_for_var3 in
  let symbol =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string "my_symbol")
  in
  let env = TE.add_definition env (Bound_name.create_symbol symbol) K.value in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var3 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!" Variable.print var3 T.print
    new_type_for_var3;
  match T.meet env first_type_for_var3 new_type_for_var3 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var3) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let meet_variants_don't_lose_aliases () =
  let env = create_env () in
  let define env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') K.value
  in
  let defines env l = List.fold_left define env l in
  let vx = Variable.create "x" in
  let vy = Variable.create "y" in
  let va = Variable.create "a" in
  let vb = Variable.create "b" in
  let v_variant = Variable.create "variant" in
  let env = defines env [vx; vy; va; vb; v_variant] in
  let const_ctors = T.bottom K.naked_immediate in
  let ty1 =
    let non_const_ctors =
      Tag.Scannable.Map.of_list
        [ Tag.Scannable.create_exn 0, [T.alias_type_of K.value (Simple.var vx)];
          Tag.Scannable.create_exn 1, [T.alias_type_of K.value (Simple.var vy)]
        ]
    in
    T.variant ~const_ctors ~non_const_ctors Alloc_mode.For_types.heap
  in
  let ty2 =
    let non_const_ctors =
      Tag.Scannable.Map.of_list
        [ Tag.Scannable.create_exn 0, [T.alias_type_of K.value (Simple.var va)];
          Tag.Scannable.create_exn 1, [T.alias_type_of K.value (Simple.var vb)]
        ]
    in
    T.variant ~const_ctors ~non_const_ctors Alloc_mode.For_types.heap
  in
  match T.meet env ty1 ty2 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) -> (
    Format.eprintf "@[<hov 2>Meet:@ %a@ /\\@ %a =>@ %a +@ %a@]@." T.print ty1
      T.print ty2 T.print meet_ty TEE.print env_extension;
    (* Env extension should be empty *)
    let env = TE.add_equation env (Name.var v_variant) meet_ty in
    let t_get_tag = T.get_tag_for_block ~block:(Name.var v_variant) in
    let t_tag_1 = T.this_naked_immediate Targetint_31_63.one in
    match T.meet env t_get_tag t_tag_1 with
    | Bottom -> assert false
    | Ok (tag_meet_ty, tag_meet_env_extension) ->
      Format.eprintf "t_get_tag: %a@.t_tag: %a@." T.print t_get_tag T.print
        t_tag_1;
      Format.eprintf "@[<hov 2>meet:@ %a@]@.@[<hov 2>env_extension:@ %a@]@."
        T.print tag_meet_ty TEE.print tag_meet_env_extension)

let test_meet_two_blocks () =
  let define env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') K.value
  in
  let defines env l = List.fold_left define env l in
  let env = create_env () in
  let block1 = Variable.create "block1" in
  let field1 = Variable.create "field1" in
  let block2 = Variable.create "block2" in
  let field2 = Variable.create "field2" in
  let env = defines env [block1; block2; field1; field2] in
  let env =
    TE.add_equation env (Name.var block1)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
         Alloc_mode.For_types.heap
         ~fields:[T.alias_type_of K.value (Simple.var field1)])
  in
  let env =
    TE.add_equation env (Name.var block2)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
         Alloc_mode.For_types.heap
         ~fields:[T.alias_type_of K.value (Simple.var field2)])
  in
  (* let test b1 b2 env =
   *   let eq_block2 = T.alias_type_of K.value (Simple.var b2) in
   *   let env =
   *     TE.add_equation env (Name.var b1) eq_block2
   *   in
   *   Format.eprintf "Res:@ %a@.@."
   *     TE.print env
   * in
   * test block1 block2 env;
   * test block2 block1 env; *)
  let f b1 b2 =
    match
      T.meet env
        (T.alias_type_of K.value (Simple.var b1))
        (T.alias_type_of K.value (Simple.var b2))
    with
    | Bottom -> assert false
    | Ok (t, tee) ->
      Format.eprintf "Res:@ %a@.%a@." T.print t TEE.print tee;
      let env = TE.add_env_extension env tee in
      Format.eprintf "Env:@.%a@.@." TE.print env
  in
  f block1 block2;
  f block2 block1

let () =
  let comp_unit = "Meet_test" |> Compilation_unit.of_string in
  Compilation_unit.set_current (Some comp_unit);
  Format.eprintf "MEET CHAINS WITH TWO VARS@\n@.";
  test_meet_chains_two_vars ();
  Format.eprintf "@.MEET CHAINS WITH THREE VARS@\n@.";
  test_meet_chains_three_vars ();
  Format.eprintf "@.MEET VARIANT@\n@.";
  meet_variants_don't_lose_aliases ();
  Format.eprintf "@.MEET TWO BLOCKS@\n@.";
  test_meet_two_blocks ()
