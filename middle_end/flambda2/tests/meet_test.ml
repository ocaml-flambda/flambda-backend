open Flambda2_bound_identifiers
open Flambda2_identifiers
open Flambda2_kinds
open Flambda2_nominal
open Flambda2_numbers
open Flambda2_term_basics
module K = Flambda_kind
module T = Flambda2_types
module TE = T.Typing_env

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
      (T.immutable_block ~is_unique:false Tag.zero
         ~shape:(K.Block_shape.Scannable Value_only) Alloc_mode.For_types.heap
         ~fields:[T.any_tagged_immediate])
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
  | Ok (meet_ty, env) ->
    Format.eprintf "Extended env:@ %a\n%!" TE.print env;
    let env = TE.add_equation env (Name.var var2) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let test_meet_chains_three_vars () =
  let env = create_env () in
  let var1 = Variable.create "var1" in
  let var1' = Bound_var.create var1 Name_mode.normal in
  let env = TE.add_definition env (Bound_name.create_var var1') K.value in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block ~is_unique:false Tag.zero
         ~shape:(K.Block_shape.Scannable Value_only) Alloc_mode.For_types.heap
         ~fields:[T.any_tagged_immediate])
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
  | Ok (meet_ty, env) ->
    Format.eprintf "Extended env:@ %a\n%!" TE.print env;
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
        [ ( Tag.Scannable.create_exn 0,
            ( K.Block_shape.Scannable Value_only,
              [T.alias_type_of K.value (Simple.var vx)] ) );
          ( Tag.Scannable.create_exn 1,
            ( K.Block_shape.Scannable Value_only,
              [T.alias_type_of K.value (Simple.var vy)] ) ) ]
    in
    T.variant ~const_ctors ~non_const_ctors Alloc_mode.For_types.heap
  in
  let ty2 =
    let non_const_ctors =
      Tag.Scannable.Map.of_list
        [ ( Tag.Scannable.create_exn 0,
            ( K.Block_shape.Scannable Value_only,
              [T.alias_type_of K.value (Simple.var va)] ) );
          ( Tag.Scannable.create_exn 1,
            ( K.Block_shape.Scannable Value_only,
              [T.alias_type_of K.value (Simple.var vb)] ) ) ]
    in
    T.variant ~const_ctors ~non_const_ctors Alloc_mode.For_types.heap
  in
  match T.meet env ty1 ty2 with
  | Bottom -> assert false
  | Ok (meet_ty, env) -> (
    Format.eprintf "@[<hov 2>Meet:@ %a@ /\\@ %a =>@ %a +@ %a@]@." T.print ty1
      T.print ty2 T.print meet_ty TE.print env;
    (* Env extension should be empty *)
    let env = TE.add_equation env (Name.var v_variant) meet_ty in
    let t_get_tag = T.get_tag_for_block ~block:(Simple.var v_variant) in
    let t_tag_1 = T.this_naked_immediate Targetint_31_63.one in
    match T.meet env t_get_tag t_tag_1 with
    | Bottom -> assert false
    | Ok (tag_meet_ty, tag_meet_env) ->
      Format.eprintf "t_get_tag: %a@.t_tag: %a@." T.print t_get_tag T.print
        t_tag_1;
      Format.eprintf "@[<hov 2>meet:@ %a@]@.@[<hov 2>env:@ %a@]@." T.print
        tag_meet_ty TE.print tag_meet_env)

let test_join_with_extensions () =
  let define ?(kind = K.value) env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') kind
  in
  let env = create_env () in
  let y = Variable.create "y" in
  let x = Variable.create "x" in
  let a = Variable.create "a" in
  let b = Variable.create "b" in
  let env = define env y in
  let env = define env x in
  let env = define ~kind:K.naked_immediate env a in
  let env = define ~kind:K.naked_immediate env b in
  let tag_0 = Tag.Scannable.zero in
  let tag_1 = Option.get (Tag.Scannable.of_tag (Tag.create_exn 1)) in
  let make ty =
    T.variant
      ~const_ctors:(T.bottom K.naked_immediate)
      ~non_const_ctors:
        (Tag.Scannable.Map.of_list
           [ tag_0, (K.Block_shape.Scannable Value_only, [ty]);
             tag_1, (K.Block_shape.Scannable Value_only, []) ])
      Alloc_mode.For_types.heap
  in
  let env = TE.add_equation env (Name.var y) (make (T.unknown K.value)) in
  let scope = TE.current_scope env in
  let scoped_env = TE.increment_scope env in
  let left_env =
    TE.add_equation scoped_env (Name.var x)
      (T.tagged_immediate_alias_to ~naked_immediate:a)
  in
  let right_env =
    TE.add_equation scoped_env (Name.var x)
      (T.tagged_immediate_alias_to ~naked_immediate:b)
  in
  let ty_a = make (T.tagged_immediate_alias_to ~naked_immediate:a) in
  let ty_b = make (T.tagged_immediate_alias_to ~naked_immediate:b) in
  let left_env = TE.add_equation left_env (Name.var y) ty_a in
  let right_env =
    match T.meet right_env ty_a ty_b with
    | Ok (ty, right_env) -> TE.add_equation right_env (Name.var y) ty
    | Bottom -> assert false
  in
  Format.eprintf "Left:@.%a@." TE.print left_env;
  Format.eprintf "Right:@.%a@." TE.print right_env;
  let joined_env =
    T.cut_and_n_way_join scoped_env
      [ left_env, Apply_cont_rewrite_id.create (), Inlinable;
        right_env, Apply_cont_rewrite_id.create (), Inlinable ]
      ~params:Bound_parameters.empty ~cut_after:scope
      ~extra_allowed_names:Name_occurrences.empty
      ~extra_lifted_consts_in_use_envs:Symbol.Set.empty
  in
  Format.eprintf "Res:@.%a@." TE.print joined_env

let test_join_with_complex_extensions () =
  let define ?(kind = K.value) env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') kind
  in
  let env = create_env () in
  let y = Variable.create "y" in
  let x = Variable.create "x" in
  let w = Variable.create "w" in
  let z = Variable.create "z" in
  let a = Variable.create "a" in
  let b = Variable.create "b" in
  let c = Variable.create "c" in
  let d = Variable.create "d" in
  let env = define env z in
  let env = define env x in
  let env = define env y in
  let env = define env w in
  let env = define ~kind:K.naked_immediate env a in
  let env = define ~kind:K.naked_immediate env b in
  let env = define ~kind:K.naked_immediate env c in
  let env = define ~kind:K.naked_immediate env d in
  let tag_0 = Tag.Scannable.zero in
  let tag_1 = Option.get (Tag.Scannable.of_tag (Tag.create_exn 1)) in
  let make tys =
    T.variant
      ~const_ctors:(T.bottom K.naked_immediate)
      ~non_const_ctors:
        (Tag.Scannable.Map.of_list
           [ tag_0, (K.Block_shape.Scannable Value_only, tys);
             tag_1, (K.Block_shape.Scannable Value_only, []) ])
      Alloc_mode.For_types.heap
  in
  let env =
    TE.add_equation env (Name.var z)
      (make [T.unknown K.value; T.unknown K.value])
  in
  let scope = TE.current_scope env in
  let scoped_env = TE.increment_scope env in
  let left_env =
    TE.add_equation scoped_env (Name.var x)
      (T.tagged_immediate_alias_to ~naked_immediate:a)
  in
  let left_env =
    TE.add_equation left_env (Name.var y)
      (T.tagged_immediate_alias_to ~naked_immediate:a)
  in
  let left_env =
    TE.add_equation left_env (Name.var w)
      (T.tagged_immediate_alias_to ~naked_immediate:a)
  in
  let right_env =
    TE.add_equation scoped_env (Name.var x)
      (T.tagged_immediate_alias_to ~naked_immediate:b)
  in
  let right_env =
    TE.add_equation right_env (Name.var y)
      (T.tagged_immediate_alias_to ~naked_immediate:c)
  in
  let right_env =
    TE.add_equation right_env (Name.var w)
      (T.tagged_immediate_alias_to ~naked_immediate:d)
  in
  let ty_a =
    make
      [ T.tagged_immediate_alias_to ~naked_immediate:b;
        T.tagged_immediate_alias_to ~naked_immediate:b ]
  in
  let ty_b =
    make
      [ T.tagged_immediate_alias_to ~naked_immediate:c;
        T.tagged_immediate_alias_to ~naked_immediate:d ]
  in
  let left_env = TE.add_equation left_env (Name.var z) ty_a in
  let right_env =
    match T.meet right_env ty_a ty_b with
    | Ok (ty, right_env) -> TE.add_equation right_env (Name.var z) ty
    | Bottom -> assert false
  in
  Format.eprintf "Left:@.%a@." TE.print left_env;
  Format.eprintf "Right:@.%a@." TE.print right_env;
  let joined_env =
    T.cut_and_n_way_join scoped_env
      [ left_env, Apply_cont_rewrite_id.create (), Inlinable;
        right_env, Apply_cont_rewrite_id.create (), Inlinable ]
      ~params:Bound_parameters.empty ~cut_after:scope
      ~extra_allowed_names:Name_occurrences.empty
      ~extra_lifted_consts_in_use_envs:Symbol.Set.empty
  in
  Format.eprintf "Res:@.%a@." TE.print joined_env

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
      (T.immutable_block ~is_unique:false Tag.zero
         ~shape:(K.Block_shape.Scannable Value_only) Alloc_mode.For_types.heap
         ~fields:[T.alias_type_of K.value (Simple.var field1)])
  in
  let env =
    TE.add_equation env (Name.var block2)
      (T.immutable_block ~is_unique:false Tag.zero
         ~shape:(K.Block_shape.Scannable Value_only) Alloc_mode.For_types.heap
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
    | Ok (t, env) ->
      Format.eprintf "Res:@ %a@.Env:@.%a@.@." T.print t TE.print env
  in
  f block1 block2;
  f block2 block1

let test_meet_recover_alias () =
  (* This test checks that we properly discover alias types when adding
     equations, even after a meet.

     If we have:

     x: (Variant (blocks {tag_0}) (tagged_imms ((= #0))))

     and we add:

     x: (Variant (blocks ⊥) (tagged_imms ⊤))

     we should get:

     x: (= 0) *)
  let define env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') K.value
  in
  let env = create_env () in
  let x = Variable.create "x" in
  let env = define env x in
  let existing_ty =
    T.variant Alloc_mode.For_types.heap
      ~const_ctors:(T.this_naked_immediate Targetint_31_63.zero)
      ~non_const_ctors:
        (Tag.Scannable.Map.of_list
           [Tag.Scannable.zero, (K.Block_shape.Scannable Value_only, [])])
  in
  Format.eprintf "@[<hov 2>first type:@ %a@]@." T.print existing_ty;
  let env = TE.add_equation env (Name.var x) existing_ty in
  Format.eprintf "@[<hov 2>second type:@ %a@]@." T.print T.any_tagged_immediate;
  let env = TE.add_equation env (Name.var x) T.any_tagged_immediate in
  let meet_ty = TE.find env (Name.var x) (Some K.value) in
  (* CR bclement: we would like an assertion that [meet_ty] is [(= 0)] here, but
     the required functions for this are not exposed. *)
  Format.eprintf "@[<hov 2>after meet:@ %a@]@." T.print meet_ty

let test_meet_bottom_after_alias () =
  (* This test checks that we discover bottom if we meet an alias to a constant
     with an incompatible type.

     If we have:

     x: { -1, 0, 1 }

     and we add:

     x: (= 3)

     we should get:

     ⊥ *)
  let define env v =
    let v' = Bound_var.create v Name_mode.normal in
    TE.add_definition env (Bound_name.create_var v') K.value
  in
  let env = create_env () in
  let x = Variable.create "x" in
  let env = define env x in
  let existing_ty =
    T.these_tagged_immediates Targetint_31_63.zero_one_and_minus_one
  in
  Format.eprintf "@[<hov 2>first type:@ %a@]@." T.print existing_ty;
  let env = TE.add_equation env (Name.var x) existing_ty in
  let new_ty = T.alias_type_of K.value (Simple.const_int_of_kind K.value 3) in
  Format.eprintf "@[<hov 2>second type:@ %a@]@." T.print new_ty;
  let env = TE.add_equation env (Name.var x) new_ty in
  let meet_ty = TE.find env (Name.var x) (Some K.value) in
  Format.eprintf "@[<hov 2>after meet:@ %a@]@." T.print meet_ty;
  assert (T.is_bottom env meet_ty)

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
  test_meet_two_blocks ();
  Format.eprintf "@.MEET ALIAS TO RECOVER @\n@.";
  test_meet_recover_alias ();
  Format.eprintf "@.MEET BOTTOM AFTER ALIAS@\n@.";
  test_meet_bottom_after_alias ();
  Format.eprintf "@.JOIN WITH EXTENSIONS@\n@.";
  test_join_with_extensions ();
  Format.eprintf "@.JOIN WITH COMPLEX EXTENSIONS@\n@.";
  test_join_with_complex_extensions ()
