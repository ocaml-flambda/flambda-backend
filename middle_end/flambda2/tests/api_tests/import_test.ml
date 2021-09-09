let test () =
  (* This test checks that importing Simples work correctly. *)
  (* All the simples we're importing will have var_ext as their base variable.
     However, importing will map var to var_bad so if for some reason importing
     is done twice then one of the output simples will be printed as "bad"
     instead of "ok". *)
  let var_ext = Variable.create "ext" in
  let var_ok = Variable.create "ok" in
  let var_bad = Variable.create "bad" in
  let variables =
    Variable.Map.add var_ok var_bad (Variable.Map.singleton var_ext var_ok)
  in
  let renaming =
    Renaming.create_import_map ~symbols:Symbol.Map.empty ~variables
      ~simples:Simple.Map.empty ~consts:Reg_width_things.Const.Map.empty
      ~code_ids:Code_id.Map.empty ~continuations:Continuation.Map.empty
      ~used_closure_vars:Var_within_closure.Set.empty
  in
  (* Now create the simples. simple0 is the raw variable, simple1 has some rec
     info with depth 1, and simple2 has rec info with depth 2. *)
  let simple0 = Simple.var var_ext in
  let rec_info1 = Rec_info.create ~depth:1 ~unroll_to:None in
  let simple1 = Simple.with_rec_info simple0 rec_info1 in
  let rec_info2 = Rec_info.create ~depth:2 ~unroll_to:None in
  let simple1_ok = Simple.with_rec_info simple0 rec_info2 in
  (* Map simple1 to simple1_ok. Normally import preserves the rec info, but here
     we're trying to detect application of the renaming so we use a rec_info
     that's noticeably distinct. *)
  let simples = Simple.Map.singleton simple1 simple1_ok in
  let renaming2 =
    Renaming.create_import_map ~symbols:Symbol.Map.empty ~variables ~simples
      ~consts:Reg_width_things.Const.Map.empty ~code_ids:Code_id.Map.empty
      ~continuations:Continuation.Map.empty
      ~used_closure_vars:Var_within_closure.Set.empty
  in
  (* Now the bad case, if importing simples was also importing the underlying
     variable (as it used to do) *)
  let simple1_bad = Simple.with_rec_info (Simple.var var_ok) rec_info2 in
  let simples_bad = Simple.Map.singleton simple1 simple1_bad in
  let renaming2_bad =
    Renaming.create_import_map ~symbols:Symbol.Map.empty ~variables
      ~simples:simples_bad ~consts:Reg_width_things.Const.Map.empty
      ~code_ids:Code_id.Map.empty ~continuations:Continuation.Map.empty
      ~used_closure_vars:Var_within_closure.Set.empty
  in
  let check renaming msg =
    let simple0' = Renaming.apply_simple renaming simple0 in
    let simple1' = Renaming.apply_simple renaming simple1 in
    Format.printf "%s:@.%a -> %a@.%a -> %a@." msg Simple.print simple0
      Simple.print simple0' Simple.print simple1 Simple.print simple1'
  in
  check renaming "Simple renaming, vars only";
  check renaming2 "Simple renaming, with simples";
  check renaming2_bad "Simple renaming, with recursive import";
  (* As reference, here is what output is expected at the time of writing this
     test:

     Simple renaming, vars only: ext/0 -> ok/1 (simple ext/0) (rec_info ((depth
     1) (unroll_to None))) -> (simple ok/1) (rec_info ((depth 1) (unroll_to
     None))) Simple renaming, with simples: ext/0 -> ok/1 (simple ext/0)
     (rec_info ((depth 1) (unroll_to None))) -> (simple ok/1) (rec_info ((depth
     2) (unroll_to None))) Simple renaming, with recursive import: ext/0 -> ok/1
     (simple ext/0) (rec_info ((depth 1) (unroll_to None))) -> (simple bad/2)
     (rec_info ((depth 2) (unroll_to None))) *)
  ()

let _ =
  let comp_unit =
    let id = Ident.create_persistent "Test" in
    let linkage_name = Linkage_name.create "camlTest" in
    Compilation_unit.create id linkage_name
  in
  Compilation_unit.set_current comp_unit;
  test ()
