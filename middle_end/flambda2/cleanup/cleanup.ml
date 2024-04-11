let unit_with_body (unit : Flambda_unit.t) (body : Flambda.Expr.t) =
  Flambda_unit.create
    ~return_continuation:(Flambda_unit.return_continuation unit)
    ~exn_continuation:(Flambda_unit.exn_continuation unit)
    ~toplevel_my_region:(Flambda_unit.toplevel_my_region unit)
    ~body
    ~module_symbol:(Flambda_unit.module_symbol unit)
    ~used_value_slots:(Flambda_unit.used_value_slots unit)

let debug_print = Global_flow_graph.debug_print

let run ~cmx_loader (unit : Flambda_unit.t) =
  let holed, deps, kinds = Traverse.run unit in
  if debug_print then Format.printf "USED %a@." Global_flow_graph.pp_used deps;
  let solved_dep = Dep_solver.fixpoint deps in
  if debug_print
  then Format.printf "RESULT@ %a@." Dep_solver.pp_result solved_dep;
  let rebuilt_expr, free_names, all_code, slot_offsets =
    Rebuild.rebuild kinds solved_dep holed
  in
  (* Is this what we really want? This keeps all the code that has not been
     deleted by this pass to be exported in the cmx. It looks like this does the
     same thing as [Simplify], but on the other hand, we might not want to
     export un-inlinable functions. *)
  let all_code =
    Exported_code.add_code
      ~keep_code:(fun _ -> true)
      all_code
      (Exported_code.mark_as_imported
         (Flambda_cmx.get_imported_code cmx_loader ()))
  in
  unit_with_body unit rebuilt_expr, free_names, all_code, slot_offsets
