let make_inlined_body ~callee ~params ~args ~my_closure ~my_depth ~rec_info
    ~body ~exn_continuation ~return_continuation ~apply_exn_continuation
    ~apply_return_continuation ~bind_params ~bind_depth ~apply_renaming =
  let perm = Renaming.empty in
  let perm =
    match (apply_return_continuation : Flambda.Apply.Result_continuation.t) with
    | Return k -> Renaming.add_continuation perm return_continuation k
    | Never_returns -> perm
  in
  let perm =
    Renaming.add_continuation perm exn_continuation apply_exn_continuation
  in
  let body =
    bind_params ~params:(my_closure :: params) ~args:(callee :: args) ~body
  in
  let body = bind_depth ~my_depth ~rec_info ~body in
  apply_renaming body perm

let wrap_inlined_body_for_exn_support acc ~extra_args ~apply_exn_continuation
    ~apply_return_continuation ~result_arity ~make_inlined_body
    ~apply_cont_create ~let_cont_create =
  (* We need to add a wrapper for the exception handler, so that exceptions
     coming from the inlined body go through the wrapper and are re-raised with
     the correct extra arguments.

     This means we also need to add a push trap before the inlined body, and a
     pop trap after.

     The push trap is simply a matter of jumping to the body, while the pop trap
     needs to replace the body's return continuation with a wrapper that pops
     then jumps back. *)
  (*
   * As a result, the application [Apply_expr f (args) <k> «k_exn»]
   * is replaced (before the actual inlining) by:
   *
   * [let_cont_exn k1 (exn: val) =
   *   Apply_cont k_exn exn extra_args
   * in
   * let_cont k_pop (args) = Apply_cont<pop k1> k args in
   * let_cont k_push () = Apply_expr f (args) <k_pop> «k1» in
   * Apply_cont<push k1> k_push ()]
   *)
  (* This feels quite heavy, but is necessary because we can rewrite neither the
     definition and other uses of k_exn nor the uses of the exception
     continuation in the body of f, so we need two distinct exception
     continuations; and of course the new exception continuation needs to be
     correctly pushed and popped.

     The most annoying part of this is that it introduces trywith blocks that
     were not part of the initial program, will not be removed, and might be
     useless (if the function never raises).

     Maybe a better solution would be to propagate through dacc a lazy
     rewriting, that would add the correct extra args to all uses of the
     exception continuation in the body. *)
  let wrapper = Continuation.create () in
  let body_with_pop acc =
    match (apply_return_continuation : Flambda.Apply.Result_continuation.t) with
    | Never_returns ->
      make_inlined_body acc ~apply_exn_continuation:wrapper
        ~apply_return_continuation
    | Return apply_return_continuation ->
      let pop_wrapper_cont = Continuation.create () in
      let new_apply_return_continuation =
        Flambda.Apply.Result_continuation.Return pop_wrapper_cont
      in
      let body acc =
        make_inlined_body acc ~apply_exn_continuation:wrapper
          ~apply_return_continuation:new_apply_return_continuation
      in
      let kinded_params =
        List.map (fun k -> Variable.create "wrapper_return", k) result_arity
      in
      let trap_action =
        Trap_action.Pop { exn_handler = wrapper; raise_kind = None }
      in
      let args = List.map (fun (v, _) -> Simple.var v) kinded_params in
      let handler acc =
        apply_cont_create acc ~trap_action apply_return_continuation ~args
          ~dbg:Debuginfo.none
      in
      let_cont_create pop_wrapper_cont
        ~handler_params:(Bound_parameter.List.create kinded_params)
        ~handler ~body:(acc, body) ~is_exn_handler:false
  in
  let param = Variable.create "exn" in
  let wrapper_handler_params =
    [Bound_parameter.create param Flambda_kind.With_subkind.any_value]
  in
  let exn_handler = Exn_continuation.exn_handler apply_exn_continuation in
  let trap_action = Trap_action.Pop { exn_handler; raise_kind = None } in
  let wrapper_handler acc =
    (* Backtrace building functions expect compiler-generated raises not to have
       any debug info *)
    apply_cont_create acc ~trap_action
      (Exn_continuation.exn_handler apply_exn_continuation)
      ~args:(Simple.var param :: List.map fst extra_args)
      ~dbg:Debuginfo.none
  in
  let body_with_push acc =
    (* Wrap the body between push and pop of the wrapper handler *)
    let push_wrapper_cont = Continuation.create () in
    let push_wrapper_handler = body_with_pop in
    let trap_action = Trap_action.Push { exn_handler = wrapper } in
    let body acc =
      apply_cont_create acc ~trap_action push_wrapper_cont ~args:[]
        ~dbg:Debuginfo.none
    in
    let_cont_create push_wrapper_cont ~handler_params:[]
      ~handler:push_wrapper_handler ~body:(acc, body) ~is_exn_handler:false
  in
  let_cont_create wrapper ~handler_params:wrapper_handler_params
    ~handler:wrapper_handler ~body:(acc, body_with_push) ~is_exn_handler:true
