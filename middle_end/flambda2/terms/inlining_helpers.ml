let make_inlined_body ~callee ~params ~args ~my_closure ~my_depth
    ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation ~bind_params ~bind_depth ~apply_renaming =
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
  let body =
    bind_depth ~my_depth ~rec_info ~body
  in
  apply_renaming body perm
