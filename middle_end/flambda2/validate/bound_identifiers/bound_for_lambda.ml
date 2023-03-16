type t =
  { return_continuation: Bound_continuation.t;
    exn_continuation: Bound_continuation.t;
    params: Bound_parameters.t }

let create ~return_continuation ~exn_continuation ~params =
  Bound_parameters.check_no_duplicates params;
  { return_continuation;
    exn_continuation;
    params;
  }

let free_names
      { return_continuation;
        exn_continuation;
        params} =
  (* See [bound_continuations.ml] for why [add_traps] is [true]. *)
  let free_names =
    Name_occurrences.add_continuation Name_occurrences.empty return_continuation
      ~has_traps:true
  in
  let free_names =
    Name_occurrences.add_continuation free_names exn_continuation
      ~has_traps:true
  in
  let free_names =
    Name_occurrences.union free_names (Bound_parameters.free_names params)
  in
  free_names

let apply_renaming
      { return_continuation;
        exn_continuation;
        params } renaming =
  let return_continuation =
    Renaming.apply_continuation renaming return_continuation
  in
  let exn_continuation =
    Renaming.apply_continuation renaming exn_continuation
  in
  let params = Bound_parameters.apply_renaming params renaming in
  { return_continuation;
    exn_continuation;
    params;
  }

let ids_for_export
      { return_continuation;
        exn_continuation;
        params } =
  let ids =
    Ids_for_export.add_continuation Ids_for_export.empty return_continuation
  in
  let ids = Ids_for_export.add_continuation ids exn_continuation in
  Ids_for_export.union ids (Bound_parameters.ids_for_export params)

let[@ocamlformat "disable"] print ppf
     { return_continuation; exn_continuation; params; } =
  Format.fprintf ppf "@[<v 0> (params %a), (ret %a), (exn %a)@]"
    Bound_parameters.print params
    Continuation.print return_continuation
    Continuation.print exn_continuation

let rename
      { return_continuation;
        exn_continuation;
        params;
      } =
  { return_continuation = Continuation.rename return_continuation;
    exn_continuation = Continuation.rename exn_continuation;
    params = Bound_parameters.rename params;
  }

let renaming
    { return_continuation = return_continuation1;
      exn_continuation = exn_continuation1;
      params = params1;
    }
    ~guaranteed_fresh:
    { return_continuation = return_continuation2;
      exn_continuation = exn_continuation2;
      params = params2;
    } =
  let renaming =
    Renaming.add_fresh_continuation Renaming.empty return_continuation1
      ~guaranteed_fresh:return_continuation2
  in
  let renaming =
    Renaming.add_fresh_continuation renaming exn_continuation1
      ~guaranteed_fresh:exn_continuation2
  in
  Renaming.compose
    ~second:(Bound_parameters.renaming params1 ~guaranteed_fresh:params2)
    ~first:renaming
