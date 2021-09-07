(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Non_recursive of {
      handler : Non_recursive_let_cont_handler.t;
      num_free_occurrences : Num_occurrences.t Or_unknown.t;
      is_applied_with_traps : bool;
    }
  | Recursive of Recursive_let_cont_handlers.t

(* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
let invariant _env _t = ()

let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
  let rec gather_let_conts let_conts let_cont =
    match let_cont with
    | Non_recursive { handler; num_free_occurrences = _;
                      is_applied_with_traps = _; } ->
      Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun k ~(body : Expr.t) ->
          let let_conts, body =
            match Expr.descr body with
            | Let_cont let_cont -> gather_let_conts let_conts let_cont
            | _ -> let_conts, body
          in
          let handler = Non_recursive_let_cont_handler.handler handler in
          (k, Recursive.Non_recursive, handler) :: let_conts, body)
    | Recursive handlers ->
      Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~(body : Expr.t) handlers ->
          let handlers = Continuation_handlers.to_map handlers in
          let let_conts, body =
            match Expr.descr body with
            | Let_cont let_cont -> gather_let_conts let_conts let_cont
            | _ -> let_conts, body
          in
          let new_let_conts =
            List.map (fun (k, handler) -> k, Recursive.Recursive, handler)
              (Continuation.Map.bindings handlers)
          in
          new_let_conts @ let_conts, body)
  in
  let let_conts, body = gather_let_conts [] t in
  fprintf ppf "@[<v 1>(%a@;" (Expr.print_with_cache ~cache) body;
  let first = ref true in
  List.iter (fun (cont, recursive, handler) ->
      Continuation_handler.print_using_where_with_cache recursive ~cache
        ppf cont handler ~first:!first;
      first := false)
    (List.rev let_conts);
  fprintf ppf ")@]"

let [@ocamlformat "disable"] print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body:num_free_occurrences
      ~is_applied_with_traps =
  let handler = Non_recursive_let_cont_handler.create cont handler ~body in
  Expr.create_let_cont
    (Non_recursive { handler; num_free_occurrences; is_applied_with_traps;})

let create_non_recursive cont handler ~body ~free_names_of_body =
  let num_free_occurrences_of_cont_in_body, is_applied_with_traps =
    (* Only the continuations of [free_names_of_body] are used.
       [Closure_conversion_aux] relies on this property. *)
    match (free_names_of_body : _ Or_unknown.t) with
    | Unknown -> Or_unknown.Unknown, true
    | Known free_names_of_body ->
      Or_unknown.Known
        (Name_occurrences.count_continuation free_names_of_body cont),
      Name_occurrences.continuation_is_applied_with_traps
        free_names_of_body cont
  in
  create_non_recursive' ~cont handler ~body
    ~num_free_occurrences_of_cont_in_body ~is_applied_with_traps

let create_recursive handlers ~body =
  if Continuation_handlers.contains_exn_handler handlers then begin
    Misc.fatal_error "Exception-handling continuations cannot be recursive"
  end;
  Expr.create_let_cont
    (Recursive (Recursive_let_cont_handlers.create handlers ~body))

let free_names t =
  match t with
  | Non_recursive { handler; num_free_occurrences = _;
                    is_applied_with_traps = _; } ->
    Non_recursive_let_cont_handler.free_names handler
  | Recursive handlers ->
    Recursive_let_cont_handlers.free_names handlers

let apply_renaming t perm =
  match t with
  | Non_recursive { handler; num_free_occurrences;
                    is_applied_with_traps; } ->
    let handler' =
      Non_recursive_let_cont_handler.apply_renaming handler perm
    in
    if handler == handler' then t
    else Non_recursive { handler = handler'; num_free_occurrences;
                         is_applied_with_traps; }
  | Recursive handlers ->
    let handlers' =
      Recursive_let_cont_handlers.apply_renaming handlers perm
    in
    if handlers == handlers' then t
    else Recursive handlers'

let all_ids_for_export t =
  match t with
  | Non_recursive { handler; num_free_occurrences = _;
                    is_applied_with_traps = _; } ->
    Non_recursive_let_cont_handler.all_ids_for_export handler
  | Recursive handlers ->
    Recursive_let_cont_handlers.all_ids_for_export handlers
