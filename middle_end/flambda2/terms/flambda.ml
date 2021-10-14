(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module BP = Bound_parameter
module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

let fprintf = Format.fprintf

module rec Continuation_handler : sig
  (** The representation of the alpha-equivalence class of bindings of a list of
      parameters, with associated relations thereon, over the code of a
      continuation handler. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  val print_using_where :
    Recursive.t ->
    Format.formatter ->
    Continuation.t ->
    t ->
    Num_occurrences.t Or_unknown.t ->
    first:bool ->
    unit

  (** Create a value of type [t] given information about a continuation
      handler. *)
  val create :
    Bound_parameter.t list ->
    handler:Expr.t ->
    free_names_of_handler:Name_occurrences.t Or_unknown.t ->
    is_exn_handler:bool ->
    t

  (** Choose a member of the alpha-equivalence class to enable examination of
      the parameters, relations thereon and the code over which they are
      scoped. *)
  val pattern_match' :
    t ->
    f:
      (Bound_parameter.t list ->
      num_normal_occurrences_of_params:Num_occurrences.t Variable.Map.t ->
      handler:Expr.t ->
      'a) ->
    'a

  val pattern_match :
    t -> f:(Bound_parameter.t list -> handler:Expr.t -> 'a) -> 'a

  module Pattern_match_pair_error : sig
    type t = Parameter_lists_have_different_lengths

    val to_string : t -> string
  end

  (** Choose members of two bindings' alpha-equivalence classes using the same
      parameters. *)
  val pattern_match_pair :
    t ->
    t ->
    f:(Bound_parameter.t list -> handler1:Expr.t -> handler2:Expr.t -> 'a) ->
    ('a, Pattern_match_pair_error.t) Result.t

  (** Whether the continuation is an exception handler.

      Continuations used as exception handlers are always [Non_recursive]. To
      enable identification of them in passes not invoked from [Simplify] (where
      they could be identified by looking at the [Apply_cont]s that reference
      them) they are marked explicitly.

      Continuations used as exception handlers may have more than one parameter
      (see [Exn_continuation]).

      (Relevant piece of background info: the backend cannot compile
      simultaneously-defined continuations when one or more of them is an
      exception handler.) *)
  val is_exn_handler : t -> bool
end = struct
  module T0 = struct
    type t =
      { num_normal_occurrences_of_params : Num_occurrences.t Variable.Map.t;
        handler : Expr.t
      }

    let [@ocamlformat "disable"] print ppf
          { handler; num_normal_occurrences_of_params = _; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(handler@ %a)@]\
          )@]"
        Expr.print handler

    let free_names { handler; num_normal_occurrences_of_params = _ } =
      Expr.free_names handler

    let apply_renaming ({ handler; num_normal_occurrences_of_params } as t) perm
        =
      let handler' = Expr.apply_renaming handler perm in
      if handler == handler'
      then t
      else { handler = handler'; num_normal_occurrences_of_params }

    let all_ids_for_export { handler; num_normal_occurrences_of_params = _ } =
      Expr.all_ids_for_export handler
  end

  module A = Name_abstraction.Make (Bound_parameters) (T0)

  type t =
    { abst : A.t;
      is_exn_handler : bool
    }

  let create params ~handler ~(free_names_of_handler : _ Or_unknown.t)
      ~is_exn_handler =
    let num_normal_occurrences_of_params =
      match free_names_of_handler with
      | Unknown -> Variable.Map.empty
      | Known free_names_of_handler ->
        ListLabels.fold_left params ~init:Variable.Map.empty
          ~f:(fun num_occurrences param ->
            let var = Bound_parameter.var param in
            let num =
              Name_occurrences.count_variable_normal_mode free_names_of_handler
                var
            in
            Variable.Map.add var num num_occurrences)
    in
    let t0 : T0.t = { num_normal_occurrences_of_params; handler } in
    let abst = A.create (Bound_parameters.create params) t0 in
    { abst; is_exn_handler }

  let pattern_match' t ~f =
    A.pattern_match t.abst
      ~f:(fun params { handler; num_normal_occurrences_of_params } ->
        f
          (Bound_parameters.to_list params)
          ~num_normal_occurrences_of_params ~handler)

  let pattern_match t ~f =
    A.pattern_match t.abst
      ~f:(fun params { handler; num_normal_occurrences_of_params = _ } ->
        f (Bound_parameters.to_list params) ~handler)

  module Pattern_match_pair_error = struct
    type t = Parameter_lists_have_different_lengths

    let to_string = function
      | Parameter_lists_have_different_lengths ->
        "Parameter lists have different lengths"
  end

  let pattern_match_pair t1 t2 ~f =
    pattern_match t1 ~f:(fun params1 ~handler:_ ->
        pattern_match t2 ~f:(fun params2 ~handler:_ ->
            (* CR lmaurer: Should this check be done by
               [Name_abstraction.Make_list]? *)
            if List.compare_lengths params1 params2 = 0
            then
              A.pattern_match_pair t1.abst t2.abst
                ~f:(fun
                     params
                     { handler = handler1; _ }
                     { handler = handler2; _ }
                   ->
                  Ok (f (Bound_parameters.to_list params) ~handler1 ~handler2))
            else
              Error
                Pattern_match_pair_error.Parameter_lists_have_different_lengths))

  let print_using_where (recursive : Recursive.t) ppf k
      ({ abst = _; is_exn_handler } as t) occurrences ~first =
    let fprintf = Format.fprintf in
    if not first then fprintf ppf "@ ";
    pattern_match t ~f:(fun params ~handler ->
        begin
          match Expr.descr handler with
          | Apply_cont _ | Invalid _ -> fprintf ppf "@[<hov 1>"
          | _ -> fprintf ppf "@[<v 1>"
        end;
        fprintf ppf "@<0>%s%a@<0>%s%s@<0>%s%s@<0>%s"
          (Flambda_colours.continuation_definition ())
          Continuation.print k
          (Flambda_colours.expr_keyword ())
          (match recursive with Non_recursive -> "" | Recursive -> " (rec)")
          (Flambda_colours.continuation_annotation ())
          (if is_exn_handler then "[eh]" else "")
          (Flambda_colours.normal ());
        if List.length params > 0
        then fprintf ppf " %a" Bound_parameter.List.print params;
        fprintf ppf "@<0>%s #%a:@<0>%s@ %a" (Flambda_colours.elide ())
          (Or_unknown.print Num_occurrences.print)
          occurrences
          (Flambda_colours.normal ())
          Expr.print handler;
        fprintf ppf "@]")

  let [@ocamlformat "disable"] print ppf { abst; is_exn_handler; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(params_and_handler@ %a)@]@ \
        @[<hov 1>(is_exn_handler@ %b)@]\
        @]"
      A.print abst
      is_exn_handler

  let is_exn_handler t = t.is_exn_handler

  let free_names t = A.free_names t.abst

  let apply_renaming ({ abst; is_exn_handler } as t) perm =
    let abst' = A.apply_renaming abst perm in
    if abst == abst' then t else { abst = abst'; is_exn_handler }

  let all_ids_for_export { abst; is_exn_handler = _ } =
    A.all_ids_for_export abst
end

and Continuation_handlers : sig
  (** The result of pattern matching on [Recursive_let_cont_handlers] (see
      above). *)
  type t

  (** Obtain the mapping from continuation to handler. *)
  val to_map : t -> Continuation_handler.t Continuation.Map.t

  (** The domain of [to_map t]. *)
  val domain : t -> Continuation.Set.t

  (** Whether any of the continuations are exception handlers. *)
  val contains_exn_handler : t -> bool

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end = struct
  type t = Continuation_handler.t Continuation.Map.t

  let to_map t = t

  let free_names t =
    Continuation.Map.fold
      (fun _k handler free_names ->
        Name_occurrences.union free_names
          (Continuation_handler.free_names handler))
      t Name_occurrences.empty

  let apply_renaming t perm =
    Continuation.Map.fold
      (fun k handler result ->
        let k = Renaming.apply_continuation perm k in
        let handler = Continuation_handler.apply_renaming handler perm in
        Continuation.Map.add k handler result)
      t Continuation.Map.empty

  let all_ids_for_export t =
    Continuation.Map.fold
      (fun k handler ids ->
        Ids_for_export.union ids
          (Ids_for_export.add_continuation
             (Continuation_handler.all_ids_for_export handler)
             k))
      t Ids_for_export.empty

  let domain t = Continuation.Map.keys t

  let contains_exn_handler t =
    Continuation.Map.exists
      (fun _cont handler -> Continuation_handler.is_exn_handler handler)
      t
end

and Expr : sig
  (** The type of alpha-equivalence classes of expressions. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  type descr = private
    | Let of Let_expr.t
        (** Bind variable(s) or symbol(s). There can be no effect on control
            flow (save for asynchronous operations such as the invocation of
            finalisers or signal handlers as a result of reaching a safe
            point). *)
    | Let_cont of Let_cont_expr.t  (** Define one or more continuations. *)
    | Apply of Apply.t
        (** Call an OCaml function, external function or method. *)
    | Apply_cont of Apply_cont.t
        (** Call a continuation, optionally adding or removing exception trap
            frames from the stack, which thus allows for the raising of
            exceptions. *)
    | Switch of Switch.t  (** Conditional control flow. *)
    | Invalid of Invalid_term_semantics.t
        (** Code proved type-incorrect and therefore unreachable. *)

  (** Extract the description of an expression. *)
  val descr : t -> descr

  val create_let : Let_expr.t -> t

  val create_let_cont : Let_cont_expr.t -> t

  (** Create an application expression. *)
  val create_apply : Apply.t -> t

  (** Create a continuation application (in the zero-arity case, "goto"). *)
  val create_apply_cont : Apply_cont.t -> t

  val create_switch : Switch_expr.t -> t

  (** Create an expression indicating type-incorrect or unreachable code. *)
  val create_invalid : ?semantics:Invalid_term_semantics.t -> unit -> t

  val bind_parameters_to_args_no_simplification :
    params:Bound_parameter.t list -> args:Simple.t list -> body:Expr.t -> Expr.t
end = struct
  module Descr = struct
    type t =
      | Let of Let_expr.t
      | Let_cont of Let_cont_expr.t
      | Apply of Apply.t
      | Apply_cont of Apply_cont.t
      | Switch of Switch.t
      | Invalid of Invalid_term_semantics.t

    let free_names t =
      match t with
      | Let let_expr -> Let_expr.free_names let_expr
      | Let_cont let_cont -> Let_cont_expr.free_names let_cont
      | Apply apply -> Apply.free_names apply
      | Apply_cont apply_cont -> Apply_cont.free_names apply_cont
      | Switch switch -> Switch.free_names switch
      | Invalid _ -> Name_occurrences.empty

    let apply_renaming t perm =
      match t with
      | Let let_expr ->
        let let_expr' = Let_expr.apply_renaming let_expr perm in
        if let_expr == let_expr' then t else Let let_expr'
      | Let_cont let_cont ->
        let let_cont' = Let_cont_expr.apply_renaming let_cont perm in
        if let_cont == let_cont' then t else Let_cont let_cont'
      | Apply apply ->
        let apply' = Apply.apply_renaming apply perm in
        if apply == apply' then t else Apply apply'
      | Apply_cont apply_cont ->
        let apply_cont' = Apply_cont.apply_renaming apply_cont perm in
        if apply_cont == apply_cont' then t else Apply_cont apply_cont'
      | Switch switch ->
        let switch' = Switch.apply_renaming switch perm in
        if switch == switch' then t else Switch switch'
      | Invalid _ -> t
  end

  (* CR mshinwell: Work out how to use [With_delayed_permutation] here. There
     were some problems with double vision etc. last time. Although we don't
     want to cache free names here. *)

  type t =
    { mutable descr : Descr.t;
      mutable delayed_permutation : Renaming.t
    }

  type descr = Descr.t =
    | Let of Let_expr.t
    | Let_cont of Let_cont_expr.t
    | Apply of Apply.t
    | Apply_cont of Apply_cont.t
    | Switch of Switch.t
    | Invalid of Invalid_term_semantics.t

  let create descr = { descr; delayed_permutation = Renaming.empty }

  let descr t =
    if Renaming.is_empty t.delayed_permutation
    then t.descr
    else
      let descr = Descr.apply_renaming t.descr t.delayed_permutation in
      t.descr <- descr;
      t.delayed_permutation <- Renaming.empty;
      descr

  let apply_renaming t perm =
    let delayed_permutation =
      Renaming.compose ~second:perm ~first:t.delayed_permutation
    in
    { t with delayed_permutation }

  let free_names t = Descr.free_names (descr t)

  let all_ids_for_export t =
    match descr t with
    | Let let_expr -> Let_expr.all_ids_for_export let_expr
    | Let_cont let_cont -> Let_cont_expr.all_ids_for_export let_cont
    | Apply apply -> Apply.all_ids_for_export apply
    | Apply_cont apply_cont -> Apply_cont.all_ids_for_export apply_cont
    | Switch switch -> Switch.all_ids_for_export switch
    | Invalid _ -> Ids_for_export.empty

  (* CR mshinwell: We might want printing functions that show the delayed
     permutation, etc. *)

  let [@ocamlformat "disable"] print ppf (t : t) =
    match descr t with
    | Let let_expr -> Let_expr.print ppf let_expr
    | Let_cont let_cont -> Let_cont_expr.print ppf let_cont
    | Apply apply ->
      Format.fprintf ppf "@[<hov 1>(@<0>%sapply@<0>%s@ %a)@]"
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
        Apply.print apply
    | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
    | Switch switch -> Switch.print ppf switch
    | Invalid semantics ->
      fprintf ppf "@[@<0>%sInvalid %a@<0>%s@]"
        (Flambda_colours.expr_keyword ())
        Invalid_term_semantics.print semantics
        (Flambda_colours.normal ())

  let create_let let_expr = create (Let let_expr)

  let create_let_cont let_cont = create (Let_cont let_cont)

  let create_apply apply = create (Apply apply)

  let create_apply_cont apply_cont = create (Apply_cont apply_cont)

  let create_switch switch = create (Switch switch)

  let create_invalid ?semantics () =
    let semantics : Invalid_term_semantics.t =
      match semantics with
      | Some semantics -> semantics
      | None ->
        if Flambda_features.treat_invalid_code_as_unreachable ()
        then Treat_as_unreachable
        else Halt_and_catch_fire
    in
    create (Invalid semantics)

  let bind_parameters_to_args_no_simplification ~params ~args ~body =
    if List.compare_lengths params args <> 0
    then
      Misc.fatal_errorf "Mismatching parameters and arguments: %a and %a"
        BP.List.print params Simple.List.print args;
    ListLabels.fold_left2 (List.rev params) (List.rev args) ~init:body
      ~f:(fun expr param arg ->
        let var = Bound_var.create (BP.var param) Name_mode.normal in
        Let_expr.create
          (Bound_pattern.singleton var)
          (Named.create_simple arg) ~body:expr ~free_names_of_body:Unknown
        |> create_let)
end

and Function_params_and_body : sig
  (** A name abstraction that comprises a function's parameters (together with
      any relations between them), the code of the function, and the
      [my_closure] variable. It also includes the return and exception
      continuations.

      From the body of the function, accesses to variables within the closure
      need to go via a [Project_var] (from [my_closure]); accesses to any other
      simultaneously-defined functions need to go likewise via a
      [Select_closure]. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Create an abstraction that binds the given parameters, with associated
      relations thereon, over the given body. *)
  val create :
    return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    Bound_parameter.t list ->
    dbg:Debuginfo.t ->
    body:Expr.t ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    my_closure:Variable.t ->
    my_depth:Variable.t ->
    t

  (** Choose a member of the alpha-equivalence class to enable examination of
      the parameters and the body over which they are scoped. *)
  val pattern_match :
    t ->
    f:
      (return_continuation:Continuation.t
         (** The continuation parameter of the function, i.e. to where we must
             jump once the result of the function has been computed. If the
             continuation takes more than one argument then the backend will
             compile the function so that it returns multiple values. *) ->
      exn_continuation:Continuation.t
        (** To where we must jump if application of the function raises an
            exception. *) ->
      Bound_parameter.t list ->
      body:Expr.t ->
      my_closure:Variable.t ->
      is_my_closure_used:bool Or_unknown.t ->
      my_depth:Variable.t ->
      free_names_of_body:Name_occurrences.t Or_unknown.t ->
      'a) ->
    'a

  (** Choose members of the alpha-equivalence classes of two definitions using
      the same names for the return continuation, the exception continuation,
      the closure, and all parameters. *)
  val pattern_match_pair :
    t ->
    t ->
    f:
      (return_continuation:Continuation.t
         (** The continuation parameter of the function, i.e. to where we must
             jump once the result of the function has been computed. If the
             continuation takes more than one argument then the backend will
             compile the function so that it returns multiple values. *) ->
      exn_continuation:Continuation.t
        (** To where we must jump if application of the function raises an
            exception. *) ->
      Bound_parameter.t list ->
      body1:Expr.t ->
      body2:Expr.t ->
      my_closure:Variable.t ->
      my_depth:Variable.t ->
      'a) ->
    'a

  val params_arity : t -> Flambda_arity.t

  val debuginfo : t -> Debuginfo.t
end = struct
  module Base = struct
    type t =
      { expr : Expr.t;
        free_names : Name_occurrences.t Or_unknown.t
      }

    let print fmt { expr; free_names = _ } =
      Format.fprintf fmt "%a" Expr.print expr

    let free_names { expr; free_names } =
      match free_names with
      | Known free_names -> free_names
      | Unknown -> Expr.free_names expr

    let apply_renaming { expr; free_names } perm =
      let expr = Expr.apply_renaming expr perm in
      let free_names =
        Or_unknown.map free_names ~f:(fun free_names ->
            Name_occurrences.apply_renaming free_names perm)
      in
      { expr; free_names }

    let all_ids_for_export { expr; free_names = _ } =
      Expr.all_ids_for_export expr
  end

  (* CR mshinwell: use [Continuation] instead of [Exn_continuation] everywhere
     for function return continuations. *)

  module A = Name_abstraction.Make (Bound_for_function) (Base)

  type t =
    { abst : A.t;
      dbg : Debuginfo.t;
      params_arity : Flambda_arity.t;
      is_my_closure_used : bool Or_unknown.t
    }

  let create ~return_continuation ~exn_continuation params ~dbg ~body
      ~free_names_of_body ~my_closure ~my_depth =
    let is_my_closure_used =
      Or_unknown.map free_names_of_body ~f:(fun free_names_of_body ->
          Name_occurrences.mem_var free_names_of_body my_closure)
    in
    let base : Base.t = { expr = body; free_names = free_names_of_body } in
    let bound_for_function =
      Bound_for_function.create ~return_continuation ~exn_continuation ~params
        ~my_closure ~my_depth
    in
    let abst = A.create bound_for_function base in
    { abst;
      dbg;
      params_arity = Bound_parameter.List.arity params;
      is_my_closure_used
    }

  let pattern_match t ~f =
    A.pattern_match t.abst ~f:(fun bound_for_function { expr; free_names } ->
        f
          ~return_continuation:
            (Bound_for_function.return_continuation bound_for_function)
          ~exn_continuation:
            (Bound_for_function.exn_continuation bound_for_function)
          (Bound_for_function.params bound_for_function)
          ~body:expr
          ~my_closure:(Bound_for_function.my_closure bound_for_function)
          ~is_my_closure_used:t.is_my_closure_used
          ~my_depth:(Bound_for_function.my_depth bound_for_function)
          ~free_names_of_body:free_names)

  let pattern_match_pair t1 t2 ~f =
    A.pattern_match_pair t1.abst t2.abst
      ~f:(fun
           bound_for_function
           { expr = body1; free_names = _ }
           { expr = body2; free_names = _ }
         ->
        f
          ~return_continuation:
            (Bound_for_function.return_continuation bound_for_function)
          ~exn_continuation:
            (Bound_for_function.exn_continuation bound_for_function)
          (Bound_for_function.params bound_for_function)
          ~body1 ~body2
          ~my_closure:(Bound_for_function.my_closure bound_for_function)
          ~my_depth:(Bound_for_function.my_depth bound_for_function))

  let [@ocamlformat "disable"] print ppf t =
    pattern_match t
      ~f:(fun ~return_continuation ~exn_continuation params ~body ~my_closure
              ~is_my_closure_used:_ ~my_depth ~free_names_of_body:_ ->
        let my_closure =
          Bound_parameter.create my_closure
            (K.With_subkind.create K.value Anything)
        in
        fprintf ppf
          "@[<hov 1>(@<0>%s@<1>\u{03bb}@<0>%s@[<hov 1>\
           @<1>\u{3008}%a@<1>\u{3009}@<1>\u{300a}%a@<1>\u{300b}\
           %a %a @<0>%s%a @<0>%s.@<0>%s@]@ %a))@]"
          (Flambda_colours.lambda ())
          (Flambda_colours.normal ())
          Continuation.print return_continuation
          Continuation.print exn_continuation
          Bound_parameter.List.print params
          Bound_parameter.print my_closure
          (Flambda_colours.depth_variable ())
          Variable.print my_depth
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
          Expr.print body)

  let params_arity t = t.params_arity

  let apply_renaming ({ abst; dbg; params_arity; is_my_closure_used } as t) perm
      =
    let abst' = A.apply_renaming abst perm in
    if abst == abst'
    then t
    else { abst = abst'; dbg; params_arity; is_my_closure_used }

  let free_names { abst; params_arity = _; dbg = _; is_my_closure_used = _ } =
    A.free_names abst

  let debuginfo { dbg; _ } = dbg

  let all_ids_for_export
      { abst; params_arity = _; dbg = _; is_my_closure_used = _ } =
    A.all_ids_for_export abst
end

and Let_cont_expr : sig
  (** Values of type [t] represent alpha-equivalence classes of the definitions
      * of continuations: * let_cont [name] [args] = [handler] in [body] * or
      using an alternative notation: * [body] * where [name] [args] = [handler]
      * * - Continuations are second-class. * - Continuations do not capture
      variables. * - Continuations may be (mutually-)recursive. *)

  (* CR mshinwell: ensure the statement about [Flambda_to_cmm] still holds. *)

  (** It is an error to mark a continuation that might be recursive as
      non-recursive. The converse is safe.

      Note: any continuation used as an exception handler must be non-recursive
      by the point it reaches [Flambda_to_cmm]. (This means that it is
      permissible to introduce mutual recursion through stubs associated with
      such continuations, so long as [Simplify] is run afterwards to inline them
      out and turn the resulting single [Recursive] handler into a
      [Non_recursive] one. *)
  type t = private
    | Non_recursive of
        { handler : Non_recursive_let_cont_handler.t;
          num_free_occurrences : Num_occurrences.t Or_unknown.t;
              (** [num_free_occurrences] can be used, for example, to decide
                  whether to inline out a linearly-used continuation. *)
          is_applied_with_traps : bool
              (** [is_applied_with_traps] is used to prevent inlining of
                  continuations that are applied with a trap action *)
        }
    | Recursive of Recursive_let_cont_handlers.t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Create a definition of a non-recursive continuation. If the continuation
      does not occur free in the [body], then just the [body] is returned,
      without any enclosing [Let_cont]. *)
  val create_non_recursive :
    Continuation.t ->
    Continuation_handler.t ->
    body:Expr.t ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    Expr.t

  val create_non_recursive' :
    cont:Continuation.t ->
    Continuation_handler.t ->
    body:Expr.t ->
    num_free_occurrences_of_cont_in_body:Num_occurrences.t Or_unknown.t ->
    is_applied_with_traps:bool ->
    Expr.t

  (** Create a definition of a set of possibly-recursive continuations. *)
  val create_recursive :
    Continuation_handler.t Continuation.Map.t -> body:Expr.t -> Expr.t
end = struct
  type t =
    | Non_recursive of
        { handler : Non_recursive_let_cont_handler.t;
          num_free_occurrences : Num_occurrences.t Or_unknown.t;
          is_applied_with_traps : bool
        }
    | Recursive of Recursive_let_cont_handlers.t

  let print ppf t =
    let rec gather_let_conts let_conts let_cont =
      match let_cont with
      | Non_recursive
          { handler; num_free_occurrences; is_applied_with_traps = _ } ->
        Non_recursive_let_cont_handler.pattern_match handler
          ~f:(fun k ~(body : Expr.t) ->
            let let_conts, body =
              match Expr.descr body with
              | Let_cont let_cont -> gather_let_conts let_conts let_cont
              | _ -> let_conts, body
            in
            let handler = Non_recursive_let_cont_handler.handler handler in
            ( (k, Recursive.Non_recursive, handler, num_free_occurrences)
              :: let_conts,
              body ))
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
              List.map
                (fun (k, handler) ->
                  k, Recursive.Recursive, handler, Or_unknown.Unknown)
                (Continuation.Map.bindings handlers)
            in
            new_let_conts @ let_conts, body)
    in
    let let_conts, body = gather_let_conts [] t in
    fprintf ppf "@[<v 1>(%a@;" Expr.print body;
    let first = ref true in
    List.iter
      (fun (cont, recursive, handler, occurrences) ->
        Continuation_handler.print_using_where recursive ppf cont handler
          occurrences ~first:!first;
        first := false)
      (List.rev let_conts);
    fprintf ppf ")@]"

  let create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body:num_free_occurrences
      ~is_applied_with_traps =
    let handler = Non_recursive_let_cont_handler.create cont handler ~body in
    Expr.create_let_cont
      (Non_recursive { handler; num_free_occurrences; is_applied_with_traps })

  let create_non_recursive cont handler ~body ~free_names_of_body =
    let num_free_occurrences_of_cont_in_body, is_applied_with_traps =
      (* Only the continuations of [free_names_of_body] are used.
         [Closure_conversion_aux] relies on this property. *)
      match (free_names_of_body : _ Or_unknown.t) with
      | Unknown -> Or_unknown.Unknown, true
      | Known free_names_of_body ->
        ( Or_unknown.Known
            (Name_occurrences.count_continuation free_names_of_body cont),
          Name_occurrences.continuation_is_applied_with_traps free_names_of_body
            cont )
    in
    create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body ~is_applied_with_traps

  let create_recursive handlers ~body =
    if Continuation_handlers.contains_exn_handler handlers
    then Misc.fatal_error "Exception-handling continuations cannot be recursive";
    Expr.create_let_cont
      (Recursive (Recursive_let_cont_handlers.create handlers ~body))

  let free_names t =
    match t with
    | Non_recursive
        { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
      Non_recursive_let_cont_handler.free_names handler
    | Recursive handlers -> Recursive_let_cont_handlers.free_names handlers

  let apply_renaming t perm =
    match t with
    | Non_recursive { handler; num_free_occurrences; is_applied_with_traps } ->
      let handler' =
        Non_recursive_let_cont_handler.apply_renaming handler perm
      in
      if handler == handler'
      then t
      else
        Non_recursive
          { handler = handler'; num_free_occurrences; is_applied_with_traps }
    | Recursive handlers ->
      let handlers' =
        Recursive_let_cont_handlers.apply_renaming handlers perm
      in
      if handlers == handlers' then t else Recursive handlers'

  let all_ids_for_export t =
    match t with
    | Non_recursive
        { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
      Non_recursive_let_cont_handler.all_ids_for_export handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.all_ids_for_export handlers
end

and Let_expr : sig
  (** The alpha-equivalence classes of expressions that bind variables. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  val create :
    Bound_pattern.t ->
    Named.t ->
    body:Expr.t ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    t

  (** The defining expression of the [Let]. *)
  val defining_expr : t -> Named.t

  (** Look inside the [Let] by choosing a member of the alpha-equivalence
      class. *)
  val pattern_match : t -> f:(Bound_pattern.t -> body:Expr.t -> 'a) -> 'a

  val pattern_match' :
    t ->
    f:
      (Bound_pattern.t ->
      num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
      body:Expr.t ->
      'a) ->
    'a

  module Pattern_match_pair_error : sig
    type t = Mismatched_let_bindings

    val to_string : t -> string
  end

  (** Look inside two [Let]s by choosing members of their alpha-equivalence
      classes, using the same bound variables for both. If they are both dynamic
      lets (that is, they both bind variables), this invokes [dynamic] having
      freshened both bodies; if they are both static (that is, they both bind
      symbols), this invokes [static] with the bodies unchanged, since no
      renaming is necessary. *)
  val pattern_match_pair :
    t ->
    t ->
    dynamic:(Bound_pattern.t -> body1:Expr.t -> body2:Expr.t -> 'a) ->
    static:
      (bound_symbols1:Bound_pattern.symbols ->
      bound_symbols2:Bound_pattern.symbols ->
      body1:Expr.t ->
      body2:Expr.t ->
      'a) ->
    ('a, Pattern_match_pair_error.t) Result.t
end = struct
  module T0 = struct
    type t =
      { num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t;
        body : Expr.t
      }

    let [@ocamlformat "disable"] print ppf
          { body; num_normal_occurrences_of_bound_vars = _; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(body@ %a)@]\
          )@]"
        Expr.print body

    let free_names { body; num_normal_occurrences_of_bound_vars = _ } =
      Expr.free_names body

    let apply_renaming ({ body; num_normal_occurrences_of_bound_vars } as t)
        perm =
      let body' = Expr.apply_renaming body perm in
      let changed = ref (body != body') in
      let num_normal_occurrences_of_bound_vars =
        Variable.Map.fold
          (fun var num result ->
            let var' = Renaming.apply_variable perm var in
            changed := !changed || var != var';
            Variable.Map.add var' num result)
          num_normal_occurrences_of_bound_vars Variable.Map.empty
      in
      if not !changed
      then t
      else { body = body'; num_normal_occurrences_of_bound_vars }

    let all_ids_for_export { body; num_normal_occurrences_of_bound_vars = _ } =
      Expr.all_ids_for_export body
  end

  module A = Name_abstraction.Make (Bound_pattern) (T0)

  type t =
    { name_abstraction : A.t;
      defining_expr : Named.t
    }

  let pattern_match t ~f =
    A.pattern_match t.name_abstraction ~f:(fun bound_pattern t0 ->
        f bound_pattern ~body:t0.body)

  let pattern_match' t ~f =
    A.pattern_match t.name_abstraction ~f:(fun bound_pattern t0 ->
        let num_normal_occurrences_of_bound_vars =
          t0.num_normal_occurrences_of_bound_vars
        in
        f bound_pattern ~num_normal_occurrences_of_bound_vars ~body:t0.body)

  module Pattern_match_pair_error = struct
    type t = Mismatched_let_bindings

    let to_string = function
      | Mismatched_let_bindings -> "Mismatched let bindings"
  end

  let pattern_match_pair t1 t2 ~dynamic ~static =
    A.pattern_match t1.name_abstraction ~f:(fun bound_pattern1 t0_1 ->
        let body1 = t0_1.body in
        A.pattern_match t2.name_abstraction ~f:(fun bound_pattern2 t0_2 ->
            let body2 = t0_2.body in
            let dynamic_case () =
              let ans =
                A.pattern_match_pair t1.name_abstraction t2.name_abstraction
                  ~f:(fun bound_pattern t0_1 t0_2 ->
                    dynamic bound_pattern ~body1:t0_1.body ~body2:t0_2.body)
              in
              Ok ans
            in
            match bound_pattern1, bound_pattern2 with
            | Bound_pattern.Singleton _, Bound_pattern.Singleton _ ->
              dynamic_case ()
            | ( Set_of_closures { closure_vars = vars1; _ },
                Set_of_closures { closure_vars = vars2; _ } ) ->
              if List.compare_lengths vars1 vars2 = 0
              then dynamic_case ()
              else Error Pattern_match_pair_error.Mismatched_let_bindings
            | Symbols bound_symbols1, Symbols bound_symbols2 ->
              let patterns1 =
                bound_symbols1.bound_symbols |> Bound_symbols.to_list
              in
              let patterns2 =
                bound_symbols2.bound_symbols |> Bound_symbols.to_list
              in
              if List.compare_lengths patterns1 patterns2 = 0
              then
                let ans =
                  static ~bound_symbols1 ~bound_symbols2 ~body1 ~body2
                in
                Ok ans
              else Error Pattern_match_pair_error.Mismatched_let_bindings
            | _, _ -> Error Pattern_match_pair_error.Mismatched_let_bindings))

  (* For printing "let symbol": *)

  type flattened_for_printing_descr =
    | Code of Function_params_and_body.t Code0.t
    | Set_of_closures of Symbol.t Closure_id.Lmap.t * Set_of_closures.t
    | Block_like of Symbol.t * Static_const.t

  type flattened_for_printing =
    { second_or_later_binding_within_one_set : bool;
      second_or_later_rec_binding : bool;
      descr : flattened_for_printing_descr
    }

  let shape_colour descr =
    match descr with
    | Code _ -> Flambda_colours.code_id ()
    | Set_of_closures _ | Block_like _ -> Flambda_colours.symbol ()

  (* CR mshinwell: Remove [second_or_later_binding_within_one_set] if it doesn't
     become used soon. *)

  let flatten_for_printing0 bound_symbols defining_exprs =
    Static_const_group.match_against_bound_symbols defining_exprs bound_symbols
      ~init:([], false)
      ~code:(fun (flattened_acc, second_or_later_rec_binding) _code_id code ->
        let flattened =
          { second_or_later_binding_within_one_set = false;
            second_or_later_rec_binding;
            descr = Code code
          }
        in
        flattened_acc @ [flattened], true)
      ~set_of_closures:
        (fun (flattened_acc, second_or_later_rec_binding) ~closure_symbols
             set_of_closures ->
        let flattened =
          if Set_of_closures.is_empty set_of_closures
          then []
          else
            let second_or_later_binding_within_one_set = false in
            [ { second_or_later_binding_within_one_set;
                second_or_later_rec_binding;
                descr = Set_of_closures (closure_symbols, set_of_closures)
              } ]
        in
        flattened_acc @ flattened, true)
      ~block_like:
        (fun (flattened_acc, second_or_later_rec_binding) symbol defining_expr ->
        let flattened =
          { second_or_later_binding_within_one_set = false;
            second_or_later_rec_binding;
            descr = Block_like (symbol, defining_expr)
          }
        in
        flattened_acc @ [flattened], true)

  let flatten_for_printing t =
    pattern_match t ~f:(fun (bound_pattern : Bound_pattern.t) ~body ->
        match bound_pattern with
        | Symbols { bound_symbols } ->
          let flattened, _ =
            flatten_for_printing0 bound_symbols
              (Named.must_be_static_consts t.defining_expr)
          in
          Some (flattened, body)
        | Singleton _ | Set_of_closures _ -> None)

  let print_closure_binding ppf (closure_id, sym) =
    Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]" Symbol.print sym
      (Flambda_colours.elide ()) (Flambda_colours.elide ()) Closure_id.print
      closure_id

  let print_flattened_descr_lhs ppf descr =
    match descr with
    | Code code -> Code_id.print ppf (Code0.code_id code)
    | Set_of_closures (closure_symbols, _) ->
      Format.fprintf ppf "@[<hov 0>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () ->
             Format.fprintf ppf "@<0>%s,@ @<0>%s" (Flambda_colours.elide ())
               (Flambda_colours.normal ()))
           print_closure_binding)
        (Closure_id.Lmap.bindings closure_symbols)
    | Block_like (symbol, _) -> Symbol.print ppf symbol

  let print_flattened_descr_rhs ppf descr =
    match descr with
    | Code code ->
      Code0.print ~print_function_params_and_body:Function_params_and_body.print
        ppf code
    | Set_of_closures (_, set) -> Set_of_closures.print ppf set
    | Block_like (_, static_const) -> Static_const.print ppf static_const

  let print_flattened ppf
      { second_or_later_binding_within_one_set = _;
        second_or_later_rec_binding;
        descr
      } =
    fprintf ppf "@[<hov 0>";
    (* if second_or_later_rec_binding && not
       second_or_later_binding_within_one_set then begin fprintf ppf
       "@<0>%sand_set @<0>%s" (Flambda_colours.elide ()) (Flambda_colours.normal
       ()) end else *)
    (if second_or_later_rec_binding
    then
      fprintf ppf "@<0>%sand @<0>%s"
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
    else
      let shape = "\u{25b7}" (* unfilled triangle *) in
      fprintf ppf "@<0>%s@<1>%s @<0>%s" (shape_colour descr) shape
        (Flambda_colours.normal ()));
    fprintf ppf "%a@<0>%s =@<0>%s@ %a@]" print_flattened_descr_lhs descr
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      print_flattened_descr_rhs descr

  let flatten_let_symbol t : _ * Expr.t =
    let rec flatten (expr : Expr.t) : _ * Expr.t =
      match Expr.descr expr with
      | Let t -> begin
        match flatten_for_printing t with
        | Some (flattened, body) ->
          let flattened', body = flatten body in
          flattened @ flattened', body
        | None -> [], expr
      end
      | _ -> [], expr
    in
    match flatten_for_printing t with
    | Some (flattened, body) ->
      let flattened', body = flatten body in
      flattened @ flattened', body
    | None -> assert false
  (* see below *)

  (* CR mshinwell: Merge the "let symbol" and "normal let" cases to use the same
     flattened type? *)
  let print_let_symbol ppf t =
    let rec print_more flattened =
      match flattened with
      | [] -> ()
      | flat :: flattened ->
        fprintf ppf "@ ";
        print_flattened ppf flat;
        print_more flattened
    in
    let flattened, body = flatten_let_symbol t in
    match flattened with
    | [] -> assert false
    | flat :: flattened ->
      fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ @[<v 0>%a"
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
        print_flattened flat;
      print_more flattened;
      fprintf ppf "@]@ %a)@]" Expr.print body

  (* For printing all kinds of let-expressions: *)

  let [@ocamlformat "disable"] print ppf
        ({ name_abstraction = _; defining_expr; } as t) =
    let let_bound_var_colour bound_pattern defining_expr =
      let name_mode = Bound_pattern.name_mode bound_pattern in
      if Name_mode.is_phantom name_mode then Flambda_colours.elide ()
      else match (defining_expr : Named.t) with
        | Rec_info _ ->
          Flambda_colours.depth_variable ()
        | Simple _ | Prim _ | Set_of_closures _ | Static_consts _ ->
          Flambda_colours.variable ()
    in
    let rec let_body (expr : Expr.t) =
      match Expr.descr expr with
      | Let ({ name_abstraction = _; defining_expr; } as t) ->
        pattern_match t
          ~f:(fun (bound_pattern : Bound_pattern.t) ~body ->
            match bound_pattern with
            | Singleton _ | Set_of_closures _ ->
              fprintf ppf
                "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
                (let_bound_var_colour bound_pattern defining_expr)
                Bound_pattern.print bound_pattern
                (Flambda_colours.elide ())
                (Flambda_colours.normal ())
                Named.print defining_expr;
              let_body body
            | Symbols _ -> expr)
      | _ -> expr
    in
    pattern_match t ~f:(fun (bound_pattern : Bound_pattern.t) ~body ->
      match bound_pattern with
      | Symbols _ -> print_let_symbol ppf t
      | Singleton _ | Set_of_closures _ ->
        fprintf ppf "@[<v 1>(@<0>%slet@<0>%s@ (@[<v 0>\
            @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
          (Flambda_colours.expr_keyword ())
          (Flambda_colours.normal ())
          (let_bound_var_colour bound_pattern defining_expr)
          Bound_pattern.print bound_pattern
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
          Named.print defining_expr;
        let expr = let_body body in
        fprintf ppf "@])@ %a)@]" Expr.print expr)

  let create (bound_pattern : Bound_pattern.t) (defining_expr : Named.t) ~body
      ~(free_names_of_body : _ Or_unknown.t) =
    begin
      match defining_expr, bound_pattern with
      | Prim _, Singleton _
      | Simple _, Singleton _
      | Rec_info _, Singleton _
      | Set_of_closures _, Set_of_closures _ ->
        ()
      | Set_of_closures _, Singleton _ ->
        Misc.fatal_errorf
          "Cannot bind a [Set_of_closures] to a [Singleton]:@ %a =@ %a"
          Bound_pattern.print bound_pattern Named.print defining_expr
      | _, Set_of_closures _ ->
        Misc.fatal_errorf
          "Cannot bind a non-[Set_of_closures] to a [Set_of_closures]:@ %a =@ \
           %a"
          Bound_pattern.print bound_pattern Named.print defining_expr
      | Static_consts _, Symbols _ -> ()
      | Static_consts _, Singleton _ ->
        Misc.fatal_errorf
          "Cannot bind a [Static_const] to a [Singleton]:@ %a =@ %a"
          Bound_pattern.print bound_pattern Named.print defining_expr
      | (Simple _ | Prim _ | Set_of_closures _ | Rec_info _), Symbols _ ->
        Misc.fatal_errorf
          "Cannot bind a non-[Static_const] to [Symbols]:@ %a =@ %a"
          Bound_pattern.print bound_pattern Named.print defining_expr
    end;
    let num_normal_occurrences_of_bound_vars =
      match free_names_of_body with
      | Unknown -> Variable.Map.empty
      | Known free_names_of_body ->
        let free_names_of_bindable = Bound_pattern.free_names bound_pattern in
        Name_occurrences.fold_variables free_names_of_bindable
          ~init:Variable.Map.empty ~f:(fun num_occurrences var ->
            let num =
              Name_occurrences.count_variable_normal_mode free_names_of_body var
            in
            Variable.Map.add var num num_occurrences)
    in
    let t0 : T0.t = { num_normal_occurrences_of_bound_vars; body } in
    { name_abstraction = A.create bound_pattern t0; defining_expr }

  let defining_expr t = t.defining_expr

  let free_names ({ name_abstraction = _; defining_expr } as t) =
    pattern_match t ~f:(fun bound_pattern ~body ->
        let from_bindable = Bound_pattern.free_names bound_pattern in
        let from_defining_expr =
          let name_mode = Bound_pattern.name_mode bound_pattern in
          Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
            (Named.free_names defining_expr)
            name_mode
        in
        let from_body = Expr.free_names body in
        (* CR mshinwell: See comment in expr.rec.ml *)
        (* Care: there can be recursive bindings. *)
        Name_occurrences.diff
          (Name_occurrences.union from_defining_expr from_body)
          from_bindable)

  let apply_renaming ({ name_abstraction; defining_expr } as t) perm =
    let name_abstraction' = A.apply_renaming name_abstraction perm in
    let defining_expr' = Named.apply_renaming defining_expr perm in
    if name_abstraction == name_abstraction' && defining_expr == defining_expr'
    then t
    else
      { name_abstraction = name_abstraction'; defining_expr = defining_expr' }

  let all_ids_for_export { name_abstraction; defining_expr } =
    let defining_expr_ids = Named.all_ids_for_export defining_expr in
    let name_abstraction_ids = A.all_ids_for_export name_abstraction in
    Ids_for_export.union defining_expr_ids name_abstraction_ids
end

and Named : sig
  (** The defining expressions of [Let] bindings. *)
  type t = private
    | Simple of Simple.t
        (** Things that fit in a register (variables, symbols, constants). These
            do not have to be [Let]-bound but are allowed here for convenience. *)
    | Prim of Flambda_primitive.t * Debuginfo.t
        (** Primitive operations (arithmetic, memory access, allocation, etc). *)
    | Set_of_closures of Set_of_closures.t
        (** Definition of a set of (dynamically allocated) possibly
            mutually-recursive closures. *)
    | Static_consts of Static_const_group.t
        (** Definition of one or more symbols representing statically-allocated
            constants (including sets of closures). *)
    (* CR mshinwell: Add comment regarding ordering, recursion, etc. *)
    | Rec_info of Rec_info_expr.t
        (** Definition of a state of recursive inlining. *)

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Convert a register-width value into the defining expression of a [Let]. *)
  val create_simple : Simple.t -> t

  (** Convert a primitive, with associated debugging information, into the
      defining expression of a [Let]. *)
  val create_prim : Flambda_primitive.t -> Debuginfo.t -> t

  (** Convert a set of closures into the defining expression of a [Let]. *)
  val create_set_of_closures : Set_of_closures.t -> t

  (** Convert one or more statically-allocated constants into the defining
      expression of a [Let]. *)
  val create_static_consts : Static_const_group.t -> t

  (** Convert one or more expressions for recursion state into the defining
      expression of a [Let]. *)
  val create_rec_info : Rec_info_expr.t -> t

  (** Build an expression boxing the name. The returned kind is the one of the
      unboxed version. *)
  val box_value :
    Name.t -> Flambda_kind.t -> Debuginfo.t -> Named.t * Flambda_kind.t

  (** Build an expression unboxing the name. The returned kind is the one of the
      unboxed version. *)
  val unbox_value :
    Name.t -> Flambda_kind.t -> Debuginfo.t -> Named.t * Flambda_kind.t

  (** Return a defining expression for a [Let] which is kind-correct, but not
      necessarily type-correct, at the given kind. *)
  val dummy_value : Flambda_kind.t -> t

  val at_most_generative_effects : t -> bool

  val is_dynamically_allocated_set_of_closures : t -> bool

  (** Returns [true] iff the given expression is one or more
      statically-allocated constants. *)
  val is_static_consts : t -> bool

  val must_be_static_consts : t -> Static_const_group.t
end = struct
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t
    | Static_consts of Static_const_group.t
    | Rec_info of Rec_info_expr.t

  let create_simple simple = Simple simple

  let create_prim prim dbg = Prim (prim, dbg)

  let create_set_of_closures set_of_closures = Set_of_closures set_of_closures

  let create_static_consts consts = Static_consts consts

  let create_rec_info rec_info_expr = Rec_info rec_info_expr

  let print_or_elide_debuginfo ppf dbg =
    if Debuginfo.is_none dbg
    then Format.pp_print_string ppf ""
    else begin
      Format.pp_print_string ppf " ";
      Debuginfo.print_compact ppf dbg
    end

  let [@ocamlformat "disable"] print ppf (t : t) =
    match t with
    | Simple simple -> Simple.print ppf simple
    | Prim (prim, dbg) ->
      fprintf ppf "@[<hov 1>(%a@<0>%s%a@<0>%s)@]"
        Flambda_primitive.print prim
        (Flambda_colours.debuginfo ())
        print_or_elide_debuginfo dbg
        (Flambda_colours.normal ())
    | Set_of_closures set_of_closures ->
      Set_of_closures.print ppf set_of_closures
    | Static_consts consts ->
      Static_const_group.print ppf consts
    | Rec_info rec_info_expr ->
      Rec_info_expr.print ppf rec_info_expr

  let free_names t =
    match t with
    | Simple simple -> Simple.free_names simple
    | Prim (prim, _dbg) -> Flambda_primitive.free_names prim
    | Set_of_closures set -> Set_of_closures.free_names set
    | Static_consts consts -> Static_const_group.free_names consts
    | Rec_info rec_info_expr -> Rec_info_expr.free_names rec_info_expr

  let apply_renaming t perm =
    match t with
    | Simple simple ->
      let simple' = Simple.apply_renaming simple perm in
      if simple == simple' then t else Simple simple'
    | Prim (prim, dbg) ->
      let prim' = Flambda_primitive.apply_renaming prim perm in
      if prim == prim' then t else Prim (prim', dbg)
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_renaming set perm in
      if set == set' then t else Set_of_closures set'
    | Static_consts consts ->
      let consts' = Static_const_group.apply_renaming consts perm in
      if consts == consts' then t else Static_consts consts'
    | Rec_info rec_info_expr ->
      let rec_info_expr' = Rec_info_expr.apply_renaming rec_info_expr perm in
      if rec_info_expr == rec_info_expr' then t else Rec_info rec_info_expr'

  let all_ids_for_export t =
    match t with
    | Simple simple -> Ids_for_export.from_simple simple
    | Prim (prim, _dbg) -> Flambda_primitive.all_ids_for_export prim
    | Set_of_closures set -> Set_of_closures.all_ids_for_export set
    | Static_consts consts -> Static_const_group.all_ids_for_export consts
    | Rec_info rec_info_expr -> Rec_info_expr.all_ids_for_export rec_info_expr

  let box_value name (kind : Flambda_kind.t) dbg : t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate -> Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Box_number Naked_float, simple), dbg), K.value
    | Naked_number Naked_int32 ->
      Prim (Unary (Box_number Naked_int32, simple), dbg), K.value
    | Naked_number Naked_int64 ->
      Prim (Unary (Box_number Naked_int64, simple), dbg), K.value
    | Naked_number Naked_nativeint ->
      Prim (Unary (Box_number Naked_nativeint, simple), dbg), K.value
    | Fabricated -> Misc.fatal_error "Cannot box values of [Fabricated] kind"
    | Rec_info -> Misc.fatal_error "Cannot box values of [Rec_info] kind"

  let unbox_value name (kind : Flambda_kind.t) dbg : t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate -> Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Unbox_number Naked_float, simple), dbg), K.naked_float
    | Naked_number Naked_int32 ->
      Prim (Unary (Unbox_number Naked_int32, simple), dbg), K.naked_int32
    | Naked_number Naked_int64 ->
      Prim (Unary (Unbox_number Naked_int64, simple), dbg), K.naked_int64
    | Naked_number Naked_nativeint ->
      ( Prim (Unary (Unbox_number Naked_nativeint, simple), dbg),
        K.naked_nativeint )
    | Fabricated -> Misc.fatal_error "Cannot unbox values of [Fabricated] kind"
    | Rec_info -> Misc.fatal_error "Cannot unbox values of [Rec_info] kind"

  let at_most_generative_effects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
    | Set_of_closures _ -> true
    | Static_consts _ -> true
    | Rec_info _ -> true

  let dummy_value (kind : K.t) : t =
    let simple =
      match kind with
      | Value -> Simple.const_zero
      | Naked_number Naked_immediate ->
        Simple.const (Reg_width_const.naked_immediate Targetint_31_63.zero)
      | Naked_number Naked_float ->
        Simple.const
          (Reg_width_const.naked_float Numeric_types.Float_by_bit_pattern.zero)
      | Naked_number Naked_int32 ->
        Simple.const (Reg_width_const.naked_int32 Int32.zero)
      | Naked_number Naked_int64 ->
        Simple.const (Reg_width_const.naked_int64 Int64.zero)
      | Naked_number Naked_nativeint ->
        Simple.const (Reg_width_const.naked_nativeint Targetint_32_64.zero)
      | Fabricated -> Misc.fatal_error "[Fabricated] kind not expected here"
      | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"
    in
    Simple simple

  let is_dynamically_allocated_set_of_closures t =
    match t with
    | Set_of_closures _ -> true
    | Simple _ | Prim _ | Static_consts _ | Rec_info _ -> false

  let is_static_consts t =
    match t with
    | Static_consts _ -> true
    | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ -> false

  let must_be_static_consts t =
    match t with
    | Static_consts consts -> consts
    | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ ->
      Misc.fatal_errorf "Must be [Static_consts], but is not: %a" print t
end

and Non_recursive_let_cont_handler : sig
  (** The representation of the alpha-equivalence class of the binding of a
      single non-recursive continuation handler over a body. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Deconstruct a continuation binding to get the name of the bound
      continuation and the expression over which it is scoped. *)
  val pattern_match : t -> f:(Continuation.t -> body:Expr.t -> 'a) -> 'a

  (** Deconstruct two continuation bindings using the same name. *)
  val pattern_match_pair :
    t -> t -> f:(Continuation.t -> body1:Expr.t -> body2:Expr.t -> 'a) -> 'a

  (** Obtain the continuation itself (rather than the body over which it is
      scoped). *)
  val handler : t -> Continuation_handler.t

  val create : Continuation.t -> body:Expr.t -> Continuation_handler.t -> t
end = struct
  module Continuation_and_body =
    Name_abstraction.Make (Bound_continuation) (Expr)

  type t =
    { continuation_and_body : Continuation_and_body.t;
      handler : Continuation_handler.t
    }

  let print _ppf _t = Misc.fatal_error "Not yet implemented"

  let create continuation ~body handler =
    let continuation_and_body =
      Continuation_and_body.create continuation body
    in
    { continuation_and_body; handler }

  let pattern_match t ~f =
    Continuation_and_body.pattern_match t.continuation_and_body
      ~f:(fun continuation body -> f continuation ~body)

  let pattern_match_pair t1 t2 ~f =
    Continuation_and_body.pattern_match_pair t1.continuation_and_body
      t2.continuation_and_body ~f:(fun continuation body1 body2 ->
        f continuation ~body1 ~body2)

  let handler t = t.handler

  let free_names { continuation_and_body; handler } =
    Name_occurrences.union
      (Continuation_and_body.free_names continuation_and_body)
      (Continuation_handler.free_names handler)

  let apply_renaming { continuation_and_body; handler } perm =
    let continuation_and_body' =
      Continuation_and_body.apply_renaming continuation_and_body perm
    in
    let handler' = Continuation_handler.apply_renaming handler perm in
    { handler = handler'; continuation_and_body = continuation_and_body' }

  let all_ids_for_export { continuation_and_body; handler } =
    let handler_ids = Continuation_handler.all_ids_for_export handler in
    let continuation_and_body_ids =
      Continuation_and_body.all_ids_for_export continuation_and_body
    in
    Ids_for_export.union handler_ids continuation_and_body_ids
end

and Recursive_let_cont_handlers : sig
  (** The representation of the alpha-equivalence class of a group of possibly
      (mutually-) recursive continuation handlers that are bound both over a
      body and their own handler code. *)
  type t

  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Deconstruct a continuation binding to get the bound continuations,
      together with the expressions and handlers over which they are scoped. *)
  val pattern_match :
    t -> f:(body:Expr.t -> Continuation_handlers.t -> 'a) -> 'a

  (** Deconstruct two continuation bindings using the same bound continuations. *)
  val pattern_match_pair :
    t ->
    t ->
    f:
      (body1:Expr.t ->
      body2:Expr.t ->
      Continuation_handlers.t ->
      Continuation_handlers.t ->
      'a) ->
    'a

  val create : body:Expr.t -> Continuation_handlers.t -> t
end = struct
  module T0 = struct
    type t =
      { handlers : Continuation_handlers.t;
        body : Expr.t
      }

    let print _ppf _t = Misc.fatal_error "Not yet implemented"

    let create ~body handlers = { handlers; body }

    let handlers t = t.handlers

    let body t = t.body

    let free_names { handlers; body } =
      Name_occurrences.union
        (Continuation_handlers.free_names handlers)
        (Expr.free_names body)

    let apply_renaming { handlers; body } perm =
      let handlers' = Continuation_handlers.apply_renaming handlers perm in
      let body' = Expr.apply_renaming body perm in
      { handlers = handlers'; body = body' }

    let all_ids_for_export { handlers; body } =
      let body_ids = Expr.all_ids_for_export body in
      let handlers_ids = Continuation_handlers.all_ids_for_export handlers in
      Ids_for_export.union body_ids handlers_ids
  end

  include Name_abstraction.Make (Bound_continuations) (T0)

  let create ~body handlers =
    let bound = Continuation_handlers.domain handlers in
    let handlers0 = T0.create ~body handlers in
    create
      (Bound_continuations.create (Continuation.Set.elements bound))
      handlers0

  let pattern_match t ~f =
    pattern_match t ~f:(fun _bound handlers0 ->
        let body = T0.body handlers0 in
        let handlers = T0.handlers handlers0 in
        f ~body handlers)

  let pattern_match_pair t1 t2 ~f =
    pattern_match_pair t1 t2 ~f:(fun _bound handlers0_1 handlers0_2 ->
        let body1 = T0.body handlers0_1 in
        let body2 = T0.body handlers0_2 in
        let handlers1 = T0.handlers handlers0_1 in
        let handlers2 = T0.handlers handlers0_2 in
        f ~body1 ~body2 handlers1 handlers2)
end

and Static_const_or_code : sig
  type t =
    | Code of Function_params_and_body.t Code0.t
    | Static_const of Static_const.t

  include Container_types.S with type t := t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  val is_fully_static : t -> bool

  val to_code : t -> Function_params_and_body.t Code0.t option

  val match_against_bound_symbols_pattern :
    t ->
    Bound_symbols.Pattern.t ->
    code:(Code_id.t -> Function_params_and_body.t Code0.t -> 'a) ->
    set_of_closures:
      (closure_symbols:Symbol.t Closure_id.Lmap.t -> Set_of_closures.t -> 'a) ->
    block_like:(Symbol.t -> Static_const.t -> 'a) ->
    'a
end = struct
  type t =
    | Code of Function_params_and_body.t Code0.t
    | Static_const of Static_const.t

  let print ppf t =
    match t with
    | Code code ->
      fprintf ppf "@[<hov 1>(@<0>%sCode@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Code0.print
           ~print_function_params_and_body:Function_params_and_body.print)
        code
    | Static_const const -> Static_const.print ppf const

  include Container_types.Make (struct
    type nonrec t = t

    let print = print

    let compare t1 t2 =
      match t1, t2 with
      | Code code1, Code code2 -> Code0.compare code1 code2
      | Static_const const1, Static_const const2 ->
        Static_const.compare const1 const2
      | Code _, Static_const _ -> -1
      | Static_const _, Code _ -> 1

    let equal t1 t2 = compare t1 t2 = 0

    let hash _t = Misc.fatal_error "Not yet implemented"

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)

  let free_names t =
    match t with
    | Code code -> Code0.free_names code
    | Static_const const -> Static_const.free_names const

  let apply_renaming t renaming =
    if Renaming.is_empty renaming
    then t
    else
      match t with
      | Code code ->
        let code' =
          Code0.apply_renaming
            ~apply_renaming_function_params_and_body:
              Function_params_and_body.apply_renaming code renaming
        in
        if code == code' then t else Code code'
      | Static_const const ->
        let const' = Static_const.apply_renaming const renaming in
        if const == const' then t else Static_const const'

  let all_ids_for_export t =
    match t with
    | Code code ->
      Code0.all_ids_for_export
        ~all_ids_for_export_function_params_and_body:
          Function_params_and_body.all_ids_for_export code
    | Static_const const -> Static_const.all_ids_for_export const

  let is_fully_static t =
    match t with
    | Code _ -> true
    | Static_const const -> Static_const.is_fully_static const

  let to_code t = match t with Code code -> Some code | Static_const _ -> None

  let match_against_bound_symbols_pattern (t : t)
      (pat : Bound_symbols.Pattern.t) ~code:code_callback ~set_of_closures
      ~block_like =
    match t, pat with
    | Code code, Code code_id ->
      if not (Code_id.equal (Code0.code_id code) code_id)
      then
        Misc.fatal_errorf "Mismatch on declared code IDs:@ %a@ =@ %a"
          Bound_symbols.Pattern.print pat print t;
      code_callback code_id code
    | Static_const const, (Set_of_closures _ | Block_like _) ->
      Static_const.match_against_bound_symbols_pattern const pat
        ~set_of_closures ~block_like
    | Static_const _, Code _ | Code _, (Set_of_closures _ | Block_like _) ->
      Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
        Bound_symbols.Pattern.print pat print t
end

and Static_const_group : sig
  type t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val empty : t

  val create : Static_const_or_code.t list -> t

  val print : Format.formatter -> t -> unit

  val to_list : t -> Static_const_or_code.t list

  val concat : t -> t -> t

  val map : t -> f:(Static_const_or_code.t -> Static_const_or_code.t) -> t

  val match_against_bound_symbols :
    t ->
    Bound_symbols.t ->
    init:'a ->
    code:('a -> Code_id.t -> Function_params_and_body.t Code0.t -> 'a) ->
    set_of_closures:
      ('a ->
      closure_symbols:Symbol.t Closure_id.Lmap.t ->
      Set_of_closures.t ->
      'a) ->
    block_like:('a -> Symbol.t -> Static_const.t -> 'a) ->
    'a

  (** This function ignores [Deleted] code. *)
  val pieces_of_code : t -> Function_params_and_body.t Code0.t Code_id.Map.t

  (** This function ignores [Deleted] code. *)
  val pieces_of_code' : t -> Function_params_and_body.t Code0.t list

  val is_fully_static : t -> bool
end = struct
  type nonrec t = Static_const_or_code.t list

  let create static_consts = static_consts

  let to_list t = t

  let empty = []

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Static_const_or_code.print) t

  let free_names t =
    List.map Static_const_or_code.free_names t |> Name_occurrences.union_list

  let apply_renaming t renaming =
    List.map
      (fun static_const ->
        Static_const_or_code.apply_renaming static_const renaming)
      t

  let all_ids_for_export t =
    List.map Static_const_or_code.all_ids_for_export t
    |> Ids_for_export.union_list

  let match_against_bound_symbols t bound_symbols ~init ~code:code_callback
      ~set_of_closures:set_of_closures_callback ~block_like:block_like_callback
      =
    let bound_symbol_pats = Bound_symbols.to_list bound_symbols in
    if List.compare_lengths t bound_symbol_pats <> 0
    then
      Misc.fatal_errorf
        "Mismatch between length of [Bound_symbols.t] and [Static_const.t \
         list]:@ %a@ =@ %a"
        Bound_symbols.print bound_symbols print t;
    ListLabels.fold_left2 t bound_symbol_pats ~init
      ~f:(fun acc static_const bound_symbols_pat ->
        Static_const_or_code.match_against_bound_symbols_pattern static_const
          bound_symbols_pat
          ~code:(fun code_id code -> code_callback acc code_id code)
          ~set_of_closures:(fun ~closure_symbols set_of_closures ->
            set_of_closures_callback acc ~closure_symbols set_of_closures)
          ~block_like:(fun symbol static_const ->
            block_like_callback acc symbol static_const))

  let pieces_of_code t =
    List.filter_map Static_const_or_code.to_code t
    |> List.filter_map (fun code ->
           if Code0.is_deleted code
           then None
           else Some (Code0.code_id code, code))
    |> Code_id.Map.of_list

  let pieces_of_code' t = pieces_of_code t |> Code_id.Map.data

  let is_fully_static t = List.for_all Static_const_or_code.is_fully_static t

  let concat t1 t2 = t1 @ t2

  let map t ~f = List.map f t
end

(* CR mshinwell: Consider counting numbers of names in Name_occurrences *)
(* CR mshinwell: Check that apply_cont is well-formed when there is a trap
   installation or removal. *)
(* CR-someday pchambart: for sum types, we should probably add an exhaustive
   pattern in ignores functions to be reminded if a type change *)
(* CR-someday mshinwell: We should make "direct applications should not have
   overapplication" be an invariant throughout. At the moment I think this is
   only true after [Simplify] has split overapplications. *)

module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures

module Import = struct
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Continuation_handler = Continuation_handler
  module Continuation_handlers = Continuation_handlers
  module Expr = Expr
  module Function_declarations = Function_declarations
  module Function_params_and_body = Function_params_and_body
  module Let = Let
  module Let_cont = Let_cont
  module Named = Named
  module Non_recursive_let_cont_handler = Non_recursive_let_cont_handler
  module Recursive_let_cont_handlers = Recursive_let_cont_handlers
  module Set_of_closures = Set_of_closures
  module Static_const = Static_const
  module Static_const_group = Static_const_group
  module Static_const_or_code = Static_const_or_code
  module Switch = Switch
end
