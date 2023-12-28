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

(** The grammar of the Flambda 2 term language.

    The language is in double-barrelled continuation-passing style (CPS).
    Continuations, used for normal and exceptional control flow, are second
    class.  Unlike some CPS-based representations there is a conventional
    "let"-binding construct; this is structured in A-normal form (ANF).  Terms
    are represented up to alpha-conversion of bound variables and continuations.

    The basic structure of the language ensures that:

    - every intermediate value (and in particular every potential value that we
    may want to statically allocate) has a name;

    - every point to which we might wish to jump has a name;

    - there are no nested "let"s or subexpressions;

    - no re-normalisation of terms is required when substituting an application
    for an inlined body (unlike in conventional ANF forms). *)

(** Modules may be found further down the file giving operations on the abstract
    types that follow.  The types for some parts of terms (e.g. Apply_expr) are
    defined in their own files. *)

type expr

type let_expr

type non_recursive_let_cont_handler

type recursive_let_cont_handlers

type function_params_and_body

type static_const_group

type expr_descr = private
  | Let of let_expr
      (** Bind variable(s), symbol(s) and/or code ID(s). The defining expression
          (the part after the "=", as in "let x = defining_expr in body")
          never has any effect on control flow. *)
  | Let_cont of let_cont_expr  (** Define one or more continuations. *)
  | Apply of Apply_expr.t
      (** Call an OCaml function, external function or method. *)
  | Apply_cont of Apply_cont_expr.t
      (** Call a continuation, optionally adding or removing exception trap
          frames from the stack, which thus allows for the raising of
          exceptions. *)
  | Switch of Switch_expr.t  (** Conditional control flow. *)
  | Invalid of { message : string }
      (** Code proved type-incorrect and therefore unreachable. *)

(** The defining expressions of [Let]-bindings. *)
and named = private
  | Simple of Simple.t
      (** Things that fit in a register (variables, symbols, constants). These
          do not have to be [Let]-bound but are allowed here for convenience. *)
  | Prim of Flambda_primitive.t * Debuginfo.t
      (** Primitive operations (arithmetic, memory access, allocation, etc). *)
  | Set_of_closures of Set_of_closures.t
      (** Definition of a set of (dynamically allocated) possibly
          mutually-recursive closures. *)
  | Static_consts of static_const_group
      (** Definition of one or more symbols representing statically-allocated
          constants (including sets of closures). *)
  | Rec_info of Rec_info_expr.t
      (** Definition of a state of recursive inlining. *)

and let_cont_expr = private
  | Non_recursive of
      { handler : non_recursive_let_cont_handler;
        num_free_occurrences : Num_occurrences.t Or_unknown.t;
            (** [num_free_occurrences] can be used, for example, to decide
                whether to inline out a linearly-used continuation. It will
                always be strictly greater than zero. *)
        is_applied_with_traps : bool
            (** [is_applied_with_traps] is used to prevent inlining of
                continuations that are applied with a trap action *)
      }
  | Recursive of recursive_let_cont_handlers

and static_const_or_code = private
  | Code of function_params_and_body Code0.t
  | Deleted_code
  | Static_const of Static_const.t

module Invalid : sig
  type t =
    | Body_of_unreachable_continuation of Continuation.t
    | Apply_cont_of_unreachable_continuation of Continuation.t
    | Defining_expr_of_let of Bound_pattern.t * named
    | Closure_type_was_invalid of Apply_expr.t
    | Calling_local_returning_closure_with_normal_apply of Apply_expr.t
    | Zero_switch_arms
    | Code_not_rebuilt
    | To_cmm_dummy_body
    | Application_never_returns of Apply_expr.t
    | Over_application_never_returns of Apply_expr.t
    | Message of string
end

module Expr : sig
  (** The type of equivalence classes of expressions up to alpha-renaming of
      bound [Variable]s and [Continuation]s. *)
  type t = expr

  type descr = expr_descr

  include Expr_std.S_no_free_names with type t := t

  val ids_for_export : t -> Ids_for_export.t

  (** Extract the description of an expression. *)
  val descr : t -> expr_descr

  val create_let : let_expr -> t

  (** Create an application expression. *)
  val create_apply : Apply_expr.t -> t

  (** Create a continuation application (in the zero-arity case, "goto"). *)
  val create_apply_cont : Apply_cont_expr.t -> t

  val create_switch : Switch_expr.t -> t

  val create_invalid : Invalid.t -> t
end

module Named : sig
  (** The defining expressions of [Let] bindings. *)
  type t = named

  include Expr_std.S with type t := t

  (** Convert a register-width value into the defining expression of a [Let]. *)
  val create_simple : Simple.t -> t

  (** Convert a primitive, with associated debugging information, into the
      defining expression of a [Let]. *)
  val create_prim : Flambda_primitive.t -> Debuginfo.t -> t

  (** Convert a set of closures into the defining expression of a [Let]. *)
  val create_set_of_closures : Set_of_closures.t -> t

  (** Convert one or more statically-allocated constants into the defining
      expression of a [Let]. *)
  val create_static_consts : static_const_group -> t

  (** Convert one or more expressions for recursion state into the defining
      expression of a [Let]. *)
  val create_rec_info : Rec_info_expr.t -> t

  (** Return a defining expression for a [Let] which is kind-correct, but not
      necessarily type-correct, at the given kind. *)
  val dummy_value : Flambda_kind.t -> t

  (** Returns [true] iff the given expression is a set of closures that will be
      allocated on the OCaml heap during execution (i.e. not a
      statically-allocated set of closures). *)
  val is_dynamically_allocated_set_of_closures : t -> bool

  (** Returns [true] iff the given expression is one or more
      statically-allocated constants. *)
  val is_static_consts : t -> bool

  val must_be_static_consts : t -> static_const_group

  val at_most_generative_effects : t -> bool

  val fold_code_and_sets_of_closures :
    t ->
    init:'a ->
    f_code:('a -> function_params_and_body Code0.t -> 'a) ->
    f_set:('a -> Set_of_closures.t -> 'a) ->
    'a
end

module Let_expr : sig
  (** The alpha-equivalence classes of expressions that bind variables; and the
      expressions that bind symbols and code IDs (which are not treated up to
      alpha equivalence).

      Variables have normal syntactic scoping.  Symbols and code IDs are
      treated as in scope in all parts of the term dominated by the
      corresponding [Let]-binding.
  *)

  type t = let_expr

  include Expr_std.S_no_free_names with type t := t

  val create :
    Bound_pattern.t ->
    named ->
    body:expr ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    t

  (** The defining expression of the [Let]. *)
  val defining_expr : t -> named

  (** Look inside the [Let] by choosing a member of the alpha-equivalence
      class. *)
  val pattern_match : t -> f:(Bound_pattern.t -> body:expr -> 'a) -> 'a

  val pattern_match' :
    t ->
    f:
      (Bound_pattern.t ->
      num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
      body:expr ->
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
    dynamic:(Bound_pattern.t -> body1:expr -> body2:expr -> 'a) ->
    static:
      (bound_static1:Bound_static.t ->
      bound_static2:Bound_static.t ->
      body1:expr ->
      body2:expr ->
      'a) ->
    ('a, Pattern_match_pair_error.t) Result.t
end

module Continuation_handler : sig
  (** The alpha-equivalence class of the binding of a list of parameters around
      an expression, forming a continuation handler, together with auxiliary
      information about such handler. *)
  type t

  val print :
    cont:Continuation.t ->
    recursive:Recursive.t ->
    Format.formatter ->
    t ->
    unit

  val apply_renaming : t -> Renaming.t -> t

  (** Create the representation of a single continuation handler. *)
  val create :
    Bound_parameters.t ->
    handler:expr ->
    free_names_of_handler:Name_occurrences.t Or_unknown.t ->
    is_exn_handler:bool ->
    is_cold:bool ->
    t

  (** Choose a member of the alpha-equivalence class to enable examination of
      the parameters and the code over which they are scoped. *)
  val pattern_match' :
    t ->
    f:
      (Bound_parameters.t ->
      num_normal_occurrences_of_params:Num_occurrences.t Variable.Map.t ->
      handler:expr ->
      'a) ->
    'a

  val pattern_match : t -> f:(Bound_parameters.t -> handler:expr -> 'a) -> 'a

  module Pattern_match_pair_error : sig
    type t = Parameter_lists_have_different_lengths

    val to_string : t -> string
  end

  (** Choose members of two bindings' alpha-equivalence classes using the same
      parameters. *)
  val pattern_match_pair :
    t ->
    t ->
    f:(Bound_parameters.t -> handler1:expr -> handler2:expr -> 'a) ->
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

  val is_cold : t -> bool
end

module Continuation_handlers : sig
  (** The result of pattern matching on [Recursive_let_cont_handlers] (see
      above). *)
  type t

  (** Obtain the mapping from continuation to handler. *)
  val to_map : t -> Continuation_handler.t Continuation.Map.t

  (** The domain of [to_map t]. *)
  val domain : t -> Continuation.Set.t

  (** Whether any of the continuations are exception handlers. *)
  val contains_exn_handler : t -> bool
end

module Let_cont_expr : sig
  (** Values of type [t] represent alpha-equivalence classes of the definitions
      of continuations:

      let_cont [name] [args] = [handler] in [body]

      or using an alternative notation:

      [body] where [name] [args] = [handler]

      (In the -drawflambda / -dflambda output, "where" is omitted, in
      favour of a simple label syntax e.g. "k42:")

      - Continuations are second-class.

      - Continuations do not capture variables.

      - Continuations may be (mutually-)recursive. *)

  (** It is an error to mark a continuation that might be recursive as
      non-recursive. The converse is safe.

      Note: any continuation used as an exception handler must be non-recursive
      by the point it reaches [Flambda_to_cmm]. (This means that it is
      permissible to introduce mutual recursion through stubs associated with
      such continuations, so long as [Simplify] is run afterwards to inline them
      out and turn the resulting single [Recursive] handler into a
      [Non_recursive] one. *)
  type t = let_cont_expr

  include Expr_std.S_no_free_names with type t := t

  (** Create a definition of a non-recursive continuation. If the continuation
      does not occur free in the [body], then just the [body] is returned,
      without any enclosing [Let_cont]. *)
  val create_non_recursive :
    Continuation.t ->
    Continuation_handler.t ->
    body:expr ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    expr

  val create_non_recursive' :
    cont:Continuation.t ->
    Continuation_handler.t ->
    body:expr ->
    num_free_occurrences_of_cont_in_body:Num_occurrences.t Or_unknown.t ->
    is_applied_with_traps:bool ->
    expr

  (** Create a definition of a set of possibly-recursive continuations. *)
  val create_recursive :
    invariant_params:Bound_parameters.t ->
    Continuation_handler.t Continuation.Map.t ->
    body:expr ->
    expr
end

module Non_recursive_let_cont_handler : sig
  (** The representation of the alpha-equivalence class of the binding of a
      single non-recursive continuation handler over a body. *)
  type t = non_recursive_let_cont_handler

  val apply_renaming : t -> Renaming.t -> t

  (** Deconstruct a continuation binding to get the name of the bound
      continuation and the expression over which it is scoped. *)
  val pattern_match : t -> f:(Continuation.t -> body:expr -> 'a) -> 'a

  (** Deconstruct two continuation bindings using the same name. *)
  val pattern_match_pair :
    t -> t -> f:(Continuation.t -> body1:expr -> body2:expr -> 'a) -> 'a

  (** Obtain the continuation itself (rather than the body over which it is
      scoped). *)
  val handler : t -> Continuation_handler.t
end

module Recursive_let_cont_handlers : sig
  (** The representation of the alpha-equivalence class of a group of possibly
      (mutually-) recursive continuation handlers that are bound both over a
      body and their own handler code. *)
  type t = recursive_let_cont_handlers

  val apply_renaming : t -> Renaming.t -> t

  (** Deconstruct a continuation binding to get the bound continuations,
      together with the expressions and handlers over which they are scoped. *)
  val pattern_match :
    t ->
    f:
      (invariant_params:Bound_parameters.t ->
      body:expr ->
      Continuation_handlers.t ->
      'a) ->
    'a

  (** Deconstruct two continuation bindings using the same bound continuations. *)
  val pattern_match_pair :
    t ->
    t ->
    f:
      (invariant_params:Bound_parameters.t ->
      body1:expr ->
      body2:expr ->
      Continuation_handlers.t ->
      Continuation_handlers.t ->
      'a) ->
    'a
end

module Function_params_and_body : sig
  (** A name abstraction that comprises a function's parameters (together with
      any relations between them), the code of the function, and the [my_*]
      variables giving access to the closure, current region, etc. It also
      includes the return and exception continuations.

      These values are bound using [Define_symbol] constructs (see
      [Flambda_static]).

      From the body of the function, accesses to variables within the closure
      need to go via a [Project_value_slot] (from [my_closure]); accesses to any
      other simultaneously-defined functions need to go likewise via a
      [Project_function_slot]. *)
  type t = function_params_and_body

  include Expr_std.S_no_free_names with type t := t

  include Contains_ids.S with type t := t

  (** Create an abstraction that binds the given parameters, with associated
      relations thereon, over the given body. *)
  val create :
    return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    Bound_parameters.t ->
    body:expr ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    my_closure:Variable.t ->
    my_region:Variable.t ->
    my_depth:Variable.t ->
    t

  (** Choose a member of the alpha-equivalence class to enable examination of
      the parameters, relations thereon and the body over which they are
      scoped. *)
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
      Bound_parameters.t ->
      body:expr ->
      my_closure:Variable.t ->
      is_my_closure_used:bool Or_unknown.t ->
      my_region:Variable.t ->
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
      Bound_parameters.t ->
      body1:expr ->
      body2:expr ->
      my_closure:Variable.t ->
      my_region:Variable.t ->
      my_depth:Variable.t ->
      'a) ->
    'a

  val is_my_closure_used : t -> bool
end

module Static_const_or_code : sig
  type t = static_const_or_code

  include Container_types.S with type t := t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  val is_fully_static : t -> bool

  val is_block : t -> bool

  val is_set_of_closures : t -> bool

  val is_code : t -> bool

  val create_code : Function_params_and_body.t Code0.t -> t

  val deleted_code : t

  val create_static_const : Static_const.t -> t

  val to_code : t -> Function_params_and_body.t Code0.t option
end

module Static_const_group : sig
  type t = static_const_group

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val empty : t

  val create : Static_const_or_code.t list -> t

  val print : Format.formatter -> t -> unit

  val to_list : t -> Static_const_or_code.t list

  val concat : t -> t -> t

  val map : t -> f:(Static_const_or_code.t -> Static_const_or_code.t) -> t

  val match_against_bound_static :
    t ->
    Bound_static.t ->
    init:'a ->
    code:('a -> Code_id.t -> Function_params_and_body.t Code0.t -> 'a) ->
    deleted_code:('a -> Code_id.t -> 'a) ->
    set_of_closures:
      ('a ->
      closure_symbols:Symbol.t Function_slot.Lmap.t ->
      Set_of_closures.t ->
      'a) ->
    block_like:('a -> Symbol.t -> Static_const.t -> 'a) ->
    'a

  (** This function ignores [Deleted] code. *)
  val pieces_of_code : t -> Function_params_and_body.t Code0.t Code_id.Map.t

  (** This function ignores [Deleted] code. *)
  val pieces_of_code' : t -> Function_params_and_body.t Code0.t list

  val is_fully_static : t -> bool
end

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures
module Switch = Switch_expr

(** The idea is that you should typically do "open! Flambda" at the top of
    files, thus bringing in the following standard set of module aliases. *)
module Import : sig
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
