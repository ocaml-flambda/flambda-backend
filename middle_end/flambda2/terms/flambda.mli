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

(* CR mshinwell: Work out how to auto-generate this file. *)

(** The terms of the intermediate language used for tree-based analysis and
    optimisation. *)

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

(** The basic structure of the language ensures that:

    - every intermediate value (and in particular every potential constant that
    we may want to lift) has a name;

    - every point to which we might wish to jump has a name;

    - there are no nested "let"s or subexpressions;

    - no re-normalisation of terms is required when substituting an application
    for an inlined body (unlike in ANF form). *)
module rec Expr : sig
  (** The type of equivalence classes of expressions up to alpha-renaming of
      bound [Variable]s and [Continuation]s. *)
  type t

  include Expr_std.S with type t := t

  val all_ids_for_export : t -> Ids_for_export.t

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

  (** Create an application expression. *)
  val create_apply : Apply.t -> t

  (** Create a continuation application (in the zero-arity case, "goto"). *)
  val create_apply_cont : Apply_cont.t -> t

  val create_switch : Switch_expr.t -> t

  (** Create an expression indicating type-incorrect or unreachable code. *)
  val create_invalid : ?semantics:Invalid_term_semantics.t -> unit -> t

  val bind_parameters_to_args_no_simplification :
    params:Kinded_parameter.t list ->
    args:Simple.t list ->
    body:Expr.t ->
    Expr.t
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
    | Static_consts of Static_const.Group.t
        (** Definition of one or more symbols representing statically-allocated
            constants (including sets of closures). *)
    | Rec_info of Rec_info_expr.t
        (** Definition of a state of recursive inlining. *)

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
  val create_static_consts : Static_const.Group.t -> t

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

  (** Returns [true] iff the given expression is a set of closures that will be
      allocated on the OCaml heap during execution (i.e. not a
      statically-allocated set of closures). *)
  val is_dynamically_allocated_set_of_closures : t -> bool

  (** Returns [true] iff the given expression is one or more
      statically-allocated constants. *)
  val is_static_consts : t -> bool

  val must_be_static_consts : t -> Static_const.Group.t

  val at_most_generative_effects : t -> bool
end

and Let_expr : sig
  (** The alpha-equivalence classes of expressions that bind variables; and the
      expressions that bind symbols (which are not treated up to alpha
      equivalence). *)
  type t

  include Expr_std.S with type t := t

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
                  whether to inline out a linearly-used continuation. It will
                  always be strictly greater than zero. *)
          is_applied_with_traps : bool
              (** [is_applied_with_traps] is used to prevent inlining of
                  continuations that are applied with a trap action *)
        }
    | Recursive of Recursive_let_cont_handlers.t

  include Expr_std.S with type t := t

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
end

and Non_recursive_let_cont_handler : sig
  (** The representation of the alpha-equivalence class of the binding of a
      single non-recursive continuation handler over a body. *)
  type t

  include Expr_std.S with type t := t

  (** Deconstruct a continuation binding to get the name of the bound
      continuation and the expression over which it is scoped. *)
  val pattern_match : t -> f:(Continuation.t -> body:Expr.t -> 'a) -> 'a

  (** Deconstruct two continuation bindings using the same name. *)
  val pattern_match_pair :
    t -> t -> f:(Continuation.t -> body1:Expr.t -> body2:Expr.t -> 'a) -> 'a

  (** Obtain the continuation itself (rather than the body over which it is
      scoped). *)
  val handler : t -> Continuation_handler.t
end

and Continuation_handler : sig
  (** The alpha-equivalence class of the binding of a list of parameters around
      an expression, forming a continuation handler, together with auxiliary
      information about such handler. *)
  type t

  include Expr_std.S with type t := t

  (** Create the representation of a single continuation handler. *)
  val create :
    Kinded_parameter.t list ->
    handler:Expr.t ->
    free_names_of_handler:Name_occurrences.t Or_unknown.t ->
    is_exn_handler:bool ->
    t

  (** Choose a member of the alpha-equivalence class to enable examination of
      the parameters and the code over which they are scoped. *)
  val pattern_match' :
    t ->
    f:
      (Kinded_parameter.t list ->
      num_normal_occurrences_of_params:Num_occurrences.t Variable.Map.t ->
      handler:Expr.t ->
      'a) ->
    'a

  val pattern_match :
    t -> f:(Kinded_parameter.t list -> handler:Expr.t -> 'a) -> 'a

  module Pattern_match_pair_error : sig
    type t = Parameter_lists_have_different_lengths

    val to_string : t -> string
  end

  (** Choose members of two bindings' alpha-equivalence classes using the same
      parameters. *)
  val pattern_match_pair :
    t ->
    t ->
    f:(Kinded_parameter.t list -> handler1:Expr.t -> handler2:Expr.t -> 'a) ->
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
end

and Recursive_let_cont_handlers : sig
  (** The representation of the alpha-equivalence class of a group of possibly
      (mutually-) recursive continuation handlers that are bound both over a
      body and their own handler code. *)
  type t

  include Expr_std.S with type t := t

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
end

and Function_params_and_body : sig
  (** A name abstraction that comprises a function's parameters (together with
      any relations between them), the code of the function, and the
      [my_closure] variable. It also includes the return and exception
      continuations.

      These values are bound using [Define_symbol] constructs (see
      [Flambda_static]).

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
    Exn_continuation.t ->
    Kinded_parameter.t list ->
    dbg:Debuginfo.t ->
    body:Expr.t ->
    free_names_of_body:Name_occurrences.t Or_unknown.t ->
    my_closure:Variable.t ->
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
      Exn_continuation.t
      (** To where we must jump if application of the function raises an
          exception. *) ->
      Kinded_parameter.t list ->
      body:Expr.t ->
      my_closure:Variable.t ->
      is_my_closure_used:bool Or_unknown.t ->
      my_depth:Variable.t ->
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
      Exn_continuation.t
      (** To where we must jump if application of the function raises an
          exception. *) ->
      Kinded_parameter.t list ->
      body1:Expr.t ->
      body2:Expr.t ->
      my_closure:Variable.t ->
      my_depth:Variable.t ->
      'a) ->
    'a

  val free_names_of_body : t -> Name_occurrences.t Or_unknown.t

  (** Return the debuginfo associated *)
  val debuginfo : t -> Debuginfo.t

  (** Return the arity of the function body *)
  val params_arity : t -> Flambda_arity.t
end

and Static_const : sig
  (** Language terms that represent statically-allocated values, bound to
      symbols. *)

  module Field_of_block : sig
    (** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
    type t =
      | Symbol of Symbol.t  (** The address of the given symbol. *)
      | Tagged_immediate of Targetint_31_63.t
          (** The given tagged immediate. *)
      | Dynamically_computed of Variable.t
          (** The value of the given variable. *)

    include Container_types.S with type t := t

    include Contains_names.S with type t := t
  end

  (* CR mshinwell: Somewhere there should be an invariant check that code has no
     free names. *)

  (** The static structure of a symbol, possibly with holes, ready to be filled
      with values computed at runtime. *)
  type t =
    | Code of Code.t
    | Set_of_closures of Set_of_closures.t
    | Block of Tag.Scannable.t * Mutability.t * Field_of_block.t list
    | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
    | Boxed_int32 of Int32.t Or_variable.t
    | Boxed_int64 of Int64.t Or_variable.t
    | Boxed_nativeint of Targetint_32_64.t Or_variable.t
    | Immutable_float_block of
        Numeric_types.Float_by_bit_pattern.t Or_variable.t list
    | Immutable_float_array of
        Numeric_types.Float_by_bit_pattern.t Or_variable.t list
    | Mutable_string of { initial_value : string }
    | Immutable_string of string

  include Container_types.S with type t := t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val is_fully_static : t -> bool

  val can_share : t -> bool

  val to_code : t -> Code.t option

  val must_be_set_of_closures : t -> Set_of_closures.t

  val match_against_bound_symbols_pattern :
    t ->
    Bound_symbols.Pattern.t ->
    code:(Code_id.t -> Code.t -> 'a) ->
    set_of_closures:
      (closure_symbols:Symbol.t Closure_id.Lmap.t -> Set_of_closures.t -> 'a) ->
    block_like:(Symbol.t -> t -> 'a) ->
    'a

  module Group : sig
    type t

    include Contains_names.S with type t := t

    include Contains_ids.S with type t := t

    val empty : t

    val create : Static_const.t list -> t

    val print : Format.formatter -> t -> unit

    val to_list : t -> Static_const.t list

    val concat : t -> t -> t

    val map : t -> f:(Static_const.t -> Static_const.t) -> t

    val match_against_bound_symbols :
      t ->
      Bound_symbols.t ->
      init:'a ->
      code:('a -> Code_id.t -> Code.t -> 'a) ->
      set_of_closures:
        ('a ->
        closure_symbols:Symbol.t Closure_id.Lmap.t ->
        Set_of_closures.t ->
        'a) ->
      block_like:('a -> Symbol.t -> Static_const.t -> 'a) ->
      'a

    (** This function ignores [Deleted] code. *)
    val pieces_of_code : t -> Code.t Code_id.Map.t

    (** This function ignores [Deleted] code. *)
    val pieces_of_code' : t -> Code.t list

    val is_fully_static : t -> bool
  end
end

and Code : sig
  (** A piece of code, comprising of the parameters and body of a function,
      together with a field indicating whether the piece of code is a newer
      version of one that existed previously (and may still exist), for example
      after a round of simplification. *)
  type t

  val code_id : t -> Code_id.t

  val params_and_body : t -> Function_params_and_body.t Or_deleted.t

  val params_and_body_must_be_present :
    error_context:string -> t -> Function_params_and_body.t

  val newer_version_of : t -> Code_id.t option

  val params_arity : t -> Flambda_arity.With_subkinds.t

  val result_arity : t -> Flambda_arity.With_subkinds.t

  val stub : t -> bool

  val inline : t -> Inline_attribute.t

  val is_a_functor : t -> bool

  val recursive : t -> Recursive.t

  val cost_metrics : t -> Cost_metrics.t

  val inlining_arguments : t -> Inlining_arguments.t

  val dbg : t -> Debuginfo.t

  val is_tupled : t -> bool

  val inlining_decision : t -> Function_decl_inlining_decision_type.t

  val create :
    Code_id.t ->
    params_and_body:
      (Function_params_and_body.t * Name_occurrences.t) Or_deleted.t ->
    newer_version_of:Code_id.t option ->
    params_arity:Flambda_arity.With_subkinds.t ->
    result_arity:Flambda_arity.With_subkinds.t ->
    stub:bool ->
    inline:Inline_attribute.t ->
    is_a_functor:bool ->
    recursive:Recursive.t ->
    cost_metrics:Cost_metrics.t ->
    inlining_arguments:Inlining_arguments.t ->
    dbg:Debuginfo.t ->
    is_tupled:bool ->
    inlining_decision:Function_decl_inlining_decision_type.t ->
    t

  val with_code_id : Code_id.t -> t -> t

  val with_params_and_body :
    (Function_params_and_body.t * Name_occurrences.t) Or_deleted.t ->
    cost_metrics:Cost_metrics.t ->
    t ->
    t

  val with_newer_version_of : Code_id.t option -> t -> t

  val print : Format.formatter -> t -> unit

  include Contains_names.S with type t := t

  val free_names_of_body : t -> Name_occurrences.t Or_unknown.t

  val all_ids_for_export : t -> Ids_for_export.t

  val make_deleted : t -> t

  val is_deleted : t -> bool
end

and Cost_metrics : sig
  type t

  val zero : t

  val from_size : Code_size.t -> t

  val size : t -> Code_size.t

  val print : Format.formatter -> t -> unit

  val ( + ) : t -> t -> t

  type code_characteristics =
    { cost_metrics : t;
      params_arity : int
    }

  val set_of_closures :
    find_code_characteristics:(Code_id.t -> code_characteristics) ->
    Set_of_closures.t ->
    t

  val increase_due_to_let_expr :
    is_phantom:bool -> cost_metrics_of_defining_expr:t -> t

  val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t

  val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t

  val notify_added : code_size:Code_size.t -> t -> t

  val notify_removed : operation:Removed_operations.t -> t -> t

  val expr_size : find_code:(Code_id.t -> Code.t) -> Expr.t -> Code_size.t

  val evaluate : args:Inlining_arguments.t -> t -> float
end

module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures

(** The idea is that you should typically do "open! Flambda" at the top of
    files, thus bringing in the following standard set of module aliases. *)
module Import : sig
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Code = Code
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
  module Switch = Switch
  module Cost_metrics = Cost_metrics
end
