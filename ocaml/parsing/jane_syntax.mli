(** Syntax for Jane Street's novel syntactic features.  This module provides
    three things:

    1. First-class ASTs for all syntax introduced by our language extensions,
       plus one for built-in features; these are split out into a different
       module each ([Comprehensions], etc.).

    2. A first-class AST for each OCaml AST, unifying all our novel syntactic
       features in modules named after the syntactic category
       ([Expression.t], etc.).

    3. A way to interpret these values as terms of the coresponding OCaml ASTs,
       and to match on terms of those OCaml ASTs to see if they're terms from
       our novel syntax.

    We keep our novel syntax separate so that we can avoid having to modify the
    existing AST, as this would break compatibility with every existing ppx and
    other such tooling.

    For details on the rationale behind this approach (and for some of the gory
    details), see [Jane_syntax_parsing]. *)

(*********************************************)
(* Individual features *)

(** The ASTs for list and array comprehensions *)
module Comprehensions : sig
  type iterator =
    | Range of { start     : Parsetree.expression
               ; stop      : Parsetree.expression
               ; direction : Asttypes.direction_flag }
    (** "= START to STOP" (direction = Upto)
        "= START downto STOP" (direction = Downto) *)
    | In of Parsetree.expression
    (** "in EXPR" *)

  (* In [Typedtree], the [pattern] moves into the [iterator]. *)
  type clause_binding =
    { pattern    : Parsetree.pattern
    ; iterator   : iterator
    ; attributes : Parsetree.attribute list }
    (** [@...] PAT (in/=) ... *)

  type clause =
    | For of clause_binding list
    (** "for PAT (in/=) ... and PAT (in/=) ... and ..."; must be nonempty *)
    | When of Parsetree.expression
    (** "when EXPR" *)

  type comprehension =
    { body : Parsetree.expression
    (** The body/generator of the comprehension *)
    ; clauses : clause list
    (** The clauses of the comprehension; must be nonempty *) }

  type expression =
    | Cexp_list_comprehension  of comprehension
    (** [BODY ...CLAUSES...] *)
    | Cexp_array_comprehension of Asttypes.mutable_flag * comprehension
    (** [|BODY ...CLAUSES...|] (flag = Mutable)
        [:BODY ...CLAUSES...:] (flag = Immutable)
          (only allowed with [-extension immutable_arrays]) *)

  val expr_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    expression -> Parsetree.expression
end

(** The ASTs for immutable arrays.  When we merge this upstream, we'll merge
    these into the existing [P{exp,pat}_array] constructors by adding a
    [mutable_flag] argument (just as we did with [T{exp,pat}_array]). *)
module Immutable_arrays : sig
  type expression =
    | Iaexp_immutable_array of Parsetree.expression list
    (** [: E1; ...; En :] *)

  type pattern =
    | Iapat_immutable_array of Parsetree.pattern list
    (** [: P1; ...; Pn :] **)

  val expr_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    expression -> Parsetree.expression
  val pat_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    pattern -> Parsetree.pattern
end

(** The ASTs for labeled tuples. When we merge this upstream, we'll replace
    existing [P{typ,exp,pat}_tuple] constructors with these. *)
module Labeled_tuples : sig
  type core_type =
    | Lttyp_tuple of (string option * Parsetree.core_type) list
    (** [Lttyp_tuple(tl)] represents a product type:
          - [T1 * ... * Tn]       when [tl] is [(None,T1);...;(None,Tn)]
          - [L1:T1 * ... * Ln:Tn] when [tl] is [(Some L1,T1);...;(Some Ln,Tn)]
          - A mix, e.g. [L1:T1,T2] when [tl] is [(Some L1,T1);(None,T2)]

          Invariant: [n >= 2] and there is at least one label.
      *)

  type expression =
    | Ltexp_tuple of (string option * Parsetree.expression) list
    (** [Ltexp_tuple(el)] represents
          - [(E1, ..., En)]
              when [el] is [(None, E1);...;(None, En)]
          - [(~L1:E1, ..., ~Ln:En)]
              when [el] is [(Some L1, E1);...;(Some Ln, En)]
          - A mix, e.g.:
              [(~L1:E1, E2)] when [el] is [(Some L1, E1); (None, E2)]

          Invariant: [n >= 2] and there is at least one label.
      *)

  type pattern =
    | Ltpat_tuple of
      (string option * Parsetree.pattern) list * Asttypes.closed_flag
    (** [Ltpat_tuple(pl, Closed)] represents
          - [(P1, ..., Pn)]       when [pl] is [(None, P1);...;(None, Pn)]
          - [(L1:P1, ..., Ln:Pn)] when [pl] is
                                              [(Some L1, P1);...;(Some Ln, Pn)]
          - A mix, e.g. [(L1:P1, P2)] when [pl] is [(Some L1, P1);(None, P2)]
          - If pattern is open, then it also ends in a [..]

        Invariant:
        - If Closed, [n >= 2] and there is at least one label.
        - If Open, [n >= 1]
      *)

  val typ_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    core_type -> Parsetree.core_type

  val expr_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    expression -> Parsetree.expression

  val pat_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    pattern -> Parsetree.pattern
end

(** The ASTs for [include functor].  When we merge this upstream, we'll merge
    these into the existing [P{sig,str}_include] constructors (similar to what
    we did with [T{sig,str}_include], but without depending on typechecking). *)
module Include_functor : sig
  type signature_item =
    | Ifsig_include_functor of Parsetree.include_description

  type structure_item =
    | Ifstr_include_functor of Parsetree.include_declaration

  val sig_item_of : loc:Location.t -> signature_item -> Parsetree.signature_item
  val str_item_of : loc:Location.t -> structure_item -> Parsetree.structure_item
end

(** The ASTs for module type strengthening. *)
module Strengthen : sig
  type module_type =
    { mty : Parsetree.module_type; mod_id : Longident.t Location.loc }

  val mty_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    module_type -> Parsetree.module_type
end

(** The ASTs for unboxed literals, like #4.0 *)
module Unboxed_constants : sig
  type t =
    | Float of string * char option
    | Integer of string * char

  type expression = t
  type pattern = t

  val expr_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    expression -> Parsetree.expression

  val pat_of :
    loc:Location.t -> attrs:Parsetree.attributes ->
    pattern -> Parsetree.pattern
end

(******************************************)
(* General facility, which we export *)

(** The module type of our extended ASTs for our novel syntax, instantiated once
    for each syntactic category.  We tend to call the pattern-matching functions
    here with unusual indentation, not indenting the [None] branch further so as
    to avoid merge conflicts with upstream. *)
module type AST = sig
  (** The AST for all our Jane Street syntax; one constructor per feature that
      extends the given syntactic category.  Some extensions are handled
      separately and thus are not listed here.

      This type will be something like [jane_syntax_ast * Parsetree.attributes]
      in cases where the Jane Syntax encoding of the AST uses attributes. In
      these cases, the [Parsetree.attributes] are the *rest* of the attributes
      after removing Jane Syntax-related attributes. Callers of [of_ast] should
      refer to these attributes rather than, for example, [pexp_attributes].
  *)
  type t

  (** The corresponding OCaml AST *)
  type ast

  (** Given an OCaml AST node, check to see if it corresponds to an embedded
      term from our novel syntax.  If it does, as long as the feature isn't a
      disabled language extension, then return it; if it's not a piece of novel
      syntax, return [None]; if it's an embedded term from a disabled language
      extension, raise an error.

      AN IMPORTANT NOTE: The design of this function is careful to make merge
      conflicts with upstream less likely: we want no edits at all -- not even
      indentation -- to surrounding code. This is why we return a [t option],
      not some structure that could include the [ast_desc] if there is no
      extension.

      Indentation: we *do not change the indentation level* when we match on
      this function's result!  E.g. from [type_expect_] in [typecore.ml]:

      {[
        match Jane_syntax.Expression.of_ast sexp with
        | Some jexp ->
            type_expect_jane_syntax
              ~loc
              ~env
              ~expected_mode
              ~ty_expected
              ~explanation
              ~attributes:sexp.pexp_attributes
              jexp
        | None      -> match sexp.pexp_desc with
        | Pexp_ident lid ->
            let path, mode, desc, kind = type_ident env ~recarg lid in
            (* ... *)
        | Pexp_constant(Pconst_string (str, _, _) as cst) ->
            register_allocation expected_mode;
            (* ... *)
        | (* ... *)
        | Pexp_unreachable ->
            re { exp_desc = Texp_unreachable;
                 exp_loc = loc; exp_extra = [];
                 exp_type = instance ty_expected;
                 exp_mode = expected_mode.mode;
                 exp_attributes = sexp.pexp_attributes;
                 exp_env = env }
      ]}

      Note that we match on the result of this function, forward to
      [type_expect_jane_syntax] if we get something, and otherwise do the real
      match on [sexp.pexp_desc] *without going up an indentation level*.  This
      is important to reduce the number of merge conflicts. *)
  val of_ast : ast -> t option
end

(******************************************)
(* Individual syntactic categories *)

(** Novel syntax in types *)
module Core_type : sig
  type t =
    | Jtyp_tuple of Labeled_tuples.core_type

  include AST
    with type t := t * Parsetree.attributes
     and type ast := Parsetree.core_type

  val typ_of :
    loc:Location.t -> attrs:Parsetree.attributes -> t -> Parsetree.core_type
end

(** Novel syntax in constructor arguments; this isn't a core AST type,
    but captures where [global_] lives *)
module Constructor_argument : sig
  type t = |

  include AST
    with type t := t * Parsetree.attributes
     and type ast := Parsetree.core_type
end

(** Novel syntax in expressions *)
module Expression : sig
  type t =
    | Jexp_comprehension    of Comprehensions.expression
    | Jexp_immutable_array  of Immutable_arrays.expression
    | Jexp_unboxed_constant of Unboxed_constants.expression
    | Jexp_tuple            of Labeled_tuples.expression

  include AST
    with type t := t * Parsetree.attributes
     and type ast := Parsetree.expression

  val expr_of :
    loc:Location.t -> attrs:Parsetree.attributes -> t -> Parsetree.expression
end

(** Novel syntax in patterns *)
module Pattern : sig
  type t =
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_unboxed_constant of Unboxed_constants.pattern
    | Jpat_tuple of Labeled_tuples.pattern

  include AST
    with type t := t * Parsetree.attributes
     and type ast := Parsetree.pattern

  val pat_of :
    loc:Location.t -> attrs:Parsetree.attributes -> t -> Parsetree.pattern
end

(** Novel syntax in module types *)
module Module_type : sig
  type t =
    | Jmty_strengthen of Strengthen.module_type

  include AST
    with type t := t * Parsetree.attributes
     and type ast := Parsetree.module_type
end

(** Novel syntax in signature items *)
module Signature_item : sig
  type t =
    | Jsig_include_functor of Include_functor.signature_item

  include AST with type t := t and type ast := Parsetree.signature_item
end

(** Novel syntax in structure items *)
module Structure_item : sig
  type t =
    | Jstr_include_functor of Include_functor.structure_item

  include AST with type t := t and type ast := Parsetree.structure_item
end

(** Novel syntax in extension constructors *)
module Extension_constructor : sig
  type t = |

  include AST with type t := t * Parsetree.attributes
               and type ast := Parsetree.extension_constructor
end
