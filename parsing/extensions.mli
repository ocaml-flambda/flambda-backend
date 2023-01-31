(** Syntax for our custom ocaml-jst language extensions.  This module provides
    two things:

    1. First-class ASTs for all syntax introduced by our language extensions,
       one for each OCaml AST we extend, divided up into one extension per
       module and all available at once through modules named after the
       syntactic category ([Expression.t], etc.).

    2. A way to interpret these values as terms of the coresponding OCaml ASTs,
       and to match on terms of those OCaml ASTs to see if they're language
       extension terms.

    We keep our language extensions separate so that we can avoid having to
    modify the existing AST, as this would break compatibility with every
    existing ppx.

    For details on the rationale behind this approach (and for some of the gory
    details), see [Extensions_parsing]. *)

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
    (** PAT (in/=) ... [@...] *)

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

  type comprehension_expr =
    | Cexp_list_comprehension  of comprehension
    (** [BODY ...CLAUSES...] *)
    | Cexp_array_comprehension of Asttypes.mutable_flag * comprehension
    (** [|BODY ...CLAUSES...|] (flag = Mutable)
        [:BODY ...CLAUSES...:] (flag = Immutable)
          (only allowed with [-extension immutable_arrays]) *)
end

(** The ASTs for immutable arrays.  When we merge this upstream, we'll merge
    these into the existing [P{exp,pat}_array] constructors by adding a
    [mutable_flag] argument (just as we did with [T{exp,pat}_array]). *)
module Immutable_arrays : sig
  type expression =
    | Iaexp_immutable_array of Parsetree.expression list
    (** [: E1; ...; En :] *)
    (* CR aspectorzabusky: Or [Iaexp_iarray]? *)

  type pattern =
    | Iapat_immutable_array of Parsetree.pattern list
    (** [: P1; ...; Pn :] **)
    (* CR aspectorzabusky: Or [Iapat_iarray]? *)
end

(** The module type of language extension ASTs, instantiated once for each
    syntactic category.  We tend to call the pattern-matching functions here
    with unusual indentation, not indenting the [None] branch further so as to
    avoid merge conflicts with upstream. *)
module type AST = sig
  (** The AST for all our ocaml-jst language extensions; one constructor per
      language extension that extends the expression language.  Some extensions
      are handled separately and thus are not listed here. *)
  type t

  (** The corresponding OCaml AST *)
  type ast

  (** Given an OCaml AST node, check to see if it corresponds to a language
      extension term.  If it is, and the extension is enabled, then return it;
      if it's not a language extension term, return [None]; if it's a disabled
      language extension term, raise an error.

      AN IMPORTANT NOTE: We indent calls to this function *very* strangely: we
      *do not change the indentation level* when we match on its result!
      E.g. from [type_expect_] in [typecore.ml]:

      {[
        match Extensions.Expression.of_ast sexp with
        | Some eexp ->
            type_expect_extension
              ~loc ~env ~expected_mode ~ty_expected ~explanation eexp
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
      [type_expect_extension] if we get something, and otherwise do the real
      match on [sexp.pexp_desc] *without going up an indentation level*.  This
      is important to reduce the number of merge conflicts with upstream by
      avoiding changing the body of every single important function in the type
      checker to add pointless indentation. *)
  val of_ast : ast -> t option

  (** Given a location, an extension, and a language-extension term, wrap our
      custom term into the existing OCaml AST.  Succeeds whether or not the
      extension is enabled.  The language extension specified *must* correspond
      to the constructor of the language extension AST, or this function will
      raise a fatal error. *)
  val ast_of : loc:Location.t -> Clflags.Extension.t -> t -> ast
end

(** Language extensions in expressions *)
module Expression : sig
  type t =
    | Eexp_comprehension   of Comprehensions.comprehension_expr
    | Eexp_immutable_array of Immutable_arrays.expression

  include AST with type t := t and type ast = Parsetree.expression
end

(** Language extensions in patterns *)
module Pattern : sig
  type t =
    | Epat_immutable_array of Immutable_arrays.pattern

  include AST with type t := t and type ast = Parsetree.pattern
end
