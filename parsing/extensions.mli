(** Syntax for our custom `ocaml-jst` language extensions.  This module provides
    two things:

    1. A first-class AST for all syntax introduced by our language extensions,
       divided up into one extension per module and all available through
       [extension_expr].
    2. A general scheme for lowering these AST nodes into the existing OCaml AST
       ([Parsetree.expression]), so that we can avoid having to modify the
       existing AST (and therefore can avoid updating every ppx to be compatible
       with `ocaml-jst`), as well as a scheme for lifting OCaml AST nodes that
       were generated this way back to our new [extension_expr] AST.

   This file exposes just the clean interface; for details on why we want this,
   as well as the scheme we use, see the comment at the start of
   [extensions.ml]. *)

(** The AST for list and array comprehensions *)
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
        (** "for PAT (in/= ...) and PAT (in/= ...) and ..."; must be nonempty *)
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
    | Cexp_array_comprehension of comprehension
        (** [|BODY ...CLAUSES...|] *)
end

(** The AST for all our `ocaml-jst` language extensions; one constructor per
    extension.  Some extensions are handled separately and thus are not listed
    here. *)
type extension_expr =
  | Eexp_comprehension of Comprehensions.comprehension_expr

(** Given an AST node representing some syntax from a language extension, along
    with the language extension that we're working with and a location, lower our
    custom AST ([extension_expr]) into the existing OCaml AST.  Always succeeds,
    whether or not the extension is enabled. *)
val expr_of_extension_expr :
  loc:Location.t -> Clflags.Extension.t -> extension_expr -> Parsetree.expression

(** Given any AST node, check to see if it's the lowered form of syntax from a
    language extension; if it is, then return it if said language extension is
    enabled or raise an error otherwise.  Also raises an error if this AST node
    looks like a lowered language extension but is from an unknown extension or
    is otherwise malformed.

    AN IMPORTANT NOTE: We indent calls to this function *very* strangely: we *do
    not change the indentation level* when we match on its result!  E.g. from
    [type_expect_] in [typecore.ml]:

    {[
      match Extensions.extension_expr_of_expr sexp with
      | Some eexp ->
          type_expect_extension ~loc ~env ~expected_mode ~ty_expected eexp
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
    [type_expect_extension] if we get something, and otherwise do the real match
    on [sexp.pexp_desc] *without going up an indentation level*.  This is
    important to reduce the number of merge conflicts with upstream by avoiding
    changing the body of every single important function in the type checker to
    add pointless indentation. *)
val extension_expr_of_expr :
  Parsetree.expression -> extension_expr option
