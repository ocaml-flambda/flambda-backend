(** This module handles the logic around the syntax of our extensions to OCaml
    for [ocaml-jst], keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make
       sure things like ppxen are cross-compatible between upstream and
       [ocaml-jst].  Thankfully, OCaml already provides places to add extra
       syntax: extension points and annotations!  Thus, we have to come up with
       a way of representing our new syntactic constructs in terms of extension
       points (or annotations, but we went with the former).

    2. We don't want to actually match on extension points whose names are
       specific strings all over the compiler; that's incredibly messy, and it's
       easy to miss cases, etc.

    3. We want to keep different language extensions distinct so that we can add
       them to upstream independently, work on them separately, and so on.

    We have come up with a design that addresses those concerns by providing
    both a nice compiler-level interface for working with our syntactic
    extensions as first-class AST nodes, as well as a uniform scheme for
    translating this to and from OCaml AST values containing extension points.
    One wrinkle is that OCaml has many ASTs, one for each syntactic category
    (expressions, patterns, etc.); we have to provide this facility for each
    syntactic category where we want to provide extensions.

    a. For each language extension, we will define a module (e.g.,
       [Comprehensions]), in which we define a proper AST type per syntactic
       category we care about (e.g., [Comprehensions.comprehension_expr] and its
       subcomponents).  This addresses concern (3); we've now contained each
       extension in a module.  But just that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* for each syntactic category that's
       just for our language extensions; for expressions, it's called
       [extension_expr].  It contains one constructor for each of the AST types
       defined as described in design point (1).  This addresses concern (2); we
       can now match on actual OCaml constructors, as long as we can get ahold
       of them.  And to do that…

    c. We define a general scheme for how we represent language extensions in terms
       of the existing ASTs, and provide a few primitives for consuming/creating
       AST nodes of this form, for each syntactic category.  There's not a lot
       of abstraction to be done, or at least it's not (yet) apparent what
       abstraction there is to do, so most of this remains manual.  (Setting up
       a full lens-based/otherwise bidirectional approach sounds like a great
       opportunity for yak-shaving, but not *actually* a good idea.)  This
       solves concern (3), and by doing it uniformly helps us address multiple
       cases at one stroke.

    We then bundle this all up for each individual extension into the type
    [ast_extension] containing, for one syntactic category, two different
    (partial) isomorphisms: the fully isomorphic (up to exceptions) ability to
    lift and lower between the custom AST type (from design point (a)) and
    existing AST expressions, leveraging the common format for representing
    things in the existing AST from design point (c); and the partial ability to
    lift and lower between the custom AST type and our overall auxiliary AST
    type (from design point (b)), which is just a constructor application in one
    direction and a pattern match against a constructor in the other.  Each
    syntactic category  (or the lack of support for one) can then be stored
    inside an existential type ([optional_ast_extension]) that hides the
    extension-specific type, allowing us to collect all of our extensions
    together.

    This module contains the logic for moving to and from OCaml ASTs; the gory
    details of the encoding are detailed in the implementation.  All the actual
    ASTs should live in [Extensions], which is the only module that should
    directly depend on this one.  Here, we parameterize by the eventual
    auxiliary all-extension ASTs; these are the ['ext_ast] type parameters seen
    below.  We must parameterize as we can't define the auxiliary types until
    we've defined every language extension; by parameterizing, we get to keep
    all the messy AST-manipulation here, and work with it abstractly while
    defining the language extensions themselves.

    When using this module, we often want to specify what our syntax extensions
    look like when desugared into OCaml ASTs, so that we can validate the
    translation code.  We generally specify this as a BNF grammar, but we don't
    want to depend on the specific details of the desugaring.  Thus, instead of
    writing out extension points or attributes directly, we write the result of
    [Some_ast.make_extension ~loc [name1; name2; ...; NameN] a] as the special
    syntax [{% 'name1.name2.....nameN' | a %}] in the BNF.  Other pieces of the
    OCaml AST are used as normal. *)

(** Errors around the extension representation.  These should mostly just be
    fatal, but they're needed for one test case
    (tests/ast-invariants/test.ml). *)
module Error : sig
  (** Someone used [[%extension.EXTNAME]] wrong *)
  type malformed_extension =
    | Has_payload of Parsetree.payload
    | Wrong_arguments of (Asttypes.arg_label * Parsetree.expression) list
    | Wrong_tuple of Parsetree.pattern list

  (** An error triggered when desugaring a language extension from an OCaml AST *)
  type error =
    | Malformed_extension of string list * malformed_extension
    | Unknown_extension of string
    | Disabled_extension of Clflags.Extension.t
    | Wrong_syntactic_category of Clflags.Extension.t * string
    | Unnamed_extension
    | Bad_introduction of string * string list

  (** The main exception type thrown when desugaring a language extension from an
      OCaml AST; we also use the occasional [Misc.fatal_errorf]. *)
  exception Error of Location.t * error
end

(** The type of modules that lift and lower language extension terms from and
    to an OCaml AST type ([ast]) *)
module type AST = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The name for this syntactic category in the plural form; used for error
      messages (e.g., "expressions") *)
  val plural : string

  (** How to get the location attached to an AST node *)
  val location : ast -> Location.t

  (** Embed a language extension term in the AST with the given name (the
      [string list]) and body (the [ast]).  The name will be joined with dots
      and preceded by [extension.].  Partial inverse of [match_extension]. *)
  val make_extension  : loc:Location.t -> string list -> ast -> ast

  (** Given an AST node, check if it's a language extension term; if it is,
      split it back up into its name (the [string list]) and the body (the
      [ast]); the resulting name is split on dots and the leading [extension]
      component is dropped..  If the language extension term is malformed in any
      way, raises an error; if the input isn't a language extension term,
      returns [None].  Partial inverse of [make_extension]. *)
  val match_extension : ast -> (string list * ast) option
end

(** One [AST] module per syntactic category we currently care about; we're
    adding these lazily as we need them. *)

module Expression : AST with type ast = Parsetree.expression
module Pattern    : AST with type ast = Parsetree.pattern

(** What makes a language extension, for a single syntactic category.  Given a
    language extension AST ['ext], an OCaml AST ['ast], and our auxiliary
    all-extension AST ['ext_ast], then we need to know how to convert any term
    of type ['ext] to ([ast_of]) and from ([of_ast]) an OCaml AST node, and how
    to convert it to ([wrap]) and from ([unwrap]) our eventual auxiliary
    all-extension AST.  The first two are expected to each correspond to a
    function defined in an eventual language-extension-specific module; the
    latter two are expected to correspond to a constructor and a
    pattern-matching function, respectively. *)
type ('ext, 'ast, 'ext_ast) ast_extension =
  { ast_of : loc:Location.t -> 'ext -> 'ast
  ; of_ast : 'ast -> 'ext
  ; wrap   : 'ext -> 'ext_ast
  ; unwrap : 'ext_ast -> 'ext option }

(** Hiding the specific extension type lets us work with [ast_extension]s
    uniformly; since we don't support every syntactic category for every
    extension, we combine this with making the presence of the [ast_extension]
    optional, which aids abstraction later. *)
type ('ast, 'ext_ast) optional_ast_extension =
  | Supported :
      (_, 'ast, 'ext_ast) ast_extension ->
      ('ast, 'ext_ast) optional_ast_extension
  | Unsupported

(** Create the two core functions of this module: lifting and lowering OCaml AST
    terms from any syntactic category to and from our auxiliary all-extension
    ASTs.

    This will only get instantiated once; however, by making it a functor, we
    can keep all the general logic together here in this module, and keep the
    extension-specific stuff in [Extensions].. *)
module Translate
         (Syntactic_category : sig
            (** A type-indexed enumeration for selecting a specific syntactic
                category *)
            type ('ast, 'ext_ast) t

            (** Given a syntactic category, get the module for manipulating
                language extensions of it *)
            val ast_module :
              ('ast, 'ext_ast) t ->
              (module AST with type ast = 'ast)

            (** Given a syntactic category and a language extension, get the
                [ast_extension] for lifting and lowering its terms if it's
                available. *)
            val ast_extension :
              ('ast, 'ext_ast) t ->
              Clflags.Extension.t ->
              ('ast, 'ext_ast) optional_ast_extension
          end) : sig
  (** Interpret an AST term in the specified syntactic category as a term of the
      appropriate auxiliary language extension AST if possible.  Raises an error
      if the extension it finds is disabled or if the language extension
      embedding is malformed.  *)
  val extension_ast_of_ast :
    ('ast, 'ext_ast) Syntactic_category.t ->
    'ast ->
    'ext_ast option

  (* CR aspectorzabusky: Is this really the right API, with the extension
     specified twice?  There's a bit of redundancy, but I don't see how to
     alleviate it without making uses of [Expression.make_extension] bulkier,
     and that's right out. *)
  (** Interpret an auxiliary extended language extension AST term from the
      specified extension as a term of the appropriate OCaml AST.  Raises an
      error if the language extension is disabled.  The language extension
      specified *must* correspond to the constructor of ['ext_ast] and provide
      support for the specified syntactic category, or this function will raise
      a fatal error or an exception (respectively); these should both be avoided
      statically, as the relevant values are expected to be literals. *)
  val ast_of_extension_ast :
    ('ast, 'ext_ast) Syntactic_category.t ->
    loc:Location.t ->
    Clflags.Extension.t ->
    'ext_ast ->
    'ast
end

(** Require that an extension is enabled, or else throw an exception (of an
    unexported type) at the provided location saying otherwise.  This is
    intended to be used in "extensions.ml" when a certain piece of syntax
    requires two extensions to be enabled at once (e.g., immutable array
    comprehensions such as [[:x for x = 1 to 10:]]). *)
val assert_extension_enabled : loc:Location.t -> Clflags.Extension.t -> unit
