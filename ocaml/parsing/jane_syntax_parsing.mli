(** This module handles the logic around the syntax of our extensions to OCaml
    for Jane Street, keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make
       sure things like ppxen are cross-compatible between upstream and our
       fork.  Thankfully, OCaml already provides places to add extra syntax:
       extension nodes and annotations!  Thus, we have to come up with a way of
       representing our new syntactic constructs in terms of these constructs.

    2. We don't want to actually match on extension nodes or attributes whose
       names are specific strings all over the compiler; that's incredibly
       messy, and it's easy to miss cases, etc.

    3. We want to keep our different novel syntactic features distinct so that
       we can add them to upstream independently, work on them separately, and
       so on.

    We have come up with a design that addresses those concerns by providing
    both a nice compiler-level interface for working with our syntactic
    extensions as first-class AST nodes, as well as a uniform scheme for
    translating this to and from OCaml AST values by using extension nodes or
    attributes.  One wrinkle is that OCaml has many ASTs, one for each syntactic
    category (expressions, patterns, etc.); we have to provide this facility for
    each syntactic category where we want to provide extensions.  A smaller
    wrinkle is that our novel syntactic features come in two varieties:
    *language extensions* (e.g., comprehensions) and *built-in features* (e.g.,
    syntactic function arity).  While the former can be disabled, the latter are
    parse tree changes we rely on (though they won't therefore show up in
    surface syntax).

    a. For each novel syntactic feature, we will define a module (e.g.,
       [Comprehensions]), in which we define a proper AST type per syntactic
       category we care about (e.g., [Comprehensions.expression] and its
       subcomponents).  This addresses concern (3); we've now contained each
       separate feature (and the built-in changes) in a module.  But just doing
       that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* for each syntactic category that's
       just for our novel syntactic features; for expressions, it's called
       [Jane_syntax.Expression.t].  It contains one constructor for each of the
       AST types defined as described in design point (1).  This addresses
       concern (2); we can now match on actual OCaml constructors, as long as we
       can get ahold of them.  And to do that…

    c. We define a general scheme for how we represent our novel syntactic
       features in terms of the existing ASTs, and provide a few primitives for
       consuming/creating AST nodes of this form, for each syntactic category.
       There's not a lot of abstraction to be done, or at least it's not (yet)
       apparent what abstraction there is to do, so most of this remains manual.
       (Setting up a full lens-based/otherwise bidirectional approach sounds
       like a great opportunity for yak-shaving, but not *actually* a good
       idea.)  This solves concern (3), and by doing it uniformly helps us
       address multiple cases at one stroke.

    Then, for each syntactic category, we define a module (in
    [jane_syntax_parsing.ml]) that contains functions for converting between the
    [Parsetree] representation and the higher-level representation. These
    modules are inhabitants of [AST.t], and the [AST] module exposes operations
    on them.

    This module contains the logic for moving to and from OCaml ASTs; the gory
    details of the encoding are detailed in the implementation.  All the actual
    ASTs should live in [Jane_syntax], which is the only module that should
    directly depend on this one.

    When using this module, we often want to specify what our syntax extensions
    look like when desugared into OCaml ASTs, so that we can validate the
    translation code.  We generally specify this as a BNF grammar, but we don't
    want to depend on the specific details of the desugaring.  Thus, instead of
    writing out extension nodes or attributes directly, we write the result of
    [Some_ast.make_extension ~loc [name1; name2; ...; NameN] a] as the special
    syntax [{% 'name1.name2.....nameN' | a %}] in the BNF.  Other pieces of the
    OCaml AST are used as normal.

    One detail which we hide as much as possible is locations: whenever
    constructing an OCaml AST node -- whether with [wrap_desc], the functions in
    [Ast_helper], or some other way -- the location should be left to be
    defaulted (and the default, [!Ast_helper.make_default], should be ghost).
    The [make_entire_jane_syntax] function will handle making sure this default
    location is set appropriately.  If this isn't done and any locations on
    subterms aren't marked as ghost, the compiler will work fine, but ppxlib may
    detect that you've violated its well-formedness constraints and fail to
    parse the resulting AST. *)

(******************************************************************************)

(** The type enumerating our novel syntactic features, which are either a
    language extension (separated out by which one) or the collection of all
    built-in features. *)
module Feature : sig
  type t =
    | Language_extension : _ Language_extension.t -> t
    | Builtin

  (** The component of an attribute or extension name that identifies the
      feature. This is the third component.
  *)
  val extension_component : t -> string
end

(** An AST-style representation of the names used when generating extension
    nodes or attributes for modular syntax.  We use this to abstract over the
    details of how they're encoded, so we have some flexibility in changing them
    (although comments may refer to the specific encoding choices).  This is
    also why we don't expose any functions for rendering or parsing these names;
    that's all handled internally. *)
module Embedded_name : sig
  (** A nonempty list of name components, without the first two components.
      (That is, without the leading root component that identifies it as part of
      the modular syntax mechanism, and without the next component that
      identifies the erasability.)

      This is a nonempty list corresponding to the different components of the
      name: first the feature, and then any subparts.
  *)
  type components = ( :: ) of string * string list

  type t

  (** Creates an embedded name whose erasability component is whether the
      feature is erasable, and whose feature component is the feature's name.
      The second argument is treated as the trailing components after the
      feature name.
  *)
  val of_feature : Feature.t -> string list -> t

  (** Extract the components from an embedded name; just includes the
      user-specified components, not the leading or erasability components, as
      with the [components] type. *)
  val components : t -> components

  (** Print out the embedded form of a Jane-syntax name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit
end

(** The collection of known "marker attributes".  These are Jane-syntax-style
    attributes, but exist outside of the full Jane syntax machinery; they can be
    added directly to syntax nodes, aren't matched on and turned into ASTs, and
    so on and so forth.  The format of the attribute name is not guaranteed to
    be stable across compiler versions, but it is guaranteed to be marked as
    erasable.

    See [Jane_syntax.Marker_attributes] for information on the specific marker
    attributes available here. *)
module Marker_attributes : sig
  (** The marker attribute for curried functions. *)
  val curry : string
end

(** Each syntactic category that contains novel syntactic features has a
    corresponding module of this module type.  We're adding these lazily as we
    need them. When you add another one, make sure also to add special handling
    in [Ast_iterator] and [Ast_mapper].

    This module type comes in two varieties: [AST_with_attributes] and
    [AST_without_attributes].  They reflect whether desugaring an OCaml AST into
    our extended one should ([with]) or shouldn't ([without]) return the
    attributes as well.  This choice is recorded in the [with_attributes]
    type.

    If you construct a value of an AST without adding an extra intervening node,
    then the locations will only be handled correctly if you are using one of
    the ASTs that has a [loc_stack]; for instance, in [local_ e], this will
    enable the Jane syntax machinery preserve the location of [e] itself, which
    would otherwise get overridden with the outer location. *)
module type AST = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** Embed a term from one of our novel syntactic features in the AST using the
      given name (in the [Feature.t]) and body (the [ast]).  Any locations in
      the generated AST will be set to [!Ast_helper.default_loc], which should
      be [ghost].  The list of components should be nonempty; if it's empty, you
      probably want [make_entire_jane_syntax] instead, and this function should
      be called within its callback.  If [payload] is specified, includes it in
      the embedding, where it can be recovered by later analysis.  The inverse
      of [match_jane_syntax] or, if the [payload] is specified,
      [match_payload_jane_syntax].

      For example, to embed the different terms in the [-extension local]
      expression AST, we write:

      {[
        let expr_of ~loc = function
          | Lexp_local expr ->
            (* See Note [Wrapping with make_entire_jane_syntax] *)
            Expression.make_entire_jane_syntax ~loc feature (fun () ->
              Expression.make_jane_syntax feature ["local"] expr)
          | Lexp_exclave expr ->
            (* See Note [Wrapping with make_entire_jane_syntax] *)
            Expression.make_entire_jane_syntax ~loc feature (fun () ->
              Expression.make_jane_syntax feature ["exclave"] expr)
      ]}
  *)
  val make_jane_syntax
    :  Feature.t
    -> string list
    -> ?payload:Parsetree.payload
    -> ast
    -> ast

  (** As [make_jane_syntax], but specifically for the AST node corresponding to
      the entire piece of novel syntax (e.g., for a list comprehension, the
      whole [[x for x in xs]], and not a subcomponent like [for x in xs]).  The
      provided location is used for the location of the resulting AST node.
      Additionally, [Ast_helper.default_loc] is set locally to the [ghost]
      version of that location, which is why the [ast] is generated from a
      function call; it is during this call that the location is so set.

      For example, to embed the single term in the [-extension immutable_arrays]
      expression AST, we write:

      {[
        let expr_of ~loc = function
          | Iaexp_immutable_array elts ->
            (* See Note [Wrapping with make_entire_jane_syntax] *)
            Expression.make_entire_jane_syntax ~loc feature (fun () ->
              Ast_helper.Exp.array elts)
      ]}

      This outermost node cannot carry extra payload information.

      For any usage where there are multiple possible terms to embed in the same
      syntactic category, you will also need to call [make_jane_syntax], which
      see. *)
  val make_entire_jane_syntax
    :  loc:Location.t
    -> Feature.t
    -> (unit -> ast)
    -> ast

  (** Given an embedding of a term from one of our novel syntactic features into
      the AST, extracts the name (a [Feature.t]) and AST of that embedding.
      This should only be used when the list of components will be nonempty; the
      outermost embedded term (as created by [make_entire_jane_syntax]) will be
      handled by [make_of_ast].  Will raise an exception if this wasn't an
      embedded term.  The inverse of [make_jane_syntax] when the [payload] was
      unspecified, raising an exception if it sees a payload; to extract the
      payload, see [match_payload_jane_syntax].

      This is usually followed by a match on the result to turn the result back
      into a node from the feature-specific AST; in the failing case, an error
      can be signaled with [raise_partial_match].

      For example, to distinguish between the different terms in the
      [-extension local] expression AST, we write:

      {[
        let of_expr expr =
          let expr, subparts = Expression.match_jane_syntax_piece feature expr in
          match subparts with
          | ["local"] -> Lexp_local expr
          | ["exclave"] -> Lexp_exclave expr
          | _ -> Expression.raise_partial_match feature expr subparts
      ]}
  *)
  val match_jane_syntax : Feature.t -> ast -> ast * string list

  (** As [make_jane_syntax], but extracts the embedded payload as well.  If you
      want not to use payloads, stick with [make_jane_syntax] instead. *)
  val match_payload_jane_syntax
    :  Feature.t
    -> ast
    -> ast * string list * Parsetree.payload

  (** Raise an error indicating that a [match_jane_syntax] call produced an
      invalid embedding for the specified feature. *)
  val raise_partial_match : Feature.t -> ast -> string list -> _

  (** As [raise_partial_match], but for [match_payload_jane_syntax]. *)
  val raise_partial_payload_match
    :  Feature.t
    -> ast
    -> string list
    -> Parsetree.payload
    -> _

  (** How to attach attributes to the result of [make_of_ast].  Will either
      return a pair (see [AST_with_attributes]) or will simply be equal to ['a]
      when there are no attributes ([AST_without_attributes]). *)
  type 'a with_attributes

  (** Build an [of_ast] function. The return value of this function should be
      used to implement [of_ast] in modules satisfying the signature
      [Jane_syntax.AST].

      The returned function interprets an AST term in the specified syntactic
      category as a term of the appropriate auxiliary extended AST if possible.
      It raises an error if it finds a term from a disabled extension or if the
      embedding is malformed.
  *)
  val make_of_ast
    :  of_ast_internal:(Feature.t -> ast -> 'a option)
    (** A function to convert [Parsetree]'s AST to our novel extended one.  The
        choice of feature and the piece of syntax will both be extracted from
        the embedding by the first argument.

        If the given syntax feature does not actually extend the given syntactic
        category, returns [None]; this will be reported as an error. (For
        example: There are no pattern comprehensions, so when building the
        extended pattern AST, this function will return [None] if it spots an
        embedding that claims to be from [Language_extension Comprehensions].)
    *)
    -> (ast -> 'a with_attributes option)
end

(** An [AST] that keeps track of attributes.  This also includes
    attribute-manipulating functions. *)
module type AST_with_attributes = sig
  include AST with type 'ast with_attributes := 'ast * Parsetree.attributes

  (** Add attributes to an AST term, appending them to the attributes already
      present. *)
  val add_attributes : Parsetree.attributes -> ast -> ast
end

(** An [AST] that does not keep track of attributes. *)
module type AST_without_attributes =
  AST with type 'ast with_attributes := 'ast

module Expression :
  AST_with_attributes with type ast = Parsetree.expression

module Pattern :
  AST_with_attributes with type ast = Parsetree.pattern

module Module_type :
  AST_with_attributes with type ast = Parsetree.module_type

module Signature_item :
  AST_without_attributes with type ast = Parsetree.signature_item

module Structure_item :
  AST_without_attributes with type ast = Parsetree.structure_item

module Core_type :
  AST_with_attributes with type ast = Parsetree.core_type

module Constructor_argument :
  AST_without_attributes with type ast = Parsetree.core_type

module Extension_constructor :
  AST_with_attributes with type ast = Parsetree.extension_constructor

module Constructor_declaration :
  AST_with_attributes with type ast = Parsetree.constructor_declaration

(** Require that an extension is enabled for at least the provided level, or
    else throw an exception (of an abstract type) at the provided location
    saying otherwise.  This is intended to be used in [jane_syntax.ml] when a
    certain piece of syntax requires two extensions to be enabled at once (e.g.,
    immutable array comprehensions such as [[:x for x = 1 to 10:]], which
    require both [Comprehensions] and [Immutable_arrays]). *)
val assert_extension_enabled :
  loc:Location.t -> 'a Language_extension.t -> 'a -> unit

(** Errors around the representation of our extended ASTs.  These should mostly
    just be fatal, but they're needed for one test case
    (language-extensions/language_extensions.ml). *)
module Error : sig
  (** An error triggered when desugaring a piece of embedded novel syntax from
      an OCaml AST; left abstract because it should always be fatal *)
  type error

  (** The exception type thrown when desugaring a piece of extended syntax from
      an OCaml AST *)
  exception Error of Location.t * error
end
