(* CR aspectorzabusky: I wrote this file before other extensions existed, and
   they grew separately from the approach here.  This is particularly true
   because my extension is the only one that adds a big lump of new syntax.
   This means it's not trivial to unify the approach taken in this file and the
   approach generally taken by extensions.  I discuss this at length in the PR
   (in the section "One other issue with representing language extensions in the
   AST").  Thoughts? *)

(** This module handles the logic around the syntax of our extensions to OCaml
    for [ocaml-jst], keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make sure
       things like ppxen are cross-compatible between upstream and [ocaml-jst].
       Thankfully, OCaml already provides places to add extra syntax: extension
       points and annotations!  Thus, we have to come up with a way of representing
       our new syntactic constructs in terms of extension points (or annotations,
       but we went with the former).

    2. We don't want to actually match on extension points whose names are specific
       strings all over the compiler; that's incredibly messy, and it's easy to miss
       cases, etc.

    3. We want to keep different language extensions distinct so that we can add
       them to upstream independently, work on them separately, and so on.

    We have come up with a design that addresses those concerns by providing both a
    nice compiler-level interface for working with our syntactic extensions as
    first-class AST nodes, as well as a uniform scheme for translating this to and
    from [Parsetree.expression] values containing extension points.

    a. For each language extension, we define a module (e.g., [Comprehensions]), in
       which define a proper AST type (e.g., [Comprehensions.comprehension_expr] and
       its subcomponents).  This addresses concern (3); we've now contained each
       extension in a module.  But just that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* that's just for our language extensions,
       [extension_expr]; it contains one constructor for each of the AST types
       defined as described in design point (1).  This addresses concern (2); we can
       now match on actual OCaml constructors, as long as we can get ahold of them.
       And to do that…

    c. We define a general scheme for how we represent language extensions in terms
       of the existing AST, and provide a few primitives for consuming/creating AST
       nodes of this form.  There's not a lot of abstraction to be done, or at least
       it's not (yet) apparent what abstraction there is to do, so most of this
       remains manual.  (Setting up a full lens-based/otherwise bidirectional
       approach sounds like a great opportunity for yak-shaving, but not *actually*
       a good idea.)  This solves concern (3), and by doing it uniformly helps us
       address multiple cases at one stroke.

    We then bundle this all up for each individual extension into the type
    [extension] (defined towards the end of this file) containing two different
    (partial) isomorphisms: the fully isomorphic (up to exceptions) ability to
    lift and lower between the custom AST type (from design point (a)) and
    existing AST expressions, leveraging the common format for representing
    thins in the existing AST from design point (c); and the partial ability to
    lift and lower between the custom AST type and our overall auxiliary AST
    type (from design point (b)), which is just a constructor application in one
    direction and a pattern match against a constructor in the other.  This type
    is an existential type that hides the extension-specific type, allowing us
    to collect all of our extensions together.

    As mentioned, there are some gory details: in particular, the specific
    translation scheme we adopt for moving to and from
    [Parsetree.expression]. of gory details, we adopt the following scheme for
    representing our language extensions in the existing AST: all of our
    language extensions are to be rendered as applications of extension nodes to
    another expression.  In particular, for a given extension named [EXTNAME]
    (i.e., one that is enabled by [-extension EXTNAME] on the command line), any
    syntax it introduces ought to be desugared as [([%extension.EXTNAME] EXPR)]
    for some [EXPR].  We also provide utilities for further desugaring similar
    applications where the extension nodes have the longer form
    [[%extension.EXTNAME.ID1.ID2.….IDn]] (with the outermost one being the [n =
    0] case); these might be used inside the [EXPR].  (For example, within the
    outermost [[%extension.comprehensions]] application, we also have
    [[%extension.comprehensions.list]], [[%extension.comprehensions.array]],
    [[%extensions.comprehensions.for.in]], etc.)  We don't use the extension
    node payload so that ppxen can see inside these extension nodes; if we put
    the subexpressions inside the extension node payload, then we couldn't write
    something like [[[%string "Hello, %{x}!"] for x in names]], as [ppx_string]
    wouldn't traverse inside the payload to find the [[%string]] extension
    point.  Language extensions are of course allowed to impose constraints on
    what the contained expression is; we're also happy for this to error in
    various ways on malformed input, since the nobody should ever be writing
    these forms directly.  They're just an implementation detail.

    Within this module, we provide some simple machinery for working with these
    [([%extension.EXTNAME.ID1.ID2.….IDn] EXPR)] wrapped forms.  To construct
    one, we provide [extension_expr]; to destructure one, we provide
    [expand_extension_expr].  We still have to write the transformations in both
    directions for all new syntax, lowering it to extension nodes and then
    (somewhat more obnoxiously) lifting it back out.

    (We often want to specify what our syntax extensions look like when
    desugared into OCaml ASTs, so that we can validate the translation code.  We
    generally specify this as a BNF grammar, but we don't want to depend on the
    specific details of the desugaring.  Thus, instead of writing out extension
    points or attributes directly, we write the result of [extension_expr ~loc
    [name1; name2; ...; NameN] expr] as [{% 'name1.name2.....nameN' | expr %}]
    in the BNF.  Other pieces of the OCaml AST are used as normal. *)

open Parsetree

type malformed_extension =
  | Has_payload of payload
  | Wrong_arguments of (Asttypes.arg_label * expression) list

type error =
  | Malformed_extension of string list * malformed_extension
  | Unknown_extension of string
  | Disabled_extension of Clflags.Extension.t
  | Unnamed_extension
  | Bad_introduction of string * string list

exception Error of Location.t * error

let report_error ~loc = function
  | Malformed_extension(name, malformed) -> begin
      let name = String.concat "." ("extension" :: name) in
      match malformed with
      | Has_payload _payload ->
          Location.errorf
            ~loc
            "Extension extension nodes are not allowed to have a payload, \
             but \"%s\" does"
          name
      | Wrong_arguments arguments ->
          Location.errorf
            ~loc
            "Extension extension nodes must be applied to exactly one \
             unlabeled argument, but \"%s\" was applied to %s"
            name
            (match arguments with
             | [Labelled _, _] -> "a labeled argument"
             | [Optional _, _] -> "an optional argument"
             | _ -> Int.to_string (List.length arguments) ^ " arguments")
    end
  | Unknown_extension name ->
      Location.errorf
        ~loc
        "Unknown extension \"%s\" referenced via an [%%extension.%s] \
         extension node"
        name
        name
  | Disabled_extension ext ->
      Location.errorf
        ~loc
        "The extension \"%s\" is disabled and cannot be used"
        (Clflags.Extension.to_string ext)
  | Unnamed_extension ->
      Location.errorf
        ~loc
        "Cannot have an extension node named [%%extension]"
  | Bad_introduction(name, subnames) ->
      Location.errorf
        ~loc
        "The extension \"%s\" was referenced improperly; it started with an \
         [%%extension.%s] extension node, not an [%%extension.%s] one"
        name
        (String.concat "." (name :: subnames))
        name

let () =
  Location.register_error_of_exn
    (function
      | Error(loc, err) -> Some (report_error ~loc err)
      | _ -> None)

let extension_tag ~loc names =
  Ast_helper.Exp.extension
    ~loc
    ({ txt = String.concat "." ("extension" :: names); loc }, PStr [])

let extension_expr ~loc names expr =
  Ast_helper.Exp.apply ~loc (extension_tag ~loc names) [Nolabel, expr]

(* Some extensions written before this file existed are handled in their own
   way; this function filters them out. *)
let uniformly_handled_extension names =
  match names with
  | [("local"|"global"|"nonlocal"|"escape"|"include_functor"|"curry")] -> false
  | _ -> true

(** Matches expressions of the form

        ([%extension.NAME] BODY)

    and returns [Some (NAME, BODY)] if successful, where NAME is a list of
    dot-separated name components.  If the expression is not an application
    headed by an extension node whose name begins with ["extension."], returns
    [None].  If the expression is a malformed extension application, raises an
    [Error].  Malformed means either:

    1. The [[%extension.NAME]] extension point has a payload; extensions must be
       empty, so other ppxes can traverse "into" them.

    2. The [[%extension.NAME]] extension point is applied to something other
       than a single unlabeled argument.

    The second requirement may be loosened in the future. *)
let expand_extension_expr expr =
  match expr.pexp_desc with
  | Pexp_apply
      ( { pexp_desc =
            Pexp_extension
              ( { txt = ext_name; loc = ext_loc }
              , ext_payload )
        ; _ }
      , arguments ) ->
    begin
      match String.split_on_char '.' ext_name with
      | "extension" :: names when uniformly_handled_extension names -> begin
          let raise_malformed err =
            raise (Error(ext_loc, Malformed_extension(names, err)))
          in
          match ext_payload with
          | PStr [] -> begin
              match arguments with
              | [Nolabel, body] -> Some (names, body)
              | _ -> raise_malformed (Wrong_arguments arguments)
            end
          | _ -> raise_malformed (Has_payload ext_payload)
        end
      | _ -> None
    end
  | _ -> None

module Comprehensions = struct
  type iterator =
    | Range of { start     : expression
               ; stop      : expression
               ; direction : Asttypes.direction_flag }
        (** "= START to STOP" (direction = Upto)
            "= START downto STOP" (direction = Downto) *)
    | In of expression
       (** "in EXPR" *)

  type clause_binding =
    { pattern    : pattern
    ; iterator   : iterator
    ; attributes : attribute list }
      (** PAT (in/= ...) [@...] *)

  type clause =
    | For of clause_binding list
        (** "for PAT (in/= ...) and PAT (in/= ...) and ..."; must be nonempty *)
    | When of expression
        (** "when EXPR" *)

  type comprehension =
    { body    : expression
        (** The body/generator of the comprehension *)
    ; clauses : clause list
        (** The clauses of the comprehension; must be nonempty *) }

  type comprehension_expr =
    | Cexp_list_comprehension  of comprehension
        (** [BODY ...CLAUSES...] *)
    | Cexp_array_comprehension of comprehension
        (** [|BODY ...CLAUSES...|] *)

  (* The desugared-to-OCaml version of comprehensions is described by the
     following BNF, where [{% '...' | expr %}] refers to the result of
     [extension_expr] as described at the start of this file.

     {v
         comprehension ::=
           | {% 'comprehension.list' | '[' clauses ']' %}
           | {% 'comprehension.array' | '[|' clauses '|]' %}

         clauses ::=
           | {% 'comprehension.for' | 'let' iterator+ 'in' clauses %}
           | {% 'comprehension.when' | expr ';' clauses %}
           | {% 'comprehension.body' | expr %}

         iterator ::=
           | pattern '=' {% 'comprehension.for.range.upto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.range.downto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.in' | expr %}
     v}
  *)

  let extension_name = Clflags.Extension.to_string Comprehensions

  let comprehension_expr ~loc names =
    extension_expr ~loc (extension_name :: names)

  let expr_of_iterator ~loc = function
    | Range { start; stop; direction } ->
        comprehension_expr
          ~loc
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        comprehension_expr ~loc ["for"; "in"] seq

  let expr_of_clause_binding ~loc { pattern; iterator; attributes } =
    Ast_helper.Vb.mk
      ~loc
      ~attrs:attributes
      pattern
      (expr_of_iterator ~loc iterator)

  let expr_of_clause ~loc clause rest = match clause with
    | For iterators ->
        comprehension_expr
          ~loc
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive
             (List.map (expr_of_clause_binding ~loc) iterators)
             rest)
    | When cond ->
        comprehension_expr
          ~loc
          ["when"]
          (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~loc ~type_ { body; clauses } =
    comprehension_expr
      ~loc
      [type_]
      (List.fold_right
         (expr_of_clause ~loc)
         clauses
         (comprehension_expr ~loc ["body"] body))

  let expr_of_comprehension_expr ~loc eexpr =
    let ghost_loc = { loc with Location.loc_ghost = true } in
    let expr_of_comprehension_type type_ =
      expr_of_comprehension ~loc:ghost_loc ~type_
    in
    match eexpr with
    | Cexp_list_comprehension  comp -> expr_of_comprehension_type "list"  comp
    | Cexp_array_comprehension comp -> expr_of_comprehension_type "array" comp

  module Desugaring_error = struct
    type error =
      | Non_comprehension_extension_point of string list
      | Non_extension
      | Bad_comprehension_extension_point of string list
      | No_clauses

    let report_error ~loc = function
      | Non_comprehension_extension_point name ->
          Location.errorf ~loc
            "Tried to desugar the non-comprehension extension point \
             \"extension.%s\" as part of a comprehension expression"
            (String.concat "." name)
      | Non_extension ->
          Location.errorf ~loc
            "Tried to desugar a non-extension expression as part of a \
             comprehension expression"
      | Bad_comprehension_extension_point name ->
          Location.errorf ~loc
            "Unknown, unexpected, or malformed comprehension extension point \
             \"extension.comprehension.%s\""
            (String.concat "." name)
      | No_clauses ->
          Location.errorf ~loc
            "Tried to desugar a comprehension with no clauses"

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise expr err = raise (Error(expr.pexp_loc, err))
  end

  let expand_comprehension_extension_expr expr =
    match expand_extension_expr expr with
    | Some (comprehensions :: name, expr)
      when String.equal comprehensions extension_name ->
        name, expr
    | Some (name, _) ->
        Desugaring_error.raise expr (Non_comprehension_extension_point name)
    | None ->
        Desugaring_error.raise expr Non_extension

  let iterator_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["for"; "range"; "upto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
        Range { start; stop; direction = Upto }
    | ["for"; "range"; "downto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
        Range { start; stop; direction = Downto }
    | ["for"; "in"], seq ->
        In seq
    | bad, _ ->
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let rec raw_comprehension_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["for"], { pexp_desc = Pexp_let(Nonrecursive, iterators, rest); _ } ->
        add_clause
          (For (List.map clause_binding_of_vb iterators))
          (raw_comprehension_of_expr rest)
    | ["when"], { pexp_desc = Pexp_sequence(cond, rest); _ } ->
        add_clause
          (When cond)
          (raw_comprehension_of_expr rest)
    | ["body"], body ->
        { body; clauses = [] }
    | bad, _ ->
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)

  let comprehension_of_expr expr =
    match raw_comprehension_of_expr expr with
    | { body = _; clauses = [] } ->
        Desugaring_error.raise expr No_clauses
    | comp -> comp

  let comprehension_expr_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["list"], comp ->
        Cexp_list_comprehension (comprehension_of_expr comp)
    | ["array"], comp ->
        Cexp_array_comprehension (comprehension_of_expr comp)
    | bad, _ ->
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)
end

type extension_expr =
  | Eexp_comprehension of Comprehensions.comprehension_expr

type extension =
  | Extension :
      { expr_of : loc:Location.t -> 'a -> Parsetree.expression
      ; of_expr : Parsetree.expression -> 'a
      ; wrap    : 'a -> extension_expr
      ; unwrap  : extension_expr -> 'a option }
    -> extension

let extension : Clflags.Extension.t -> extension = function
  | Comprehensions ->
      Extension { expr_of = Comprehensions.expr_of_comprehension_expr
                ; of_expr = Comprehensions.comprehension_expr_of_expr
                ; wrap    = (fun cexp -> Eexp_comprehension cexp)
                ; unwrap  = (fun (Eexp_comprehension cexp) -> Some cexp) }
  | (Local | Include_functor | Polymorphic_parameters) as ext ->
      (* CR aspectorzabusky: See the comment at the start of this file. *)
      Misc.fatal_errorf
        "The extension \"%s\" should be handled through its own mechanism, not \
         this uniform one."
        (Clflags.Extension.to_string ext)

let extension_expr_of_expr expr =
  let raise_error err = raise (Error (expr.pexp_loc, err)) in
  match expand_extension_expr expr with
  | None -> None
  | Some ([name], expr) -> begin
      match Clflags.Extension.of_string name with
      | Some ext ->
          if Clflags.Extension.is_enabled ext
          then
            let Extension ext = extension ext in
            Some (ext.wrap (ext.of_expr expr))
          else
            raise_error (Disabled_extension ext)
      | _ -> raise_error (Unknown_extension name)
    end
  | Some ([], _) ->
      raise_error Unnamed_extension
  | Some (name :: subnames, _) ->
      raise_error (Bad_introduction(name, subnames))

(* CR aspectorzabusky: Is this really the right API?  There's a bit of
   redundancy, but I don't see how to alleviate it without making uses of
   `extension_expr` bulkier, and that's right out.  It's also a little weird
   that this doesn't check if the extension is enabled; it used to -- the whole
   function was wrapped in
   {[
     if Clflags.Extension.is_enabled extn
     then
       ...
     else
       failwith
         (Printf.sprintf
            "The extension \"%s\" is not enabled\""
            (Clflags.Extension.to_string extn))
   ]}
   -- but we call this function from the parser, and the parser can't throw
   exceptions or it fails *terribly*.  I think this is okay, because we're
   guaranteed to check that when trying to desugar back in the other direction,
   but it's a little awkward. *)
let expr_of_extension_expr ~loc extn eexpr =
  let Extension ext = extension extn in
  match ext.unwrap eexpr with
  | Some eexpr' ->
     extension_expr
       ~loc
       [Clflags.Extension.to_string extn]
       (ext.expr_of ~loc eexpr')
  | None ->
      failwith
        (Printf.sprintf
           "Wrong extension expression for the extension \"%s\""
           (Clflags.Extension.to_string extn))
