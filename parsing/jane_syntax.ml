open Asttypes
open Parsetree
open Jane_syntax_parsing

(** We carefully regulate which bindings we import from [Language_extension]
    to ensure that we can import this file into the Jane Street internal
    repo with no changes.
*)
module Language_extension = struct
  include Language_extension_kernel

  include (
    Language_extension :
      Language_extension_kernel.Language_extension_for_jane_syntax)
end

(* Suppress the unused module warning so it's easy to keep around the
   shadowing even if we delete use sites of the module. *)
module _ = Language_extension

(****************************************)
(* Helpers used just within this module *)

module type Extension = sig
  val feature : Feature.t
end

module Ast_of (AST : AST) (Ext : Extension) : sig
  (* Wrap a bit of AST with a jane-syntax annotation *)
  val wrap_jane_syntax :
    string list ->
    (* these strings describe the bit of new syntax *)
    ?payload:payload ->
    AST.ast ->
    AST.ast
end = struct
  let wrap_jane_syntax suffixes ?payload to_be_wrapped =
    AST.make_jane_syntax Ext.feature suffixes ?payload to_be_wrapped
end

module Of_ast (Ext : Extension) : sig
  type unwrapped := string list * payload * attributes

  (* Find and remove a jane-syntax attribute marker, throwing an exception
     if the attribute name does not have the right format or extension. *)
  val unwrap_jane_syntax_attributes_exn :
    loc:Location.t -> attributes -> unwrapped
end = struct
  let extension_string = Feature.extension_component Ext.feature

  module Desugaring_error = struct
    type error =
      | Not_this_embedding of Embedded_name.t
      | Non_embedding

    let report_error ~loc = function
      | Not_this_embedding name ->
        Location.errorf ~loc
          "Tried to desugar the embedded term %a@ as belonging to the %s \
           extension"
          Embedded_name.pp_quoted_name name extension_string
      | Non_embedding ->
        Location.errorf ~loc
          "Tried to desugar a non-embedded expression@ as belonging to the %s \
           extension"
          extension_string

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn (function
        | Error (loc, err) -> Some (report_error ~loc err)
        | _ -> None)

    let raise ~loc err = raise (Error (loc, err))
  end

  let unwrap_jane_syntax_attributes attrs : (_, Desugaring_error.error) result =
    match find_and_remove_jane_syntax_attribute attrs with
    | Some (ext_name, _loc, payload, attrs) -> (
      match Jane_syntax_parsing.Embedded_name.components ext_name with
      | extension_occur :: names
        when String.equal extension_occur extension_string ->
        Ok (names, payload, attrs)
      | _ -> Error (Not_this_embedding ext_name))
    | None -> Error Non_embedding

  let unwrap_jane_syntax_attributes_exn ~loc attrs =
    match unwrap_jane_syntax_attributes attrs with
    | Ok x -> x
    | Error error -> Desugaring_error.raise ~loc error
end

(******************************************************************************)
(** Individual language extension modules *)

(* Note [Check for immutable extension in comprehensions code]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   When we spot a comprehension for an immutable array, we need to make sure
   that both [comprehensions] and [immutable_arrays] are enabled.  But our
   general mechanism for checking for enabled extensions (in [of_ast]) won't
   work well here: it triggers when converting from
   e.g. [[%jane.non_erasable.comprehensions.array] ...] to the
   comprehensions-specific AST. But if we spot a
   [[%jane.non_erasable.comprehensions.immutable]], there is no expression to
   translate. So we just check for the immutable arrays extension when
   processing a comprehension expression for an immutable array.

   Note [Wrapping with make_entire_jane_syntax]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The topmost node in the encoded AST must always look like e.g.
   [%jane.non_erasable.comprehensions]. (More generally,
   [%jane.ERASABILITY.FEATURE] or [@jane.ERASABILITY.FEATURE].) This allows the
   decoding machinery to know what extension is being used and what function to
   call to do the decoding. Accordingly, during encoding, after doing the hard
   work of converting the extension syntax tree into e.g. Parsetree.expression,
   we need to make a final step of wrapping the result in a [%jane.*.xyz] node.
   Ideally, this step would be done by part of our general structure, like we
   separate [of_ast] and [of_ast_internal] in the decode structure; this design
   would make it structurally impossible/hard to forget taking this final step.

   However, the final step is only one line of code (a call to
   [make_entire_jane_syntax]), but yet the name of the feature varies, as does
   the type of the payload. It would thus take several lines of code to execute
   this command otherwise, along with dozens of lines to create the structure in
   the first place. And so instead we just manually call
   [make_entire_jane_syntax] and refer to this Note as a reminder to authors of
   future syntax features to remember to do this wrapping.

   Note [Outer attributes at end]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The order of attributes matters for several reasons:
   - If the user writes attributes on a Jane Street OCaml construct, where
     should those appear with respect to the Jane Syntax attribute that
     introduces the construct?
   - Some Jane Syntax embeddings use attributes, and sometimes an AST node will
     have multiple Jane Syntax-related attributes on it. Which attribute should
     Jane Syntax interpret first?

   Both of these questions are settled by a convention where attributes
   appearing later in an attribute list are considered to be "outer" to
   attributes appearing earlier. (ppxlib adopted this convention, and thus we
   need to as well for compatibility.)

   - User-written attributes appear later in the attribute list than
     a Jane Syntax attribute that introduces a syntactic construct.
   - If multiple Jane Syntax attributes appear on an AST node, the ones
     appearing later in the attribute list should be interpreted first.
*)

module Arrow_curry = struct
  let curry_attr_name = "extension.curry"

  let curry_attr loc =
    Ast_helper.Attr.mk ~loc:Location.none
      (Location.mkloc curry_attr_name loc)
      (PStr [])
end

(** List and array comprehensions *)
module Comprehensions = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Comprehensions
  end

  module Ast_of = Ast_of (Expression) (Ext)
  module Of_ast = Of_ast (Ext)
  include Ext

  type iterator =
    | Range of
        { start : expression;
          stop : expression;
          direction : direction_flag
        }
    | In of expression

  type clause_binding =
    { pattern : pattern;
      iterator : iterator;
      attributes : attribute list
    }

  type clause =
    | For of clause_binding list
    | When of expression

  type comprehension =
    { body : expression;
      clauses : clause list
    }

  type expression =
    | Cexp_list_comprehension of comprehension
    | Cexp_array_comprehension of mutable_flag * comprehension

  (* The desugared-to-OCaml version of comprehensions is described by the
     following BNF, where [{% '...' | expr %}] refers to the result of
     [Expression.make_jane_syntax] (via [comprehension_expr]) as described at
     the top of [jane_syntax_parsing.mli].

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

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in
      [expr_of_comprehension_expr]. *)

  let expr_of_iterator = function
    | Range { start; stop; direction } ->
      Ast_of.wrap_jane_syntax
        [ "for";
          "range";
          (match direction with Upto -> "upto" | Downto -> "downto") ]
        (Ast_helper.Exp.tuple [None, start; None, stop])
    | In seq -> Ast_of.wrap_jane_syntax ["for"; "in"] seq

  let expr_of_clause_binding { pattern; iterator; attributes } =
    Ast_helper.Vb.mk ~attrs:attributes pattern (expr_of_iterator iterator)

  let expr_of_clause clause rest =
    match clause with
    | For iterators ->
      Ast_of.wrap_jane_syntax ["for"]
        (Ast_helper.Exp.let_ Nonrecursive
           (List.map expr_of_clause_binding iterators)
           rest)
    | When cond ->
      Ast_of.wrap_jane_syntax ["when"] (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~type_ { body; clauses } =
    (* We elect to wrap the body in a new AST node (here, [Pexp_lazy])
       because it makes it so there is no AST node that can carry multiple Jane
       Syntax-related attributes in addition to user-written attributes. This
       choice simplifies the definition of [comprehension_expr_of_expr], as
       part of its contract is threading through the user-written attributes
       on the outermost node.
    *)
    Ast_of.wrap_jane_syntax type_
      (Ast_helper.Exp.lazy_
         (List.fold_right expr_of_clause clauses
            (Ast_of.wrap_jane_syntax ["body"] body)))

  let expr_of ~loc cexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
        match cexpr with
        | Cexp_list_comprehension comp ->
          expr_of_comprehension ~type_:["list"] comp
        | Cexp_array_comprehension (amut, comp) ->
          expr_of_comprehension
            ~type_:
              [ "array";
                (match amut with
                | Mutable -> "mutable"
                | Immutable -> "immutable") ]
            comp)

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in
      [comprehension_expr_of_expr]. *)

  module Desugaring_error = struct
    type error =
      | Has_payload of payload
      | Bad_comprehension_embedding of string list
      | No_clauses

    let report_error ~loc = function
      | Has_payload payload ->
        Location.errorf ~loc
          "Comprehensions attribute has an unexpected payload:@;%a"
          (Printast.payload 0) payload
      | Bad_comprehension_embedding subparts ->
        Location.errorf ~loc
          "Unknown, unexpected, or malformed@ comprehension embedded term %a"
          Embedded_name.pp_quoted_name
          (Embedded_name.of_feature feature subparts)
      | No_clauses ->
        Location.errorf ~loc "Tried to desugar a comprehension with no clauses"

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn (function
        | Error (loc, err) -> Some (report_error ~loc err)
        | _ -> None)

    let raise expr err = raise (Error (expr.pexp_loc, err))
  end

  (* Returns the expression node with the outermost Jane Syntax-related
     attribute removed. *)
  let expand_comprehension_extension_expr expr =
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes_exn ~loc:expr.pexp_loc
        expr.pexp_attributes
    in
    match payload with
    | PStr [] -> names, { expr with pexp_attributes = attributes }
    | _ -> Desugaring_error.raise expr (Has_payload payload)

  let iterator_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ( ["for"; "range"; "upto"],
        { pexp_desc = Pexp_tuple [(None, start); (None, stop)]; _ } ) ->
      Range { start; stop; direction = Upto }
    | ( ["for"; "range"; "downto"],
        { pexp_desc = Pexp_tuple [(None, start); (None, stop)]; _ } ) ->
      Range { start; stop; direction = Downto }
    | ["for"; "in"], seq -> In seq
    | bad, _ -> Desugaring_error.raise expr (Bad_comprehension_embedding bad)

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat;
      iterator = iterator_of_expr pvb_expr;
      attributes = pvb_attributes
    }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let comprehension_of_expr =
    let rec raw_comprehension_of_expr expr =
      match expand_comprehension_extension_expr expr with
      | ["for"], { pexp_desc = Pexp_let (Nonrecursive, iterators, rest); _ } ->
        add_clause
          (For (List.map clause_binding_of_vb iterators))
          (raw_comprehension_of_expr rest)
      | ["when"], { pexp_desc = Pexp_sequence (cond, rest); _ } ->
        add_clause (When cond) (raw_comprehension_of_expr rest)
      | ["body"], body -> { body; clauses = [] }
      | bad, _ -> Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    fun expr ->
      match raw_comprehension_of_expr expr with
      | { body = _; clauses = [] } -> Desugaring_error.raise expr No_clauses
      | comp -> comp

  (* Returns remaining unconsumed attributes on outermost expression *)
  let comprehension_expr_of_expr expr =
    let name, wrapper = expand_comprehension_extension_expr expr in
    let comp =
      match name, wrapper.pexp_desc with
      | ["list"], Pexp_lazy comp ->
        Cexp_list_comprehension (comprehension_of_expr comp)
      | ["array"; "mutable"], Pexp_lazy comp ->
        Cexp_array_comprehension (Mutable, comprehension_of_expr comp)
      | ["array"; "immutable"], Pexp_lazy comp ->
        (* assert_extension_enabled:
           See Note [Check for immutable extension in comprehensions code] *)
        assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays ();
        Cexp_array_comprehension (Immutable, comprehension_of_expr comp)
      | bad, _ -> Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    comp, wrapper.pexp_attributes
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression = Iaexp_immutable_array of expression list

  type nonrec pattern = Iapat_immutable_array of pattern list

  let feature : Feature.t = Language_extension Immutable_arrays

  let expr_of ~loc = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Exp.array elts)

  (* Returns remaining unconsumed attributes *)
  let of_expr expr =
    match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts, expr.pexp_attributes
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Pat.array elts)

  (* Returns remaining unconsumed attributes *)
  let of_pat pat =
    match pat.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts, pat.ppat_attributes
    | _ -> failwith "Malformed immutable array pattern"
end

(** Module strengthening *)
module Strengthen = struct
  type nonrec module_type =
    { mty : Parsetree.module_type;
      mod_id : Longident.t Location.loc
    }

  let feature : Feature.t = Language_extension Module_strengthening

  (* Encoding: [S with M] becomes [functor (_ : S) -> (module M)], where
     the [(module M)] is a [Pmty_alias].  This isn't syntax we can write, but
     [(module M)] can be the inferred type for [M], so this should be fine. *)

  let mty_of ~loc { mty; mod_id } =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Module_type.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Mty.functor_
          (Named (Location.mknoloc None, mty))
          (Ast_helper.Mty.alias mod_id))

  (* Returns remaining unconsumed attributes *)
  let of_mty mty =
    match mty.pmty_desc with
    | Pmty_functor (Named (_, mty), { pmty_desc = Pmty_alias mod_id }) ->
      { mty; mod_id }, mty.pmty_attributes
    | _ -> failwith "Malformed strengthened module type"
end

module Instances = struct
  type instance =
    { head : string;
      args : (string * instance) list
    }

  type module_expr = Imod_instance of instance

  let feature : Feature.t = Language_extension Instances

  let module_expr_of_string ~loc str =
    Ast_helper.Mod.ident ~loc { txt = Lident str; loc }

  let rec module_expr_of_instance ~loc { head; args } =
    let head = module_expr_of_string ~loc head in
    match args with
    | [] -> head
    | _ ->
      let args =
        List.concat_map
          (fun (param, value) ->
            let param = module_expr_of_string ~loc param in
            let value = module_expr_of_instance ~loc value in
            [param; value])
          args
      in
      List.fold_left (Ast_helper.Mod.apply ~loc) head args

  let module_expr_of ~loc = function
    | Imod_instance instance ->
      Module_expr.make_entire_jane_syntax ~loc feature (fun () ->
          module_expr_of_instance ~loc instance)

  let head_of_ident (lid : Longident.t Location.loc) =
    match lid with
    | { txt = Lident s; loc = _ } -> s
    | _ -> failwith "Malformed instance identifier"

  let gather_args mexpr =
    let rec loop mexpr rev_acc =
      match mexpr.pmod_desc with
      | Pmod_apply (f, v) -> (
        match f.pmod_desc with
        | Pmod_apply (f, n) -> loop f ((n, v) :: rev_acc)
        | _ -> failwith "Malformed instance identifier")
      | head -> head, List.rev rev_acc
    in
    loop mexpr []

  let string_of_module_expr mexpr =
    match mexpr.pmod_desc with
    | Pmod_ident i -> head_of_ident i
    | _ -> failwith "Malformed instance identifier"

  let rec instance_of_module_expr mexpr =
    match gather_args mexpr with
    | Pmod_ident i, args ->
      let head = head_of_ident i in
      let args = List.map instances_of_arg_pair args in
      { head; args }
    | _ -> failwith "Malformed instance identifier"

  and instances_of_arg_pair (n, v) =
    string_of_module_expr n, instance_of_module_expr v

  let of_module_expr mexpr = Imod_instance (instance_of_module_expr mexpr)
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t

  type ast

  val of_ast : ast -> t option
end

module Expression = struct
  type t =
    | Jexp_comprehension of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression

  let of_ast_internal (feat : Feature.t) expr =
    match feat with
    | Language_extension Comprehensions ->
      let expr, attrs = Comprehensions.comprehension_expr_of_expr expr in
      Some (Jexp_comprehension expr, attrs)
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_expr expr in
      Some (Jexp_immutable_array expr, attrs)
    | _ -> None

  let of_ast = Expression.make_of_ast ~of_ast_internal

  let expr_of ~loc ~attrs t =
    let expr =
      match t with
      | Jexp_comprehension x -> Comprehensions.expr_of ~loc x
      | Jexp_immutable_array x -> Immutable_arrays.expr_of ~loc x
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> expr
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { expr with pexp_attributes = expr.pexp_attributes @ attrs }
end

module Pattern = struct
  type t = Jpat_immutable_array of Immutable_arrays.pattern

  let of_ast_internal (feat : Feature.t) pat =
    match feat with
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_pat pat in
      Some (Jpat_immutable_array expr, attrs)
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let pat_of ~loc ~attrs t =
    let pat =
      match t with Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc x
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> pat
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { pat with ppat_attributes = pat.ppat_attributes @ attrs }
end

module Module_type = struct
  type t = Jmty_strengthen of Strengthen.module_type

  let of_ast_internal (feat : Feature.t) mty =
    match feat with
    | Language_extension Module_strengthening ->
      let mty, attrs = Strengthen.of_mty mty in
      Some (Jmty_strengthen mty, attrs)
    | _ -> None

  let of_ast = Module_type.make_of_ast ~of_ast_internal

  let mty_of ~loc ~attrs t =
    let mty = match t with Jmty_strengthen x -> Strengthen.mty_of ~loc x in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> mty
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { mty with pmty_attributes = mty.pmty_attributes @ attrs }
end

module Module_expr = struct
  type t = Emod_instance of Instances.module_expr

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Instances ->
      Some (Emod_instance (Instances.of_module_expr sigi))
    | _ -> None

  let of_ast = Module_expr.make_of_ast ~of_ast_internal
end
