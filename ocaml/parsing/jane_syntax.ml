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

module type Payload_protocol = sig
  type t

  module Encode : sig
    val as_payload : t loc -> payload

    val list_as_payload : t loc list -> payload

    val option_list_as_payload : t loc option list -> payload
  end

  module Decode : sig
    val from_payload : loc:Location.t -> payload -> t loc

    val list_from_payload : loc:Location.t -> payload -> t loc list

    val option_list_from_payload :
      loc:Location.t -> payload -> t loc option list
  end
end

module type Structure_item_encodable = sig
  type t

  val of_structure_item : structure_item -> t loc option

  val to_structure_item : t loc -> structure_item

  (** For error messages: a name that can be used to identify the
      [t] being converted to and from string, and its indefinite
      article (either "a" or "an").
  *)
  val indefinite_article_and_name : string * string
end

module type Stringable = sig
  type t

  val of_string : string -> t option

  val to_string : t -> string

  (** For error messages: a name that can be used to identify the
      [t] being converted to and from string, and its indefinite
      article (either "a" or "an").
  *)
  val indefinite_article_and_name : string * string
end

module Make_structure_item_encodable_of_stringable (Stringable : Stringable) :
  Structure_item_encodable with type t = Stringable.t = struct
  include Stringable

  let to_structure_item t_loc =
    let string = Stringable.to_string t_loc.txt in
    let expr =
      Ast_helper.Exp.ident (Location.mkloc (Longident.Lident string) t_loc.loc)
    in
    { pstr_desc = Pstr_eval (expr, []); pstr_loc = Location.none }

  let of_structure_item = function
    | { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident payload_lid; _ }, _) }
      -> (
      match Stringable.of_string (Longident.last payload_lid.txt) with
      | Some t -> Some (Location.mkloc t payload_lid.loc)
      | None -> None)
    | _ -> None
end

module Make_payload_protocol_of_structure_item_encodable
    (Encodable : Structure_item_encodable) :
  Payload_protocol with type t := Encodable.t = struct
  module Encode = struct
    let structure_item_of_none =
      { pstr_desc =
          Pstr_attribute
            { attr_name = Location.mknoloc "jane.none";
              attr_payload = PStr [];
              attr_loc = Location.none
            };
        pstr_loc = Location.none
      }

    let as_payload t_loc = PStr [Encodable.to_structure_item t_loc]

    let list_as_payload t_locs =
      let items = List.map Encodable.to_structure_item t_locs in
      PStr items

    let option_list_as_payload t_locs =
      let items =
        List.map
          (function
            | None -> structure_item_of_none
            | Some t_loc -> Encodable.to_structure_item t_loc)
          t_locs
      in
      PStr items
  end

  module Desugaring_error = struct
    type error = Unknown_payload of payload

    let report_error ~loc = function
      | Unknown_payload payload ->
        let indefinite_article, name = Encodable.indefinite_article_and_name in
        Location.errorf ~loc "Attribute payload does not name %s %s:@;%a"
          indefinite_article name (Printast.payload 0) payload

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn (function
        | Error (loc, err) -> Some (report_error ~loc err)
        | _ -> None)

    let raise ~loc err = raise (Error (loc, err))
  end

  module Decode = struct
    (* Avoid exporting a definition that raises [Unexpected]. *)
    open struct
      exception Unexpected

      let is_none_structure_item = function
        | { pstr_desc = Pstr_attribute { attr_name = { txt = "jane.none" } } }
          ->
          true
        | _ -> false

      let from_structure_item item =
        match Encodable.of_structure_item item with
        | Some t_loc -> t_loc
        | None -> raise Unexpected

      let from_payload payload =
        match payload with
        | PStr [item] -> from_structure_item item
        | _ -> raise Unexpected

      let list_from_payload payload =
        match payload with
        | PStr items -> List.map (fun item -> from_structure_item item) items
        | _ -> raise Unexpected

      let option_list_from_payload payload =
        match payload with
        | PStr items ->
          List.map
            (fun item ->
              if is_none_structure_item item
              then None
              else Some (from_structure_item item))
            items
        | _ -> raise Unexpected
    end

    let from_payload ~loc payload : _ loc =
      try from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)

    let list_from_payload ~loc payload : _ list =
      try list_from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)

    let option_list_from_payload ~loc payload : _ list =
      try option_list_from_payload payload
      with Unexpected -> Desugaring_error.raise ~loc (Unknown_payload payload)
  end
end

module Make_payload_protocol_of_stringable (Stringable : Stringable) :
  Payload_protocol with type t := Stringable.t =
  Make_payload_protocol_of_structure_item_encodable
    (Make_structure_item_encodable_of_stringable (Stringable))

module Arrow_curry = struct
  let curry_attr_name = "extension.curry"

  let curry_attr loc =
    Ast_helper.Attr.mk ~loc:Location.none
      (Location.mkloc curry_attr_name loc)
      (PStr [])
end

(* only used for [Jkind] below *)
module Mode = struct
  module Protocol = Make_payload_protocol_of_stringable (struct
    type t = mode

    let indefinite_article_and_name = "a", "mode"

    let to_string (Mode s) = s

    let of_string' s = Mode s

    let of_string s = Some (of_string' s)
  end)

  let list_as_payload = Protocol.Encode.list_as_payload

  let list_from_payload = Protocol.Decode.list_from_payload
end

module Jkind = struct
  module Const : sig
    type t = Parsetree.jkind_const_annotation

    val mk : string -> Location.t -> t

    val of_structure_item : structure_item -> t option

    val to_structure_item : t -> structure_item
  end = struct
    type raw = string

    module Protocol = Make_structure_item_encodable_of_stringable (struct
      type t = raw

      let indefinite_article_and_name = "a", "primitive kind"

      let to_string t = t

      let of_string t = Some t
    end)

    type t = raw loc

    let mk txt loc : t = { txt; loc }

    let of_structure_item = Protocol.of_structure_item

    let to_structure_item = Protocol.to_structure_item
  end

  type t = Parsetree.jkind_annotation =
    | Default
    | Abbreviation of Const.t
    | Mod of t * modes
    | With of t * core_type
    | Kind_of of core_type
    | Product of t list

  type annotation = t loc

  let indefinite_article_and_name = "a", "kind"

  let prefix = "jane.erasable.layouts."

  let struct_item_of_attr attr =
    { pstr_desc = Pstr_attribute attr; pstr_loc = Location.none }

  let struct_item_to_attr item =
    match item with
    | { pstr_desc = Pstr_attribute attr; _ } -> Some attr
    | _ -> None

  let struct_item_of_type ty =
    { pstr_desc =
        Pstr_type
          (Recursive, [Ast_helper.Type.mk ~manifest:ty (Location.mknoloc "t")]);
      pstr_loc = Location.none
    }

  let struct_item_to_type item =
    match item with
    | { pstr_desc = Pstr_type (Recursive, [decl]); _ } -> decl.ptype_manifest
    | _ -> None

  let struct_item_of_list name list loc =
    struct_item_of_attr
      { attr_name = Location.mknoloc (prefix ^ name);
        attr_payload = PStr list;
        attr_loc = loc
      }

  let struct_item_to_list item =
    let strip_prefix s =
      let prefix_len = String.length prefix in
      String.sub s prefix_len (String.length s - prefix_len)
    in
    match item with
    | { pstr_desc =
          Pstr_attribute
            { attr_name = name; attr_payload = PStr list; attr_loc = loc };
        _
      }
      when String.starts_with ~prefix name.txt ->
      Some (strip_prefix name.txt, list, loc)
    | _ -> None

  let rec to_structure_item t_loc =
    let to_structure_item t = to_structure_item (Location.mknoloc t) in
    match t_loc.txt with
    | Default -> struct_item_of_list "default" [] t_loc.loc
    | Abbreviation c ->
      struct_item_of_list "abbrev" [Const.to_structure_item c] t_loc.loc
    | Mod (t, modes) ->
      let mode_list_item =
        struct_item_of_attr
          { attr_name = Location.mknoloc (prefix ^ "mod");
            attr_payload = Mode.list_as_payload modes;
            attr_loc = Location.none
          }
      in
      struct_item_of_list "mod" [to_structure_item t; mode_list_item] t_loc.loc
    | With (t, ty) ->
      struct_item_of_list "with"
        [to_structure_item t; struct_item_of_type ty]
        t_loc.loc
    | Kind_of ty ->
      struct_item_of_list "kind_of" [struct_item_of_type ty] t_loc.loc
    | Product ts ->
      struct_item_of_list "product" (List.map to_structure_item ts) t_loc.loc

  let rec of_structure_item item =
    let bind = Option.bind in
    let ret loc v = Some (Location.mkloc v loc) in
    match struct_item_to_list item with
    | Some ("default", [], loc) -> ret loc Default
    | Some ("mod", [item_of_t; item_of_mode_expr], loc) ->
      bind (of_structure_item item_of_t) (fun { txt = t } ->
          bind (struct_item_to_attr item_of_mode_expr) (fun attr ->
              let modes = Mode.list_from_payload ~loc attr.attr_payload in
              ret loc (Mod (t, modes))))
    | Some ("with", [item_of_t; item_of_ty], loc) ->
      bind (of_structure_item item_of_t) (fun { txt = t } ->
          bind (struct_item_to_type item_of_ty) (fun ty ->
              ret loc (With (t, ty))))
    | Some ("kind_of", [item_of_ty], loc) ->
      bind (struct_item_to_type item_of_ty) (fun ty -> ret loc (Kind_of ty))
    | Some ("abbrev", [item], loc) ->
      bind (Const.of_structure_item item) (fun c -> ret loc (Abbreviation c))
    | Some ("product", items, loc) ->
      bind (Misc.Stdlib.List.map_option of_structure_item items) (fun tls ->
          ret loc (Product (List.map (fun tl -> tl.txt) tls)))
    | Some _ | None -> None
end

(** Jkind annotations' encoding as attribute payload, used in both n-ary
    functions and jkinds. *)
module Jkind_annotation : sig
  include Payload_protocol with type t := Jkind.t

  module Decode : sig
    include module type of Decode

    val bound_vars_from_vars_and_payload :
      loc:Location.t ->
      string Location.loc list ->
      payload ->
      (string Location.loc * Jkind.annotation option) list
  end
end = struct
  module Protocol = Make_payload_protocol_of_structure_item_encodable (Jkind)

  (*******************************************************)
  (* Conversions with a payload *)

  module Encode = Protocol.Encode

  module Decode = struct
    include Protocol.Decode

    module Desugaring_error = struct
      type error =
        | Wrong_number_of_jkinds of int * Jkind.annotation option list

      let report_error ~loc = function
        | Wrong_number_of_jkinds (n, _jkinds) ->
          Location.errorf ~loc
            "Wrong number of kinds in an kind attribute;@;expecting %i." n

      exception Error of Location.t * error

      let () =
        Location.register_error_of_exn (function
          | Error (loc, err) -> Some (report_error ~loc err)
          | _ -> None)

      let raise ~loc err = raise (Error (loc, err))
    end

    let bound_vars_from_vars_and_payload ~loc var_names payload =
      let jkinds = option_list_from_payload ~loc payload in
      try List.combine var_names jkinds
      with
      (* seems silly to check the length in advance when [combine] does *)
      | Invalid_argument _ ->
        Desugaring_error.raise ~loc
          (Wrong_number_of_jkinds (List.length var_names, jkinds))
  end
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

(** Layouts *)
module Layouts = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Layouts
  end

  include Ext
  module Of_ast = Of_ast (Ext)

  type constant =
    | Float of string * char option
    | Integer of string * char

  type nonrec expression =
    | Lexp_constant of constant
    | Lexp_newtype of string loc * Jkind.annotation * expression

  type nonrec pattern = Lpat_constant of constant

  type nonrec core_type =
    | Ltyp_var of
        { name : string option;
          jkind : Jkind.annotation
        }
    | Ltyp_poly of
        { bound_vars : (string loc * Jkind.annotation option) list;
          inner_type : core_type
        }
    | Ltyp_alias of
        { aliased_type : core_type;
          name : string loc option;
          jkind : Jkind.annotation
        }

  type nonrec extension_constructor =
    | Lext_decl of
        (string Location.loc * Jkind.annotation option) list
        * constructor_arguments
        * Parsetree.core_type option

  type signature_item =
    | Lsig_kind_abbrev of string Location.loc * Jkind.annotation

  type structure_item =
    | Lstr_kind_abbrev of string Location.loc * Jkind.annotation

  (*******************************************************)
  (* Errors *)

  module Desugaring_error = struct
    type error =
      | Unexpected_wrapped_type of Parsetree.core_type
      | Unexpected_wrapped_ext of Parsetree.extension_constructor
      | Unexpected_attribute of string list
      | No_integer_suffix
      | Unexpected_constant of Parsetree.constant
      | Unexpected_wrapped_expr of Parsetree.expression
      | Unexpected_wrapped_pat of Parsetree.pattern

    (* Most things here are unprintable because we can't reference any
       [Printast] functions that aren't exposed by the upstream compiler, as we
       want this file to be compatible with the upstream compiler; see Note
       [Buildable with upstream] in jane_syntax.mli for details. *)
    let report_error ~loc = function
      | Unexpected_wrapped_type _typ ->
        Location.errorf ~loc "Layout attribute on wrong core type"
      | Unexpected_wrapped_ext _ext ->
        Location.errorf ~loc "Layout attribute on wrong extension constructor"
      | Unexpected_attribute names ->
        Location.errorf ~loc
          "Layout extension does not understand these attribute names:@;[%a]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             Format.pp_print_text)
          names
      | No_integer_suffix ->
        Location.errorf ~loc
          "All unboxed integers require a suffix to determine their size."
      | Unexpected_constant _c ->
        Location.errorf ~loc "Unexpected unboxed constant"
      | Unexpected_wrapped_expr expr ->
        Location.errorf ~loc "Layout attribute on wrong expression:@;%a"
          (Printast.expression 0) expr
      | Unexpected_wrapped_pat _pat ->
        Location.errorf ~loc "Layout attribute on wrong pattern"

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn (function
        | Error (loc, err) -> Some (report_error ~loc err)
        | _ -> None)

    let raise ~loc err = raise (Error (loc, err))
  end

  module Encode = Jkind_annotation.Encode
  module Decode = Jkind_annotation.Decode

  (*******************************************************)
  (* Constants *)

  let constant_of = function
    | Float (x, suffix) -> Pconst_float (x, suffix)
    | Integer (x, suffix) -> Pconst_integer (x, Some suffix)

  let of_constant ~loc = function
    | Pconst_float (x, suffix) -> Float (x, suffix)
    | Pconst_integer (x, Some suffix) -> Integer (x, suffix)
    | Pconst_integer (_, None) -> Desugaring_error.raise ~loc No_integer_suffix
    | const -> Desugaring_error.raise ~loc (Unexpected_constant const)

  (*******************************************************)
  (* Encoding expressions *)

  let expr_of ~loc expr =
    let module Ast_of = Ast_of (Expression) (Ext) in
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
        match expr with
        | Lexp_constant c ->
          let constant = constant_of c in
          Ast_of.wrap_jane_syntax ["unboxed"]
          @@ Ast_helper.Exp.constant constant
        | Lexp_newtype (name, jkind, inner_expr) ->
          let payload = Encode.as_payload jkind in
          Ast_of.wrap_jane_syntax ["newtype"] ~payload
          @@ Ast_helper.Exp.newtype name inner_expr)

  (*******************************************************)
  (* Desugaring expressions *)

  let of_expr expr =
    let loc = expr.pexp_loc in
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes_exn ~loc expr.pexp_attributes
    in
    let lexpr =
      match names with
      | ["unboxed"] -> (
        match expr.pexp_desc with
        | Pexp_constant const -> Lexp_constant (of_constant ~loc const)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_expr expr))
      | ["newtype"] -> (
        let jkind = Decode.from_payload ~loc payload in
        match expr.pexp_desc with
        | Pexp_newtype (name, inner_expr) ->
          Lexp_newtype (name, jkind, inner_expr)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_expr expr))
      | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lexpr, attributes

  (*******************************************************)
  (* Encoding patterns *)

  let pat_of ~loc t =
    Pattern.make_entire_jane_syntax ~loc feature (fun () ->
        match t with
        | Lpat_constant c ->
          let constant = constant_of c in
          Ast_helper.Pat.constant constant)

  (*******************************************************)
  (* Desugaring patterns *)

  let of_pat pat =
    let loc = pat.ppat_loc in
    let lpat =
      match pat.ppat_desc with
      | Ppat_constant const -> Lpat_constant (of_constant ~loc const)
      | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_pat pat)
    in
    lpat, pat.ppat_attributes

  (*******************************************************)
  (* Encoding types *)

  module Type_of = Ast_of (Core_type) (Ext)

  let type_of ~loc typ =
    let exception No_wrap_necessary of Parsetree.core_type in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature (fun () ->
          match typ with
          | Ltyp_var { name; jkind } -> (
            let payload = Encode.as_payload jkind in
            Type_of.wrap_jane_syntax ["var"] ~payload
            @@
            match name with
            | None -> Ast_helper.Typ.any ~loc ()
            | Some name -> Ast_helper.Typ.var ~loc name)
          | Ltyp_poly { bound_vars; inner_type } ->
            let var_names, jkinds = List.split bound_vars in
            (* Pass the loc because we don't want a ghost location here *)
            let tpoly = Ast_helper.Typ.poly ~loc var_names inner_type in
            if List.for_all Option.is_none jkinds
            then raise (No_wrap_necessary tpoly)
            else
              let payload = Encode.option_list_as_payload jkinds in
              Type_of.wrap_jane_syntax ["poly"] ~payload tpoly
          | Ltyp_alias { aliased_type; name; jkind } ->
            let payload = Encode.as_payload jkind in
            let has_name, inner_typ =
              match name with
              | None -> "anon", aliased_type
              | Some name -> "named", Ast_helper.Typ.alias aliased_type name
            in
            Type_of.wrap_jane_syntax ["alias"; has_name] ~payload inner_typ)
    with No_wrap_necessary result_type -> result_type

  (*******************************************************)
  (* Desugaring types *)

  let of_type typ =
    let loc = typ.ptyp_loc in
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes_exn ~loc typ.ptyp_attributes
    in
    let lty =
      match names with
      | ["var"] -> (
        let jkind = Decode.from_payload ~loc payload in
        match typ.ptyp_desc with
        | Ptyp_any -> Ltyp_var { name = None; jkind }
        | Ptyp_var name -> Ltyp_var { name = Some name; jkind }
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ))
      | ["poly"] -> (
        match typ.ptyp_desc with
        | Ptyp_poly (var_names, inner_type) ->
          let bound_vars =
            Decode.bound_vars_from_vars_and_payload ~loc var_names payload
          in
          Ltyp_poly { bound_vars; inner_type }
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ))
      | ["alias"; "anon"] ->
        let jkind = Decode.from_payload ~loc payload in
        Ltyp_alias
          { aliased_type = { typ with ptyp_attributes = attributes };
            name = None;
            jkind
          }
      | ["alias"; "named"] -> (
        let jkind = Decode.from_payload ~loc payload in
        match typ.ptyp_desc with
        | Ptyp_alias (inner_typ, name) ->
          Ltyp_alias { aliased_type = inner_typ; name = Some name; jkind }
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ))
      | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lty, attributes

  (*******************************************************)
  (* Encoding extension constructor *)

  module Ext_ctor_of = Ast_of (Extension_constructor) (Ext)

  let extension_constructor_of ~loc ~name ?info ?docs ext =
    (* using optional parameters to hook into existing defaulting
       in [Ast_helper.Te.decl], which seems unwise to duplicate *)
    let exception No_wrap_necessary of Parsetree.extension_constructor in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Extension_constructor.make_entire_jane_syntax ~loc feature (fun () ->
          match ext with
          | Lext_decl (bound_vars, args, res) ->
            let vars, jkinds = List.split bound_vars in
            let ext_ctor =
              (* Pass ~loc here, because the constructor declaration is
                 not a ghost *)
              Ast_helper.Te.decl ~loc ~vars ~args ?info ?docs ?res name
            in
            if List.for_all Option.is_none jkinds
            then raise (No_wrap_necessary ext_ctor)
            else
              let payload = Encode.option_list_as_payload jkinds in
              Ext_ctor_of.wrap_jane_syntax ["ext"] ~payload ext_ctor)
    with No_wrap_necessary ext_ctor -> ext_ctor

  (*******************************************************)
  (* Desugaring extension constructor *)

  let of_extension_constructor ext =
    let loc = ext.pext_loc in
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes_exn ~loc ext.pext_attributes
    in
    let lext =
      match names with
      | ["ext"] -> (
        match ext.pext_kind with
        | Pext_decl (var_names, args, res) ->
          let bound_vars =
            Decode.bound_vars_from_vars_and_payload ~loc var_names payload
          in
          Lext_decl (bound_vars, args, res)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_ext ext))
      | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lext, attributes

  (*********************************************************)
  (* Constructing a [constructor_declaration] with jkinds *)

  module Ctor_decl_of = Ast_of (Constructor_declaration) (Ext)

  let constructor_declaration_of ~loc ~attrs ~info ~vars_jkinds ~args ~res name
      =
    let vars, jkinds = List.split vars_jkinds in
    let ctor_decl =
      Ast_helper.Type.constructor ~loc ~info ~vars ~args ?res name
    in
    let ctor_decl =
      if List.for_all Option.is_none jkinds
      then ctor_decl
      else
        let payload = Encode.option_list_as_payload jkinds in
        Constructor_declaration.make_entire_jane_syntax ~loc feature (fun () ->
            Ctor_decl_of.wrap_jane_syntax ["vars"] ~payload ctor_decl)
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> ctor_decl
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { ctor_decl with pcd_attributes = ctor_decl.pcd_attributes @ attrs }

  let of_constructor_declaration_internal (feat : Feature.t) ctor_decl =
    match feat with
    | Language_extension Layouts ->
      let loc = ctor_decl.pcd_loc in
      let names, payload, attributes =
        Of_ast.unwrap_jane_syntax_attributes_exn ~loc ctor_decl.pcd_attributes
      in
      let vars_jkinds =
        match names with
        | ["vars"] ->
          Decode.bound_vars_from_vars_and_payload ~loc ctor_decl.pcd_vars
            payload
        | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
      in
      Some (vars_jkinds, attributes)
    | _ -> None

  let of_constructor_declaration =
    Constructor_declaration.make_of_ast
      ~of_ast_internal:of_constructor_declaration_internal

  (*********************************************************)
  (* Constructing a [type_declaration] with jkinds *)

  module Type_decl_of = Ast_of (Type_declaration) (Ext)

  let type_declaration_of ~loc ~attrs ~docs ~text ~params ~cstrs ~kind ~priv
      ~manifest ~jkind name =
    let type_decl =
      Ast_helper.Type.mk ~loc ~docs ?text ~params ~cstrs ~kind ~priv ?manifest
        name
    in
    let type_decl =
      match jkind with
      | None -> type_decl
      | Some jkind ->
        Type_declaration.make_entire_jane_syntax ~loc feature (fun () ->
            let payload = Encode.as_payload jkind in
            Type_decl_of.wrap_jane_syntax ["annot"] ~payload type_decl)
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> type_decl
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { type_decl with ptype_attributes = type_decl.ptype_attributes @ attrs }

  let of_type_declaration_internal (feat : Feature.t) type_decl =
    match feat with
    | Language_extension Layouts ->
      let loc = type_decl.ptype_loc in
      let names, payload, attributes =
        Of_ast.unwrap_jane_syntax_attributes_exn ~loc type_decl.ptype_attributes
      in
      let jkind_annot =
        match names with
        | ["annot"] -> Decode.from_payload ~loc payload
        | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
      in
      Some (jkind_annot, attributes)
    | _ -> None

  let of_type_declaration =
    Type_declaration.make_of_ast ~of_ast_internal:of_type_declaration_internal

  (*********************************************************)
  (* Constructing a [signature_item] for kind_abbrev *)

  let attr_name_of { txt = name; loc } =
    let embed = Embedded_name.of_feature feature ["kind_abbrev"; name] in
    Location.mkloc (Embedded_name.to_string embed) loc

  let of_attr_name { txt = attr_name; loc } =
    let name =
      match Embedded_name.of_string attr_name with
      | Some (Ok embed) -> (
        match Embedded_name.components embed with
        | _ :: ["kind_abbrev"; name] -> name
        | _ -> failwith "Malformed [kind_abbrev] attribute")
      | None | Some (Error _) -> failwith "Malformed [kind_abbrev] attribute"
    in
    Location.mkloc name loc

  let sig_item_of ~loc = function
    | Lsig_kind_abbrev (name, jkind) ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Signature_item.make_entire_jane_syntax ~loc feature (fun () ->
          let payload = Encode.as_payload jkind in
          Ast_helper.Sig.attribute
            (Ast_helper.Attr.mk (attr_name_of name) payload))

  let of_sig_item sigi =
    match sigi.psig_desc with
    | Psig_attribute { attr_name; attr_payload; _ } ->
      Lsig_kind_abbrev
        ( of_attr_name attr_name,
          Decode.from_payload ~loc:sigi.psig_loc attr_payload )
    | _ -> failwith "Malformed [kind_abbrev] in signature"

  let str_item_of ~loc = function
    | Lstr_kind_abbrev (name, jkind) ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Structure_item.make_entire_jane_syntax ~loc feature (fun () ->
          let payload = Encode.as_payload jkind in
          Ast_helper.Str.attribute
            (Ast_helper.Attr.mk (attr_name_of name) payload))

  let of_str_item stri =
    match stri.pstr_desc with
    | Pstr_attribute { attr_name; attr_payload; _ } ->
      Lstr_kind_abbrev
        ( of_attr_name attr_name,
          Decode.from_payload ~loc:stri.pstr_loc attr_payload )
    | _ -> failwith "Malformed [kind_abbrev] in structure"
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t

  type ast

  val of_ast : ast -> t option
end

module Core_type = struct
  type t = Jtyp_layout of Layouts.core_type

  let of_ast_internal (feat : Feature.t) typ =
    match feat with
    | Language_extension Layouts ->
      let typ, attrs = Layouts.of_type typ in
      Some (Jtyp_layout typ, attrs)
    | _ -> None

  let of_ast = Core_type.make_of_ast ~of_ast_internal

  let core_type_of ~loc ~attrs t =
    let core_type = match t with Jtyp_layout x -> Layouts.type_of ~loc x in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> core_type
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { core_type with ptyp_attributes = core_type.ptyp_attributes @ attrs }
end

module Constructor_argument = struct
  type t = |

  let of_ast_internal (feat : Feature.t) _carg = match feat with _ -> None

  let of_ast = Constructor_argument.make_of_ast ~of_ast_internal
end

module Expression = struct
  type t =
    | Jexp_comprehension of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression
    | Jexp_layout of Layouts.expression

  let of_ast_internal (feat : Feature.t) expr =
    match feat with
    | Language_extension Comprehensions ->
      let expr, attrs = Comprehensions.comprehension_expr_of_expr expr in
      Some (Jexp_comprehension expr, attrs)
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_expr expr in
      Some (Jexp_immutable_array expr, attrs)
    | Language_extension Layouts ->
      let expr, attrs = Layouts.of_expr expr in
      Some (Jexp_layout expr, attrs)
    | _ -> None

  let of_ast = Expression.make_of_ast ~of_ast_internal

  let expr_of ~loc ~attrs t =
    let expr =
      match t with
      | Jexp_comprehension x -> Comprehensions.expr_of ~loc x
      | Jexp_immutable_array x -> Immutable_arrays.expr_of ~loc x
      | Jexp_layout x -> Layouts.expr_of ~loc x
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> expr
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { expr with pexp_attributes = expr.pexp_attributes @ attrs }
end

module Pattern = struct
  type t =
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_layout of Layouts.pattern

  let of_ast_internal (feat : Feature.t) pat =
    match feat with
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_pat pat in
      Some (Jpat_immutable_array expr, attrs)
    | Language_extension Layouts ->
      let pat, attrs = Layouts.of_pat pat in
      Some (Jpat_layout pat, attrs)
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let pat_of ~loc ~attrs t =
    let pat =
      match t with
      | Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc x
      | Jpat_layout x -> Layouts.pat_of ~loc x
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

module Signature_item = struct
  type t = Jsig_layout of Layouts.signature_item

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Layouts ->
      Some (Jsig_layout (Layouts.of_sig_item sigi))
    | _ -> None

  let of_ast = Signature_item.make_of_ast ~of_ast_internal
end

module Structure_item = struct
  type t = Jstr_layout of Layouts.structure_item

  let of_ast_internal (feat : Feature.t) stri =
    match feat with
    | Language_extension Layouts ->
      Some (Jstr_layout (Layouts.of_str_item stri))
    | _ -> None

  let of_ast = Structure_item.make_of_ast ~of_ast_internal
end

module Extension_constructor = struct
  type t = Jext_layout of Layouts.extension_constructor

  let of_ast_internal (feat : Feature.t) ext =
    match feat with
    | Language_extension Layouts ->
      let ext, attrs = Layouts.of_extension_constructor ext in
      Some (Jext_layout ext, attrs)
    | _ -> None

  let of_ast = Extension_constructor.make_of_ast ~of_ast_internal

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs t =
    let ext_ctor =
      match t with
      | Jext_layout lext ->
        Layouts.extension_constructor_of ~loc ~name ?info ?docs lext
    in
    (* Performance hack: save an allocation if [attrs] is empty. *)
    match attrs with
    | [] -> ext_ctor
    | _ :: _ as attrs ->
      (* See Note [Outer attributes at end] *)
      { ext_ctor with pext_attributes = ext_ctor.pext_attributes @ attrs }
end
