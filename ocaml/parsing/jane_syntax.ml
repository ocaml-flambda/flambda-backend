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
    Language_extension
    : Language_extension_kernel.Language_extension_for_jane_syntax)
end

(* Suppress the unused module warning so it's easy to keep around the
   shadowing even if we delete use sites of the module. *)
module _ = Language_extension

(****************************************)
(* Helpers used just within this module *)

module type Extension = sig
  val feature : Feature.t
end

module Ast_of (AST : AST with type 'a with_attributes := 'a * attributes)
              (Ext : Extension) : sig
  (* Wrap a bit of AST with a jane-syntax annotation *)
  val wrap_jane_syntax :
    string list ->   (* these strings describe the bit of new syntax *)
    ?payload:payload ->
    AST.ast ->
    AST.ast
end = struct
  let wrap_jane_syntax suffixes ?payload to_be_wrapped =
    AST.make_jane_syntax Ext.feature suffixes ?payload to_be_wrapped
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

module Make_payload_protocol_of_stringable (Stringable : Stringable)
  : Payload_protocol with type t := Stringable.t = struct
  module Encode = struct
    let as_expr t_loc =
      let string = Stringable.to_string t_loc.txt in
      Ast_helper.Exp.ident
        (Location.mkloc (Longident.Lident string) t_loc.loc)

    let structure_item_of_expr expr =
      { pstr_desc = Pstr_eval (expr, []); pstr_loc = Location.none }

    let structure_item_of_none =
      { pstr_desc = Pstr_attribute { attr_name = Location.mknoloc "none"
                                   ; attr_payload = PStr []
                                   ; attr_loc = Location.none }
      ; pstr_loc = Location.none }

    let as_payload t_loc =
      let expr = as_expr t_loc in
      PStr [ structure_item_of_expr expr ]

    let list_as_payload t_locs =
      let items =
        List.map (fun t_loc -> structure_item_of_expr (as_expr t_loc)) t_locs
      in
      PStr items

    let option_list_as_payload t_locs =
      let items =
        List.map (function
            | None -> structure_item_of_none
            | Some t_loc -> structure_item_of_expr (as_expr t_loc))
          t_locs
      in
      PStr items
  end

  module Desugaring_error = struct
    type error =
      | Unknown_payload of payload

    let report_error ~loc = function
      | Unknown_payload payload ->
        let indefinite_article, name = Stringable.indefinite_article_and_name in
        Location.errorf ~loc
          "Attribute payload does not name %s %s:@;%a"
          indefinite_article name
          (Printast.payload 0) payload

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) ->
              Some (report_error ~loc err)
          | _ -> None)

    let raise ~loc err =
      raise (Error(loc, err))
  end

  module Decode = struct
    (* Avoid exporting a definition that raises [Unexpected]. *)
    open struct
      exception Unexpected

      let from_expr = function
        | { pexp_desc = Pexp_ident payload_lid; _ } ->
            let t =
              match Stringable.of_string (Longident.last payload_lid.txt) with
              | None -> raise Unexpected
              | Some t -> t
            in
            Location.mkloc t payload_lid.loc
        | _ -> raise Unexpected

      let expr_of_structure_item = function
        | { pstr_desc = Pstr_eval (expr, _) } -> expr
        | _ -> raise Unexpected

      let is_none_structure_item = function
        | { pstr_desc = Pstr_attribute { attr_name = { txt = "none" } } } ->
            true
        | _ -> false

      let from_payload payload =
        match payload with
        | PStr [ item ] -> from_expr (expr_of_structure_item item)
        | _ -> raise Unexpected

      let list_from_payload payload =
        match payload with
        | PStr items ->
            List.map (fun item -> from_expr (expr_of_structure_item item)) items
        | _ -> raise Unexpected

      let option_list_from_payload payload =
        match payload with
        | PStr items ->
            List.map (fun item ->
                if is_none_structure_item item
                then None
                else Some (from_expr (expr_of_structure_item item)))
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

module Builtin = struct
  let is_curry_attr = function
    | { attr_name = { txt = name; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _ } ->
      String.equal Jane_syntax_parsing.Marker_attributes.curry name
    | _ -> false

  let is_curried typ = List.exists is_curry_attr typ.ptyp_attributes

  let mark_curried ~loc typ = match typ.ptyp_desc with
    | Ptyp_arrow _ when not (is_curried typ) ->
      let loc = Location.ghostify loc in
      let curry_attr =
        Ast_helper.Attr.mk
          ~loc
          (Location.mkloc Jane_syntax_parsing.Marker_attributes.curry loc)
          (PStr [])
      in
      Core_type.add_attributes [curry_attr] typ
    | _ -> typ

  let non_syntax_attributes attrs =
    List.filter (fun attr -> not (is_curry_attr attr)) attrs
end

(** Locality modes *)
module Local = struct
  let feature : Feature.t = Language_extension Local

  type constructor_argument = Lcarg_global of core_type

  type nonrec core_type = Ltyp_local of core_type
  (* Invariant: [Ltyp_local] is only used in arrow types and has no attributes.
     For more, see the [.mli] file. *)

  type nonrec expression =
    | Lexp_local of expression
    | Lexp_exclave of expression

  type nonrec pattern = Lpat_local of pattern
  (* Invariant: [Lpat_local] is always the outermost part of a pattern. *)

  let type_of ~loc = function
    | Ltyp_local typ ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature (fun () ->
        (* Although there's only one constructor here, the use of
           [constructor_argument] means we need to be able to tell the two uses
           apart *)
        Core_type.make_jane_syntax feature ["type"; "local"] typ)

  let of_type typ =
    let typ, subparts = Core_type.match_jane_syntax feature typ in
    match subparts with
    | ["type"; "local"] -> Ltyp_local typ
    | _ -> Core_type.raise_partial_match feature typ subparts

  let constr_arg_of ~loc lcarg =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Constructor_argument.make_entire_jane_syntax ~loc feature (fun () ->
      match lcarg with
      | Lcarg_global carg ->
        (* Although there's only one constructor here, the use of [core_type]
           means we need to be able to tell the two uses apart *)
        Constructor_argument.make_jane_syntax
          feature ["constructor_argument"; "global"]
          carg)

  let of_constr_arg carg =
    let carg, subparts =
      Constructor_argument.match_jane_syntax feature carg
    in
    match subparts with
    | ["constructor_argument"; "global"] -> Lcarg_global carg
    | _ -> Constructor_argument.raise_partial_match feature carg subparts

  let expr_of ~loc = function
    | Lexp_local expr ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Expression.make_jane_syntax feature ["local"] expr)
    | Lexp_exclave expr ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Expression.make_jane_syntax feature ["exclave"] expr)

  let of_expr expr =
    let expr, subparts = Expression.match_jane_syntax feature expr in
    match subparts with
    | ["local"] -> Lexp_local expr
    | ["exclave"] -> Lexp_exclave expr
    | _ -> Expression.raise_partial_match feature expr subparts

  let pat_of ~loc = function
    | Lpat_local pat ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () -> pat)

  let of_pat pat = Lpat_local pat
end

(** Uniqueness modes *)
module Unique = struct
  let feature : Feature.t = Language_extension Unique

  type nonrec core_type =
    | Utyp_unique of core_type
    | Utyp_once of core_type
  (* Invariant: These are only used in arrow types and have no attributes.  For
     more, see the [.mli] file. *)

  type expression =
    | Uexp_unique of Parsetree.expression
    | Uexp_once of Parsetree.expression

  type pattern =
    | Upat_unique of Parsetree.pattern (** [unique_ PAT] *)
    | Upat_once of Parsetree.pattern (** [once_ PAT] *)
  (* Invariant: These are always the outermost part of a pattern. *)

  let type_of ~loc utyp =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Core_type.make_entire_jane_syntax ~loc feature (fun () ->
      match utyp with
      | Utyp_unique typ ->
        Core_type.make_jane_syntax feature ["type"; "unique"] typ
      | Utyp_once typ ->
        Core_type.make_jane_syntax feature ["type"; "once"] typ)

  let of_type typ =
    let typ, subparts = Core_type.match_jane_syntax feature typ in
    match subparts with
    | ["type"; "unique"] -> Utyp_unique typ
    | ["type"; "once"] -> Utyp_once typ
    | _ -> Core_type.raise_partial_match feature typ subparts

  let expr_of ~loc uexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
      match uexpr with
      | Uexp_unique expr ->
        Expression.make_jane_syntax feature ["unique"] expr
      | Uexp_once expr ->
        Expression.make_jane_syntax feature ["once"] expr)

  let of_expr expr =
    let expr, subparts = Expression.match_jane_syntax feature expr in
    match subparts with
    | ["unique"] -> Uexp_unique expr
    | ["once"] -> Uexp_once expr
    | _ -> Expression.raise_partial_match feature expr subparts

  let pat_of ~loc upat =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Pattern.make_entire_jane_syntax ~loc feature (fun () ->
      match upat with
      | Upat_unique pat ->
        Pattern.make_jane_syntax feature ["pattern"; "unique"] pat
      | Upat_once pat ->
        Pattern.make_jane_syntax feature ["pattern"; "once"] pat)

  let of_pat pat =
    let pat, subparts = Pattern.match_jane_syntax feature pat in
    match subparts with
    | ["pattern"; "unique"] -> Upat_unique pat
    | ["pattern"; "once"] -> Upat_once pat
    | _ -> Pattern.raise_partial_match feature pat subparts
end

module Modes = struct
  type mode =
    | Local
    | Unique
    | Once

  type modality =
    | Global

  type core_type =
    | Mtyp_mode of mode * Parsetree.core_type

  type constructor_argument =
    | Mcarg_modality of modality * Parsetree.core_type

  type expression =
    | Mexp_mode of mode * Parsetree.expression
    | Mexp_exclave of Parsetree.expression

  type pattern =
    | Mpat_mode of mode * Parsetree.pattern

  let type_of ~loc = function
    | Mtyp_mode (mode, typ) ->
      begin match mode with
      | Local -> Local.type_of ~loc (Ltyp_local typ)
      | Unique -> Unique.type_of ~loc (Utyp_unique typ)
      | Once -> Unique.type_of ~loc (Utyp_once typ)
      end

  let of_local_type : Local.core_type -> _ = function
    | Ltyp_local typ -> Mtyp_mode (Local, typ)

  let of_unique_type : Unique.core_type -> _ = function
    | Utyp_unique typ -> Mtyp_mode (Unique, typ)
    | Utyp_once typ -> Mtyp_mode (Once, typ)

  let constr_arg_of ~loc = function
    | Mcarg_modality (modality, carg) ->
      begin match modality with
      | Global -> Local.constr_arg_of ~loc (Lcarg_global carg)
      end

  let of_local_constr_arg : Local.constructor_argument -> _ = function
    | Lcarg_global carg -> Mcarg_modality (Global, carg)

  let expr_of ~loc = function
    | Mexp_mode (mode, expr) ->
      begin match mode with
      | Local -> Local.expr_of ~loc (Lexp_local expr)
      | Unique -> Unique.expr_of ~loc (Uexp_unique expr)
      | Once -> Unique.expr_of ~loc (Uexp_once expr)
      end
    | Mexp_exclave expr ->
      Local.expr_of ~loc (Lexp_exclave expr)

  let of_local_expr : Local.expression -> _ = function
    | Lexp_local expr -> Mexp_mode (Local, expr)
    | Lexp_exclave expr -> Mexp_exclave expr

  let of_unique_expr : Unique.expression -> _ = function
    | Uexp_unique expr -> Mexp_mode (Unique, expr)
    | Uexp_once expr -> Mexp_mode (Once, expr)

  let pat_of ~loc = function
    | Mpat_mode (mode, pat) ->
      begin match mode with
      | Local -> Local.pat_of ~loc (Lpat_local pat)
      | Unique -> Unique.pat_of ~loc (Upat_unique pat)
      | Once -> Unique.pat_of ~loc (Upat_once pat)
      end

  let of_local_pat : Local.pattern -> _ = function
    | Lpat_local pat -> Mpat_mode (Local, pat)

  let of_unique_pat : Unique.pattern -> _ = function
    | Upat_unique pat -> Mpat_mode (Unique, pat)
    | Upat_once pat -> Mpat_mode (Once, pat)
end

(** Layout annotations' encoding as attribute payload, used in both n-ary
    functions and layouts. *)
module Layout_annotation : sig
  include Payload_protocol with type t := const_layout

  module Decode : sig
    include module type of Decode

    val bound_vars_from_vars_and_payload :
      loc:Location.t -> string Location.loc list -> payload ->
      (string Location.loc * layout_annotation option) list
  end
end = struct
  module Protocol = Make_payload_protocol_of_stringable (struct
      type t = const_layout

      let indefinite_article_and_name = "a", "layout"

      let to_string = function
        | Any -> "any"
        | Value -> "value"
        | Void -> "void"
        | Immediate64 -> "immediate64"
        | Immediate -> "immediate"
        | Float64 -> "float64"

      (* CR layouts v1.5: revise when moving layout recognition away from parser*)
      let of_string = function
        | "any" -> Some Any
        | "value" -> Some Value
        | "void" -> Some Void
        | "immediate" -> Some Immediate
        | "immediate64" -> Some Immediate64
        | "float64" -> Some Float64
        | _ -> None
    end)
  (*******************************************************)
  (* Conversions with a payload *)

  module Encode = Protocol.Encode

  module Decode = struct
    include Protocol.Decode

    module Desugaring_error = struct
      type error =
        | Wrong_number_of_layouts of int * layout_annotation option list

      let report_error ~loc = function
        | Wrong_number_of_layouts (n, layouts) ->
            Location.errorf ~loc
              "Wrong number of layouts in an layout attribute;@;\
              expecting %i but got this list:@;%a"
              n
              (Format.pp_print_list
                (Format.pp_print_option
                    ~none:(fun ppf () -> Format.fprintf ppf "None")
                    (Printast.layout_annotation 0)))
              layouts

      exception Error of Location.t * error

      let () =
        Location.register_error_of_exn
          (function
            | Error(loc, err) ->
                Some (report_error ~loc err)
            | _ -> None)

      let raise ~loc err =
        raise (Error(loc, err))
    end

    let bound_vars_from_vars_and_payload ~loc var_names payload =
      let layouts = option_list_from_payload ~loc payload in
      try
        List.combine var_names layouts
      with
      (* seems silly to check the length in advance when [combine] does *)
        Invalid_argument _ ->
        Desugaring_error.raise ~loc
          (Wrong_number_of_layouts(List.length var_names, layouts))
  end
end

module Mode_annotation = struct
  type t = Modes.mode =
    | Local
    | Unique
    | Once

  include Make_payload_protocol_of_stringable (struct
      type nonrec t = t

      let indefinite_article_and_name = "a", "mode"

      let to_string = function
        | Local -> "local"
        | Unique -> "unique"
        | Once -> "once"

      let of_string = function
        | "local" -> Some Local
        | "unique" -> Some Unique
        | "once" -> Some Once
        | _ -> None
    end)
end

(** List and array comprehensions *)
module Comprehensions = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Comprehensions
  end

  module Ast_of = Ast_of (Expression) (Ext)

  include Ext

  type iterator =
    | Range of { start     : expression
               ; stop      : expression
               ; direction : direction_flag }
    | In of expression

  type clause_binding =
    { pattern    : pattern
    ; iterator   : iterator
    ; attributes : attribute list }

  type clause =
    | For of clause_binding list
    | When of expression

  type comprehension =
    { body    : expression
    ; clauses : clause list
    }

  type expression =
    | Cexp_list_comprehension  of comprehension
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

  (* let comprehension_expr = Expression.make_jane_syntax feature *)

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in [expr_of]. *)

  let expr_of_iterator = function
    | Range { start; stop; direction } ->
        Ast_of.wrap_jane_syntax
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        Ast_of.wrap_jane_syntax ["for"; "in"] (Ast_helper.Exp.lazy_ seq)
        (* See Note [Wrapping with Pexp_lazy] *)

  let expr_of_clause_binding { pattern; iterator; attributes } =
    Ast_helper.Vb.mk ~attrs:attributes pattern (expr_of_iterator iterator)

  let expr_of_clause clause rest = match clause with
    | For iterators ->
        Ast_of.wrap_jane_syntax
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive (List.map expr_of_clause_binding iterators)
             rest)
    | When cond ->
        Ast_of.wrap_jane_syntax ["when"] (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~type_ { body; clauses } =
    (* See Note [Wrapping with Pexp_lazy] *)
    Ast_of.wrap_jane_syntax
      type_
      (Ast_helper.Exp.lazy_
        (List.fold_right
          expr_of_clause
          clauses
          (Ast_of.wrap_jane_syntax ["body"] (Ast_helper.Exp.lazy_ body))))

  let expr_of ~loc cexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature (fun () ->
      match cexpr with
      | Cexp_list_comprehension comp ->
          expr_of_comprehension ~type_:["list"] comp
      | Cexp_array_comprehension (amut, comp) ->
          expr_of_comprehension
            ~type_:[ "array"
                   ; match amut with
                     | Mutable   -> "mutable"
                     | Immutable -> "immutable"
                   ]
            comp)

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in [of_expr]. *)

  module Desugaring_error = struct
    type error =
      | No_clauses
      | Unexpected_attributes of attributes
      (* Note [Wrapping with Pexp_lazy]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         We require that every internal comprehensions node contain at least one
         constructor, using [Pexp_lazy] by convention when there isn't another
         obvious choice.  This means that every internal AST node synthesized
         for comprehensions can contain no other attributes, which we can then
         check for and raise [Unexpected_attributes] if we get this wrong.  This
         helps guard against attribute errors. *)

    let report_error ~loc = function
      | No_clauses ->
          Location.errorf ~loc
            "Tried to desugar a comprehension with no clauses"
      | Unexpected_attributes attrs ->
          Location.errorf ~loc
            "An internal synthesized comprehension node had extra attributes.@.\
             The attributes had the following names:@ %a"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
               (fun ppf attr -> Format.fprintf ppf "\"%s\"" attr.attr_name.txt))
            attrs

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise expr err = raise (Error(expr.pexp_loc, err))
  end

  let match_comprehension_piece expr =
    let expr, subparts = Expression.match_jane_syntax feature expr in
    match expr.pexp_attributes with
    | [] -> expr, subparts
    | _ :: _ as attrs ->
      Desugaring_error.raise expr (Unexpected_attributes attrs)

  let iterator_of_expr expr =
    let expr, subparts = match_comprehension_piece expr in
    match subparts, expr.pexp_desc with
    | ["for"; "range"; "upto"], Pexp_tuple [start; stop] ->
        Range { start; stop; direction = Upto }
    | ["for"; "range"; "downto"], Pexp_tuple [start; stop] ->
        Range { start; stop; direction = Downto }
    | ["for"; "in"], Pexp_lazy seq ->
        In seq
    | _ -> Expression.raise_partial_match feature expr subparts

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let comprehension_of_expr =
    let rec raw_comprehension_of_expr expr =
      let expr, subparts = match_comprehension_piece expr in
      match subparts, expr.pexp_desc with
      | ["for"], Pexp_let(Nonrecursive, iterators, rest) ->
          add_clause
            (For (List.map clause_binding_of_vb iterators))
            (raw_comprehension_of_expr rest)
      | ["when"], Pexp_sequence(cond, rest) ->
          add_clause
            (When cond)
            (raw_comprehension_of_expr rest)
      | ["body"], Pexp_lazy body ->
          { body; clauses = [] }
      | _ ->
          Expression.raise_partial_match feature expr subparts
    in
    fun expr ->
      match raw_comprehension_of_expr expr with
      | { body = _; clauses = [] } ->
          Desugaring_error.raise expr No_clauses
      | comp -> comp

  let of_expr expr =
    let expr, subparts = match_comprehension_piece expr in
    (* See Note [Wrapping with Pexp_lazy] *)
    match subparts, expr.pexp_desc with
    | ["list"], Pexp_lazy comp ->
        Cexp_list_comprehension (comprehension_of_expr comp)
    | ["array"; "mutable"], Pexp_lazy comp ->
        Cexp_array_comprehension (Mutable,
                                  comprehension_of_expr comp)
    | ["array"; "immutable"], Pexp_lazy comp ->
        (* assert_extension_enabled:
           See Note [Check for immutable extension in comprehensions code]
        *)
        assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays ();
        Cexp_array_comprehension (Immutable,
                                  comprehension_of_expr comp)
    | _ -> Expression.raise_partial_match feature expr subparts
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list

  type nonrec pattern =
    | Iapat_immutable_array of pattern list

  let feature : Feature.t = Language_extension Immutable_arrays

  let expr_of ~loc = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Exp.array elts)

  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Pat.array elts)

  let of_pat pat = match pat.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts
    | _ -> failwith "Malformed immutable array pattern"
end

module N_ary_functions = struct
  module Ext = struct
    let feature : Feature.t = Builtin
  end

  module Ast_of = Ast_of (Expression) (Ext)
  open Ext

  type function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  type function_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * layout_annotation option

  type function_param =
    { pparam_desc : function_param_desc
    ; pparam_loc : Location.t
    }

  type mode_annotation = Modes.mode =
    | Local
    | Unique
    | Once

  type type_constraint =
    | Pconstraint of core_type
    | Pcoerce of core_type option * core_type

  type function_constraint =
    { mode_annotations: mode_annotation loc list;
      type_constraint: type_constraint;
    }

  type expression =
    function_param list * function_constraint option * function_body

  (** An attribute of the form [@jane.erasable._builtin.*] that's relevant
      to n-ary functions. The "*" in the example is what we call the "suffix".
      See the below BNF for the meaning of the attributes.
  *)
  module Attribute_node = struct
    type after_fun =
      | Cases
      | Constraint_then_cases

    type t =
      | Top_level
      | Fun_then of after_fun
      | Mode_constraint of mode_annotation loc list
      | Layout_annotation of const_layout loc

    (* We return an [of_suffix_result] from [of_suffix] rather than having
       [of_suffix] interpret the payload for two reasons:
         1. It's nice to keep the string production / matching extremely
            visually simple so it's easy to check that [to_suffix_and_payload]
            and [of_suffix] correspond.
         2. We want to raise a [Desugaring_error.Has_payload] in the case that
            a [No_payload t] has an improper payload, but this creates a
            dependency cycle between [Attribute_node] and [Desugaring_error].
            Moving the interpretation of the payload to the caller of
            [of_suffix] breaks this cycle.
    *)

    type of_suffix_result =
      | No_payload of t
      | Payload of (payload -> loc:Location.t -> t)
      | Unknown_suffix

    let to_suffix_and_payload = function
      | Top_level -> [], None
      | Fun_then Cases -> [ "cases" ], None
      | Fun_then Constraint_then_cases -> [ "constraint"; "cases" ], None
      | Mode_constraint mode_annotation ->
          let payload =
            Mode_annotation.Encode.list_as_payload mode_annotation
          in
          [ "mode_constraint" ], Some payload
      | Layout_annotation layout_annotation ->
          let payload = Layout_annotation.Encode.as_payload layout_annotation in
          [ "layout_annotation" ], Some payload

    let of_suffix suffix =
      match suffix with
      | [] -> No_payload Top_level
      | [ "cases" ] -> No_payload (Fun_then Cases)
      | [ "constraint"; "cases" ] -> No_payload (Fun_then Constraint_then_cases)
      | [ "mode_constraint" ] ->
          Payload (fun payload ~loc ->
              let mode_annotations =
                Mode_annotation.Decode.list_from_payload payload ~loc
              in
              List.iter (fun mode_annotation ->
                assert_extension_enabled ~loc
                  (match (mode_annotation.txt : mode_annotation) with
                   | Local -> Local
                   | Unique | Once -> Unique)
                  ())
                mode_annotations;
              Mode_constraint mode_annotations)
      | [ "layout_annotation" ] ->
          Payload (fun payload ~loc ->
              assert_extension_enabled ~loc Layouts
                (Stable : Language_extension.maturity);
              let layout_annotation =
                Layout_annotation.Decode.from_payload payload ~loc
              in
              Layout_annotation layout_annotation)
      | _ -> Unknown_suffix

    let format ppf t =
      let suffix, _ = to_suffix_and_payload t in
      Embedded_name.pp_quoted_name ppf (Embedded_name.of_feature feature suffix)
  end

  module Desugaring_error = struct
    type error =
      | Has_payload of payload
      | Expected_constraint_or_coerce
      | Expected_function_cases of Attribute_node.t
      | Expected_fun_or_newtype of Attribute_node.t
      | Expected_newtype_with_layout_annotation of layout_annotation
      | Parameterless_function

    let report_error ~loc = function
      | Has_payload payload ->
          Location.errorf ~loc
            "Syntactic arity attribute has an unexpected payload:@;%a"
            (Printast.payload 0) payload
      | Expected_constraint_or_coerce ->
          Location.errorf ~loc
            "Expected a Pexp_constraint or Pexp_coerce node at this position."
      | Expected_function_cases attribute ->
          Location.errorf ~loc
            "Expected a Pexp_function node in this position, as the enclosing \
             Pexp_fun is annotated with %a."
            Attribute_node.format attribute
      | Expected_fun_or_newtype attribute ->
          Location.errorf ~loc
            "Only Pexp_fun or Pexp_newtype may carry the attribute %a."
            Attribute_node.format attribute
      | Expected_newtype_with_layout_annotation annotation ->
          Location.errorf ~loc
            "Only Pexp_newtype may carry the attribute %a."
            Attribute_node.format (Attribute_node.Layout_annotation annotation)
      | Parameterless_function ->
          Location.errorf ~loc
            "The expression is a Jane Syntax encoding of a function with no \
             parameters, which is an invalid expression."

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise_with_loc loc err = raise (Error (loc, err))
    let raise expr err = raise (Error (expr.pexp_loc, err))
  end

  (* The desugared-to-OCaml version of an n-ary function is described by the
     following BNF, where [{% '...' | expr %}] refers to the result of
     [Expression.make_jane_syntax] (via n_ary_function_expr) as described at the
     top of [jane_syntax_parsing.mli]. Within the '...' string, I use <...>
     brackets to denote string interpolation.

     {v
         (* The entry point.

            The encoding only puts attributes on:
              - [fun] nodes
              - constraint/coercion nodes, on the rare occasions
                that a constraint should be interpreted at the [local] mode

            This ensures that we rarely put attributes on the *body* of the
            function, which means that ppxes that move or transform the body
            of a function won't make Jane Syntax complain.
         *)
         n_ary_function ::=
           | nested_n_ary_function
           (* A function need not have [fun] params; it can be a function
              or a constrained function. These need not have extra attributes,
              except in the rare case that the function is constrained at the
              local mode.
           *)
           | pexp_function
           | constraint_with_mode_then(pexp_function)

         nested_n_ary_function ::=
           | fun_then(nested_n_ary_function)
           | fun_then(constraint_with_mode_then(expression))
           | {% '_builtin.cases' | fun_then(pexp_function) }
           | {% '_builtin.constraint.cases' |
               fun_then(constraint_with_mode_then(pexp_function)) }
           | fun_then(expression)


         fun_then(body) ::=
           | 'fun' pattern '->' body (* Pexp_fun *)
           | 'fun' '(' 'type' ident ')' '->' body (* Pexp_newtype *)
           |{% '_builtin.layout_annotation' |
              'fun' '(' 'type' ident ')' '->' body %} (* Pexp_newtype *)

         pexp_function ::=
           | 'function' cases

         constraint_then(ast) ::=
           | ast (':' type)? ':>' type (* Pexp_coerce *)
           | ast ':' type              (* Pexp_constraint *)

         constraint_with_mode_then(ast) ::=
           | constraint_then(ast)
           | {% '_builtin.local_constraint' | constraint_then(ast) %}
     v}
  *)

  let expand_n_ary_expr expr =
    match Expression.match_payload_jane_syntax feature expr with
    | expr, suffix, payload -> begin
        match Attribute_node.of_suffix suffix, payload with
        | No_payload t, PStr [] -> Some t
        | Payload f, payload -> Some (f payload ~loc:expr.pexp_loc)
        | No_payload _, payload ->
            Desugaring_error.raise expr (Has_payload payload)
        | Unknown_suffix, _ -> None
      end
    | exception _ -> None

  let require_function_cases expr ~arity_attribute =
    match expr.pexp_desc with
    | Pexp_function cases -> cases
    | _ -> Desugaring_error.raise expr (Expected_function_cases arity_attribute)

  let constraint_modes expr : mode_annotation loc list =
    match expand_n_ary_expr expr with
    | Some (Mode_constraint modes) -> modes
    | _ -> []

  let check_constraint expr =
    match expr.pexp_desc with
    | Pexp_constraint (e, ty) ->
        let mode_annotations = constraint_modes expr in
        Some ({ mode_annotations; type_constraint = Pconstraint ty }, e)
    | Pexp_coerce (e, ty1, ty2) ->
        let mode_annotations = constraint_modes expr in
        Some ({ mode_annotations; type_constraint = Pcoerce (ty1, ty2) }, e)
    | _ -> None

  let require_constraint expr =
    match check_constraint expr with
    | Some constraint_ -> constraint_
    | None -> Desugaring_error.raise expr Expected_constraint_or_coerce

  let check_param pexp_desc (pexp_loc : Location.t) ~layout =
    match pexp_desc, layout with
    | Pexp_fun (lbl, def, pat, body), None ->
        let pparam_loc : Location.t =
          { loc_ghost = true;
            loc_start = pexp_loc.loc_start;
            loc_end = pat.ppat_loc.loc_end;
          }
        in
        let pparam_desc = Pparam_val (lbl, def, pat) in
        Some ({ pparam_desc; pparam_loc }, body)
    | Pexp_newtype (newtype, body), layout ->
        (* This imperfectly estimates where a newtype parameter ends: it uses
           the end of the type name rather than the closing paren. The closing
           paren location is not tracked anywhere in the parsetree. We don't
           think merlin is affected.
        *)
        let pparam_loc : Location.t =
          { loc_ghost = true;
            loc_start = pexp_loc.loc_start;
            loc_end = newtype.loc.loc_end;
          }
        in
        let pparam_desc = Pparam_newtype (newtype, layout) in
        Some ({ pparam_desc; pparam_loc }, body)
    | _, None -> None
    | _, Some layout ->
        Desugaring_error.raise_with_loc pexp_loc
          (Expected_newtype_with_layout_annotation layout)

  let require_param pexp_desc pexp_loc ~arity_attribute ~layout =
    match check_param pexp_desc pexp_loc ~layout with
    | Some x -> x
    | None ->
        Desugaring_error.raise_with_loc pexp_loc
          (Expected_fun_or_newtype arity_attribute)

  (* Should only be called on [Pexp_fun] and [Pexp_newtype]. *)
  let extract_fun_params =
    let open struct
      type continue_or_stop =
        | Continue of Parsetree.expression
        | Stop of function_constraint option * function_body
    end
    in
    (* Returns: the next parameter, together with whether there are possibly
       more parameters available ("Continue") or whether all parameters have
       been consumed ("Stop").

       The returned attributes are the remaining unconsumed attributes on the
       Pexp_fun or Pexp_newtype node.

       The [layout] parameter gives the layout at which to interpret the type
       introduced by [expr = Pexp_newtype _]. It is only supplied in a recursive
       call to [extract_next_fun_param] in the event that it sees a
       [Layout_annotation] attribute.
    *)
    let rec extract_next_fun_param expr ~layout
        : function_param option * continue_or_stop
      =
      match expand_n_ary_expr expr with
      | None -> begin
          match check_param expr.pexp_desc expr.pexp_loc ~layout with
          | Some (param, body) ->
              Some param, Continue body
          | None ->
              None, Stop (None, Pfunction_body expr)
        end
      | Some Top_level -> None, Stop (None, Pfunction_body expr)
      | Some (Layout_annotation next_layout) ->
          extract_next_fun_param expr ~layout:(Some next_layout)
      | Some (Mode_constraint _) ->
          let function_constraint, body = require_constraint expr in
          None, Stop (Some function_constraint, Pfunction_body body)
      | Some (Fun_then after_fun as arity_attribute) ->
          let param, body =
            require_param expr.pexp_desc expr.pexp_loc ~arity_attribute ~layout
          in
          let continue_or_stop =
            match after_fun with
            | Cases ->
                let cases = require_function_cases body ~arity_attribute in
                let function_body =
                  Pfunction_cases
                    (cases, body.pexp_loc, body.pexp_attributes)
                in
                Stop (None, function_body)
            | Constraint_then_cases ->
                let function_constraint, body = require_constraint body in
                let cases = require_function_cases body ~arity_attribute in
                let function_body =
                  Pfunction_cases
                    (cases, body.pexp_loc, body.pexp_attributes)
                in
                Stop (Some function_constraint, function_body)
          in
          Some param, continue_or_stop
    in
    let rec loop expr ~rev_params =
      let next_param, continue_or_stop =
        extract_next_fun_param expr ~layout:None
      in
      let rev_params =
        match next_param with
        | None -> rev_params
        | Some x -> x :: rev_params
      in
      match continue_or_stop with
      | Continue body -> loop body ~rev_params
      | Stop (function_constraint, body) ->
          let params = List.rev rev_params in
          params, function_constraint, body
    in
    fun expr ->
      begin match expr.pexp_desc with
        | Pexp_newtype _ | Pexp_fun _ -> ()
        | _ ->
            Misc.fatal_error "called on something that isn't a newtype or fun"
      end;
      begin match extract_next_fun_param expr ~layout:None with
      | Some _, _ -> ()
      | None, _ -> Desugaring_error.raise expr Parameterless_function
      end;
      loop expr ~rev_params:[]

  (* Returns remaining unconsumed attributes on outermost expression *)
  let of_expr =
    let function_without_additional_params cases constraint_ loc : expression =
      (* If the outermost node is function cases, we place the
          attributes on the function node as a whole rather than on the
          [Pfunction_cases] body.
      *)
      [], constraint_, Pfunction_cases (cases, loc, [])
    in
    (* Hack: be more permissive toward a way that a ppx can mishandle an
       attribute, which is to duplicate the top-level Jane Syntax
       attribute.
    *)
    let rec remove_top_level_attributes expr =
      match expand_n_ary_expr expr with
      | Some Top_level -> remove_top_level_attributes expr
      | _ -> expr
    in
    fun expr ->
      let expr = remove_top_level_attributes expr in
      match expr.pexp_desc with
      | Pexp_fun _ | Pexp_newtype _ -> Some (extract_fun_params expr)
      | Pexp_function cases ->
          let n_ary =
            function_without_additional_params cases None expr.pexp_loc
          in
          Some n_ary
      | _ -> begin
          match check_constraint expr with
          | Some (constraint_, { pexp_desc = Pexp_function cases }) ->
              let n_ary =
                function_without_additional_params cases (Some constraint_)
                  expr.pexp_loc
              in
              Some n_ary
          | _ -> None
        end

  let n_ary_function_expr ext x =
    let suffix, payload = Attribute_node.to_suffix_and_payload ext in
    Ast_of.wrap_jane_syntax ?payload suffix x

  let expr_of =
    let add_param ?after_fun_attribute { pparam_desc; pparam_loc } body =
      let fun_ =
        let loc =
          { !Ast_helper.default_loc with loc_start = pparam_loc.loc_start }
        in
        match pparam_desc with
        | Pparam_val (label, default, pat) ->
            (Ast_helper.Exp.fun_ label default pat body ~loc
              [@alert "-prefer_jane_syntax"])
        | Pparam_newtype (newtype, layout) ->
            match layout with
            | None -> Ast_helper.Exp.newtype newtype body ~loc
            | Some layout ->
                n_ary_function_expr
                  (Layout_annotation layout)
                  (Ast_helper.Exp.newtype newtype body ~loc)
      in
      match after_fun_attribute with
      | None -> fun_
      | Some after_fun -> n_ary_function_expr (Fun_then after_fun) fun_
    in
    fun ~loc (params, constraint_, function_body) ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        let body =
          match function_body with
          | Pfunction_body body -> body
          | Pfunction_cases (cases, loc, attrs) ->
              (Ast_helper.Exp.function_ cases ~loc ~attrs
                 [@alert "-prefer_jane_syntax"])
        in
        let possibly_constrained_body =
          match constraint_ with
          | None -> body
          | Some { mode_annotations; type_constraint } ->
              let constrained_body =
                let loc = Location.ghostify body.pexp_loc in
                match type_constraint with
                | Pconstraint ty -> Ast_helper.Exp.constraint_ body ty ~loc
                | Pcoerce (ty1, ty2) -> Ast_helper.Exp.coerce body ty1 ty2 ~loc
              in
              match mode_annotations with
              | _ :: _ as mode_annotations ->
                  n_ary_function_expr
                    (Mode_constraint mode_annotations)
                    constrained_body
              | [] -> constrained_body
        in
        match params with
        | [] -> possibly_constrained_body
        | params ->
            let init_params, last_param = Misc.split_last params in
            let after_fun_attribute : Attribute_node.after_fun option =
              match constraint_, function_body with
              | Some _, Pfunction_cases _ -> Some Constraint_then_cases
              | None, Pfunction_cases _ -> Some Cases
              | Some _, Pfunction_body _ -> None
              | None, Pfunction_body _ -> None
            in
            let body_with_last_param =
              add_param last_param ?after_fun_attribute
                possibly_constrained_body
            in
            List.fold_right add_param init_params body_with_last_param)
end

(** [include functor] *)
module Include_functor = struct
  type signature_item =
    | Ifsig_include_functor of include_description

  type structure_item =
    | Ifstr_include_functor of include_declaration

  let feature : Feature.t = Language_extension Include_functor

  let sig_item_of ~loc = function
    | Ifsig_include_functor incl ->
        (* See Note [Wrapping with make_entire_jane_syntax] *)
        Signature_item.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Sig.include_ incl)

  let of_sig_item sigi = match sigi.psig_desc with
    | Psig_include incl -> Ifsig_include_functor incl
    | _ -> failwith "Malformed [include functor] in signature"

  let str_item_of ~loc = function
    | Ifstr_include_functor incl ->
        (* See Note [Wrapping with make_entire_jane_syntax] *)
        Structure_item.make_entire_jane_syntax ~loc feature (fun () ->
          Ast_helper.Str.include_ incl)

  let of_str_item stri = match stri.pstr_desc with
    | Pstr_include incl -> Ifstr_include_functor incl
    | _ -> failwith "Malformed [include functor] in structure"
end

(** Module strengthening *)
module Strengthen = struct
  type nonrec module_type =
    { mty : Parsetree.module_type; mod_id : Longident.t Location.loc }

  let feature : Feature.t = Language_extension Module_strengthening

  (* Encoding: [S with M] becomes [functor (_ : S) -> (module M)], where
     the [(module M)] is a [Pmty_alias].  This isn't syntax we can write, but
     [(module M)] can be the inferred type for [M], so this should be fine. *)

  let mty_of ~loc { mty; mod_id } =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Module_type.make_entire_jane_syntax ~loc feature (fun () ->
      Ast_helper.Mty.functor_ (Named (Location.mknoloc None, mty))
        (Ast_helper.Mty.alias mod_id))

  let of_mty mty = match mty.pmty_desc with
    | Pmty_functor(Named(_, mty), {pmty_desc = Pmty_alias mod_id}) ->
       { mty; mod_id }
    | _ -> failwith "Malformed strengthened module type"
end

(** Layouts *)
module Layouts = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Layouts
  end

  include Ext

  type constant =
    | Float of string * char option
    | Integer of string * char

  type nonrec expression =
    | Lexp_constant of constant
    | Lexp_newtype of string loc * layout_annotation * expression

  type nonrec pattern =
    | Lpat_constant of constant

  type nonrec core_type =
    | Ltyp_var of { name : string option
                  ; layout : Asttypes.layout_annotation }
    | Ltyp_poly of { bound_vars : (string loc * layout_annotation option) list
                   ; inner_type : core_type }
    | Ltyp_alias of { aliased_type : core_type
                    ; name : string option
                    ; layout : Asttypes.layout_annotation }

  type nonrec extension_constructor =
    | Lext_decl of (string Location.loc *
                    Asttypes.layout_annotation option) list *
                   constructor_arguments *
                   Parsetree.core_type option

  (*******************************************************)
  (* Errors *)

  module Desugaring_error = struct
    type error =
      | No_integer_suffix
      | Unexpected_constant of Parsetree.constant

    let report_error ~loc = function
      | No_integer_suffix ->
        Location.errorf ~loc
          "All unboxed integers require a suffix to determine their size."
      | Unexpected_constant c ->
        Location.errorf ~loc
          "Unexpected unboxed constant:@ %a"
          (Printast.constant) c

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise ~loc err = raise (Error(loc, err))
  end

  module Encode = Layout_annotation.Encode
  module Decode = Layout_annotation.Decode

  (*******************************************************)
  (* Constants *)

  let constant_of = function
    | Float (x, suffix) -> Pconst_float (x, suffix)
    | Integer (x, suffix) -> Pconst_integer (x, Some suffix)

  let of_constant ~loc = function
    | Pconst_float (x, suffix) -> Float (x, suffix)
    | Pconst_integer (x, Some suffix) -> Integer (x, suffix)
    | Pconst_integer (_, None) ->
      Desugaring_error.raise ~loc No_integer_suffix
    | const -> Desugaring_error.raise ~loc (Unexpected_constant const)

  (*******************************************************)
  (* Encoding expressions *)

  let expr_of ~loc expr =
    let module Ast_of = Ast_of (Expression) (Ext) in
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature begin fun () ->
      match expr with
      | Lexp_constant c ->
        let constant = constant_of c in
        Ast_of.wrap_jane_syntax ["unboxed"] @@
        Ast_helper.Exp.constant constant
      | Lexp_newtype (name, layout, inner_expr) ->
        let payload = Encode.as_payload layout in
        Ast_of.wrap_jane_syntax ["newtype"] ~payload @@
        Ast_helper.Exp.newtype name inner_expr
    end

  (*******************************************************)
  (* Desugaring expressions *)

  let of_expr expr =
    let loc = expr.pexp_loc in
    let expr, subparts, payload =
      Expression.match_payload_jane_syntax feature expr
    in
    match subparts, expr.pexp_desc, payload with
    | [ "unboxed" ], Pexp_constant const, PStr [] ->
        Lexp_constant (of_constant ~loc const)
    | [ "newtype" ], Pexp_newtype (name, inner_expr), payload ->
        let layout = Decode.from_payload ~loc payload in
        Lexp_newtype (name, layout, inner_expr)
    | _ ->
	      Expression.raise_partial_payload_match feature expr subparts payload

  (*******************************************************)
  (* Encoding patterns *)

  let pat_of ~loc t =
    Pattern.make_entire_jane_syntax ~loc feature begin fun () ->
      match t with
      | Lpat_constant c ->
        let constant = constant_of c in
        Ast_helper.Pat.constant constant
    end

  (*******************************************************)
  (* Desugaring patterns *)

  let of_pat pat =
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_constant const -> Lpat_constant (of_constant ~loc const)
    | _ -> Pattern.raise_partial_match feature pat []

  (*******************************************************)
  (* Encoding types *)

  module Type_of = Ast_of (Core_type) (Ext)

  let type_of ~loc typ =
    let exception No_wrap_necessary of Parsetree.core_type in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature begin fun () ->
        match typ with
        | Ltyp_var { name; layout } ->
          let payload = Encode.as_payload layout in
          Type_of.wrap_jane_syntax ["var"] ~payload @@
          begin match name with
          | None -> Ast_helper.Typ.any ~loc ()
          | Some name -> Ast_helper.Typ.var ~loc name
          end
        | Ltyp_poly { bound_vars; inner_type } ->
          let var_names, layouts = List.split bound_vars in
          (* Pass the loc because we don't want a ghost location here *)
          let tpoly = Ast_helper.Typ.poly ~loc var_names inner_type in
          if List.for_all Option.is_none layouts
          then raise (No_wrap_necessary tpoly)
          else
            let payload = Encode.option_list_as_payload layouts in
            Type_of.wrap_jane_syntax ["poly"] ~payload tpoly

        | Ltyp_alias { aliased_type; name; layout } ->
          let payload = Encode.as_payload layout in
          let has_name, inner_typ = match name with
            | None -> "anon", aliased_type
            | Some name -> "named", Ast_helper.Typ.alias aliased_type name
          in
          Type_of.wrap_jane_syntax ["alias"; has_name] ~payload inner_typ
      end
    with
      No_wrap_necessary result_type -> result_type

  (*******************************************************)
  (* Desugaring types *)

  let of_type typ =
    let loc = typ.ptyp_loc in
    let typ, subparts, payload =
      Core_type.match_payload_jane_syntax feature typ
    in
    match subparts, typ.ptyp_desc, payload with
    | [ "var" ], _, _ ->
        let layout = Decode.from_payload ~loc payload in
        let name = match typ.ptyp_desc with
          | Ptyp_any -> None
          | Ptyp_var name -> Some name
          | _ ->
              Core_type.raise_partial_payload_match feature typ subparts payload
        in
        Ltyp_var { name; layout }
    | [ "poly" ], Ptyp_poly (var_names, inner_type), PStr [] ->
        let bound_vars =
          Decode.bound_vars_from_vars_and_payload ~loc var_names payload
        in
        Ltyp_poly { bound_vars; inner_type }
    | [ "alias"; "anon" ], _, _ ->
        let layout = Decode.from_payload ~loc payload in
        Ltyp_alias { aliased_type = typ
                   ; name = None
                   ; layout }
    | [ "alias"; "named" ], Ptyp_alias (inner_type, name), _ ->
        let layout = Decode.from_payload ~loc payload in
        Ltyp_alias { aliased_type = inner_type
                   ; name = Some name
                   ; layout }
    | _ ->
	      Core_type.raise_partial_payload_match feature typ subparts payload

  (*******************************************************)
  (* Encoding extension constructor *)

  module Ext_ctor_of = Ast_of (Extension_constructor) (Ext)

  let extension_constructor_of ~loc ~name ?info ?docs ext =
    (* using optional parameters to hook into existing defaulting
       in [Ast_helper.Te.decl], which seems unwise to duplicate *)
    let exception No_wrap_necessary of Parsetree.extension_constructor in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
        Extension_constructor.make_entire_jane_syntax ~loc feature
          begin fun () ->
            match ext with
            | Lext_decl (bound_vars, args, res) ->
              let vars, layouts = List.split bound_vars in
              let ext_ctor =
                (* Pass ~loc here, because the constructor declaration is
                   not a ghost *)
                Ast_helper.Te.decl ~loc ~vars ~args ?info ?docs ?res name
              in
              if List.for_all Option.is_none layouts
              then raise (No_wrap_necessary ext_ctor)
              else
                let payload = Encode.option_list_as_payload layouts in
                Ext_ctor_of.wrap_jane_syntax ["ext"] ~payload ext_ctor
          end
    with
      No_wrap_necessary ext_ctor -> ext_ctor

  (*******************************************************)
  (* Desugaring extension constructor *)

  let of_extension_constructor ext =
    let loc = ext.pext_loc in
    let ext, subparts, payload =
      Extension_constructor.match_payload_jane_syntax feature ext
    in
    match subparts, ext.pext_kind with
    | [ "ext" ], Pext_decl (var_names, args, res) ->
        let bound_vars =
          Decode.bound_vars_from_vars_and_payload ~loc var_names payload
        in
        Lext_decl (bound_vars, args, res)
    | _ ->
        Extension_constructor.raise_partial_payload_match
          feature ext subparts payload

  (*********************************************************)
  (* Constructing a [constructor_declaration] with layouts *)

  module Ctor_decl_of = Ast_of (Constructor_declaration) (Ext)

  let constructor_declaration_of ~loc ~attrs ~info ~vars_layouts ~args
        ~res name =
    let vars, layouts = List.split vars_layouts in
    let ctor_decl =
      Ast_helper.Type.constructor ~loc ~info ~vars ~args ?res name
    in
    let ctor_decl =
      if List.for_all Option.is_none layouts
      then ctor_decl
      else
        let payload = Encode.option_list_as_payload layouts in
        Constructor_declaration.make_entire_jane_syntax ~loc feature
          begin fun () ->
            Ctor_decl_of.wrap_jane_syntax ["vars"] ~payload ctor_decl
          end
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
      let ctor_decl, subparts, payload =
        Constructor_declaration.match_payload_jane_syntax feature ctor_decl
      in
      begin match subparts with
      | [ "vars" ] ->
        Some
          (Decode.bound_vars_from_vars_and_payload
             ~loc ctor_decl.pcd_vars payload)
      | _ ->
        Constructor_declaration.raise_partial_payload_match
          feature ctor_decl subparts payload
      end
    | _ ->
      None

  let of_constructor_declaration =
    Constructor_declaration.make_of_ast
       ~of_ast_internal:of_constructor_declaration_internal
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
  val ast_of : loc:Location.t -> t -> ast
end

module Core_type = struct
  type t =
    | Jtyp_modes of Modes.core_type
    | Jtyp_layout of Layouts.core_type

  let of_ast_internal (feat : Feature.t) typ = match feat with
    | Language_extension Local ->
      Some (Jtyp_modes (Modes.of_local_type (Local.of_type typ)))
    | Language_extension Unique ->
      Some (Jtyp_modes (Modes.of_unique_type (Unique.of_type typ)))
    | Language_extension Layouts ->
      Some (Jtyp_layout (Layouts.of_type typ))
    | _ ->
      None

  let of_ast = Core_type.make_of_ast ~of_ast_internal

  let ast_of ~loc (jtyp, attrs) =
    Core_type.add_attributes attrs @@
    match jtyp with
    | Jtyp_modes x -> Modes.type_of ~loc x
    | Jtyp_layout x -> Layouts.type_of ~loc x
end

module Constructor_argument = struct
  type t =
    | Jcarg_modes of Modes.constructor_argument

  let of_ast_internal (feat : Feature.t) carg = match feat with
    | Language_extension Local ->
      Some (Jcarg_modes (Modes.of_local_constr_arg (Local.of_constr_arg carg)))
    | _ ->
      None

  let of_ast = Constructor_argument.make_of_ast ~of_ast_internal

  let ast_of ~loc jcarg = match jcarg with
    | Jcarg_modes x -> Modes.constr_arg_of ~loc x
end

module Expression = struct
  type t =
    | Jexp_modes of Modes.expression
    | Jexp_comprehension of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression
    | Jexp_layout of Layouts.expression
    | Jexp_n_ary_function  of N_ary_functions.expression

  let of_ast_internal (feat : Feature.t) expr = match feat with
    | Language_extension Local ->
      Some (Jexp_modes (Modes.of_local_expr (Local.of_expr expr)))
    | Language_extension Unique ->
      Some (Jexp_modes (Modes.of_unique_expr (Unique.of_expr expr)))
    | Language_extension Comprehensions ->
      Some (Jexp_comprehension (Comprehensions.of_expr expr))
    | Language_extension Immutable_arrays ->
      Some (Jexp_immutable_array (Immutable_arrays.of_expr expr))
    | Language_extension Layouts ->
      Some (Jexp_layout (Layouts.of_expr expr))
    | Builtin -> begin
        match N_ary_functions.of_expr expr with
        | Some expr -> Some (Jexp_n_ary_function expr)
        | None -> None
      end
    | _ -> None

  let of_ast = Expression.make_of_ast ~of_ast_internal

  let ast_of ~loc (jexp, attrs) =
    Expression.add_attributes attrs @@
    match jexp with
    | Jexp_modes           x -> Modes.expr_of            ~loc x
    | Jexp_comprehension   x -> Comprehensions.expr_of   ~loc x
    | Jexp_immutable_array x -> Immutable_arrays.expr_of ~loc x
    | Jexp_layout          x -> Layouts.expr_of          ~loc x
    | Jexp_n_ary_function  x -> N_ary_functions.expr_of  ~loc x
end

module Pattern = struct
  type t =
    | Jpat_modes of Modes.pattern
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_layout of Layouts.pattern

  let of_ast_internal (feat : Feature.t) pat = match feat with
    | Language_extension Local ->
      Some (Jpat_modes (Modes.of_local_pat (Local.of_pat pat)))
    | Language_extension Unique ->
      Some (Jpat_modes (Modes.of_unique_pat (Unique.of_pat pat)))
    | Language_extension Immutable_arrays ->
      Some (Jpat_immutable_array (Immutable_arrays.of_pat pat))
    | Language_extension Layouts ->
      Some (Jpat_layout (Layouts.of_pat pat))
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let ast_of ~loc (jpat, attrs) =
    Pattern.add_attributes attrs @@
    match jpat with
    | Jpat_modes x -> Modes.pat_of ~loc x
    | Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc x
    | Jpat_layout x -> Layouts.pat_of ~loc x
end

module Module_type = struct
  type t =
    | Jmty_strengthen of Strengthen.module_type

  let of_ast_internal (feat : Feature.t) mty = match feat with
    | Language_extension Module_strengthening ->
      Some (Jmty_strengthen (Strengthen.of_mty mty))
    | _ -> None

  let of_ast = Module_type.make_of_ast ~of_ast_internal

  let ast_of ~loc (jmty, attrs) =
    Module_type.add_attributes attrs @@
    match jmty with
    | Jmty_strengthen x -> Strengthen.mty_of ~loc x
end

module Signature_item = struct
  type t =
    | Jsig_include_functor of Include_functor.signature_item

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Include_functor ->
      Some (Jsig_include_functor (Include_functor.of_sig_item sigi))
    | _ -> None

  let of_ast = Signature_item.make_of_ast ~of_ast_internal

  let ast_of ~loc jsig = match jsig with
    | Jsig_include_functor x -> Include_functor.sig_item_of ~loc x
end

module Structure_item = struct
  type t =
    | Jstr_include_functor of Include_functor.structure_item

  let of_ast_internal (feat : Feature.t) stri =
    match feat with
    | Language_extension Include_functor ->
      Some (Jstr_include_functor (Include_functor.of_str_item stri))
    | _ -> None

  let of_ast = Structure_item.make_of_ast ~of_ast_internal

  let ast_of ~loc jstr = match jstr with
    | Jstr_include_functor x -> Include_functor.str_item_of ~loc x
end

module Extension_constructor = struct
  type t =
    | Jext_layout of Layouts.extension_constructor

  let of_ast_internal (feat : Feature.t) ext = match feat with
    | Language_extension Layouts ->
      Some (Jext_layout (Layouts.of_extension_constructor ext))
    | _ ->
      None

  let of_ast = Extension_constructor.make_of_ast ~of_ast_internal

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs t =
    let ext_ctor =
      match t with
      | Jext_layout lext ->
          Layouts.extension_constructor_of ~loc ~name ?info ?docs lext
    in
    Extension_constructor.add_attributes attrs ext_ctor

  let ast_of ~loc:_ = assert false
end
