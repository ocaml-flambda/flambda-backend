open Asttypes
open Parsetree
open Jane_syntax_parsing

(****************************************)
(* Helpers used just within this module *)

module type Extension_string = sig
  val feature : Feature.t
  val extension_string : string
end

module Ast_of (AST : AST)
              (Ext : Extension_string) : sig
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

module Of_ast (Ext : Extension_string) : sig
  (* Find and remove a jane-syntax attribute marker, throwing an exception
     if the attribute name does not have the right format or extension. *)
  val unwrap_jane_syntax_attributes :
    loc:Location.t ->
    attributes ->
    string list * payload * attributes
end = struct
  module Desugaring_error = struct
    type error =
      | Not_this_embedding of Embedded_name.t
      | Non_embedding

    let report_error ~loc = function
      | Not_this_embedding name ->
          Location.errorf ~loc
            "Tried to desugar the embedded term %a@ \
             as belonging to the %s extension"
            Embedded_name.pp_quoted_name name Ext.extension_string
      | Non_embedding ->
          Location.errorf ~loc
            "Tried to desugar a non-embedded expression@ \
             as belonging to the %s extension"
            Ext.extension_string

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

  let unwrap_jane_syntax_attributes ~loc attrs =
    match find_and_remove_jane_syntax_attribute attrs with
    | Some (ext_name, _loc, payload, attrs) -> begin
        match Jane_syntax_parsing.Embedded_name.components ext_name with
        | extension_occur :: names
             when String.equal extension_occur Ext.extension_string ->
           names, payload, attrs
        | _ ->
           Desugaring_error.raise ~loc (Not_this_embedding ext_name)
      end
    | None -> Desugaring_error.raise ~loc Non_embedding
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
*)

(** List and array comprehensions *)
module Comprehensions = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Comprehensions
    let extension_string = Feature.extension_component feature
  end

  module Ast_of = Ast_of (Expression) (Ext)
  module Of_ast = Of_ast (Ext)

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

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in
      [expr_of_comprehension_expr]. *)

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
        Ast_of.wrap_jane_syntax ["for"; "in"] seq

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
    (* We elect to wrap the body in a new AST node (here, [Pexp_lazy])
       because it makes it so there is no AST node that can carry multiple Jane
       Syntax-related attributes in addition to user-written attributes. This
       choice simplifies the definition of [comprehension_expr_of_expr], as
       part of its contract is threading through the user-written attributes
       on the outermost node.
    *)
    Ast_of.wrap_jane_syntax
      type_
      (Ast_helper.Exp.lazy_
        (List.fold_right
          expr_of_clause
          clauses
          (Ast_of.wrap_jane_syntax ["body"] body)))

  let expr_of ~loc ~attrs cexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    let expr = Expression.make_entire_jane_syntax ~loc feature (fun () ->
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
    in
    { expr with pexp_attributes = expr.pexp_attributes @ attrs }

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

  (* Returns the expression node with the outermost Jane Syntax-related
     attribute removed. *)
  let expand_comprehension_extension_expr expr =
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes ~loc:expr.pexp_loc expr.pexp_attributes
    in
    match payload with
    | PStr [] -> names, { expr with pexp_attributes = attributes }
    | _ -> Desugaring_error.raise expr (Has_payload payload)

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
        Desugaring_error.raise expr (Bad_comprehension_embedding bad)

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let comprehension_of_expr =
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
          Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    fun expr ->
      match raw_comprehension_of_expr expr with
      | { body = _; clauses = [] } ->
          Desugaring_error.raise expr No_clauses
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
      | bad, _ ->
          Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    comp, wrapper.pexp_attributes
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list

  type nonrec pattern =
    | Iapat_immutable_array of pattern list

  let feature : Feature.t = Language_extension Immutable_arrays

  let expr_of ~loc ~attrs = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Expression.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Exp.array ~attrs elts)

  (* Returns remaining unconsumed attributes *)
  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts, expr.pexp_attributes
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc ~attrs = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Pattern.make_entire_jane_syntax ~loc feature (fun () ->
        Ast_helper.Pat.array ~attrs elts)

  (* Returns remaining unconsumed attributes *)
  let of_pat pat = match pat.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts, pat.ppat_attributes
    | _ -> failwith "Malformed immutable array pattern"
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

  let mty_of ~loc ~attrs { mty; mod_id } =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Module_type.make_entire_jane_syntax ~loc feature (fun () ->
      Ast_helper.Mty.functor_ ~attrs (Named (Location.mknoloc None, mty))
        (Ast_helper.Mty.alias mod_id))

  (* Returns remaining unconsumed attributes *)
  let of_mty mty = match mty.pmty_desc with
    | Pmty_functor(Named(_, mty), {pmty_desc = Pmty_alias mod_id}) ->
       { mty; mod_id }, mty.pmty_attributes
    | _ -> failwith "Malformed strengthened module type"
end

(** Layouts *)
module Layouts = struct
  module Ext = struct
    let feature : Feature.t = Language_extension Layouts
    let extension_string = Feature.extension_component feature
  end

  include Ext

  module Of_ast = Of_ast (Ext)

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
      | Not_a_layout of Parsetree.payload
      | Unexpected_wrapped_type of Parsetree.core_type
      | Unexpected_wrapped_ext of Parsetree.extension_constructor
      | Unexpected_attribute of string list
      | Wrong_number_of_layouts of int * layout_annotation option list
      | No_integer_suffix
      | Unexpected_constant of Parsetree.constant
      | Unexpected_wrapped_expr of Parsetree.expression
      | Unexpected_wrapped_pat of Parsetree.pattern

    let report_error ~loc = function
      | Not_a_layout payload ->
        Location.errorf ~loc
          "Layout attribute does not name a layout:@;%a"
          (Printast.payload 0) payload
      | Unexpected_wrapped_type typ ->
        Location.errorf ~loc
          "Layout attribute on wrong core type:@;%a"
          (Printast.core_type 0) typ
      | Unexpected_wrapped_ext ext ->
        Location.errorf ~loc
          "Layout attribute on wrong extension constructor:@;%a"
          (Printast.extension_constructor 0) ext
      | Unexpected_attribute names ->
        Location.errorf ~loc
          "Layout extension does not understand these attribute names:@;[%a]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             Format.pp_print_text) names
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
      | No_integer_suffix ->
        Location.errorf ~loc
          "All unboxed integers require a suffix to determine their size."
      | Unexpected_constant c ->
        Location.errorf ~loc
          "Unexpected unboxed constant:@ %a"
          (Printast.constant) c
      | Unexpected_wrapped_expr expr ->
        Location.errorf ~loc
          "Layout attribute on wrong expression:@;%a"
          (Printast.expression 0) expr
      | Unexpected_wrapped_pat pat ->
        Location.errorf ~loc
          "Layout attribute on wrong pattern:@;%a"
          (Printast.pattern 0) pat

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise ~loc err = raise (Error(loc, err))
  end

  (*******************************************************)
  (* Conversions with a payload *)

  module Encode : sig
    val as_payload : layout_annotation -> payload
    val option_list_as_payload : layout_annotation option list -> payload
  end = struct
    let as_expr layout =
      (* CR layouts v1.5: revise when moving layout recognition away from parser*)
      let layout_string = match layout.txt with
        | Any -> "any"
        | Value -> "value"
        | Void -> "void"
        | Immediate64 -> "immediate64"
        | Immediate -> "immediate"
        | Float64 -> "float64"
      in
      Ast_helper.Exp.ident
        (Location.mkloc (Longident.Lident layout_string) layout.loc)

    let structure_item_of_expr expr =
      { pstr_desc = Pstr_eval (expr, []); pstr_loc = Location.none }

    let structure_item_of_none =
      { pstr_desc = Pstr_attribute { attr_name = Location.mknoloc "none"
                                   ; attr_payload = PStr []
                                   ; attr_loc = Location.none }
      ; pstr_loc = Location.none }

    let as_payload layout =
      let expr = as_expr layout in
      PStr [ structure_item_of_expr expr ]

    let option_list_as_payload layouts =
      let items =
        List.map (function
          | None -> structure_item_of_none
          | Some layout -> structure_item_of_expr (as_expr layout))
          layouts
      in
      PStr items
  end

  module Decode : sig
    val from_payload : loc:Location.t -> payload -> layout_annotation
    val bound_vars_from_vars_and_payload :
      loc:Location.t -> string Location.loc list -> payload ->
      (string Location.loc * layout_annotation option) list
  end = struct
    exception Unexpected

    let from_expr = function
      | { pexp_desc = Pexp_ident layout_lid; _ } ->
        (* CR layouts v1.5: revise when moving layout recognition away from parser*)
        let layout = match Longident.last layout_lid.txt with
          | "any" -> Any
          | "value" -> Value
          | "void" -> Void
          | "immediate" -> Immediate
          | "immediate64" -> Immediate64
          | "float64" -> Float64
          | _ -> raise Unexpected
        in
        Location.mkloc layout layout_lid.loc
      | _ -> raise Unexpected

    let expr_of_structure_item = function
      | { pstr_desc = Pstr_eval (expr, _) } -> expr
      | _ -> raise Unexpected

    let is_none_structure_item = function
      | { pstr_desc = Pstr_attribute { attr_name = { txt = "none" } } } -> true
      | _ -> false

    let from_payload ~loc payload =
      try
        match payload with
        | PStr [ item ] -> from_expr (expr_of_structure_item item)
        | _ -> raise Unexpected
      with
        Unexpected -> Desugaring_error.raise ~loc (Not_a_layout payload)

    let option_list_from_payload ~loc payload =
      try
        match payload with
        | PStr items ->
          List.map (fun item ->
            if is_none_structure_item item
            then None
            else Some (from_expr (expr_of_structure_item item)))
            items
        | _ -> raise Unexpected
      with
        Unexpected -> Desugaring_error.raise ~loc (Not_a_layout payload)

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

  let expr_of ~loc ~attrs expr =
    let module Ast_of = Ast_of (Expression) (Ext) in
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    Expression.make_entire_jane_syntax ~loc feature begin fun () ->
      match expr with
      | Lexp_constant c ->
        let constant = constant_of c in
        Ast_of.wrap_jane_syntax ["unboxed"] @@
        Ast_helper.Exp.constant ~attrs constant
      | Lexp_newtype (name, layout, inner_expr) ->
        let payload = Encode.as_payload layout in
        Ast_of.wrap_jane_syntax ["newtype"] ~payload @@
        Ast_helper.Exp.newtype ~attrs name inner_expr
    end

  (*******************************************************)
  (* Desugaring expressions *)

  let of_expr expr =
    let loc = expr.pexp_loc in
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes ~loc expr.pexp_attributes
    in
    let lexpr = match names with
      | [ "unboxed" ] ->
        begin match expr.pexp_desc with
        | Pexp_constant const -> Lexp_constant (of_constant ~loc const)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_expr expr)
        end
      | [ "newtype" ] ->
        let layout = Decode.from_payload ~loc payload in
        begin match expr.pexp_desc with
        | Pexp_newtype (name, inner_expr) ->
          Lexp_newtype (name, layout, inner_expr)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_expr expr)
        end
      | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lexpr, attributes

  (*******************************************************)
  (* Encoding patterns *)

  let pat_of ~loc ~attrs t =
    Pattern.make_entire_jane_syntax ~loc feature begin fun () ->
      match t with
      | Lpat_constant c ->
        let constant = constant_of c in
        Ast_helper.Pat.constant ~attrs constant
    end

  (*******************************************************)
  (* Desugaring patterns *)

  let of_pat pat =
    let loc = pat.ppat_loc in
    let lpat = match pat.ppat_desc with
      | Ppat_constant const -> Lpat_constant (of_constant ~loc const)
      | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_pat pat)
    in
    lpat, pat.ppat_attributes

  (*******************************************************)
  (* Encoding types *)

  module Type_of = Ast_of (Core_type) (Ext)

  let type_of ~loc ~attrs typ =
    let exception No_wrap_necessary of Parsetree.core_type in
    try
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      Core_type.make_entire_jane_syntax ~loc feature begin fun () ->
        match typ with
        | Ltyp_var { name; layout } ->
          let payload = Encode.as_payload layout in
          Type_of.wrap_jane_syntax ["var"] ~payload @@
          begin match name with
          | None -> Ast_helper.Typ.any ~loc ~attrs ()
          | Some name -> Ast_helper.Typ.var ~loc ~attrs name
          end
        | Ltyp_poly { bound_vars; inner_type } ->
          let var_names, layouts = List.split bound_vars in
          (* Pass the loc because we don't want a ghost location here *)
          let tpoly = Ast_helper.Typ.poly ~loc ~attrs var_names inner_type in
          if List.for_all Option.is_none layouts
          then raise (No_wrap_necessary tpoly)
          else
            let payload = Encode.option_list_as_payload layouts in
            Type_of.wrap_jane_syntax ["poly"] ~payload tpoly

        | Ltyp_alias { aliased_type; name; layout } ->
          let payload = Encode.as_payload layout in
          let has_name, inner_typ = match name with
            | None -> "anon", { aliased_type with
                                ptyp_attributes =
                                  aliased_type.ptyp_attributes @ attrs }
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
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes ~loc typ.ptyp_attributes
    in
    let lty = match names with
      | [ "var" ] ->
        let layout = Decode.from_payload ~loc payload in
        begin match typ.ptyp_desc with
        | Ptyp_any ->
          Ltyp_var { name = None; layout }
        | Ptyp_var name ->
          Ltyp_var { name = Some name; layout }
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ)
        end

      | [ "poly" ] ->
        begin match typ.ptyp_desc with
        | Ptyp_poly (var_names, inner_type) ->
          let bound_vars =
            Decode.bound_vars_from_vars_and_payload ~loc var_names payload
          in
          Ltyp_poly { bound_vars; inner_type }
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ)
        end

      | [ "alias"; "anon" ] ->
        let layout = Decode.from_payload ~loc payload in
        Ltyp_alias { aliased_type = { typ with ptyp_attributes = attributes }
                   ; name = None
                   ; layout }

      | [ "alias"; "named" ] ->
        let layout = Decode.from_payload ~loc payload in
        begin match typ.ptyp_desc with
        | Ptyp_alias (inner_typ, name) ->
          Ltyp_alias { aliased_type = inner_typ
                     ; name = Some name
                     ; layout }

        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_type typ)
        end

      | _ ->
        Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lty, attributes

  (*******************************************************)
  (* Encoding extension constructor *)

  module Ext_ctor_of = Ast_of (Extension_constructor) (Ext)

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs ext =
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
                Ast_helper.Te.decl ~loc ~attrs ~vars ~args ?info ?docs ?res name
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
    let names, payload, attributes =
      Of_ast.unwrap_jane_syntax_attributes ~loc ext.pext_attributes
    in
    let lext = match names with
      | [ "ext" ] ->
        begin match ext.pext_kind with
        | Pext_decl (var_names, args, res) ->
          let bound_vars =
            Decode.bound_vars_from_vars_and_payload ~loc var_names payload
          in
          Lext_decl (bound_vars, args, res)
        | _ -> Desugaring_error.raise ~loc (Unexpected_wrapped_ext ext)
        end

      | _ ->
        Desugaring_error.raise ~loc (Unexpected_attribute names)
    in
    lext, attributes

  (*********************************************************)
  (* Constructing a [constructor_declaration] with layouts *)

  module Ctor_decl_of = Ast_of (Constructor_declaration) (Ext)

  let constructor_declaration_of ~loc ~attrs ~info ~vars_layouts ~args
        ~res name =
    let vars, layouts = List.split vars_layouts in
    let ctor_decl =
      Ast_helper.Type.constructor ~loc ~attrs ~info ~vars ~args ?res name
    in
    if List.for_all Option.is_none layouts
    then ctor_decl
    else
      let payload = Encode.option_list_as_payload layouts in
      Constructor_declaration.make_entire_jane_syntax ~loc feature
        begin fun () ->
          Ctor_decl_of.wrap_jane_syntax ["vars"] ~payload ctor_decl
        end

  let of_constructor_declaration_internal (feat : Feature.t) ctor_decl =
    match feat with
    | Language_extension Layouts ->
      let loc = ctor_decl.pcd_loc in
      let names, payload, attributes =
        Of_ast.unwrap_jane_syntax_attributes ~loc ctor_decl.pcd_attributes
      in
      let vars_layouts = match names with
        | [ "vars" ] ->
          Decode.bound_vars_from_vars_and_payload
            ~loc ctor_decl.pcd_vars payload
        | _ -> Desugaring_error.raise ~loc (Unexpected_attribute names)
      in
      Some (vars_layouts, attributes)
    | _ -> None

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
end

module Core_type = struct
  type t =
    | Jtyp_layout of Layouts.core_type

  let of_ast_internal (feat : Feature.t) typ = match feat with
    | Language_extension Layouts ->
      let typ, attrs = Layouts.of_type typ in
      Some (Jtyp_layout typ, attrs)
    | _ -> None

  let of_ast = Core_type.make_of_ast ~of_ast_internal
end

module Constructor_argument = struct
  type t = |

  let of_ast_internal (feat : Feature.t) _carg = match feat with
    | _ -> None

  let of_ast = Constructor_argument.make_of_ast ~of_ast_internal
end

module Expression = struct
  type t =
    | Jexp_comprehension of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression
    | Jexp_layout of Layouts.expression

  let of_ast_internal (feat : Feature.t) expr = match feat with
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

  let expr_of ~loc ~attrs = function
    | Jexp_comprehension x -> Comprehensions.expr_of ~loc ~attrs x
    | Jexp_immutable_array x -> Immutable_arrays.expr_of ~loc ~attrs x
    | Jexp_layout x -> Layouts.expr_of ~loc ~attrs x
end

module Pattern = struct
  type t =
    | Jpat_immutable_array of Immutable_arrays.pattern
    | Jpat_layout of Layouts.pattern

  let of_ast_internal (feat : Feature.t) pat = match feat with
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_pat pat in
      Some (Jpat_immutable_array expr, attrs)
    | Language_extension Layouts ->
      let pat, attrs = Layouts.of_pat pat in
      Some (Jpat_layout pat, attrs)
    | _ -> None

  let of_ast = Pattern.make_of_ast ~of_ast_internal

  let pat_of ~loc ~attrs = function
    | Jpat_immutable_array x -> Immutable_arrays.pat_of ~loc ~attrs x
    | Jpat_layout x -> Layouts.pat_of ~loc ~attrs x
end

module Module_type = struct
  type t =
    | Jmty_strengthen of Strengthen.module_type

  let of_ast_internal (feat : Feature.t) mty = match feat with
    | Language_extension Module_strengthening ->
      let mty, attrs = Strengthen.of_mty mty in
      Some (Jmty_strengthen mty, attrs)
    | _ -> None

  let of_ast = Module_type.make_of_ast ~of_ast_internal
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
end

module Extension_constructor = struct
  type t =
    | Jext_layout of Layouts.extension_constructor

  let of_ast_internal (feat : Feature.t) ext = match feat with
    | Language_extension Layouts ->
      let ext, attrs = Layouts.of_extension_constructor ext in
      Some (Jext_layout ext, attrs)
    | _ -> None

  let of_ast = Extension_constructor.make_of_ast ~of_ast_internal

  let extension_constructor_of ~loc ~name ~attrs ?info ?docs = function
    | Jext_layout lext ->
      Layouts.extension_constructor_of ~loc ~name ~attrs ?info ?docs lext
end

