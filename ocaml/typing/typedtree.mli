(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstract syntax tree after typing *)


(** By comparison with {!Parsetree}:
    - Every {!Longindent.t} is accompanied by a resolved {!Path.t}.

*)

open Asttypes
module Uid = Shape.Uid

(* We define a new constant type that can represent unboxed values.
   This is currently used only in [Typedtree], but the long term goal
   is to share this definition with [Lambda] and completely replace the
   usage of [Asttypes.constant] *)
type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int32 of int32
  | Const_int64 of int64
  (* CR mshinwell: This should use [Targetint.t] not [nativeint] *)
  | Const_nativeint of nativeint
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint

(* Value expressions for the core language *)

type partial = Partial | Total

(** {1 Extension points} *)

type attribute = Parsetree.attribute
type attributes = attribute list

(** {1 Core language} *)

type value = Value_pattern
type computation = Computation_pattern

type _ pattern_category =
| Value : value pattern_category
| Computation : computation pattern_category

(* CR zqian: use this field when overwriting is supported. *)
(** Access mode for a field projection, represented by the usage of the record
  immediately following the projection. If the following usage is unique, the
  projection must be borrowed and cannot be moved. If the following usage is
  aliased, the projection can be aliased and moved. *)
type unique_barrier = Mode.Uniqueness.r

type unique_use = Mode.Uniqueness.r * Mode.Linearity.l

type alloc_mode = {
  mode : Mode.Alloc.r;
  locality_context : Env.locality_context option;
}

type texp_field_boxing =
  | Boxing of alloc_mode * unique_use
  (** Projection requires boxing. [unique_use] describes the usage of the
      unboxed field as argument to boxing. *)
  | Non_boxing of unique_use
  (** Projection does not require boxing. [unique_use] describes the usage of
      the field as the result of direct projection. *)

val aliased_many_use : unique_use

type pattern = value general_pattern
and 'k general_pattern = 'k pattern_desc pattern_data

and 'a pattern_data =
  { pat_desc: 'a;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attributes) list;
    pat_type: Types.type_expr;
    pat_env: Env.t;
    pat_attributes: attributes;
   }

and pat_extra =
  | Tpat_constraint of core_type
        (** P : T          { pat_desc = P
                           ; pat_extra = (Tpat_constraint T, _, _) :: ... }
         *)
  | Tpat_type of Path.t * Longident.t loc
        (** #tconst        { pat_desc = disjunction
                           ; pat_extra = (Tpat_type (P, "tconst"), _, _) :: ...}

                           where [disjunction] is a [Tpat_or _] representing the
                           branches of [tconst].
         *)
  | Tpat_open of Path.t * Longident.t loc * Env.t
  | Tpat_unpack
        (** (module P)     { pat_desc  = Tpat_var "P"
                           ; pat_extra = (Tpat_unpack, _, _) :: ... }
            (module _)     { pat_desc  = Tpat_any
            ; pat_extra = (Tpat_unpack, _, _) :: ... }
         *)

and 'k pattern_desc =
  (* value patterns *)
  | Tpat_any : value pattern_desc
        (** _ *)
  | Tpat_var : Ident.t * string loc * Uid.t * Mode.Value.l -> value pattern_desc
        (** x *)
  | Tpat_alias :
      value general_pattern * Ident.t * string loc * Uid.t * Mode.Value.l
        -> value pattern_desc
        (** P as a *)
  | Tpat_constant : constant -> value pattern_desc
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple : (string option * value general_pattern) list -> value pattern_desc
        (** (P1, ..., Pn)                  [(None,P1); ...; (None,Pn)])
            (L1:P1, ... Ln:Pn)             [(Some L1,P1); ...; (Some Ln,Pn)])
            Any mix, e.g. (L1:P1, P2)      [(Some L1,P1); ...; (None,P2)])

            Invariant: n >= 2
         *)
  | Tpat_unboxed_tuple :
      (string option * value general_pattern * Jkind.sort) list ->
      value pattern_desc
        (** #(P1, ..., Pn)              [(None,P1,s1); ...; (None,Pn,sn)])
            #(L1:P1, ... Ln:Pn)         [(Some L1,P1,s1); ...; (Some Ln,Pn,sn)])
            Any mix, e.g. #(L1:P1, P2)  [(Some L1,P1,s1); ...; (None,P2,s2)])

            Invariant: n >= 2
         *)
  | Tpat_construct :
      Longident.t loc * Types.constructor_description *
        value general_pattern list * (Ident.t loc list * core_type) option ->
      value pattern_desc
        (** C                             ([], None)
            C P                           ([P], None)
            C (P1, ..., Pn)               ([P1; ...; Pn], None)
            C (P : t)                     ([P], Some ([], t))
            C (P1, ..., Pn : t)           ([P1; ...; Pn], Some ([], t))
            C (type a) (P : t)            ([P], Some ([a], t))
            C (type a) (P1, ..., Pn : t)  ([P1; ...; Pn], Some ([a], t))
          *)
  | Tpat_variant :
      label * value general_pattern option * Types.row_desc ref ->
      value pattern_desc
        (** `A             (None)
            `A P           (Some P)

            See {!Types.row_desc} for an explanation of the last parameter.
         *)
  | Tpat_record :
      (Longident.t loc * Types.label_description * value general_pattern) list *
        closed_flag ->
      value pattern_desc
        (** { l1=P1; ...; ln=Pn }     (flag = Closed)
            { l1=P1; ...; ln=Pn; _}   (flag = Open)

            Invariant: n > 0
         *)
  | Tpat_array :
      Types.mutability * Jkind.sort * value general_pattern list -> value pattern_desc
        (** [| P1; ...; Pn |]    (flag = Mutable)
            [: P1; ...; Pn :]    (flag = Immutable) *)
  | Tpat_lazy : value general_pattern -> value pattern_desc
        (** lazy P *)
  (* computation patterns *)
  | Tpat_value : tpat_value_argument -> computation pattern_desc
        (** P

            Invariant: Tpat_value pattern should not carry
            pat_attributes or pat_extra metadata coming from user
            syntax, which must be on the inner pattern node -- to
            facilitate searching for a certain value pattern
            constructor with a specific attributed.

            To enforce this restriction, we made the argument of
            the Tpat_value constructor a private synonym of [pattern],
            requiring you to use the [as_computation_pattern] function
            below instead of using the [Tpat_value] constructor directly.
         *)
  | Tpat_exception : value general_pattern -> computation pattern_desc
        (** exception P *)
  (* generic constructions *)
  | Tpat_or :
      'k general_pattern * 'k general_pattern * Types.row_desc option ->
      'k pattern_desc
        (** P1 | P2

            [row_desc] = [Some _] when translating [Ppat_type _],
                         [None] otherwise.
         *)

and tpat_value_argument = private value general_pattern

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attributes) list;
    exp_type: Types.type_expr;
    exp_env: Env.t;
    exp_attributes: attributes;
   }

and exp_extra =
  | Texp_constraint of core_type option * Mode.Alloc.Const.Option.t
        (** E : T @@ M *)
  | Texp_coerce of core_type option * core_type
        (** E :> T           [Texp_coerce (None, T)]
            E : T0 :> T      [Texp_coerce (Some T0, T)]
         *)
  | Texp_poly of core_type option
        (** Used for method bodies. *)
  | Texp_newtype of Ident.t * string loc * Jkind.annotation option * Uid.t
        (** fun (type t : immediate) ->

        The [Ident.t] and [Uid.t] fields are unused by the compiler, but Merlin needs
        them. Merlin cannot be cleanly patched to include these fields because Merlin
        must be able to deserialize typedtrees produced by the compiler. Thus, we include
        them here, as the cost of tracking this additional information is minimal. *)
  | Texp_stack
        (** stack_ E *)

and arg_label = Types.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

(** Jkinds in the typed tree: Compilation of the typed tree to lambda
    sometimes requires jkind information.  Our approach is to
    propagate jkind information inward during compilation.  This
    requires us to annotate places in the typed tree where the jkind
    of a type of a subexpression is not determined by the jkind of the
    type of the expression containing it.  For example, to the left of
    a semicolon, or in value_bindings.

    CR layouts v1.5: Some of these were mainly needed for void (e.g., left of a
    semicolon).  If we redo how void is compiled, perhaps we can drop those.  On
    the other hand, there are some places we're not annotating now (e.g.,
    function arguments) that will need annotations in the future because we'll
    allow other jkinds there.  Just do a rationalization pass on this.
*)
and expression_desc =
    Texp_ident of
      Path.t * Longident.t loc * Types.value_description * ident_kind * unique_use
        (** x
            M.x
         *)
  | Texp_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of
      { params : function_param list;
        body : function_body;
        ret_mode : Mode.Alloc.l;
        (* Mode where the function allocates, ie local for a function of
           type 'a -> local_ 'b, and heap for a function of type 'a -> 'b *)
        ret_sort : Jkind.sort;
        alloc_mode : alloc_mode;
        (* Mode at which the closure is allocated *)
        zero_alloc : Zero_alloc.t;
        (* zero-alloc attributes *)
      }
      (** fun P0 P1 -> function p1 -> e1 | p2 -> e2  (body = Tfunction_cases _)
          fun P0 P1 -> E                             (body = Tfunction_body _)
          This construct has the same arity as the originating
          {{!Jane_syntax.Expression.Jexp_n_ary_function}[Jexp_n_ary_function]}.
          Arity determines when side-effects for effectful parameters are run
          (e.g. optional argument defaults, matching against lazy patterns).
          Parameters' effects are run left-to-right when an n-ary function is
          saturated with n arguments.
      *)
  | Texp_apply of
      expression * (arg_label * apply_arg) list * apply_position *
        Mode.Locality.l * Zero_alloc.assume option
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be Omitted if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, Omitted _);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])

            The [Zero_alloc.assume option] records the optional [@zero_alloc
            assume] attribute that may appear on applications. *)
  | Texp_match of expression * Jkind.sort * computation case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 | exception P3 -> E2
            | exception P4 -> E3

            [Texp_match (E0, sort_of_E0, [(P1, E1); (P2 | exception P3, E2);
                              (exception P4, E3)], _)]
         *)
  | Texp_try of expression * value case list
        (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple of (string option * expression) list * alloc_mode
        (** [Texp_tuple(el)] represents
            - [(E1, ..., En)]
                when [el] is [(None, E1);...;(None, En)],
            - [(L1:E1, ..., Ln:En)]
                when [el] is [(Some L1, E1);...;(Some Ln, En)],
            - Any mix, e.g. [(L1: E1, E2)]
                when [el] is [(Some L1, E1); (None, E2)]
          *)
  | Texp_unboxed_tuple of (string option * expression * Jkind.sort) list
        (** [Texp_unboxed_tuple(el)] represents
            - [#(E1, ..., En)]
                when [el] is [(None, E1, s1);...;(None, En, sn)],
            - [#(L1:E1, ..., Ln:En)]
                when [el] is [(Some L1, E1, s1);...;(Some Ln, En, sn)],
            - Any mix, e.g. [#(L1: E1, E2)]
                when [el] is [(Some L1, E1, s1); (None, E2, s2)]
          *)
  | Texp_construct of
      Longident.t loc * Types.constructor_description *
      expression list * alloc_mode option
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]

            [alloc_mode] is the allocation mode of the construct,
            or [None] if the constructor is [Cstr_unboxed] or [Cstr_constant],
            in which case it does not need allocation.
         *)
  | Texp_variant of label * (expression * alloc_mode) option
        (** [alloc_mode] is the allocation mode of the variant,
            or [None] if the variant has no argument,
            in which case it does not need allocation.
          *)
  | Texp_record of {
      fields : ( Types.label_description * record_label_definition ) array;
      representation : Types.record_representation;
      extended_expression : expression option;
      alloc_mode : alloc_mode option
    }
        (** { l1=P1; ...; ln=Pn }           (extended_expression = None)
            { E0 with l1=P1; ...; ln=Pn }   (extended_expression = Some E0)

            Invariant: n > 0

            If the type is { l1: t1; l2: t2 }, the expression
            { E0 with t2=P2 } is represented as
            Texp_record
              { fields = [| l1, Kept t1; l2 Override P2 |]; representation;
                extended_expression = Some E0 }
            [alloc_mode] is the allocation mode of the record,
            or [None] if it is [Record_unboxed],
            in which case it does not need allocation.
          *)
  | Texp_field of expression * Longident.t loc * Types.label_description *
      texp_field_boxing
    (** [texp_field_boxing] provides extra information depending on if the
        projection requires boxing. *)
  | Texp_setfield of
      expression * Mode.Locality.l * Longident.t loc *
      Types.label_description * expression
    (** [alloc_mode] translates to the [modify_mode] of the record *)
  | Texp_array of Types.mutability * Jkind.Sort.t * expression list * alloc_mode
  | Texp_list_comprehension of comprehension
  | Texp_array_comprehension of Types.mutability * Jkind.sort * comprehension
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * Jkind.sort * expression
  | Texp_while of {
      wh_cond : expression;
      wh_body : expression;
      wh_body_sort : Jkind.sort
    }
  | Texp_for of {
      for_id  : Ident.t;
      for_pat : Parsetree.pattern;
      for_from : expression;
      for_to   : expression;
      for_dir  : direction_flag;
      for_body : expression;
      for_body_sort : Jkind.sort;
    }
  | Texp_send of expression * meth * apply_position
  | Texp_new of
      Path.t * Longident.t loc * Types.class_declaration * apply_position
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Ident.t * string loc * expression) list
  | Texp_letmodule of
      Ident.t option * string option loc * Types.module_presence * module_expr *
        expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression * Location.t
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      param_sort : Jkind.sort;
      body : value case;
      body_sort : Jkind.sort;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  | Texp_open of open_declaration * expression
        (** let open[!] M in e *)
  | Texp_probe of { name:string; handler:expression; enabled_at_init:bool }
  | Texp_probe_is_enabled of { name:string }
  | Texp_exclave of expression
  | Texp_src_pos
    (* A source position value which has been automatically inferred, either
       as a result of [%call_pos] occuring in an expression, or omission of a
       Position argument in function application *)

and function_curry =
  | More_args of { partial_mode : Mode.Alloc.l }
  | Final_arg

and function_param =
  {
    fp_arg_label: arg_label;
    fp_param: Ident.t;
    (** [fp_param] is the identifier that is to be used to name the
        parameter of the function.
    *)
    fp_partial: partial;
    (**
       [fp_partial] =
       [Partial] if the pattern match is partial
       [Total] otherwise.
    *)
    fp_kind: function_param_kind;
    fp_sort: Jkind.sort;
    fp_mode: Mode.Alloc.l;
    fp_curry: function_curry;
    fp_newtypes: (Ident.t * string loc * Jkind.annotation option * Uid.t) list;
    (** [fp_newtypes] are the new type declarations that come *after* that
        parameter. The newtypes that come before the first parameter are
        placed as exp_extras on the Texp_function node. This is just used in
        {!Untypeast}.

        The [Ident.t] and [Uid.t] fields are unused by the compiler, but Merlin needs
        them. Merlin cannot be cleanly patched to include these fields because Merlin
        must be able to deserialize typedtrees produced by the compiler. Thus, we include
        them here, as the cost of tracking this additional information is minimal. *)
    fp_loc: Location.t;
    (** [fp_loc] is the location of the entire value parameter, not including
        the [fp_newtypes].
    *)
  }

and function_param_kind =
  | Tparam_pat of pattern
  (** [Tparam_pat p] is a non-optional argument with pattern [p]. *)
  | Tparam_optional_default of pattern * expression * Jkind.sort
  (** [Tparam_optional_default (p, e, sort)] is an optional argument [p] with
      default value [e], i.e. [?x:(p = e)]. If the parameter is of type
      [a option], the pattern and expression are of type [a]. [sort] is the
      sort of [e]. *)

and function_body =
  | Tfunction_body of expression
  | Tfunction_cases of function_cases
(** The function body binds a final argument in [Tfunction_cases],
    and this argument is pattern-matched against the cases.
*)

and function_cases =
  { fc_cases: value case list;
    fc_env : Env.t;
    (** [fc_env] contains entries from all parameters except
        for the last one being matched by the cases.
    *)
    fc_arg_mode: Mode.Alloc.l;
    fc_arg_sort: Jkind.sort;
    fc_ret_type : Types.type_expr;
    fc_partial: partial;
    fc_param: Ident.t;
    fc_loc: Location.t;
    fc_exp_extra: exp_extra option;
    fc_attributes: attributes;
    (** [fc_attributes] is just used in untypeast. *)
  }

and ident_kind =
  | Id_value
  | Id_prim of Mode.Locality.l option * Jkind.Sort.t option

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t
  | Tmeth_ancestor of Ident.t * Path.t

and comprehension =
  {
    comp_body : expression;
    comp_clauses : comprehension_clause list
  }

and comprehension_clause =
  | Texp_comp_for of comprehension_clause_binding list
  | Texp_comp_when of expression

and comprehension_clause_binding =
  {
    comp_cb_iterator : comprehension_iterator;
    comp_cb_attributes : attribute list
    (** No built-in attributes are meaningful here; this would correspond to
        [[body for[@attr] x in xs]], and there are no built-in attributes that
        would be efficacious there.  (The only ones that might make sense would
        be inlining, but you can't do that with list/array items that are being
        iterated over.) *)
  }
  (** We move the pattern into the [comprehension_iterator], compared to the
      untyped syntax tree, so that range-based iterators can have just an
      identifier instead of a full pattern *)

and comprehension_iterator =
  | Texp_comp_range of
      { ident     : Ident.t
      ; pattern   : Parsetree.pattern (** Redundant with [ident] *)
      ; start     : expression
      ; stop      : expression
      ; direction : direction_flag }
  | Texp_comp_in of
      { pattern  : pattern
      ; sequence : expression }

and 'k case =
    {
     c_lhs: 'k general_pattern;
     c_guard: expression option;
     c_rhs: expression;
    }

and record_label_definition =
  | Kept of Types.type_expr * Types.mutability * unique_use
  | Overridden of Longident.t loc * expression

and binding_op =
  {
    bop_op_path : Path.t;
    bop_op_name : string loc;
    bop_op_val : Types.value_description;
    bop_op_type : Types.type_expr;
    (* This is the type at which the operator was used.
       It is always an instance of [bop_op_val.val_type] *)
    bop_op_return_sort : Jkind.sort;
    bop_exp : expression;
    bop_exp_sort : Jkind.sort;
    bop_loc : Location.t;
  }

and ('a, 'b) arg_or_omitted =
  | Arg of 'a
  | Omitted of 'b

and omitted_parameter =
  { mode_closure : Mode.Alloc.r;
    mode_arg : Mode.Alloc.l;
    mode_ret : Mode.Alloc.l;
    sort_arg : Jkind.sort }

and apply_arg = (expression * Jkind.sort, omitted_parameter) arg_or_omitted

and apply_position =
  | Tail          (* must be tail-call optimised *)
  | Nontail       (* must not be tail-call optimised *)
  | Default       (* tail-call optimised if in tail position *)

(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: Types.class_type;
     cl_env: Env.t;
     cl_attributes: attributes;
    }

and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * apply_arg) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list
      * Types.MethSet.t
  (* Visible instance variables, methods and concrete methods *)
  | Tcl_open of open_description * class_expr

and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: Types.class_signature;
   cstr_meths: Ident.t Types.Meths.t;
  }

and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attributes;
  }

and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression

and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
  (** The module type constraint has been synthesized during typechecking. *)
  | Tmodtype_explicit of module_type
  (** The module type was in the source file. *)

and functor_parameter =
  | Unit
  | Named of Ident.t option * string option loc * module_type

and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of functor_parameter * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_apply_unit of module_expr
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
    (** ME          (constraint = Tmodtype_implicit)
        (ME : MT)   (constraint = Tmodtype_explicit MT)
     *)
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * Jkind.sort * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of type_exception
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding =
    {
     mb_id: Ident.t option; (** [None] for [module _ = struct ... end] *)
     mb_name: string option loc;
     mb_uid: Uid.t;
     mb_presence: Types.module_presence;
     mb_expr: module_expr;
     mb_attributes: attributes;
     mb_loc: Location.t;
    }

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_rec_kind: Value_rec_types.recursive_binding_kind;
    vb_sort: Jkind.sort;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  (** External declaration coerced to a regular value.
      {[
        module M : sig val ext : a -> b end =
        struct external ext : a -> b = "my_c_function" end
      ]}
      Only occurs inside a [Tcoerce_structure] coercion. *)
  | Tcoerce_alias of Env.t * Path.t * module_coercion
  (** Module alias coerced to a regular module.
      {[
        module M : sig module Sub : T end =
        struct module Sub = Some_alias end
      ]}
      Only occurs inside a [Tcoerce_structure] coercion. *)

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attributes;
   }

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of functor_parameter * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc
  | Tmty_strengthen of module_type * Path.t * Longident.t loc

and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: Types.type_expr;
    pc_poly_mode: Mode.Locality.l option;
    pc_poly_sort: Jkind.Sort.t option;
    pc_env: Env.t;
    pc_loc : Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of type_exception
  | Tsig_module of module_declaration
  | Tsig_modsubst of module_substitution
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_modtypesubst of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description * Mode.Modality.Value.Const.t
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration =
    {
     md_id: Ident.t option;
     md_name: string option loc;
     md_uid: Uid.t;
     md_presence: Types.module_presence;
     md_type: module_type;
     md_attributes: attributes;
     md_loc: Location.t;
    }

and module_substitution =
    {
     ms_id: Ident.t;
     ms_name: string loc;
     ms_uid: Uid.t;
     ms_manifest: Path.t;
     ms_txt: Longident.t loc;
     ms_attributes: attributes;
     ms_loc: Location.t;
    }

and module_type_declaration =
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_uid: Uid.t;
     mtd_type: module_type option;
     mtd_attributes: attributes;
     mtd_loc: Location.t;
    }

and 'a open_infos =
    {
     open_expr: 'a;
     open_bound_items: Types.signature;
     open_override: override_flag;
     open_env: Env.t;
     open_loc: Location.t;
     open_attributes: attribute list;
    }

and open_description = (Path.t * Longident.t loc) open_infos

and open_declaration = module_expr open_infos

and include_kind =
  | Tincl_structure
  | Tincl_functor of (Ident.t * module_coercion) list
      (* S1 -> S2 *)
  | Tincl_gen_functor of (Ident.t * module_coercion) list
      (* S1 -> () -> S2 *)

and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_kind: include_kind;
     incl_attributes: attribute list;
    }

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_modtype of module_type
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc
  | Twith_modtypesubst of module_type

and core_type =
  { mutable ctyp_desc : core_type_desc;
      (** mutable because of [Typeclass.declare_method] *)
    mutable ctyp_type : Types.type_expr;
      (** mutable because of [Typeclass.declare_method] *)
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attributes;
   }

and core_type_desc =
  | Ttyp_var of string option * Jkind.annotation option
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of (string option * core_type) list
  | Ttyp_unboxed_tuple of (string option * core_type) list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string loc option * Jkind.annotation option
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of (string * Jkind.annotation option) list * core_type
  | Ttyp_package of package_type
  | Ttyp_open of Path.t * Longident.t loc * core_type
  | Ttyp_call_pos
      (** [Ttyp_call_pos] represents the type of the value of a Position
          argument ([lbl:[%call_pos] -> ...]). *)

and package_type = {
  pack_path : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and row_field = {
  rf_desc : row_field_desc;
  rf_loc : Location.t;
  rf_attributes : attributes;
}

and row_field_desc =
    Ttag of string loc * bool * core_type list
  | Tinherit of core_type

and object_field = {
  of_desc : object_field_desc;
  of_loc : Location.t;
  of_attributes : attributes;
}

and object_field_desc =
  | OTtag of string loc * core_type
  | OTinherit of core_type

and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attributes;
    }

and type_declaration =
  {
    typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * (variance * injectivity)) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attributes;
    typ_jkind_annotation: Parsetree.jkind_annotation Location.loc option;
   }

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_uid: Uid.t;
     ld_mutable: Types.mutability;
     ld_modalities: Mode.Modality.Value.Const.t;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attributes;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_uid: Uid.t;
     cd_vars: (string * Jkind.annotation option) list;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attributes;
    }

and constructor_argument =
  {
    ca_modalities: Mode.Modality.Value.Const.t;
    ca_type: core_type;
    ca_loc: Location.t;
  }

and constructor_arguments =
  | Cstr_tuple of constructor_argument list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * (variance * injectivity)) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_loc: Location.t;
    tyext_attributes: attributes;
  }

and type_exception =
  {
    tyexn_constructor: extension_constructor;
    tyexn_loc: Location.t;
    tyexn_attributes: attribute list;
  }

and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type : Types.extension_constructor;
    ext_kind : extension_constructor_kind;
    ext_loc : Location.t;
    ext_attributes: attributes;
  }

and extension_constructor_kind =
    Text_decl of (string * Jkind.annotation option) list *
                 constructor_arguments *
                 core_type option
  | Text_rebind of Path.t * Longident.t loc

and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: Types.class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attributes;
    }

and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type
  | Tcty_open of open_description * class_type

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
  }

and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attributes;
  }

and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * (variance * injectivity)) list;
    ci_id_name : string loc;
    ci_id_class: Ident.t;
    ci_id_class_type : Ident.t;
    ci_id_object : Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl : Types.class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attributes;
   }

type argument_interface = {
  ai_signature: Types.signature;
  ai_coercion_from_primary: module_coercion;
}
(** For a module [M] compiled with [-as-argument-for P] for some parameter
    module [P], the signature of [P] along with the coercion from [M]'s
    exported signature (the _primary interface_) to [P]'s signature (the
    _argument interface_). *)

type implementation = {
  structure: structure;
  coercion: module_coercion;
  signature: Types.signature;
  argument_interface: argument_interface option;
  shape: Shape.t;
}
(** A typechecked implementation including its module structure, its exported
    signature, and a coercion of the module against that signature.

    If an .mli file is present, the signature will come from that file and be
    the exported signature of the module.

    If there isn't one, the signature will be inferred from the module
    structure.

    If the module is compiled with [-as-argument-for] and is thus typechecked
    against the .mli for a parameter in addition to its own .mli, it has an
    additional signature stored in [argument_interface].
*)

type item_declaration =
  | Value of value_description
  | Value_binding of value_binding
  | Type of type_declaration
  | Constructor of constructor_declaration
  | Extension_constructor of extension_constructor
  | Label of label_declaration
  | Module of module_declaration
  | Module_substitution of module_substitution
  | Module_binding of module_binding
  | Module_type of module_type_declaration
  | Class of class_declaration
  | Class_type of class_type_declaration
(** [item_declaration] groups together items that correspond to the syntactic
    category of "declarations" which include types, values, modules, etc.
    declarations in signatures and their definitions in implementations. *)

(* Auxiliary functions over the a.s.t. *)

(** [as_computation_pattern p] is a computation pattern with description
    [Tpat_value p], which enforces a correct placement of pat_attributes
    and pat_extra metadata (on the inner value pattern, rather than on
    the computation pattern). *)
val as_computation_pattern: pattern -> computation general_pattern

val classify_pattern_desc: 'k pattern_desc -> 'k pattern_category
val classify_pattern: 'k general_pattern -> 'k pattern_category

type pattern_action =
  { f : 'k . 'k general_pattern -> unit }
val shallow_iter_pattern_desc:
    pattern_action -> 'k pattern_desc -> unit

type pattern_transformation =
  { f : 'k . 'k general_pattern -> 'k general_pattern }
val shallow_map_pattern_desc:
    pattern_transformation -> 'k pattern_desc -> 'k pattern_desc

val iter_general_pattern: pattern_action -> 'k general_pattern -> unit
val iter_pattern: (pattern -> unit) -> pattern -> unit

type pattern_predicate = { f : 'k . 'k general_pattern -> bool }
val exists_general_pattern: pattern_predicate -> 'k general_pattern -> bool
val exists_pattern: (pattern -> bool) -> pattern -> bool

val let_bound_idents: value_binding list -> Ident.t list
val let_bound_idents_full:
    value_binding list -> (Ident.t * string loc * Types.type_expr * Uid.t) list

(* [let_bound_idents_with_modes_sorts_and_checks] finds all the idents in the
   let bindings and computes their modes, sorts, and whether they have any check
   attributes (zero_alloc).

   Note that:
   * The list associated with each ident can only have more than one element in
     the case of or pattern, where the ident is bound on both sides.
   * We return just one check_attribute per identifier, because this attribute
     can only be something other than [Default_check] in the case of a simple
     variable pattern bound to a function (in which case the list will also
     have just one element).
*)
val let_bound_idents_with_modes_sorts_and_checks:
  value_binding list
  -> (Ident.t * (Location.t * Mode.Value.l * Jkind.sort) list
              * Zero_alloc.t) list

(** Alpha conversion of patterns *)
val alpha_pat:
  (Ident.t * Ident.t) list -> 'k general_pattern -> 'k general_pattern

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc

val pat_bound_idents: 'k general_pattern -> Ident.t list
val pat_bound_idents_with_types:
  'k general_pattern -> (Ident.t * Types.type_expr) list
val pat_bound_idents_full:
  Jkind.sort -> 'k general_pattern
  -> (Ident.t * string loc * Types.type_expr * Types.Uid.t * Jkind.sort) list

(** Splits an or pattern into its value (left) and exception (right) parts. *)
val split_pattern:
  computation general_pattern -> pattern option * pattern option

(** Whether an expression looks nice as the subject of a sentence in a error
    message. *)
val exp_is_nominal : expression -> bool

(** Calculates the syntactic arity of a function based on its parameters and body. *)
val function_arity : function_param list -> function_body -> int

(** Given a declaration, return the location of the bound identifier *)
val loc_of_decl : uid:Shape.Uid.t -> item_declaration -> string Location.loc
