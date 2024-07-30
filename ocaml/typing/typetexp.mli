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

(* Typechecking of type expressions for the core language *)

open Types
open Mode

type jkind_initialization_choice = Sort | Any

module TyVarEnv : sig
  (* this is just the subset of [TyVarEnv] that is needed outside
     of [Typetexp]. See the ml file for more. *)

  val reset : unit -> unit
  (** removes all type variables from scope *)

  val with_local_scope : (unit -> 'a) -> 'a
  (** Evaluate in a narrowed type-variable scope *)

  type poly_univars
  val make_poly_univars : string Location.loc list -> poly_univars
    (** A variant of [make_poly_univars_jkinds] that gets variables
        without jkind annotations *)

  val make_poly_univars_jkinds :
    context:(string -> Jkind.History.annotation_context) ->
    (string Location.loc * Jane_syntax.Jkind.annotation option) list ->
    poly_univars
    (** remember that a list of strings connotes univars; this must
        always be paired with a [check_poly_univars]. *)

  val check_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
    (** Verify that the given univars are universally quantified,
       and return the list of variables. The type in which the
       univars are used must be generalised *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
    (** Same as [check_poly_univars], but instantiates the resulting
       type scheme (i.e. variables become Tvar rather than Tunivar) *)

  val ttyp_poly_arg : poly_univars -> (string * Jkind.annotation option) list
    (** A suitable arg to the corresponding [Ttyp_poly] type. *)
end

val valid_tyvar_name : string -> bool

(** [transl_label lbl ty] produces a Typedtree argument label for an argument
    with label [lbl] and type [ty].

    Position arguments ([lbl:[%call_pos] -> ...]) are parsed as
    {{!Parsetree.arg_label.Labelled}[Labelled l]}. This function converts them
    to {{!Types.arg_label.Position}[Position l]} when the type is of the form
    [[%call_pos]]. *)
val transl_label :
        Parsetree.arg_label -> Parsetree.core_type option -> Types.arg_label

(** Produces a Typedtree argument label, as well as the pattern corresponding
    to the argument. [transl_label lbl pat] is equal to:

    - [Position l, P] when [lbl] is {{!Parsetree.arg_label.Labelled}[Labelled l]}
      and [pat] represents [(P : [%call_pos])]
    - [transl_label lbl None, pat] otherwise.
  *)
val transl_label_from_pat :
        Parsetree.arg_label -> Parsetree.pattern
        -> Types.arg_label * Parsetree.pattern

(* Note about [new_var_jkind]

   This is exposed as an option because the same initialization doesn't work in all
   typing contexts.

   If it's always [Sort], then it becomes difficult to get a type variable with jkind
   any in type annotations on expressions and patterns due to the lack of explicit
   binding sites.

   If it's always [Any], then we risk breaking backwards compatibility with examples
   such as:

   [external to_bytes : 'a -> extern_flags list -> bytes = "caml_output_value_to_bytes"]

   The general rule for selecting between [Sort] and [Any] is to use [Sort] in places
   that allows users to explictly binding type variables to certain jkinds and [Any]
   otherwise.

   There are some exceptions made around type manifests and type constraints to not
   constrain the type parameters to representable jkinds unnecessarily while maintaining
   the most amount of backwards compatibility. It is for this reason, the left hand side
   of a constraint is typed using [Any] while the right hand side uses [Sort]. *)
val transl_simple_type:
        Env.t -> new_var_jkind:jkind_initialization_choice
        -> ?univars:TyVarEnv.poly_univars -> closed:bool -> Alloc.Const.t
        -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_univars:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_delayed
  :  Env.t -> Alloc.Const.t
  -> Parsetree.core_type
  -> Typedtree.core_type * type_expr * (unit -> unit)
        (* Translate a type using [Any] as the [jkind_initialization_choice],
           but leave type variables unbound.
           Returns the type, an instance of the corresponding type_expr, and a
           function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val transl_type_param:
  Env.t -> Path.t -> Parsetree.core_type -> Typedtree.core_type
(* the Path.t above is of the type/class whose param we are processing;
   the level defaults to the current level *)

val get_type_param_jkind: Path.t -> Parsetree.core_type -> jkind
val get_type_param_name: Parsetree.core_type -> string option

val get_alloc_mode : Parsetree.core_type -> Alloc.Const.t

exception Already_bound

type value_loc =
    Tuple | Poly_variant | Object_field

type sort_loc =
    Fun_arg | Fun_ret

type cannot_quantify_reason
type jkind_info
type error =
  | Unbound_type_variable of string * string list
  | No_type_wildcards
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * cannot_quantify_reason
  | Bad_univar_jkind of
      { name : string; jkind_info : jkind_info; inferred_jkind : jkind }
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unsupported_extension : _ Language_extension.t -> error
  | Polymorphic_optional_param
  | Non_value of
      {vloc : value_loc; typ : type_expr; err : Jkind.Violation.t}
  | Non_sort of
      {vloc : sort_loc; typ : type_expr; err : Jkind.Violation.t}
  | Bad_jkind_annot of type_expr * Jkind.Violation.t
  | Jkind_mismatch_in_application of Jkind.t list * Jkind.t * Jkind.Violation.t option
  | Unknown_jkind_at_application of type_expr
  | Did_you_mean_unboxed of Longident.t
  | Invalid_label_for_call_pos of Parsetree.arg_label

exception Error of Location.t * Env.t * error

val report_error: Env.t -> Format.formatter -> error -> unit

(* Support for first-class modules. *)
val transl_modtype_longident:  (* from Typemod *)
    (Location.t -> Env.t -> Longident.t -> Path.t) ref
val transl_modtype: (* from Typemod *)
    (Env.t -> Parsetree.module_type -> Typedtree.module_type) ref
val check_package_with_type_constraints: (* from Typemod *)
    (Location.t -> Env.t -> Types.module_type ->
     (Longident.t Asttypes.loc * Typedtree.core_type) list ->
     Types.module_type) ref
