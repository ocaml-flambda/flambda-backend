module Loc : sig
  type t

  val unknown : t

  val known :
    file:string ->
    start_line:int ->
    start_col:int ->
    end_line:int ->
    end_col:int ->
    t
end

module Name : sig
  type t

  val mk : string -> t
end

module Var : sig
  module Module : sig
    type t

    val name : t -> Name.t
  end

  module Value : sig
    type t

    val name : t -> Name.t
  end

  module Type_constr : sig
    type t

    val name : t -> Name.t
  end

  module Type_var : sig
    type t

    val name : t -> Name.t
  end

  type t

  val name : t -> Name.t
end

module Constant : sig
  type t

  val int : int -> t

  val char : char -> t

  val string : string -> string option -> t

  val float : string -> t

  val float32 : string -> t

  val int32 : int32 -> t

  val int64 : int64 -> t

  val nativeint : nativeint -> t

  val unboxed_float : string -> t

  val unboxed_float32 : string -> t

  val unboxed_int32 : int32 -> t

  val unboxed_int64 : int64 -> t

  val unboxed_nativeint : nativeint -> t
end

module Identifier : sig
  module Module : sig
    type t

    val compilation_unit : string -> t

    val dot : t -> string -> t

    val var : Var.Module.t -> Loc.t -> t
  end

  module Value : sig
    type t

    val dot : Module.t -> string -> t

    val var : Var.Value.t -> Loc.t -> t
  end

  module Type : sig
    type t

    val dot : Module.t -> string -> t

    val var : Var.Type_constr.t -> Loc.t -> t

    val int : t

    val char : t

    val string : t

    val bytes : t

    val float : t

    val float32 : t

    val bool : t

    val unit : t

    val exn : t

    val array : t

    val iarray : t

    val list : t

    val option : t

    val nativeint : t

    val int32 : t

    val int64 : t

    val lazy_t : t

    val extension_constructor : t

    val floatarray : t

    val lexing_position : t

    val code : t

    val unboxed_float : t

    val unboxed_nativeint : t

    val unboxed_int32 : t

    val unboxed_int64 : t

    val int8x16 : t

    val int16x8 : t

    val int32x4 : t

    val int64x2 : t

    val float32x4 : t

    val float64x2 : t
  end

  module Module_type : sig
    type t

    val dot : Module.t -> string -> t
  end

  module Constructor : sig
    type t

    val dot : Module.t -> string -> t

    val false_ : t

    val true_ : t

    val void : t

    val nil : t

    val cons : t

    val none : t

    val some : t

    val match_failure : t

    val out_of_memory : t

    val invalid_argument : t

    val failure : t

    val not_found : t

    val sys_error : t

    val end_of_file : t

    val division_by_zero : t

    val stack_overflow : t

    val sys_blocked_io : t

    val assert_failure : t

    val undefined_recursive_module : t
  end

  module Field : sig
    type t

    val dot : Module.t -> string -> t
  end
end

module Label : sig
  type t

  module Nonoptional : sig
    type t

    val no_label : t

    val labelled : string -> t
  end

  val nonoptional : Nonoptional.t -> t

  val no_label : t

  val labelled : string -> t

  val optional : string -> t
end

module Variant : sig
  type t

  val of_string : string -> t
end

module Method : sig
  type t

  val of_string : string -> t
end

module Fragment : sig
  type t

  val name : string -> t

  val dot : t -> string -> t
end

module Module : sig
  type t

  val ident : Identifier.Module.t -> t

  val apply : t -> t -> t

  val apply_unit : t -> t
end

module Constructor : sig
  type t

  val ident : Identifier.Constructor.t -> t

  val of_string : string -> t
end

module Field : sig
  type t

  val ident : Identifier.Field.t -> t

  val of_string : string -> t
end

module Module_type : sig
  type t

  val ident : Identifier.Module_type.t -> t

  val of_string : string -> t
end

module rec Variant_type : sig
  module Variant_form : sig
    type t

    val fixed : t

    val open_ : t

    val closed : string list -> t
  end

  module Row_field : sig
    type t

    val inherit_ : Type.t -> t

    val tag : Variant.t -> bool -> Type.t list -> t
  end

  type t

  val of_row_fields_list : Row_field.t list -> Variant_form.t -> t
end

and Object_field : sig
  type t

  val inherit_ : Type.t -> t

  val tag : Name.t -> Type.t -> t
end

and Type : sig
  type t

  val var : Var.Type_var.t option -> t

  val arrow : Label.t -> t -> t -> t

  val tuple : (Label.Nonoptional.t * t) list -> t

  val unboxed_tuple : (Label.Nonoptional.t * t) list -> t

  val constr : Identifier.Type.t -> t list -> t

  val object_ : Object_field.t list -> bool -> t

  val class_ : Name.t -> t list -> t

  val alias : t -> Var.Type_var.t -> t

  val variant : Variant_type.t -> t

  val poly : Loc.t -> Name.t list -> (Var.Type_constr.t list -> t) -> t

  val package : Module_type.t -> (Fragment.t * t) list -> t

  val call_pos : t
end

module Pat : sig
  type t

  val any : t

  val var : Var.Value.t -> t

  val alias : t -> Var.Value.t -> t

  val constant : Constant.t -> t

  val tuple : (Label.Nonoptional.t * t) list -> t

  val unboxed_tuple : (Label.Nonoptional.t * t) list -> t

  val construct : Constructor.t -> t option -> t

  val variant : string -> t option -> t

  val record : (Field.t * t) list -> bool -> t

  val unboxed_record : (Field.t * t) list -> bool -> t

  val array : t list -> t

  val or_ : t -> t -> t

  val lazy_ : t -> t

  val any_module : t

  val unpack : Var.Module.t -> t

  val exception_ : t -> t

  val constraint_ : t -> Type.t -> t
end

module rec Case : sig
  type t

  val nonbinding : Loc.t -> Pat.t -> Exp.t -> t

  val simple : Loc.t -> Name.t -> (Var.Value.t -> Exp.t) -> t

  val pattern :
    Loc.t ->
    bound_values:Name.t list ->
    bound_modules:Name.t list ->
    (Var.Value.t list -> Var.Module.t list -> Pat.t * Exp.t) ->
    t

  val guarded :
    Loc.t ->
    bound_values:Name.t list ->
    bound_modules:Name.t list ->
    (Var.Value.t list -> Var.Module.t list -> Pat.t * Exp.t * Exp.t) ->
    t

  val refutation :
    Loc.t ->
    bound_values:Name.t list ->
    bound_modules:Name.t list ->
    (Var.Value.t list -> Var.Module.t list -> Pat.t) ->
    t
end

and Type_constraint : sig
  type t

  val constraint_ : Type.t -> t

  val coercion : Type.t option -> Type.t -> t
end

and Function : sig
  type t

  val body : Exp.t -> Type_constraint.t option -> t

  val cases : Case.t list -> Type_constraint.t option -> t

  val param :
    Label.t ->
    Exp.t option ->
    Loc.t ->
    Name.t list ->
    (Var.Value.t list -> Pat.t * t) ->
    t

  val param_module_nonbinding : Label.t -> Loc.t -> Pat.t -> t -> t

  val param_module :
    Label.t ->
    Loc.t ->
    Name.t ->
    (Var.Module.t -> Pat.t * t) ->
    t

  val newtype : Loc.t -> Name.t -> (Var.Type_var.t -> t) -> t
end

and Comprehension : sig
  type t

  val body : Exp.t -> t

  val when_clause : Exp.t -> t -> t

  val for_range :
    Loc.t -> Name.t -> Exp.t -> Exp.t -> bool -> (Var.Value.t -> t) -> t

  val for_in :
    Loc.t -> Exp.t -> Name.t list -> (Var.Value.t list -> Pat.t * t) -> t
end

and Exp : sig
  type t

  val ident : Identifier.Value.t -> t

  val constant : Constant.t -> t

  val let_rec_simple :
    Loc.t -> Name.t list -> (Var.Value.t list -> t list * t) -> t

  val let_ :
    Loc.t ->
    Name.t list ->
    Name.t list ->
    t list ->
    (Var.Value.t list -> Var.Module.t list -> Pat.t * t) ->
    t

  val function_ : Function.t -> t

  val apply : t -> (Label.t * t) list -> t

  val match_ : t -> Case.t list -> t

  val try_ : t -> Case.t list -> t

  val tuple : (Label.Nonoptional.t * t) list -> t

  val construct : Constructor.t -> t option -> t

  val variant : Name.t -> t option -> t

  val record : (Field.t * t) list -> t option -> t

  val field : t -> Field.t -> t

  val setfield : t -> Field.t -> t -> t

  val array : t list -> t

  val ifthenelse : t -> t -> t option -> t

  val sequence : t -> t -> t

  val while_ : t -> t -> t

  val for_nonbinding : Loc.t -> Pat.t -> t -> t -> bool -> t -> t

  val for_simple : Loc.t -> Name.t -> t -> t -> bool -> (Var.Value.t -> t) -> t

  val send : t -> Method.t -> t

  val assert_ : t -> t

  val lazy_ : t -> t

  val letmodule_nonbinding : Module.t -> t -> t

  val letmodule : Loc.t -> Name.t -> Module.t -> (Var.Module.t -> t) -> t

  val constraint_ : t -> Type_constraint.t -> t

  val new_ : Identifier.Value.t -> t

  val pack : Module.t -> t

  val unreachable : t

  val src_pos : t

  val stack : t -> t

  val extension_constructor : Name.t -> t

  val let_exception : Name.t -> t -> t

  val let_op : Identifier.Value.t list -> t list -> Case.t -> t

  val exclave : t -> t

  val list_comprehension : Comprehension.t -> t

  val array_comprehension : Comprehension.t -> t

  val unboxed_tuple : (Label.Nonoptional.t * t) list -> t

  val unboxed_record_product : (Field.t * t) list -> t option -> t

  val unboxed_field : t -> Field.t -> t

  val quote : t -> t

  val antiquote : t -> t

  val splice : Code.t -> t

  val print : Format.formatter -> t -> unit
end

and Code : sig
  type t

  val to_exp : t -> Exp.t

  val of_exp : Exp.t -> Loc.t -> t

  val of_exp_with_type_vars :
    Loc.t -> Name.t list -> (Var.Type_constr.t list -> Exp.t) -> t

  module Closed : sig
    type exp = t

    type t

    val close : exp -> t

    val open_ : t -> exp
  end

  val print : Format.formatter -> t -> unit
end
