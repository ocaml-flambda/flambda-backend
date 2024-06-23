# 2 "camlinternalQuote.mli"
module Loc : sig

  type t

  val unknown : t

  val known :
    file:string
    -> start_line:int
    -> start_col:int
    -> end_line:int
    -> end_col:int
    -> t

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

end

module Constant : sig

  type t

  val int : int -> t
  val char : char -> t
  val string : string -> string option -> t
  val float : string -> t
  val int32 : int32 -> t
  val int64 : int64 -> t
  val nativeint : nativeint -> t

end

module Ident : sig

  module Module : sig

    type t

    val compilation_unit : string -> t

    val dot : t -> string -> t

    val var : Loc.t -> Var.Module.t -> t

  end

  module Value : sig

    type t

    val dot : Module.t -> string -> t

    val var : Loc.t -> Var.Value.t -> t

  end

  module Type : sig

    type t

    val dot : Module.t -> string -> t

    val var : Loc.t -> Var.Type_constr.t -> t

    val int : t
    val char : t
    val string : t
    val bytes : t
    val float : t
    val float32 : t
    val bool : t
    val unit : t
    val exn : t
    val array : t -> t
    val iarray : t -> t
    val list : t -> t
    val option : t -> t
    val nativeint : t
    val int32 : t
    val int64 : t
    val lazy_t : t -> t
    val extension_constructor : t
    val floatarray : t
    val lexing_position : t
    val code : t -> t
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

module Package : sig

  type t

end

module Type : sig

  type t

  val any : t

  val var : Var.Type_var.t -> t

  val arrow : Label.t -> t -> t -> t

  val tuple : (Label.Nonoptional.t * t) list -> t

  val constr : t list -> Ident.Type.t -> t

  val alias : t -> Var.Type_var.t -> t

  val variant : Variant.t -> t

  val poly : Name.t list -> (Var.Type_var.t -> t) -> t

  val package : Package.t -> t

end

module Pat : sig

  type t

  val any : t

  val var : Var.Value.t -> t

  val alias : t -> Var.Value.t -> t

  val constant : Constant.t -> t

  val interval : Constant.t -> Constant.t -> t

  val tuple : t list -> t

  val construct : Ident.Constructor.t -> t option -> t

  val variant : Variant.t -> t option -> t

  val record : (Ident.Field.t * t) list -> bool -> t

  val array : t list -> t

  val or_ : t -> t -> t

  val constraint_ : t -> Type.t -> t

  val lazy_ : t -> t

  val unpack : Var.Module.t -> t -> t

  val exception_ : t -> t

end

module rec Case : sig

  type t

  val nonbinding : Pat.t -> Exp.t -> t

  val simple : Name.t -> (Var.Value.t -> Exp.t) -> t

  val pattern :
    bound_vars:Name.t list
    -> bound_modules:Name.t list
    -> (bound_vars:Var.Value.t list
        -> bound_modules:Var.Value.t list
        -> Pat.t * Exp.t)
    -> t

  val guarded :
    bound_vars:Name.t list
    -> bound_modules:Name.t list
    -> (bound_vars:Var.Value.t list
        -> bound_modules:Var.Value.t list
        -> Pat.t * Exp.t * Exp.t)
    -> t

  val refutation :
    bound_vars:Name.t list
    -> bound_modules:Name.t list
    -> (bound_vars:Var.Value.t list
        -> bound_modules:Var.Value.t list
        -> Pat.t)
    -> t 

end

and Type_constraint : sig

  type t

  val constraint_ : Type.t -> t

  val coercion : Type.t -> Type.t -> t

end

and Function : sig

  type t

  val body : Exp.t -> Type_constraint.t option -> t

  val cases : Case.t list -> Type_constraint.t option -> t

  val add_params_nonbinding : (Label.t * Pat.t) list -> t -> t

  val add_params_simple :
    (Label.t * Name.t) list
    -> (Var.Value.t list -> t)
    -> t

  val add_params :
    Name.t list
    -> (Var.Value.t list -> (Label.t * Pat.t) list * t)
    -> t

  val add_param_optional :
    Label.t
    -> Exp.t option
    -> Name.t list
    -> (Var.Value.t list -> Pat.t * t)
    -> t

  val add_newtype : Name.t -> (Var.Type_constr.t -> t) -> t

end

and Exp : sig

  type t

  val ident : Ident.Value.t -> t

  val constant : Constant.t -> t

  val let_nonbinding : Pat.t -> t -> t -> t

  val let_simple : Name.t -> t -> (Var.Value.t -> t) -> t

  val let_rec_simple : Name.t list -> (Var.Value.t list -> t list * t) -> t

  val let_ : Name.t list -> t -> (Var.Value.t list -> Pat.t * t) -> t

  val function_ : Function.t -> t

  val fun_nonbinding : (Label.t * Pat.t) list -> t -> t

  val fun_simple : (Label.t * Name.t) list -> (Var.Value.t list -> t) -> t

  val fun_ : Name.t list -> (Var.Value.t list -> (Label.t * Pat.t) list * t) -> t

  val apply : t -> (Label.t * t) list -> t

  val match_ : t -> Case.t list -> t

  val try_ : t -> Case.t list -> t

  val tuple : t list -> t

  val construct : Ident.Constructor.t -> t option -> t

  val variant : Variant.t -> t option -> t

  val record : (Ident.Field.t * t) list -> t option -> t

  val field : t -> Ident.Field.t -> t

  val setfield : t -> Ident.Field.t -> t -> t

  val array : t list -> t

  val ifthenelse : t -> t -> t option -> t

  val sequence : t -> t -> t

  val while_ : t -> t -> t

  val for_nonbinding : Pat.t -> t -> t -> bool -> t -> t

  val for_simple : Name.t -> t -> t -> bool -> (Var.Value.t -> t) -> t

  val assert_ : t -> t

  val lazy_ : t -> t

  val open_ : bool -> Ident.Module.t -> t -> t

  val constraint_ : t -> Type_constraint.t -> t

  val quote : t -> t

  val escape : t -> t

  val splice : Code.t -> t

end

and Code : sig

  type t

  val of_exp : Exp.t -> Loc.t -> t

  val of_exp_with_type_vars : Name.t list -> (Var.Type_var.t list -> Exp.t) -> Loc.t -> t

  module Closed : sig

    type exp = t

    type t

    val close : exp -> t

    val open_ : t -> exp

  end

end
