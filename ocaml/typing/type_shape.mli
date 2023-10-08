module Uid = Shape.Uid

module Type_shape : sig
  module Predef : sig
    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Unboxed_float
  end

  type t =
    | Ts_constr of (Uid.t * Path.t) * t list
    | Ts_tuple of t list
    | Ts_var of string option
    | Ts_predef of Predef.t * t list
    | Ts_other

  include Identifiable.S with type t := t
end

module Type_decl_shape : sig
  type tds =
    | Tds_variant of
        { simple_constructors : string list;
          complex_constructors :
            (string * (string option * Type_shape.t) list) list
        }
    | Tds_record of (string * Type_shape.t) list
    | Tds_alias of Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.t list
    }

  val print : Format.formatter -> t -> unit

  val replace_tvar : t -> Type_shape.t list -> t
end

val all_type_decls : Type_decl_shape.t Uid.Tbl.t

val all_type_shapes : Type_shape.t Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> unit

val add_to_type_shapes :
  Uid.t -> Types.type_expr -> (Path.t -> Uid.t option) -> unit

val find_in_type_decls :
  Uid.t ->
  Path.t ->
  load_decls_from_cms:(string -> Type_decl_shape.t Uid.Tbl.t) ->
  Type_decl_shape.t option

val type_name :
  Type_shape.t ->
  load_decls_from_cms:(string -> Type_decl_shape.t Uid.Tbl.t) ->
  string

val attach_compilation_unit_to_paths :
  Type_decl_shape.t -> compilation_unit:Compilation_unit.t -> Type_decl_shape.t
