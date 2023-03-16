module Pattern : sig
  type t =
    | Code of Code_id.t
    | Set_of_closures of Bound_var.t
    | Block_like of Symbol.t

  val code : Code_id.t -> t

  val set_of_closures : Bound_var.t -> t

  val block_like : Symbol.t -> t

  val is_code : t -> bool

  val must_be_code : t -> Code_id.t option

  val print : Format.formatter -> t -> unit
end

type t = Pattern.t list

val empty : t

(** All recursive cycles between the names bound by the provided pattern(s) must
    go through at least one code ID. (So for example the declaration of just a
    block that points to itself is forbidden.) *)
val create : Pattern.t list -> t

val singleton : Pattern.t -> t

val to_list : t -> Pattern.t list

val binds_code : t -> bool

val binds_symbols : t -> bool

val symbols_being_defined : t -> Symbol.Set.t

val code_being_defined : t -> Code_id.Set.t

val everything_being_defined : t -> Code_id_or_symbol.Set.t

val concat : t -> t -> t

val gc_roots : t -> Symbol.t list

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
