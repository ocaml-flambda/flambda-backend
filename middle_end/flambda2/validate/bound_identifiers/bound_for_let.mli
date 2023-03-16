(** Let-expression binders *)
type t =
| Singleton of Bound_var.t
    (** The binding of a single variable, which is statically scoped. This
case is not used for sets of closures. *)
| Static of Bound_codelike.t
    (** The binding of symbols and code IDs to statically-allocated constants
    and pieces of code. The scoping of the symbols and code IDs follows
    the dominator tree, not syntactic scope. *)

include Bindable.S with type t := t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

val fold_all_bound_vars : t -> init:'a -> f:('a -> Bound_var.t -> 'a) -> 'a

val fold_all_bound_names :
  t ->
  init:'a ->
  var:('a -> Bound_var.t -> 'a) ->
  symbol:('a -> Symbol.t -> 'a) ->
  code_id:('a -> Code_id.t -> 'a) ->
  'a
