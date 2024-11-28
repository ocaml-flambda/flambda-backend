open Import

val append3_semi
   : Semigroup.t option
  -> Semigroup.t option
  -> Semigroup.t option
  -> Semigroup.t option

val concat_string_options : string option list -> string option

val concat_semi : Semigroup.t option list -> Monoid_of_semigroup.t

val concat_chain_semi
  : (unit, unit) Chain_of_semigroup.t -> Monoid_of_semigroup.t

val append3_lists
  : List_monoid.t -> List_monoid.t -> List_monoid.t -> List_monoid.t

val concat_lists : List_monoid.t list -> List_element.t list

val concat_chain_lists : (_, _) Chain_of_lists.t -> List_element.t list
