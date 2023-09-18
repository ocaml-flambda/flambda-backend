(* Dummy expressions *)

open Typedtree
open Compat

(** [cases_view] is similar to an old version of the [texp_function] type.
   It would take some work to update old clients to use the new [texp_function]
   type, so instead we have this compatibility layer between [cases_view] and
   the new version of [texp_function].

    (Though, at some point we should just update the clients and remove this
    compatibility layer.)
*)

type cases_view_identifier =
  | Cases of texp_function_cases_identifier
  | Param of texp_function_param_identifier

type cases_view = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
  partial : partial;
  optional_default : expression option;
  cases_view_identifier : cases_view_identifier;
}

val cases_view_to_function : cases_view -> texp_function
val function_to_cases_view : texp_function -> cases_view
