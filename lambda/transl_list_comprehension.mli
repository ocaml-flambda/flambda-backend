open Lambda
open Typedtree
open Debuginfo.Scoped_location

(** Translate list comprehensions; see the .ml file for more details *)

(** Translate a list comprehension ([Typedtree.comprehension], when it's the
    body of a [Typedtree.Texp_list_comprehension]) into Lambda.

    The only variables and types this term directly refers to are those from
    [CamlinternalComprehension] and those that come from the list comprehension
    itself.

    This function needs to translate expressions from Typedtree into Lambda, and
    so is parameterized by [Translcore.transl_exp], its [scopes] argument, and
    the [loc]ation. *)
val comprehension :
  transl_exp:(scopes:scopes -> Jkind.sort -> expression -> lambda) ->
  scopes:scopes ->
  loc:scoped_location ->
  comprehension ->
  lambda
