type t

val create : ?verbose:bool -> ?seed:int -> unit -> t

(** Check whether the function [f] returns [true] for randomly-generated inputs.
    The number and types of the arguments to [f] are determined by [types],
    which is (syntactically) a list of [Type.t]s. For example:

    [check ~types:Type.[int; int] ~f:(fun i j -> i + j = j + i) ~name:"+ comm"]

    If [f] returns [false], testing stops and the failing arguments are printed
    on standard error. If [f] returns true for all [n] cases, a success message
    is printed on standard error. *)
val check :
  ?n:int (** Number of runs (default is 1000) *) ->
  ?seed:int (** Seed (default taken from [create]) *) ->
  t ->
  types:('a, _, bool) Tuple.Of2(Type.T).t ->
  f:'a ->
  name:string ->
  unit

val something_has_failed : t -> bool
