type t

val create : unit -> t

(*_ CR-someday lmaurer: Fix ocamlformat's handling of code/verbatim blocks in
  comments. *)

(** Check whether the function [f] returns [true] for randomly-generated inputs.
    The number and types of the arguments to [f] are determined by
    [arbitrary_impls], which is (syntactically) a list of [Arbitrary.t]s. For
    example:

    [check ~arbitrary_impls:Arbitrary.[int; int] ~f:(fun i j -> i + j = j + i)
    ~name:"+ comm"]

    If [f] returns [false], testing stops and the failing arguments are printed
    on standard error. If [f] returns true for all [n] cases, a success message
    is printed on standard error. *)
val check :
  ?n:int (** Number of runs (default is 1000) *) ->
  ?seed:int (** Seed (default is 0) *) ->
  ?verbose:bool (** Whether to print all test cases (default is false) *) ->
  t ->
  arbitrary_impls:('a, _, bool) Tuple.Of2(Arbitrary.T).t ->
  f:'a ->
  name:string ->
  unit

val failure_count : t -> int
