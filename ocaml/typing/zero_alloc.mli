type t = Builtin_attributes.zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of { strict: bool;
               opt: bool;
               arity: int;
               loc: Location.t;
             }
  | Assume of { strict: bool;
                never_returns_normally: bool;
                never_raises: bool;
                arity: int;
                loc: Location.t;
              }

type error

exception Error of error

val print_error : Format.formatter -> error -> unit

(* [sub_exn t1 t2] checks whether the zero_alloc check t1 is stronger than the
   zero_alloc check t2. If not, it raises [Error]. *)
val sub_exn : t -> t -> unit
