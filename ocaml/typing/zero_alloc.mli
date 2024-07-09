type check = Builtin_attributes.zero_alloc_check =
  { strict: bool;
    opt: bool;
    arity: int;
    loc: Location.t;
  }

type assume = Builtin_attributes.zero_alloc_assume =
  { strict: bool;
    never_returns_normally: bool;
    never_raises: bool;
    arity: int;
    loc: Location.t;
  }

type const = Builtin_attributes.zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of check
  | Assume of assume

(* This type represents whether or not a function will be checked for
   zero-alloc-ness, and with what configuration (strict, opt, etc). It can be a
   variable which will be filled in when the module the function is in is
   compared against its signature, allowing to infer zero-alloc checks. *)
type t

(* [default] corresponds to [Default_zero_alloc], meaning no check will be
   done. *)
val default : t

val create_const : const -> t

(* [create_var loc n] creates a variable. [loc] is the location of the function
   you are creating a variable for, and [n] is its syntactic arity of the
   function the variable is being created for. *)
val create_var : Location.t -> int -> t

(* In the case [t] is a variable, [get t] returns its current contents as a
   [const] and has no effect. *)
val get : t -> const

(* For types.ml's backtracking mechanism. *)
type change
val set_change_log : (change -> unit) -> unit
val undo_change : change -> unit

(* These are the errors that may be raised by [sub_exn] below. *)
type error
val print_error : Format.formatter -> error -> unit

(* [sub t1 t2] checks whether the zero_alloc check t1 is stronger than the
   zero_alloc check t2. It returns [Ok ()] if so, and [Error e] if not.  If [t1]
   is a variable, it may be set to make the relation hold. *)
val sub : t -> t -> (unit, error) Result.t
