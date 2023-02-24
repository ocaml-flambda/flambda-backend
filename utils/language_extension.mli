(** Language extensions provided by ocaml-jst *)

(** The type of language extensions *)
type t =
  | Comprehensions
  | Local
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays

(** Equality on language extensions *)
val equal : t -> t -> bool

(** A list of all possible language extensions *)
val all : t list

(** Check if a language extension is "erasable", i.e. whether it can be
    harmlessly translated to attributes and compiled with the upstream
    compiler. *)
val is_erasable : t -> bool

(** Print and parse language extensions; parsing is case-insensitive *)
val to_string : t -> string
val of_string : string -> t option
val of_string_exn : string -> t

(** Enable and disable language extensions; these operations are idempotent *)
val set : t -> enabled:bool -> unit
val enable : t -> unit
val disable : t -> unit

(** Check if a language extension is currently enabled *)
val is_enabled : t -> bool

(** Tooling support: Temporarily enable and disable language extensions; these
    operations are idempotent.  Calls to [set], [enable], [disable], and
    [disallow_extensions] inside the body of the function argument will also
    be rolled back when the function finishes, but this behavior may change;
    nest multiple [with_*] functions instead.  *)
val with_set : t -> enabled:bool -> (unit -> unit) -> unit
val with_enabled : t -> (unit -> unit) -> unit
val with_disabled : t -> (unit -> unit) -> unit

(** Permanently restrict the allowable extensions to those that are
    "erasable", i.e. those that can be harmlessly translated to attributes and
    compiled with the upstream compiler.  Used for [-only-erasable-extensions]
    to ensure that some code is guaranteed to be compatible with upstream
    OCaml after rewriting to attributes.  When called, disables any
    currently-enabled non-erasable extensions, including any that are on by
    default.  Causes any future uses of [set ~enabled:true], [enable], and
    their [with_] variants to raise if used with a non-erasable extension.
    The [is_enabled] function will still work on any extensions, it will just
    always return [false] on non-erasable ones.  Will raise if called after
    [disallow_extensions]; the ratchet of extension restriction only goes one
    way. *)
val restrict_to_erasable_extensions : unit -> unit

(** Permanently ban all extensions; used for [-disable-all-extensions] to
    ensure that some code is 100% extension-free.  When called, disables any
    currently-enabled extensions, including the defaults.  Causes any future
    uses of [set ~enabled:true], [enable], and their [with_] variants to
    raise; also causes any future uses of [only_erasable_extensions] to raise.
    The [is_enabled] function will still work, it will just always return
    [false].*)
val disallow_extensions : unit -> unit
