(** Language extensions provided by the Jane Street version of the OCaml
    compiler.
*)

(** A setting for extensions that track multiple maturity levels *)
type maturity = Language_extension_kernel.maturity =
  | Stable
  | Beta
  | Alpha

module Maturity : sig
  (* Maturities are ordered such that the most experimental (Alpha) is
     greatest *)
  val max : maturity -> maturity -> maturity
end

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type 'a t = 'a Language_extension_kernel.t =
  | Comprehensions : unit t
  | Mode : maturity t
  | Unique : maturity t
  | Overwriting : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t
  | SIMD : maturity t
  | Labeled_tuples : unit t
  | Small_numbers : maturity t
  | Instances : unit t
  | Separability : unit t
  | Let_mutable : unit t

(** Require that an extension is enabled for at least the provided level, or
    else throw an exception at the provided location saying otherwise. *)
val assert_enabled : loc:Location.t -> 'a t -> 'a -> unit

val maturity_of_unique_for_drf : maturity

val maturity_of_unique_for_destruction : maturity

(** Existentially packed language extension *)
module Exist : sig
  type 'a extn = 'a t
  (* this is removed from the sig by the [with] below; ocamldoc doesn't like
     [:=] in sigs *)

  type t = Pack : 'a extn -> t

  val to_string : t -> string

  val is_enabled : t -> bool

  val is_erasable : t -> bool

  (** Returns a list of all strings, like ["layouts_beta"], that
      correspond to this extension. *)
  val to_command_line_strings : t -> string list

  val all : t list
end
with type 'a extn := 'a t

(** Equality on language extensions *)
val equal : 'a t -> 'b t -> bool

(** The type of language extension universes. Each universe allows a set of
    extensions, and every successive universe includes the previous one.

    Each variant corresponds to the [-extension-universe <variant>] CLI flag.

    Each extension universe, except for [No_extensions], should also have
    a corresponding library in [otherlibs/]. Those libraries must contain
    OCaml code for corresponding extensions that would normally go into Stdlib.
*)
module Universe : sig
  type t =
    | No_extensions
    | Upstream_compatible
        (** Upstream compatible extensions, also known as "erasable". *)
    | Stable  (** Extensions of [Stable] maturity. *)
    | Beta  (** Extensions of [Beta] maturity. *)
    | Alpha
        (** All extensions. This is the universe enabled by default
        for the time being. *)

  val all : t list

  (** Equal to [Alpha]. *)
  val maximal : t

  val to_string : t -> string

  val of_string : string -> t option
end

(** Disable all extensions *)
val disable_all : unit -> unit

(** Check if a language extension is "erasable", i.e. whether it can be
    harmlessly translated to attributes and compiled with the upstream
    compiler. *)
val is_erasable : 'a t -> bool

(** Print and parse language extensions; parsing is case-insensitive *)
val to_string : 'a t -> string

val to_command_line_string : 'a t -> 'a -> string

val of_string : string -> Exist.t option

val maturity_to_string : maturity -> string

(** Get the command line string enabling the given extension, if it's
    enabled; otherwise None *)
val get_command_line_string_if_enabled : 'a t -> string option

(** Enable and disable according to command-line strings; these raise
    an exception if the input string is invalid. *)
val enable_of_string_exn : string -> unit

val disable_of_string_exn : string -> unit

(** Enable and disable language extensions; these operations are idempotent *)
val set : unit t -> enabled:bool -> unit

val enable : 'a t -> 'a -> unit

val disable : 'a t -> unit

(** Check if a language extension is currently enabled (at any maturity level)
*)
val is_enabled : 'a t -> bool

(** Check if a language extension is enabled at least at the given level *)
val is_at_least : 'a t -> 'a -> bool

(** Tooling support: Temporarily enable and disable language extensions; these
    operations are idempotent.  Calls to [set], [enable], [disable] inside the body
    of the function argument will also be rolled back when the function finishes,
    but this behavior may change; nest multiple [with_*] functions instead.  *)
val with_set : unit t -> enabled:bool -> (unit -> unit) -> unit

val with_enabled : 'a t -> 'a -> (unit -> unit) -> unit

val with_disabled : 'a t -> (unit -> unit) -> unit

(** Check if the allowable extensions are restricted to only those that are
    "erasable". This is true when the universe is set to [No_extensions] or
    [Upstream_compatible]. *)
val erasable_extensions_only : unit -> bool

(** Set the extension universe and enable all allowed extensions. *)
val set_universe_and_enable_all : Universe.t -> unit

(** Parse a command-line string and call [set_universe_and_enable_all].
    Raises if the argument is invalid. *)
val set_universe_and_enable_all_of_string_exn : string -> unit

(**/**)

(** Special functionality that can only be used in "pprintast.ml" *)
module For_pprintast : sig
  (** A function for wrapping a printer from "pprintast.ml" so that it will
      unconditionally print Jane Syntax instead of raising an exception when
      trying to print syntax from disabled extensions. *)
  type printer_exporter =
    { print_with_maximal_extensions :
        'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    }

  (** Raises if called more than once ever. *)
  val make_printer_exporter : unit -> printer_exporter
end

(** Expose the exception type raised by [assert_extension_enabled] to help
    the exception printer. *)
module Error : sig
  type error = private
    | Disabled_extension :
        { ext : _ t;
          maturity : maturity option
        }
        -> error

  type exn += private Error of Location.t * error
end
