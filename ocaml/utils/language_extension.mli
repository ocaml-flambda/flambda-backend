(** Language extensions provided by the Jane Street version of the OCaml
    compiler.
*)

(** A setting for extensions that track multiple maturity levels *)
type maturity = Language_extension_kernel.maturity =
  | Stable
  | Beta
  | Alpha

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type 'a t = 'a Language_extension_kernel.t =
  | Comprehensions : unit t
  | Mode : unit t
  | Unique : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t
  | SIMD : unit t
  | Labeled_tuples : unit t
  | Small_numbers : unit t

(** Existentially packed language extension *)
module Exist : sig
  type 'a extn = 'a t
  (* this is removed from the sig by the [with] below; ocamldoc doesn't like
     [:=] in sigs *)

  type t = Language_extension_kernel.Exist.t = Pack : 'a extn -> t

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

(** The type of langauge extension universes. *)
module Universe : sig
  type t =
    | No_extensions
    | Upstream_compatible
    | Stable
    | Beta
    | Alpha

  val all : t list

  val to_string : t -> string

  val of_string : string -> t option
end

(** Disable all extensions *)
val disable_all : unit -> unit

(** Maximally enable all extensions (that is, set to [Alpha] for [maturity]
    extensions. *)
val enable_maximal : unit -> unit

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
    operations are idempotent.  Calls to [set], [enable], [disable], and
    [disallow_extensions] inside the body of the function argument will also
    be rolled back when the function finishes, but this behavior may change;
    nest multiple [with_*] functions instead.  *)
val with_set : unit t -> enabled:bool -> (unit -> unit) -> unit

val with_enabled : 'a t -> 'a -> (unit -> unit) -> unit

val with_disabled : 'a t -> (unit -> unit) -> unit

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

(** Permanently ban all extensions; used for [-disable-all-extensions] to ensure
    that some code is 100% extension-free.  When called, disables any
    currently-enabled extensions, including the defaults.  Causes any future
    uses of [set ~enabled:true], [enable], and their [with_] variants to raise;
    also causes any future uses of [restrict_to_erasable_extensions] to raise.
    The [is_enabled] function will still work, it will just always return
    [false].*)
val disallow_extensions : unit -> unit

(** Check if the allowable extensions are restricted to only those that are
    "erasable". This is true when [restrict_to_erasable_extensions] was called. *)
val erasable_extensions_only : unit -> bool

val set_universe : Universe.t -> unit

val set_universe_of_string_exn : string -> unit

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
