(** Language extensions provided by the Jane Street version of the OCaml
    compiler.

    This is the signature of the {!Language_extension_kernel} module that is
    directly imported into [ppxlib_jane].
*)

type maturity = Stable | Beta | Alpha

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type _ t =
  | Comprehensions : unit t
  | Mode : maturity t
  | Unique : unit t
  | Overwriting : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t
  | SIMD : unit t
  | Labeled_tuples : unit t
  | Small_numbers : maturity t

module Exist : sig
  type 'a extn = 'a t
  type t = Pack : _ extn -> t

  val all : t list
end with type 'a extn := 'a t

module Exist_pair : sig
  type 'a extn = 'a t
  type t = Pair : 'a extn * 'a -> t
end with type 'a extn := 'a t

(** Print and parse language extensions; parsing is case-insensitive *)
val to_string : _ t -> string
val of_string : string -> Exist.t option
val pair_of_string : string -> Exist_pair.t option
val maturity_to_string : maturity -> string

(** Check if a language extension is "erasable", i.e. whether it can be
    harmlessly translated to attributes and compiled with the upstream
    compiler. *)
val is_erasable : _ t -> bool

module type Language_extension_for_jane_syntax = sig
  (** This module type defines the pieces of functionality used by
      {!Jane_syntax_parsing} and {!Jane_syntax} so that we can more easily
      import these modules into [ppxlib_jane], without also including all of the
      [Language_extension] machinery.

      It includes the stateful operations that {!Jane_syntax_parsing} relies on.
      This limits the number of bindings that [ppxlib_jane] needs to have mock
      implementations for.
  *)

  type nonrec 'a t = 'a t

  (** Check if a language extension is currently enabled. *)
  val is_enabled : _ t -> bool
  val is_at_least : 'a t -> 'a -> bool
end
