module type Language_extension_kernel = sig
  (** Language extensions provided by the Jane Street version of the OCaml
      compiler.

      In contrast to the {!Language_extension} module, which defines both
      these types and the state that tracks which extensions are enabled, this
      module just defines the language extension types and stateless functions
      over them, such as the string component of OCaml attributes corresponding
      to that language extension.

    Comments are kept in-sync with {!Language_extension} on a best-effort basis.
  *)

  type maturity = Stable | Beta | Alpha

  (** The type of language extensions. An ['a t] is an extension that can either
      be off or be set to have any value in ['a], so a [unit t] can be either on
      or off, while a [maturity t] can have different maturity settings. *)
  type _ t =
    | Comprehensions : unit t
    | Local : unit t
    | Include_functor : unit t
    | Polymorphic_parameters : unit t
    | Immutable_arrays : unit t
    | Module_strengthening : unit t
    | Layouts : maturity t

  module Exist : sig
    type 'a extn = 'a t
    type t = Pack : _ extn -> t

    val all : t list
  end with type 'a extn := 'a t

  (** Print and parse language extensions; parsing is case-insensitive *)
  val to_string : _ t -> string
  val of_string : string -> Exist.t option
  val maturity_to_string : maturity -> string

  (** Check if a language extension is "erasable", i.e. whether it can be
      harmlessly translated to attributes and compiled with the upstream
      compiler. *)
  val is_erasable : _ t -> bool
end


module type Language_extension_kernel_for_jane_syntax = sig
  (** This is a winnowed-down version of the [Language_extension] module used
      in the Jane Street OCaml compiler.

      It exports the pieces of functionality used by {!Jane_syntax_parsing} and
      {!Jane_syntax} so that we can more easily put these modules into public
      release (without also including all of the [Language_extension]
      machinery.)

      In addition to the bindings from {!Language_extension_kernel}, it includes
      the stateful operations that {!Jane_syntax_parsing} relies on. This
      limits the number of bindings we have to write mock implementations
      for.
  *)

  include Language_extension_kernel

  (** Check if a language extension is currently enabled. *)
  val is_enabled : _ t -> bool
  val is_at_least : 'a t -> 'a -> bool
end
