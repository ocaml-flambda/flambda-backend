module type S = sig
  (** This is a winnowed-down version of the [Language_extension] module used
      in the Jane Street OCaml compiler.

      It exports the pieces of functionality used by {!Jane_syntax_parsing} and
      {!Jane_syntax} so that we can more easily put these modules into public
      release (without also including all of the [Language_extension] machinery.)

      Comments are kept in-sync with [Language_extension] on a best-effort basis.
  *)

  type maturity = Stable | Beta | Alpha

  type 'a t =
    | Comprehensions : unit t
    | Local : unit t
    | Include_functor : unit t
    | Polymorphic_parameters : unit t
    | Immutable_arrays : unit t
    | Module_strengthening : unit t
    | Layouts : maturity t

  type 'a language_extension := 'a t

  module Exist : sig
    type t = Pack : _ language_extension -> t
  end

  (** Print and parse language extensions; parsing is case-insensitive *)
  val to_string : _ t -> string
  val of_string : string -> Exist.t option
  val maturity_to_string : maturity -> string

  (** Check if a language extension is currently enabled *)
  val is_enabled : _ t -> bool
  val is_at_least : 'a t -> 'a -> bool

  (** Check if a language extension is "erasable", i.e. whether it can be
      harmlessly translated to attributes and compiled with the upstream
      compiler. *)
  val is_erasable : _ t -> bool
end
