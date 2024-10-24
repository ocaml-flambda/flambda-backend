(** Language extensions provided by the Jane Street version of the OCaml
    compiler.
*)

(* CR nroberts: Now that we've deleted Jane Syntax, we can fold this into
   [Language_extension] and get rid of the extra file.
*)

type maturity = Stable | Beta | Alpha

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type _ t =
  | Comprehensions : unit t
  | Mode : maturity t
  | Unique : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t
  | SIMD : maturity t
  | Labeled_tuples : unit t
  | Small_numbers : maturity t
  | Instances : unit t

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
