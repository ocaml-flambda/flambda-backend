(** Language extensions provided by the Jane Street version of the OCaml
    compiler. These are the parts of [Language_extension] that are required
    by [Profile_counters_functions]. Forward declaring these allow us to
    avoid a mutual dependency between files in utils/ and parsing/. Such
    a dependency prevents Merlin from compiling. *)

(* CR lstevenson: consider moving [Profile_counters_functions] to parsing/ so we
   can get rid of this file. *)

type maturity =
  | Stable
  | Beta
  | Alpha

(** The type of language extensions. An ['a t] is an extension that can either
    be off or be set to have any value in ['a], so a [unit t] can be either on
    or off, while a [maturity t] can have different maturity settings. *)
type _ t =
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

(** Print and parse language extensions; parsing is case-insensitive *)
val to_string : _ t -> string
