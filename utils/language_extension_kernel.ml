type maturity =
  | Stable
  | Beta
  | Alpha

(* Remember to update [Language_extension.Exist.all] when changing this type. *)
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

(* When you update this, update [pair_of_string] below too. *)
let to_string : type a. a t -> string = function
  | Comprehensions -> "comprehensions"
  | Mode -> "mode"
  | Unique -> "unique"
  | Overwriting -> "overwriting"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays"
  | Module_strengthening -> "module_strengthening"
  | Layouts -> "layouts"
  | SIMD -> "simd"
  | Labeled_tuples -> "labeled_tuples"
  | Small_numbers -> "small_numbers"
  | Instances -> "instances"
  | Separability -> "separability"
  | Let_mutable -> "let_mutable"
