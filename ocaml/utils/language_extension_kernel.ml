type maturity = Stable | Beta | Alpha

(* Remember to update [all] when changing this type. *)
type _ t =
  | Comprehensions : unit t
  | Local : unit t
  | Unique : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t
  | SIMD : unit t
  | Labeled_tuples : unit t
  | Small_numbers : unit t

type 'a language_extension_kernel = 'a t

module Exist = struct
  type t = Pack : _ language_extension_kernel -> t

  let all =
    [ Pack Comprehensions
    ; Pack Local
    ; Pack Unique
    ; Pack Include_functor
    ; Pack Polymorphic_parameters
    ; Pack Immutable_arrays
    ; Pack Module_strengthening
    ; Pack Layouts
    ; Pack SIMD
    ; Pack Labeled_tuples
    ; Pack Small_numbers
    ]
end

module Exist_pair = struct
  type t = Pair : 'a language_extension_kernel * 'a -> t
end

(* When you update this, update [pair_of_string] below too. *)
let to_string : type a. a t -> string = function
  | Comprehensions -> "comprehensions"
  | Local -> "local"
  | Unique -> "unique"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays"
  | Module_strengthening -> "module_strengthening"
  | Layouts -> "layouts"
  | SIMD -> "simd"
  | Labeled_tuples -> "labeled_tuples"
  | Small_numbers -> "small_numbers"

(* converts full extension names, like "layouts_alpha" to a pair of
   an extension and its maturity. For extensions that don't take an
   argument, the conversion is just [Language_extension_kernel.of_string].
*)
let pair_of_string extn_name : Exist_pair.t option =
  match String.lowercase_ascii extn_name with
  | "comprehensions" -> Some (Pair (Comprehensions, ()))
  | "local" -> Some (Pair (Local, ()))
  | "unique" -> Some (Pair (Unique, ()))
  | "include_functor" -> Some (Pair (Include_functor, ()))
  | "polymorphic_parameters" -> Some (Pair (Polymorphic_parameters, ()))
  | "immutable_arrays" -> Some (Pair (Immutable_arrays, ()))
  | "module_strengthening" -> Some (Pair (Module_strengthening, ()))
  | "layouts" -> Some (Pair (Layouts, Stable))
  | "layouts_alpha" -> Some (Pair (Layouts, Alpha))
  | "layouts_beta" -> Some (Pair (Layouts, Beta))
  | "simd" -> Some (Pair (SIMD, ()))
  | "labeled_tuples" -> Some (Pair (Labeled_tuples, ()))
  | "small_numbers" -> Some (Pair (Small_numbers, ()))
  | _ -> None

let maturity_to_string = function
  | Alpha -> "alpha"
  | Beta -> "beta"
  | Stable -> "stable"

let of_string extn_name : Exist.t option =
  match pair_of_string extn_name with
  | Some (Pair (ext, _)) -> Some (Pack ext)
  | None -> None

(* We'll do this in a more principled way later. *)
(* CR layouts: Note that layouts is only "mostly" erasable, because of annoying
   interactions with the pre-layouts [@@immediate] attribute like:

     type ('a : immediate) t = 'a [@@immediate]

   But we've decided to punt on this issue in the short term.
*)
let is_erasable : type a. a t -> bool = function
  | Local
  | Unique
  | Layouts ->
      true
  | Comprehensions
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays
  | Module_strengthening
  | SIMD
  | Labeled_tuples
  | Small_numbers ->
      false

(* See the mli. *)
module type Language_extension_for_jane_syntax = sig
  type nonrec 'a t = 'a t

  val is_enabled : _ t -> bool
  val is_at_least : 'a t -> 'a -> bool
end
