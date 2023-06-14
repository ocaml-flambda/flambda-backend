type maturity = Stable | Beta | Alpha

(* Remember to update [all] when changing this type. *)
type _ t =
  | Comprehensions : unit t
  | Local : unit t
  | Include_functor : unit t
  | Polymorphic_parameters : unit t
  | Immutable_arrays : unit t
  | Module_strengthening : unit t
  | Layouts : maturity t

type 'a language_extension_kernel = 'a t

module Exist = struct
  type t = Pack : _ language_extension_kernel -> t

  let all =
    [ Pack Comprehensions
    ; Pack Local
    ; Pack Include_functor
    ; Pack Polymorphic_parameters
    ; Pack Immutable_arrays
    ; Pack Module_strengthening
    ; Pack Layouts
    ]
end

(* When you update this, update [of_string] below too. *)
let to_string : type a. a t -> string = function
  | Comprehensions -> "comprehensions"
  | Local -> "local"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays"
  | Module_strengthening -> "module_strengthening"
  | Layouts -> "layouts"

let of_string extn_name : Exist.t option =
  match String.lowercase_ascii extn_name with
  | "comprehensions" -> Some (Pack Comprehensions)
  | "local" -> Some (Pack Local)
  | "include_functor" -> Some (Pack Include_functor)
  | "polymorphic_parameters" -> Some (Pack Polymorphic_parameters)
  | "immutable_arrays" -> Some (Pack Immutable_arrays)
  | "module_strengthening" -> Some (Pack Module_strengthening)
  | "layouts" -> Some (Pack Layouts)
  | _ -> None

let maturity_to_string = function
  | Alpha -> "alpha"
  | Beta -> "beta"
  | Stable -> "stable"

(* We'll do this in a more principled way later. *)
(* CR layouts: Note that layouts is only "mostly" erasable, because of annoying
   interactions with the pre-layouts [@@immediate] attribute like:

     type ('a : immediate) t = 'a [@@immediate]

   But we've decided to punt on this issue in the short term.
*)
let is_erasable : type a. a t -> bool = function
  | Local
  | Layouts ->
      true
  | Comprehensions
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays
  | Module_strengthening ->
      false
