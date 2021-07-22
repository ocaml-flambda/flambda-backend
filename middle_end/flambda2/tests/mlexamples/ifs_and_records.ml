external ( - ) : int -> int -> int = "%subint"

module Record = struct
  type t =
    { a : int
    ; b : int
    }
end

module Variant = struct
  type t =
    | T of
        { a : int
        ; b : int
        }
end

let[@inline] help_desired ~should_swap ~a ~b =
  if should_swap then { Record.a = b; b = a } else { Record.a; b }
;;

let[@inline] help_wrap ~should_swap ~a ~b =
  let a, b = if should_swap then b, a else a, b in
  { Record.a; b }
;;

let[@inline] help_variant ~should_swap ~a ~b =
  if should_swap then Variant.T { a = b; b = a } else Variant.T { a; b }
;;

let[@inline] help_wrap_variant ~should_swap ~a ~b =
  let a, b = if should_swap then b, a else a, b in
  Variant.T { a; b }
;;

let[@inline] help_tuple ~should_swap ~a ~b = if should_swap then b, a else a, b

let[@inline] help_interior_if ~should_swap ~a ~b =
  { Record.a = (if should_swap then b else a); b = (if should_swap then a else b) }
;;

let[@inline] help_no_if ~should_swap:_ ~a ~b = { Record.a; b }
let[@inline] help_no_if_tuple ~should_swap:_ ~a ~b = a, b

(* None of the below should allocate: *)

let main_desired ~should_swap ~a ~b =
  let { Record.a; b } = help_desired ~should_swap ~a ~b in
  a - b
;;

let main_wrap ~should_swap ~a ~b =
  let { Record.a; b } = help_wrap ~should_swap ~a ~b in
  a - b
;;

let main_variant ~should_swap ~a ~b =
  let (Variant.T { a; b }) = help_variant ~should_swap ~a ~b in
  a - b
;;

let main_wrap_variant ~should_swap ~a ~b =
  let (Variant.T { a; b }) = help_wrap_variant ~should_swap ~a ~b in
  a - b
;;

let main_tuple ~should_swap ~a ~b =
  let a, b = help_tuple ~should_swap ~a ~b in
  a - b
;;

let main_interior_if ~should_swap ~a ~b =
  let { Record.a; b } = help_interior_if ~should_swap ~a ~b in
  a - b
;;

let main_no_if ~should_swap ~a ~b =
  let { Record.a; b } = help_no_if ~should_swap ~a ~b in
  a - b
;;

let main_no_if_tuple ~should_swap ~a ~b =
  let a, b = help_no_if_tuple ~should_swap ~a ~b in
  a - b
;;

let main_hand_inline ~should_swap ~a ~b =
  let { Record.a; b } =
    if should_swap then { Record.a = b; b = a } else { Record.a; b }
  in
  a - b
;;

let main_hand_inline_tuple ~should_swap ~a ~b =
  let a, b = if should_swap then b, a else a, b in
  a - b
;;
