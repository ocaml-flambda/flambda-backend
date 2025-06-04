(* TEST
 reference = "${test_source_directory}/omitted_arguments.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 } {
   bytecode;
 }
*)

(* This is a test for applications where some arguments are omitted. We
   previously had a bug where the layouts recorded in the eta-expansion the
   compiler generates for such functions were incorrect. *)

external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"

(* Omitting named arg *)
let f1 ~name:x y = unbox_float (x +. y)

let f2 (f : name:float -> float#) = f ~name:3.14

let f3 () = f2 (f1 3.15)

let _ =
  Printf.printf "Omitting named arg (6.29): %.2f\n" (box_float (f3 ()))

(* Omitting unnamed arg *)
let f4 x ~name:y = unbox_float (x +. y)

let f5 (f : float -> float#) = f 3.14

let f6 () = f5 (f4 ~name:3.13)

let _ =
  Printf.printf "Omitting named arg (6.27): %.2f\n" (box_float (f6 ()))

(* Omitting two named args*)
let f7 ~name1 ~name2 x = unbox_float (name1 +. name2 +. x)

let f8 (f : name1:float -> name2:float -> float#) = f ~name1:3.13 ~name2:3.15

let f9 () = f8 (f7 3.14)

let _ =
  Printf.printf "Omitting named arg (9.42): %.2f\n" (box_float (f9 ()))

(* Example adapted from real code *)

let float_test : float# -> bool =
  fun f -> Stdlib_upstream_compatible.Float_u.(equal f #3.0)
let e : int -> size:int -> float# =
  fun s ~size -> unbox_float (Float.of_int s +. Float.of_int size)

let evaluate_1 () : 'a Array.t -> eval:local_ ('a -> float#) -> float# =
  fun ts ~(local_ eval) ->
    let eval i = eval (Array.get ts i) in
    if float_test (eval 0) then #3.14 else #10.0

let evaluate =
  let evaluate_2 = evaluate_1 () in
  fun t ~size ->
    evaluate_2 t ~eval:(e ~size)

let _ =
  Printf.printf "\"real\" code (3.14): %.2f\n"
    (box_float (evaluate [|2; 3; 4|] ~size:1))

(* Omitting optional arg, where let binding is needed for function. *)
module M = struct
  (* Putting this in a module defeats an optimization in simplif that removes an
     intermediate let binding where the bug occurs (it reduces [let x = y in
     ...], but only if [y] is precisely a variable, which a projection from a
     module is not). *)
  let to_string ?(explicit_plus = false) x =
    if explicit_plus then "blah" else string_of_float (box_float x)
end

let pi_to_string ~value_to_string = value_to_string #3.14
let pi = pi_to_string ~value_to_string:M.to_string
let () = Printf.printf "Omitting optional arg (3.14): %s\n" pi
