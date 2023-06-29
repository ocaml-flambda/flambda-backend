(* TEST
   flags = "-extension layouts"

   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This file contains various tests for float#.  It's not an expect test to make
   sure it gets tested for native code. *)

(* CR layouts: This should work when we allow unboxed floats by default.  The
   test stanza above should be edit to match the one in the alpha version of the
   test, with an updated flag, and the reference file changed. *)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module type Float_u = sig
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  val ( + ) : float# -> float# -> float#
  val ( - ) : float# -> float# -> float#
  val ( * ) : float# -> float# -> float#
  val ( / ) : float# -> float# -> float#
  val ( ** ) : float# -> float# -> float#
  val ( > ) : float# -> float# -> bool
end

module Float_u : Float_u = struct
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  (* We may in the future add primitives for these, but for now this has proven
     to be good enough - the boxing/unboxing is eliminated on all
     middle-ends. *)
  let[@inline always] ( + ) x y = of_float ((to_float x) +. (to_float y))
  let[@inline always] ( - ) x y = of_float ((to_float x) -. (to_float y))
  let[@inline always] ( * ) x y = of_float ((to_float x) *. (to_float y))
  let[@inline always] ( / ) x y = of_float ((to_float x) /. (to_float y))
  let[@inline always] ( ** ) x y = of_float ((to_float x) ** (to_float y))
  let[@inline always] ( > ) x y = (to_float x) > (to_float y)
end

(* CR layouts v2: move this into a stand-alone [Float_u] module *)

(*********************************)
(* Test 1: some basic arithmetic *)

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)

(* Tests all the operators above *)
let test1 () =
  (* CR layouts: When float64 defs are allowed at the module level, get rid of
     [test1] and move these definitions there. *)
  let open Float_u in
  let pi = of_float 3.14 in
  print_floatu "Test 1, pi" pi;

  let twice_pi = pi + (of_float 3.14) in
  print_floatu "Test 1, twice_pi" twice_pi;

  let thrice_pi = (of_float 3.0) * pi in
  print_floatu "Test 1, thrice_pi" thrice_pi;

  let twice_pi_again = thrice_pi - pi in
  print_floatu "Test 1, twice_pi_again" twice_pi;

  let pi_again = twice_pi_again / (of_float 2.0) in
  print_floatu "Test 1, pi_again" pi_again;

  let twice_pi_to_the_pi = twice_pi ** pi in
  print_floatu "Test 1, twice_pi_to_the_pi" twice_pi_to_the_pi;

  let twice_pi_greater_than_pi = twice_pi > pi in
  Printf.printf "Test 1, twice_pi_greater_than_pi: %b\n"
    twice_pi_greater_than_pi;

  let pi_with_effort =
    ((of_float 3.14) + twice_pi) * (of_float 2.0) / (of_float 6.0) in
  print_floatu "Test 1, pi_with_effort" pi_with_effort

let _ = test1 ()

(*******************************************)
(* Test 2: higher-order functions, capture *)

(* CR layouts v1.5: This type definition can be eliminated once we have
   annotations. *)
type ('a : float64) t_float64 = 'a

let[@inline never] twice f (x : 'a t_float64) = f (f x)
let[@inline never] compose f g (x : 'a t_float64) = f (g x)

let[@inline never] twice_on_pi f =
  let pi = Float_u.of_float 3.14 in
  twice f pi

let times_four = twice Float_u.(fun x -> x * (of_float 2.0))

let _ =
  let open Float_u in
  print_floatu "Test 2, add pi twice"
    (twice (fun x -> x + (of_float 3.14)) (of_float 0.0));
  print_floatu "Test 2, add pi four times"
    (twice (twice (fun x -> x + (of_float 3.14))) (of_float 0.0));
  print_floatu "Test 2, increment pi twice"
    (twice_on_pi (fun x -> (of_float 1.0) + x));
  print_floatu "Test 2, increment pi four times"
    (twice_on_pi (twice (fun x -> (of_float 1.0) + x)));
  print_floatu "Test 2, e times four"
    (times_four (of_float 2.72));
  print_floatu "Test 2, pi times sixteen"
    (twice_on_pi times_four);
  print_floatu "Test 2, pi times sixteen again"
    (compose times_four times_four (of_float 3.14));
  print_floatu "Test 2, pi minus four"
    (let two = twice (fun x -> x + (of_float 1.0)) (of_float 0.0) in
     let add_two = Float_u.(+) two in
     let add_two_after = compose add_two in
     let minus_four = add_two_after (twice (fun x -> x - (of_float 3.0))) in
     minus_four (of_float 3.14))
