(* TEST
 stack-allocation;
 native;
*)

(* Regression test for a bad interaction between Cmm unboxing
   and regions present after inlining *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let[@inline always] reg k c f =
  let _ = opaque_local (local_ ref 42) in
  if c then
    f ()
  else
    Int64.add 1L k

let[@inline never] h k c f =
  let n = reg k c f in
  Int64.add 1L n

let () =
  let f () = 2L in
  Printf.printf "%Ld %Ld\n"
    (h 42L false f)
    (h 42L true f)


let[@inline always] f a b =
  let _ = opaque_local (local_ ref 42) in
  if a < b then
    a +. (a *. b)
  else
    a *. b

let[@inline never] g a b =
  let n = f a b in
  assert (n = 3.)

let () =
  let prebefore = Gc.minor_words () in
  let before = Gc.minor_words () in
  g 1. 2.;
  let after = Gc.minor_words () in
  Printf.printf "%.0f words\n"
    (after -. before -. (before -. prebefore))


let[@inline always] f a b =
  let _ = opaque_local (local_ ref 42) in
  if a < b then
    Int64.add a (Int64.mul a b)
  else
    Int64.mul a b

let[@inline never] g a b =
  let n = f a b in
  assert (n = 3L)

let () =
  let prebefore = Gc.minor_words () in
  let before = Gc.minor_words () in
  g 1L 2L;
  let after = Gc.minor_words () in
  Printf.printf "%.0f words\n"
    (after -. before -. (before -. prebefore));
