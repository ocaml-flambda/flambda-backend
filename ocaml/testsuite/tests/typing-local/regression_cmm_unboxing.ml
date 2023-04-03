(* TEST *)

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
