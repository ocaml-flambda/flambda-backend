(* TEST *)

type t =
| A of string * float
| B of string * float

let () =
  assert (Obj.dup (Obj.repr (A ("hello", 10.))) = Obj.repr (A ("hello", 10.)));
  assert (Obj.with_tag 1 (Obj.repr (A ("hello", 10.))) = Obj.repr (B ("hello", 10.)))

let () =
  assert (Obj.tag (Obj.with_tag 42 (Obj.repr [| |])) = 42)

(* CR mshinwell/vlaviron: Disabling these for now.  Suggestions from Vincent:

   A proper solution could be to have a proper %with_tag primitive, with type
   int -> 'a -> 'a, that doesn't require going through Obj.t.

   As a bonus, that would allow things like LexiFi's Obj.with_tag Obj.object_tag
   foo, which currently doesn't work because Obj.object_tag isn't a Lambda
   constant.
*)

(*


(* check optimisations *)
let raw_allocs f =
  let before = Gc.minor_words () in
  f ();
  let after = Gc.minor_words () in
  int_of_float (after -. before)

let allocs =
  let overhead = raw_allocs (fun () -> ()) in
  fun f -> raw_allocs f - overhead

let () =
  assert (allocs (fun () -> Obj.with_tag 1 (Obj.repr (A ("hello", 10.)))) = 0);
  assert (allocs (fun [@inline never] () -> Obj.with_tag 1 (Obj.repr (ref 10))) = 2)

*)

let () =
  print_endline "ok"

