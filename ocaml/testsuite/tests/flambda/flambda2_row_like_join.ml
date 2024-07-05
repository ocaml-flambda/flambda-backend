(* TEST *)

(* Tests a corner case of the Flambda2 join algorithm.
   This checks that joining a row-like type where the tags are known,
   with a row-like type where the tag is not known, correctly
   takes both sides into account. *)

(* GADTs allow us to hide kind information *)
type _ t = A : (int * int) t | B : (int * int) t

let[@inline never] f (type a) (x : a) (cond : a t) =
  let result : a =
    match cond with
    | A -> (0, 1) (* Known tag *)
    | B -> begin
        let r = fst x in (* Creates an equation on [x] with unknown tag *)
        ignore (Sys.opaque_identity r);
        x
      end
  in
  (* At this point, [result] has been joined with no kind information.
     If everything went well, we should know that:
     - It can have tag 0 (as branch A has this tag)
     - It can have any tag (as branch B doesn't restrict the tag)
     The point of this test is to check that the approximation for
     [result] doesn't assume that if it has tag 0, it must have come
     from branch A. *)
  (* We now need to cast it back to a tuple: *)
  let result : int * int = match cond with A -> result | B -> result in
  (* Then we need to actually introduce the constraint on the tag.
     This is done by storing it into a block with a known shape: *)
  let ignored : (unit * (int * int)) = (), result in
  (* Now, if we wrongly assumed that only branch A has tag 0,
     we would be able to propagate that information here and replace
     [a + b] by the constant 1.*)
  let a, b = result in
  (* [ignored] is returned to make sure we don't remove the block
     creation primitive that adds the tag constraint *)
  a + b, ignored

let test () =
  let r, _ = f (2, 3) B in
  assert (r == 5);
  ()

let () = test ()
