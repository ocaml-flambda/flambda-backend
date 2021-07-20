external opaque : 'a -> 'a = "%opaque"

type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

(* If the .mli is deleted, [bar] shouldn't be deleted either, as it
   doesn't satisfy the check on the shape of the code age relation
   (see [Simplify_common]). *)

let [@inlined always] bar map_foo = ();
  fun () -> (map_foo [@inlined never]) (fun x -> x) (fun () -> Nil) ()

let rec map_foo f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
    Cons (f x, if opaque false then bar map_foo else bar map_foo)
