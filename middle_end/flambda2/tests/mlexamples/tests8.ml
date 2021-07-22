type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let rec map_foo f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
    Cons (f x,
      fun () -> (map_foo [@inlined never]) (fun x -> x) (fun () -> Nil) ())
