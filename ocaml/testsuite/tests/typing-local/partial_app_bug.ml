(* TEST *)

type 'a box = Box of 'a
module X : sig
  val f : int -> string -> local_ int box
end = struct
  let[@inline never] f x y = exclave_ (local_ (Box x))
end

let[@inline always] h x = x

let g a = (h X.f) a