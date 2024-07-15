(* TEST
 expect;
*)

module M : sig
  module type S = sig
    type t
    val foo : t -> t
    val bar : t list -> t
  end
end = struct
  module type S = _
end ;;
[%%expect {|
Uncaught exception: Failure("underscore not implemented")

|}]

module M : sig
  module type S = _
end = struct
  module type S = sig
    type t
    val foo : t -> t
    val bar : t list -> t
  end
end ;;
[%%expect {|
Uncaught exception: Failure("underscore not implemented")

|}]

module M = struct
  module type S = _
end ;;
[%%expect {|
Uncaught exception: Failure("underscore not implemented")

|}]

module M : sig end = struct
  module type S = _
end ;;
[%%expect {|
Uncaught exception: Failure("underscore not implemented")

|}]

module M : sig
  module type S = sig
    type t
    val foo : t -> t
    val bar : t list -> t
  end
end = struct
  module type S1 = _
end ;;
[%%expect {|
Uncaught exception: Failure("underscore not implemented")

|}]

(* Test approx_modtype_info with mutually recursive types
   Example from manual section 12.2
   https://ocaml.org/manual/5.2/recursivemodules.html#s%3Arecursive-modules *)

module rec A : sig
  type t = Leaf of string | Node of ASet.t
  val compare: t -> t -> int
  module type S = _
end = struct
  type t = Leaf of string | Node of ASet.t
  let compare t1 t2 =
    match (t1, t2) with
    | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
    | (Leaf _, Node _) -> 1
    | (Node _, Leaf _) -> -1
    | (Node n1, Node n2) -> ASet.compare n1 n2

  module type S = _
end
and ASet
  : Set.S with type elt = A.t
  = Set.Make(A)

[%%expect{|
Uncaught exception: Failure("underscore not implemented")

|}]
