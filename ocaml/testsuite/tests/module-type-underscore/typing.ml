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
module M :
  sig
    module type S = sig type t val foo : t -> t val bar : t list -> t end
  end
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
Line 2, characters 2-19:
2 |   module type S = _
      ^^^^^^^^^^^^^^^^^
Error: Inference of module types is not allowed within a signature.
|}]

module M = struct
  module type S = _
end ;;
[%%expect {|
Line 2, characters 2-19:
2 |   module type S = _
      ^^^^^^^^^^^^^^^^^
Error: Cannot infer module type without a corresponding definition.
|}]

module M : sig end = struct
  module type S = _
end ;;
[%%expect {|
Line 2, characters 2-19:
2 |   module type S = _
      ^^^^^^^^^^^^^^^^^
Error: Cannot infer module type without a corresponding definition.
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
Line 8, characters 2-20:
8 |   module type S1 = _
      ^^^^^^^^^^^^^^^^^^
Error: Cannot infer module type without a corresponding definition.
|}]

(* Test approx_modtype_info with mutually recursive types
   Example from manual section 12.2
   https://ocaml.org/manual/5.2/recursivemodules.html#s%3Arecursive-modules *)

module rec A : sig
  type t = Leaf of string | Node of ASet.t
  val compare: t -> t -> int
  module type S = sig
    type t'
    val foo : t' -> t'
  end
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
module rec A :
  sig
    type t = Leaf of string | Node of ASet.t
    val compare : t -> t -> int
    module type S = sig type t' val foo : t' -> t' end
  end
and ASet :
  sig
    type elt = A.t
    type t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
|}]

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
Line 4, characters 2-19:
4 |   module type S = _
      ^^^^^^^^^^^^^^^^^
Error: Inference of module types is not allowed within a signature.
|}]

module M = (struct
  module type S = _
end : sig
  module type S = sig
    type t
    val foo : t -> t
    val bar : t list -> t
  end
end);;
[%%expect {|
module M :
  sig
    module type S = sig type t val foo : t -> t val bar : t list -> t end
  end
|}]


module M : sig
  module type S = sig
    module type S1 = sig
      module M1 : sig
        module type S2 = sig
          type t
          val foo : t -> t
          val bar : t list -> t
        end
      end
    end
  end
end = struct
  module type S = _
end
[%%expect {|
module M :
  sig
    module type S =
      sig
        module type S1 =
          sig
            module M1 :
              sig
                module type S2 =
                  sig type t val foo : t -> t val bar : t list -> t end
              end
          end
      end
  end
|}]

module M : sig
  module type S = sig
    type t
    val do_something : t -> unit
    val make_something : unit -> t
    val foo : t -> t
  end
end = struct
  module type S = _

  module M1 : S = struct
    type t = int
    let do_something = ignore
    let make_something () = 0
    let foo t = t * t
    let bar (t : t) = t
  end
end
[%%expect{|
module M :
  sig
    module type S =
      sig
        type t
        val do_something : t -> unit
        val make_something : unit -> t
        val foo : t -> t
      end
  end
|}]

module M : sig
  module type S = sig
    type t
    val do_something : t -> unit
    val make_something : unit -> t
    val foo : t -> t
  end
end = struct
  module type S = _

  module M1 : S = struct
    type t = int
    let do_something = ignore
    let make_something () = 0
    let foo t = ignore (t * t)
  end
end
[%%expect{|
Lines 11-16, characters 18-5:
11 | ..................struct
12 |     type t = int
13 |     let do_something = ignore
14 |     let make_something () = 0
15 |     let foo t = ignore (t * t)
16 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = int
           val do_something : 'a -> unit
           val make_something : unit -> int
           val foo : int -> unit
         end
       is not included in
         S
       Values do not match:
         val foo : int -> unit
       is not included in
         val foo : t -> t
       The type int -> unit is not compatible with the type t -> t
       Type unit is not compatible with type t = int
|}]

module M : sig
  module type S = sig
    type t
    val do_something : t -> unit
    val make_something : unit -> t
    val foo : t -> t
  end
end = struct
  module type S = _

  module M1 : S with type t=string = struct
    type t = int
    let do_something = ignore
    let make_something () = 0
    let foo t = t * t
    let bar (t : t) = t
  end
end

[%%expect{|
Lines 11-17, characters 37-5:
11 | .....................................struct
12 |     type t = int
13 |     let do_something = ignore
14 |     let make_something () = 0
15 |     let foo t = t * t
16 |     let bar (t : t) = t
17 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = int
           val do_something : 'a -> unit
           val make_something : unit -> int
           val foo : int -> int
           val bar : t -> t
         end
       is not included in
         sig
           type t = string
           val do_something : t -> unit
           val make_something : unit -> t
           val foo : t -> t
         end
       Type declarations do not match:
         type t = int
       is not included in
         type t = string
       The type int is not equal to the type string
|}]

module M : sig
  module type S1 = sig
    type t

    val foo : t -> unit
  end

  module type S2 = sig
    type t
    val bar : unit -> t
  end
end = struct
  module type S2 = _
  module type S1 = _
end

[%%expect{|
module M :
  sig
    module type S1 = sig type t val foo : t -> unit end
    module type S2 = sig type t val bar : unit -> t end
  end
|}]

module M : sig
  module type A = module type of
    struct
      module type B = sig type t end
    end
end = struct
  module type A = _
end

[%%expect {|
module M : sig module type A = sig module type B = sig type t end end end
|}]

module M : sig
  module type A = module type of (
    struct
      module type B = _
    end : sig
      module type B = sig type t end
    end)
end = struct
  module type A = _
end

[%%expect {|
module M : sig module type A = sig module type B = sig type t end end end
|}]

module M1: sig
  module type S = sig
    type t
  end
end = struct
  module type S = _
end

module M2 = struct
  include M1

  module M3 : S = struct
    type t = int
  end
end

[%%expect {|
module M1 : sig module type S = sig type t end end
module M2 : sig module type S = M1.S module M3 : S end
|}]

module M : sig
  module type S = sig
    module M1 : sig
      type t
    end
    val foo : M1.t -> M1.t
  end
end = struct
  module M1 = struct
    type t = string
  end

  module type S = _

  module M2 : S with module M1=M1 = struct
    module M1 = M1
    let foo x = x * x
  end
end

[%%expect {|
Lines 15-18, characters 36-5:
15 | ....................................struct
16 |     module M1 = M1
17 |     let foo x = x * x
18 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module M1 = M1 val foo : int -> int end
       is not included in
         sig module M1 : sig type t = string end val foo : M1.t -> M1.t end
       Values do not match:
         val foo : int -> int
       is not included in
         val foo : M1.t -> M1.t
       The type int -> int is not compatible with the type M1.t -> M1.t
       Type int is not compatible with type M1.t = string
|}]

module M : sig
  module type S = sig
    module M1 : sig
      type t
    end
    val foo : M1.t -> M1.t
  end
end = struct
  module M1 = struct
    type t = int
  end

  module type S = _

  module M2 : S with module M1=M1 = struct
    module M1 = M1
    let foo x = x * x
  end
end

[%%expect {|
module M :
  sig
    module type S = sig module M1 : sig type t end val foo : M1.t -> M1.t end
  end
|}]

(* Test substitutions, when inferred type references another type/module/module type
   from the signature *)

module M : sig
  module type A = sig
    type t

    val foo : t -> t
  end

  module type B = A
end = struct
  module type A = sig
    type t
    val foo : t -> t
  end

  module type B = _
end

[%%expect {|
module M :
  sig module type A = sig type t val foo : t -> t end module type B = A end
|}]

module M : sig
  module type A = sig
    type t

    val foo : t -> t
  end

  module type B = A
end = struct
  module type A = _

  module type B = _
end

[%%expect {|
module M :
  sig module type A = sig type t val foo : t -> t end module type B = A end
|}]

module M : sig
  module type A

  module type B = A
end = struct
  module type A

  module type B = _
end

[%%expect {|
module M : sig module type A module type B = A end
|}]


module M : sig
  type t

  module type A = sig
    type nonrec t = t
  end
end = struct
  type t = int
  module type A = _
end

[%%expect {|
module M : sig type t module type A = sig type nonrec t = t end end
|}]


module M : sig
  module M1 : sig
    type t1
  end

  module type A = sig
    module M2 = M1
  end
end = struct
  module M1 = struct
    type t1 = int
  end

  module type A = _
end

[%%expect {|
module M :
  sig module M1 : sig type t1 end module type A = sig module M2 = M1 end end
|}]

module M : sig
  module type A

  module type B = sig
    module type A
    module type C = A
  end

end = struct
  module type A

  module type B = _
end

[%%expect {|
module M :
  sig
    module type A
    module type B = sig module type A module type C = A end
  end
|}]

module M : sig
  module rec A : sig
    type t
    val foo : B.t -> B.t
  end
  and B : sig
    type t
    val bar : A.t -> A.t
  end

  module type S = sig
    val baz : A.t -> B.t
  end
end = struct
  module rec A : sig
    type t
    val foo : B.t -> B.t
  end = struct
    type t = int
    let foo s = s
  end
  and B : sig
    type t
    val bar : A.t -> A.t
  end = struct
    type t = string
    let bar n = n
  end

  module type S = _
end

[%%expect {|
module M :
  sig
    module rec A : sig type t val foo : B.t -> B.t end
    and B : sig type t val bar : A.t -> A.t end
    module type S = sig val baz : A.t -> B.t end
  end
|}]

module M : sig
  module type A = sig
    type t

    val foo : t -> t
  end

  module type B = A
end = struct
  module type B = _

  module type A = _
end

(* CR selee: Give a better error message later *)
[%%expect{|
Lines 9-13, characters 6-3:
 9 | ......struct
10 |   module type B = _
11 |
12 |   module type A = _
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type B = A
           module type A = sig type t val foo : t -> t end
         end
       is not included in
         sig
           module type A = sig type t val foo : t -> t end
           module type B = A
         end
       Module type declarations do not match:
         module type B = A/2
       does not match
         module type B = A/1
       At position module type B = <here>
       Module types do not match: A/2 is not equal to A/1

       Line 12, characters 2-19:
         Definition of module type A/1
|}]
