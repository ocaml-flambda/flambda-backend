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

(* Test substitutions, when inferred type references another item
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

  class c : object
    val mutable v : int
  end

  class type d = object
    val mutable v : int
  end

  module type A = sig
    type nonrec t = t

    class e : c
    class type e1 = c
    class f : d
    class type f1 = d
  end
end = struct
  type t = int

  class c = object
    val mutable v = 5
  end

  class type d = object
    val mutable v : int
  end

  module type A = _
end

[%%expect {|
module M :
  sig
    type t
    class c : object val mutable v : int end
    class type d = object val mutable v : int end
    module type A =
      sig
        type nonrec t = t
        class e : c
        class type e1 = c
        class f : d
        class type f1 = d
      end
  end
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
    module type D
    module type C = D
  end

end = struct
  module type A

  module type B = sig
    module type D
    module type C = D
  end
end

[%%expect {|
module M :
  sig
    module type A
    module type B = sig module type D module type C = D end
  end
|}]

module M : sig
  module type A = sig
    type t
  end

  module type B = A

  module type C = B
end = struct
  module type A = sig
    type t
  end

  module type C = A

  module type B = _
end

[%%expect{|
module M :
  sig module type A = sig type t end module type B = A module type C = B end
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

[%%expect{|
Line 10, characters 2-19:
10 |   module type B = _
       ^^^^^^^^^^^^^^^^^
Error: The inferred module type refers to module type A, which is unbound here.
|}]

module M : sig
  module A : sig
    type t
  end

  module type B = sig
    val f : A.t -> A.t
  end
end = struct
  module type B = _

  module A = struct
    type t = int
  end
end

[%%expect {|
Line 10, characters 2-19:
10 |   module type B = _
       ^^^^^^^^^^^^^^^^^
Error: The inferred module type refers to type A.t, which is unbound here.
|}]


module M : sig
  type 'a t

  module type S = sig
    val foo : int t -> int t
  end
end = struct
  type t = int

  module type S = _

  module F(X:S) = struct
    let _ = X.foo 42
  end
end

[%%expect {|
Line 8, characters 2-14:
8 |   type t = int
      ^^^^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type 'a t.
|}]

module M : sig
  module S : sig
    type 'a t
  end

  module type T = sig
    val foo : int S.t -> int S.t
  end
end = struct
  module S = struct
    type t = int
  end

  module type T = _

  module F(X:T) = struct
    let _ = X.foo 42
  end
end

[%%expect{|
Line 11, characters 4-16:
11 |     type t = int
         ^^^^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type 'a t.
|}]

module M : sig
  module A : sig
    module B : sig
      module C : sig
        module D : sig
          type 'a t
        end
      end
    end
  end

  module type T = sig
    val foo : int A.B.C.D.t -> int A.B.C.D.t
  end
end = struct
  module A = struct
    module B = struct
      module C = struct
        module D = struct
          type t = int
        end
      end
    end
  end

  module type T = _

  module F(X:T) = struct
    let _ = X.foo 42
  end
end

[%%expect {|
Line 20, characters 10-22:
20 |           type t = int
               ^^^^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type 'a t.
|}]

module M : sig
  module A : sig
    type ta
    module B : sig
      type tb
      module C : sig
        type ('a, 'b) tc
        module D : sig
          type 'a td = (ta, tb) tc
        end
      end
    end
  end

  module type T = sig
    val foo : int A.B.C.D.td -> int A.B.C.D.td
  end
end = struct
  module A = struct
    type ta
    module B = struct
      type tb
      module C = struct
        type ('a, 'b) tc
        module D = struct
          type td = int
        end
      end
    end
  end

  module type T = _

  module F(X:T) = struct
    let _ = X.foo 42
  end
end

[%%expect {|
Line 26, characters 10-23:
26 |           type td = int
               ^^^^^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type 'a td = (ta, tb) tc.
|}]

module M : sig
  module type A = sig
    type 'a t
    module type B = sig
      type nonrec t = int t
    end
  end
end = struct
  module type A = _
end

[%%expect {|
module M :
  sig
    module type A =
      sig type 'a t module type B = sig type nonrec t = int t end end
  end
|}]

module M : sig
  module type A = sig
    type 'a t

    val foo : int t -> int t
  end

  module type B = A

end = struct
  module type B = _

  module F(X:B) = struct
    let _ = X.foo 42
  end

  module type A = _
end

[%%expect {|
Line 11, characters 2-19:
11 |   module type B = _
       ^^^^^^^^^^^^^^^^^
Error: The inferred module type refers to module type A, which is unbound here.
|}]

module M : sig
  module type A = sig
    type 'a t

    val foo : int t -> int t
  end

  module type B = A -> A

end = struct
  module type B = _

  module type A = _
end

[%%expect {|
Line 11, characters 2-19:
11 |   module type B = _
       ^^^^^^^^^^^^^^^^^
Error: The inferred module type refers to module type A, which is unbound here.
|}]

module M : sig
  type 'a t

  module type S = sig
    type t' = int t -> int t

    val foo : t'
  end
end = struct
  module type S = _

  module F(X:S) = struct
    let _ = X.foo "hi"
  end

  type t = string
end

[%%expect {|
Line 10, characters 2-19:
10 |   module type S = _
       ^^^^^^^^^^^^^^^^^
Error: The inferred module type refers to type t, which is unbound here.
|}]

module M : sig
  type t
  type s = t -> t
end = struct
  type s = int -> int
  type t = int
end

[%%expect {|
module M : sig type t type s = t -> t end
|}]

module M : sig
  module N : sig
    module type S = sig
      type t
    end
  end
end = struct
  module N = struct
    module type S = _
  end
end

(* CR selee: We want to be able to infer this *)
[%%expect {|
Line 9, characters 4-21:
9 |     module type S = _
        ^^^^^^^^^^^^^^^^^
Error: Cannot infer module type without a corresponding definition.
|}]

module M : sig
  module type A = sig
    type 'a t

    val foo : int t -> int t
  end

  module type B = A

  module type C = B
end = struct
  module type C = sig
    type t = int

    val foo : int -> int
  end

  module type B = _

  module type A = _
end

[%%expect {|
Line 13, characters 4-16:
13 |     type t = int
         ^^^^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type 'a t.
|}]

module A = struct
  class c = object
    val mutable v = 5
  end

  class type d = object
    val mutable v : int
  end

  module type B = sig
    type t = c -> int

    class e : c
    class type e1 = c
    class f : d
    class type f1 = d
  end
end

module M : sig
  class c : object
    val mutable v : int
  end

  class type d = object
    val mutable v : int
  end

  module type B = sig
    type t = c -> int

    class e : c
    class type e1 = c
    class f : d
    class type f1 = d
  end

  module type C = B
end = struct
  include A

  module type C = _
end

[%%expect {|
module A :
  sig
    class c : object val mutable v : int end
    class type d = object val mutable v : int end
    module type B =
      sig
        type t = c -> int
        class e : c
        class type e1 = c
        class f : d
        class type f1 = d
      end
  end
module M :
  sig
    class c : object val mutable v : int end
    class type d = object val mutable v : int end
    module type B =
      sig
        type t = c -> int
        class e : c
        class type e1 = c
        class f : d
        class type f1 = d
      end
    module type C = B
  end
|}]

module A = struct
  module type D = sig
    type 'a t
  end

  module type B = D
end

module M : sig
  module type B = sig
    type t
  end

  module type C = B
end = struct
  include A

  module type C = _
end

[%%expect {|
module A : sig module type D = sig type 'a t end module type B = D end
Line 3, characters 4-13:
3 |     type 'a t
        ^^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type t.
|}]

module A = struct
  module type B = sig
    type t = int
  end
end

module M : sig
  include module type of A

  module type C = B
end = struct
  include A

  module type C = _
end

[%%expect {|
module A : sig module type B = sig type t = int end end
module M : sig module type B = sig type t = int end module type C = B end
|}]

module M : sig
  type extend = ..

  class istack : object
    val mutable v : int list
  end

  class type sstack = object
    val mutable v : string list
    method pop : string option
    method push : string -> unit
  end

  module type T = sig type t end

  module N : sig type t end

  module O : T

  class type ['a] stack = object
    val mutable v : 'a list
    method pop : 'a option
    method push : 'a -> unit
  end

  type t

  module type S = sig
    type extend +=
      | Maybe of int                  (* it_extension_constructor *)
    ;;

    class istack2 : istack            (* class_declaration *)
    class type sstack2 = sstack       (* class_type_declaration *)

    type f = istack2 -> unit          (* class as type *)
    type g = sstack2 -> unit          (* class type as type *)

    module type T = T                 (* module_type Mty_ident *)
    module A = N                      (* module_type Mty_alias *)
    module type U = T with O          (* module_type Mty_strengthen *)

    class type istack3 = [t] stack    (* class_type *)

    type x = t list                   (* type_expr Tconstr *)
    type y = { content: t }           (* type_expr Tobject *)
    type z = N                        (* type_expr Tpackage *)
    type w = [ `X of t ]              (* type_expr Tvariant *)
  end
end = struct
  type extend = ..

  class istack = object
    val mutable v = [1]
  end

  class type sstack = object
    val mutable v : string list
    method pop : string option
    method push : string -> unit
  end

  module type T = sig type t end

  module N = struct type t = int end

  module O = struct type t = int end

  class type ['a] stack = object
    val mutable v : 'a list
    method pop : 'a option
    method push : 'a -> unit
  end

  type t

  module type S = _
end

[%%expect {|
module M :
  sig
    type extend = ..
    class istack : object val mutable v : int list end
    class type sstack =
      object
        val mutable v : string list
        method pop : string option
        method push : string -> unit
      end
    module type T = sig type t end
    module N : sig type t end
    module O : T
    class type ['a] stack =
      object
        val mutable v : 'a list
        method pop : 'a option
        method push : 'a -> unit
      end
    type t
    module type S =
      sig
        type extend += Maybe of int
        class istack2 : istack
        class type sstack2 = sstack
        type f = istack2 -> unit
        type g = sstack2 -> unit
        module type T = T
        module A = N
        module type U = sig type t = O.t end
        class type istack3 = [t] stack
        type x = t list
        type y = { content : t; }
        type z = N
        type w = [ `X of t ]
      end
  end
|}]

module M : sig
  module type S = module type of List
end = struct
  module type S = _
end

[%%expect {|
module M :
  sig
    module type S =
      sig
        type 'a t = 'a list = [] | (::) of 'a * 'a list
        val length : 'a list -> int
        val compare_lengths : 'a list -> 'b list -> int
        val compare_length_with : 'a list -> int -> int
        val is_empty : 'a list -> bool
        val cons : 'a -> 'a list -> 'a list
        val hd : 'a list -> 'a
        val tl : 'a list -> 'a list
        val nth : 'a list -> int -> 'a
        val nth_opt : 'a list -> int -> 'a option
        val rev : 'a list -> 'a list
        val init : int -> (int -> 'a) -> 'a list
        val append : 'a list -> 'a list -> 'a list
        val rev_append : 'a list -> 'a list -> 'a list
        val concat : 'a list list -> 'a list
        val flatten : 'a list list -> 'a list
        val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
        val iter : ('a -> unit) -> 'a list -> unit
        val iteri : (int -> 'a -> unit) -> 'a list -> unit
        val map : ('a -> 'b) -> 'a list -> 'b list
        val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
        val rev_map : ('a -> 'b) -> 'a list -> 'b list
        val filter_map : ('a -> 'b option) -> 'a list -> 'b list
        val concat_map : ('a -> 'b list) -> 'a list -> 'b list
        val fold_left_map :
          ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
        val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
        val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
        val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        val fold_left2 :
          ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc
        val fold_right2 :
          ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc
        val for_all : ('a -> bool) -> 'a list -> bool
        val exists : ('a -> bool) -> 'a list -> bool
        val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        val mem : 'a -> 'a list -> bool
        val memq : 'a -> 'a list -> bool
        val find : ('a -> bool) -> 'a list -> 'a
        val find_opt : ('a -> bool) -> 'a list -> 'a option
        val find_index : ('a -> bool) -> 'a list -> int option
        val find_map : ('a -> 'b option) -> 'a list -> 'b option
        val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option
        val filter : ('a -> bool) -> 'a list -> 'a list
        val find_all : ('a -> bool) -> 'a list -> 'a list
        val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
        val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
        val partition_map :
          ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list
        val assoc : 'a -> ('a * 'b) list -> 'b
        val assoc_opt : 'a -> ('a * 'b) list -> 'b option
        val assq : 'a -> ('a * 'b) list -> 'b
        val assq_opt : 'a -> ('a * 'b) list -> 'b option
        val mem_assoc : 'a -> ('a * 'b) list -> bool
        val mem_assq : 'a -> ('a * 'b) list -> bool
        val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
        val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
        val split : ('a * 'b) list -> 'a list * 'b list
        val combine : 'a list -> 'b list -> ('a * 'b) list
        val sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
        val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
        val to_seq : 'a list -> 'a Seq.t
        val of_seq : 'a Seq.t -> 'a list
      end
  end
|}]

module M : sig
  module A : sig type t end
  module type S = sig type t = A.t -> A.t end
end = struct
  module A = struct end
  module type S = _
end

(* CR selee: Currently our boundness check relies on the compatibility check.
   As this branch doesn't have the compatibility check, it doesn't error properly
   in this case. This should error properly before inclusion checking *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   module A = struct end
6 |   module type S = _
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module A : sig end
           module type S = sig type t = A.t -> A.t end
         end
       is not included in
         sig
           module A : sig type t end
           module type S = sig type t = A.t -> A.t end
         end
       In module A:
       Modules do not match: sig end is not included in sig type t end
       In module A:
       The type `t' is required but not provided
|}]
