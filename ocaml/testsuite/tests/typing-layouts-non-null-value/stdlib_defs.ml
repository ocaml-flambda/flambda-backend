(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

let id_non_null_value : ('a : non_null_value). 'a -> 'a = fun x -> x

module Possibly_null : sig
  type t : value

  val create : int -> t
  val destroy : t -> int
  val compare : t -> t -> int
end = struct
  type t = int

  let create x = x
  let destroy x = x
  let compare = Int.compare
end
;;

[%%expect{|
val id_non_null_value : ('a : non_null_value). 'a -> 'a = <fun>
module Possibly_null :
  sig
    type t : value
    val create : int -> t
    val destroy : t -> int
    val compare : t -> t -> int
  end
|}]
;;

(* CR layouts v3.0: decide whether [Atomic.t] should be non-null and accept
   nullable values. *)

let _ = id_non_null_value (Atomic.make 3)
;;

[%%expect{|
Line 1, characters 26-41:
1 | let _ = id_non_null_value (Atomic.make 3)
                              ^^^^^^^^^^^^^^^
Error: This expression has type int Atomic.t
       but an expression was expected of type ('a : non_null_value)
       The layout of int Atomic.t is value, because
         of layout requirements from an imported definition.
       But the layout of int Atomic.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]


let _ = Atomic.make (Possibly_null.create 2)
;;

[%%expect{|
- : Possibly_null.t Atomic.t = <abstr>
|}]

(* CR layouts v3.0: [Bigarray.t] should be non-null. *)

let _ = id_non_null_value (Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 3)
;;

[%%expect{|
Line 1, characters 26-87:
1 | let _ = id_non_null_value (Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 3)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type
         (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
       but an expression was expected of type ('a : non_null_value)
       The layout of (float, Bigarray.float64_elt, Bigarray.c_layout)
                     Bigarray.Array1.t is value, because
         of layout requirements from an imported definition.
       But the layout of (float, Bigarray.float64_elt, Bigarray.c_layout)
                         Bigarray.Array1.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Buffer.t] should be non-null. *)

let _ = id_non_null_value (Buffer.create 5)
;;

[%%expect{|
Line 1, characters 26-43:
1 | let _ = id_non_null_value (Buffer.create 5)
                              ^^^^^^^^^^^^^^^^^
Error: This expression has type Buffer.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Buffer.t is value, because
         of layout requirements from an imported definition.
       But the layout of Buffer.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Condition.t] should be non-null. *)

let _ = id_non_null_value (Condition.create ())
;;

[%%expect{|
Line 1, characters 26-47:
1 | let _ = id_non_null_value (Condition.create ())
                              ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Condition.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Condition.t is value, because
         of layout requirements from an imported definition.
       But the layout of Condition.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Hashtbl.t] should be non-null and accept nullable values. *)

let _ = id_non_null_value (Hashtbl.create 2)
;;

[%%expect{|
Line 1, characters 26-44:
1 | let _ = id_non_null_value (Hashtbl.create 2)
                              ^^^^^^^^^^^^^^^^^^
Error: This expression has type ('a, 'b) Hashtbl.t
       but an expression was expected of type ('c : non_null_value)
       The layout of ('a, 'b) Hashtbl.t is value, because
         of layout requirements from an imported definition.
       But the layout of ('a, 'b) Hashtbl.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = Hashtbl.add (Hashtbl.create 1) "test" (Possibly_null.create 7)
;;

[%%expect{|
- : unit = ()
|}]

(* CR layouts v3.0: [In_channel.t] and [Out_channel.t] should be non-null. *)

let _ = id_non_null_value stdin
;;

[%%expect{|
Line 1, characters 26-31:
1 | let _ = id_non_null_value stdin
                              ^^^^^
Error: This expression has type in_channel
       but an expression was expected of type ('a : non_null_value)
       The layout of in_channel is value, because
         of layout requirements from an imported definition.
       But the layout of in_channel must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = id_non_null_value stdout
;;

[%%expect{|
Line 1, characters 26-32:
1 | let _ = id_non_null_value stdout
                              ^^^^^^
Error: This expression has type out_channel
       but an expression was expected of type ('a : non_null_value)
       The layout of out_channel is value, because
         of layout requirements from an imported definition.
       But the layout of out_channel must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Map.t] should be non-null and accept nullable values. *)

module M0 = Map.Make(Possibly_null)
;;

[%%expect{|
module M0 :
  sig
    type key = Possibly_null.t
    type 'a t = 'a Map.Make(Possibly_null).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
|}]

module M1 = Map.Make(String)
;;

[%%expect{|
module M1 :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
|}]

let _ = id_non_null_value M1.empty
;;

[%%expect{|
Line 1, characters 26-34:
1 | let _ = id_non_null_value M1.empty
                              ^^^^^^^^
Error: This expression has type 'a M1.t = 'a Map.Make(String).t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a M1.t is value, because
         of layout requirements from an imported definition.
       But the layout of 'a M1.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = M1.add "test" (Possibly_null.create 8) M1.empty
;;

[%%expect{|
- : Possibly_null.t M1.t = <abstr>
|}]

(* CR layouts v3.0: [Mutex.t] should be non-null. *)

let _ = id_non_null_value (Mutex.create ())
;;

[%%expect{|
Line 1, characters 26-43:
1 | let _ = id_non_null_value (Mutex.create ())
                              ^^^^^^^^^^^^^^^^^
Error: This expression has type Mutex.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Mutex.t is value, because
         of layout requirements from an imported definition.
       But the layout of Mutex.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Queue.t] should be non-null and accept nullable values. *)

let _ = id_non_null_value (Queue.create ())
;;
[%%expect{|
Line 1, characters 26-43:
1 | let _ = id_non_null_value (Queue.create ())
                              ^^^^^^^^^^^^^^^^^
Error: This expression has type 'a Queue.t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a Queue.t is value, because
         of layout requirements from an imported definition.
       But the layout of 'a Queue.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]


let _ = Queue.add (Possibly_null.create 8) (Queue.create ())
;;

[%%expect{|
- : unit = ()
|}]

(* CR layouts v3.0: [Random.State.t] should be non-null. *)

let _ = id_non_null_value (Random.State.make_self_init ())
;;

[%%expect{|
Line 1, characters 26-58:
1 | let _ = id_non_null_value (Random.State.make_self_init ())
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Random.State.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Random.State.t is value, because
         of layout requirements from an imported definition.
       But the layout of Random.State.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Semaphore.Counting.t] and [Semaphore.Binary.t]
   should be non-null. *)

let _ = id_non_null_value (Semaphore.Counting.make 3)
;;

[%%expect{|
Line 1, characters 26-53:
1 | let _ = id_non_null_value (Semaphore.Counting.make 3)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Semaphore.Counting.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Semaphore.Counting.t is value, because
         of layout requirements from an imported definition.
       But the layout of Semaphore.Counting.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = id_non_null_value (Semaphore.Binary.make false)
;;

[%%expect{|
Line 1, characters 26-55:
1 | let _ = id_non_null_value (Semaphore.Binary.make false)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Semaphore.Binary.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Semaphore.Binary.t is value, because
         of layout requirements from an imported definition.
       But the layout of Semaphore.Binary.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Set.t] should be non-null and accept nullable values. *)

module M2 = Set.Make(Possibly_null)
;;

[%%expect{|
module M2 :
  sig
    type elt = Possibly_null.t
    type t = Set.Make(Possibly_null).t
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

let _ = id_non_null_value M2.empty
;;

[%%expect{|
Line 1, characters 26-34:
1 | let _ = id_non_null_value M2.empty
                              ^^^^^^^^
Error: This expression has type M2.t = Set.Make(Possibly_null).t
       but an expression was expected of type ('a : non_null_value)
       The layout of M2.t is value, because
         of layout requirements from an imported definition.
       But the layout of M2.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

(* CR layouts v3.0: [Stack.t] should be non-null and accept nullable values. *)

let _ = id_non_null_value (Stack.create ())
;;

[%%expect{|
Line 1, characters 26-43:
1 | let _ = id_non_null_value (Stack.create ())
                              ^^^^^^^^^^^^^^^^^
Error: This expression has type 'a Stack.t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a Stack.t is value, because
         of layout requirements from an imported definition.
       But the layout of 'a Stack.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = Stack.push (Possibly_null.create 0) (Stack.create ())
;;

[%%expect{|
- : unit = ()
|}]

(* CR layouts v3.0: [Uchar.t] should be non-null. *)

let _ = id_non_null_value (Uchar.of_char 'x')
;;

[%%expect{|
- : Uchar.t = <abstr>
|}]

(* CR layouts v3.0: [Weak.t] should be non-null and possibly accept nullable values. *)

let _ = id_non_null_value (Weak.create 2)
;;

[%%expect{|
Line 1, characters 26-41:
1 | let _ = id_non_null_value (Weak.create 2)
                              ^^^^^^^^^^^^^^^
Error: This expression has type 'a Weak.t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a Weak.t is value, because
         of layout requirements from an imported definition.
       But the layout of 'a Weak.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 1, characters 4-21.
|}]

let _ = Weak.set (Weak.create 2) 0 (Some (Possibly_null.create 0))
;;

[%%expect{|
- : unit = ()
|}]
