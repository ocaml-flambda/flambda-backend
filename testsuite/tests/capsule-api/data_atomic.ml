(* TEST
 include stdlib_alpha;
 flags += "-extension-universe alpha";
 runtime5;
 { bytecode; }
 { native; }
*)

(** Testing that capsules can use atomic values *)

module Capsule = Stdlib_alpha.Capsule

type 't myref = { mutable v : 't }

module Atomic : sig
  type !'a t : atomically_mutable_data

  val make : 'a -> 'a t @@ portable coordinate_nothing
  val get : 'a t @ coordinated_read -> 'a @@ portable coordinate_nothing
  val set : 'a t -> 'a -> unit @@ portable coordinate_nothing
end = struct
  type !'a t : atomically_mutable_data

  external make : 'a -> 'a t @@ portable coordinate_nothing = "%makemutable"
  external get : 'a t @ coordinated_read -> 'a @@ portable coordinate_nothing = "%atomic_load"
  external ignore : 'a -> unit @@ portable coordinate_nothing = "%ignore"
  external exchange : 'a t -> 'a -> 'a @@ portable coordinate_nothing = "%atomic_exchange"
  let set r x = ignore (exchange r x)
end

let a @ portable coordinated_write = Atomic.make 42

type 'a guarded =
  | Mk : 'k Capsule.Mutex.t * ('a, 'k) Capsule.Data.t -> 'a guarded

let with_guarded x
  (f : 'k . 'k Capsule.Password.t @ local
            -> ('a, 'k) Capsule.Data.t
            -> 'b) =
  let (Mk (m, p)) = x in
  Capsule.Mutex.with_lock m (fun k -> f k p)
;;

let map_with_guarded x
  (f : 'k . 'k Capsule.Password.t @ local
            -> ('a, 'k) Capsule.Data.t
            -> ('b, 'k) Capsule.Data.t) =
  let (Mk (m, p)) = x in
  Mk (m, Capsule.Mutex.with_lock m (fun k -> f k p))
;;

let atomic_then_id @ portable coordinate_writing =
  fun i w ->
    Atomic.set a i; w

(* [Data.create] *)
let ptr : (int myref) guarded =
  let (P m) = Capsule.create_with_mutex () in
  Mk (m, Capsule.Data.create (fun () -> atomic_then_id 41 {v = 42}))

(* [Data.map] *)
let ptr : (int myref) guarded =
  map_with_guarded ptr (fun pwd data -> Capsule.Data.map pwd (atomic_then_id 42) data)
;;

(* [Data.extract] *)
let () =
  let v =
    with_guarded ptr (fun pwd data ->
      Capsule.Data.extract pwd (fun w -> Atomic.set a w.v; w.v) data)
  in
  assert (v = 42);
  assert (Atomic.get a = 42)
;;

(* values at kind [atomically_mutable_data] can be [Data.inject] into capsules,
  and can be used when [Data.project] out of a capsule *)
let ptr' =
  let (Mk (m, _)) = ptr in
  let a = Atomic.make 1 in
  let ptr = Capsule.Data.inject a in
  Mk (m, ptr)
;;

let () =
  let a = with_guarded ptr' (fun _ data -> Capsule.Data.project data) in
  Atomic.set a 9

external (+) : int -> int -> int @@ portable = "%addint"

let () =
  let (Mk (m, data1)) = ptr in
  let data2 = Capsule.Data.inject a in
  let data3 = Capsule.Data.both data1 data2 in
  let a =
    Capsule.Mutex.with_lock m (fun pwd ->
      Capsule.Data.extract pwd (fun (w,a) -> Atomic.set a w.v; w.v + 1) data3)
  in
  assert (a = 43)
