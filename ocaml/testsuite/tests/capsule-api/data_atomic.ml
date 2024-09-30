(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 runtime5;
 { bytecode; }
 { native; }
*)

(** Testing that capsules can use atomic values *)

module Capsule = Stdlib_alpha.Capsule

(* Dummy API to stand in for Atomic, here hardcoded to store unit *)
module GhostUnitAtomic : sig
  type t : atomically_mutable_data

  val make : unit -> t @ coordinated_write @@ portable coordinate_nothing

  val get : t @ coordinated_read -> unit @ contended @@ portable coordinate_nothing

  val set : t @ coordinated_write -> unit @ contended -> unit @@ portable coordinate_nothing
end = struct
  type t = unit
  let make () = ()
  let get _ = ()
  let set _ _ = ()
end

type myintref = { mutable v : int }

let a @ portable coordinated_write = GhostUnitAtomic.make ()

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
  fun w ->
    GhostUnitAtomic.set a ();
    GhostUnitAtomic.get a;  w

(* [Data.create] *)
let ptr : myintref guarded =
  let (P m) = Capsule.create_with_mutex () in
  Mk (m, Capsule.Data.create (fun () -> atomic_then_id {v = 42}))

(* [Data.map] *)
let ptr =
  map_with_guarded ptr (fun pwd data -> Capsule.Data.map pwd atomic_then_id data)
;;

(* [Data.extract] *)
let () =
  let v =
    with_guarded ptr (fun pwd data ->
      Capsule.Data.extract pwd (fun w -> GhostUnitAtomic.set a (); w.v) data)
  in
  assert (v = 42)
;;

(* values at kind [atomically_mutable_data] can be [Data.inject] into capsules,
  and can be used when [Data.project] out of a capsule *)
let ptr' =
  let (Mk (m, _)) = ptr in
  let a = GhostUnitAtomic.make () in
  let ptr = Capsule.Data.inject a in
  Mk (m, ptr)
;;

let () =
  let a = with_guarded ptr' (fun _ data -> Capsule.Data.project data) in
  GhostUnitAtomic.set a ()

let () =
  let (Mk (m, data1)) = ptr in
  let data2 = Capsule.Data.inject a in
  let data3 = Capsule.Data.both data1 data2 in
  let a =
    Capsule.Mutex.with_lock m (fun pwd ->
      Capsule.Data.extract pwd (fun (w,a) -> GhostUnitAtomic.set a (); w.v) data3)
  in
  assert (a = 42)
