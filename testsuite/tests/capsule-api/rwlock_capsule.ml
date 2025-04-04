(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
*)

module Capsule = Stdlib_alpha.Capsule

(* [Rwlock.t] and [Data.t] are [value mod portable contended]. *)

type 'k _rwlock : value mod portable contended = 'k Capsule.Rwlock.t

type ('a, 'k) _data : value mod portable contended = ('a, 'k) Capsule.Data.t

(* Packed rwlocks are [value mod portable contended]. *)

type _packed :  value mod portable contended = Capsule.Rwlock.packed

(* CR: without [with] syntax and mode crossing inference, we need to depend on
   [@@unsafe_allow_any_mode_crossing] to determine that 'a myref crosses portabilility.
   This only holds when 'a also crosses portability *)

type 'a myref : value mod portable = { mutable v : 'a}
[@@unsafe_allow_any_mode_crossing
  "CR layouts v2.8: This can go away once we have with-kinds"]


module RwCell = struct
  type 'a t =
    | Mk : 'k Capsule.Rwlock.t * ('a myref, 'k) Capsule.Data.t -> 'a t

    let create (type a : value mod portable contended) (x : a) : a t =
      let (P k) = Capsule.create () in
      let m = Capsule.Rwlock.create k in
      let p = Capsule.Data.create (fun () -> {v = x}) in
      Mk (m, p)

    let read (type a : value mod portable contended) (t : a t) : a =
      let (Mk (m, p)) = t in
      Capsule.Rwlock.with_read_lock m (fun k ->
        let read' : a myref @ shared -> a @ portable contended = (fun r -> r.v) in
        Capsule.Data.extract_shared k read' p)

    let write (type a : value mod portable contended) (t : a t) (x : a) =
      let (Mk (m, p)) = t in
      Capsule.Rwlock.with_write_lock m (fun k ->
        Capsule.Data.iter k (fun r -> r.v <- x) p)

    let copy (type a : value mod portable contended) (t : a t) : a t =
      let (Mk (m, p)) = t in
      Capsule.Rwlock.with_read_lock m (fun k ->
        let p = Capsule.Data.map_shared k (fun r ->
          let v : a = r.v in {v = v}) p
        in
        Mk (m, p))
end

let () =
  let ptr = RwCell.create 42 in
  let ptr' = RwCell.copy ptr in
  assert (RwCell.read ptr = 42);
  RwCell.write ptr 43;
  assert (RwCell.read ptr = 43);
  assert (RwCell.read ptr' = 42)

(** Testing individual capsule operations over a password captured by a rwlock *)

external reraise : exn -> 'a @ portable @@ portable = "%reraise"

type 'a guarded =
  | Mk : 'k Capsule.Rwlock.t * ('a, 'k) Capsule.Data.t -> 'a guarded

let with_write_guarded x (f : 'k . 'k Capsule.Password.t @ local -> ('a, 'k) Capsule.Data.t -> 'b) =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_write_lock m (fun k -> f k p)
;;

let with_read_guarded x (f : 'k . 'k Capsule.Password.Shared.t @ local -> ('a, 'k) Capsule.Data.t -> 'b) =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_read_lock m (fun k -> f k p)
;;

(* reading from myref with the expected modes *)
let read_ref : ('a : value mod portable) .
  ('a myref @ shared -> 'a @ portable contended) @@ portable = fun r -> r.v

(* writing to myref with the expected modes *)
 let write_ref : ('a : value mod portable contended) .
  'a -> ('a myref -> unit) @ portable = fun v r -> r.v <- v

(* [create]. *)
let ptr =
  let (P k) = Capsule.create () in
  let m = Capsule.Rwlock.create k in
  Mk (m, Capsule.Data.create (fun () -> { v = 42 }))
;;

(* [extract]. *)
let () =
  with_write_guarded ptr (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 42))
;;

let ptr' =
  let (Mk (m, p)) = ptr in
  Mk (m, Capsule.Data.create (fun () -> { v = 2 }))

(* [iter]. *)
let () =
  with_write_guarded ptr (fun k p ->
    Capsule.Data.iter k (write_ref 15) p)
;;

let () =
  with_write_guarded ptr (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 15))
;;

let () =
  with_write_guarded ptr' (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 2))
;;

(* [extract_shared]. *)
let () =
  with_read_guarded ptr (fun k p ->
    assert (Capsule.Data.extract_shared k read_ref p = 15))
;;

external (+) : int -> int -> int @@ portable = "%addint"

(* [map_shared]. *)
let ptr2 =
let (Mk (m, p)) = ptr in
  let p' =
    Capsule.Rwlock.with_read_lock m (fun k ->
      Capsule.Data.map_shared k (fun (r @ shared) ->
        let v : int = r.v in { v = v + 2 }) p)
  in
  Mk (m, p')

(* [map_shared] and [extract_shared]. *)
let () =
  with_read_guarded ptr2 (fun k p ->
    let ptr' = Capsule.Data.map_shared k (fun (r @ shared) -> { v = r.v + 3}) p in
    assert (Capsule.Data.extract_shared k read_ref ptr' = 20))

(* [access_shared] and [unwrap_shared]. *)
let () =
  with_read_guarded ptr2 (fun k p ->
    Capsule.access_shared k (fun a ->
      let r = Capsule.Data.unwrap_shared a p in
      let ptr' = Capsule.Data.Shared.wrap a { v = r.v + 3 } in
      let res = Capsule.Data.Shared.extract k read_ref ptr' in
      assert (res = 20)) [@nontail])

exception Leak of int myref

(* An exception raised from [iter] is marked as [contended]: *)
let () =
  with_write_guarded ptr (fun (type k) (k : k Capsule.Password.t) p ->
    match Capsule.Data.iter k (fun r -> reraise (Leak r)) p with
    | exception Capsule.Encapsulated (id, exn_data) ->
      (match Capsule.Password.Id.equality_witness id (Capsule.Password.id k) with
       | Some Equal ->
         Capsule.Data.iter k (function
           | Leak r -> ()
           | _ -> assert false)
           exn_data
       | None -> assert false)
    | _ -> assert false)
;;

(* [map], [both]. *)
let ptr2 =
  let (Mk (m, p)) = ptr in
  let p' =
    Capsule.Rwlock.with_write_lock m (fun k -> Capsule.Data.map k (fun _ -> { v = 3 }) p)
  in
  Mk (m, Capsule.Data.both p p')
;;


(* [destroy]. *)
let () =
  let (Mk (m, p)) = ptr2 in
  let a = Capsule.Key.destroy (Capsule.Rwlock.destroy m) in
  let (r1, r2) = Capsule.Data.unwrap a p in
  assert (read_ref r1 = 15 && read_ref r2 = 3)
;;

let () =
  match with_write_guarded ptr (fun _ _ -> ()) with
  | exception Capsule.Rwlock.Poisoned -> ()
  | _ -> assert false
;;

let () =
  match with_write_guarded ptr' (fun _ _ -> ()) with
  | exception Capsule.Rwlock.Poisoned -> ()
  | _ -> assert false
;;

let () =
  match with_write_guarded ptr2 (fun _ _ -> ()) with
  | exception Capsule.Rwlock.Poisoned -> ()
  | _ -> assert false
;;

(* [inject], [project]. *)
let () =
  let ptr = Capsule.Data.inject 100 in
  assert (Capsule.Data.project ptr = 100)
;;

type lost_capsule = |

(* [bind]. *)
let ptr' : (int, lost_capsule) Capsule.Data.t =
  let (P k) = Capsule.create () in
  let m = Capsule.Rwlock.create k in
  let ptr = Capsule.Data.inject 100 in
  Capsule.Rwlock.with_write_lock m (fun k ->
    Capsule.Data.bind k (fun x -> Capsule.Data.inject (((+) x) 11)) ptr)
;;

let () =
  assert (Capsule.Data.project ptr' = 111)
;;

(* [freeze]. *)
let () =
  let (P k) = Capsule.create () in
  let m = Capsule.Rwlock.create k in
  let data = Capsule.Data.create (fun () -> { v = 42 }) in
  with_write_guarded (Mk (m, data)) (fun k p ->
    Capsule.Data.iter k (fun r -> r.v <- 999) p);
  let _freeze_key = Capsule.Rwlock.freeze m in
  with_read_guarded (Mk (m, data)) (fun k p ->
    assert (Capsule.Data.extract_shared k (fun r -> r.v) p = 999));
  match with_write_guarded (Mk (m, data)) (fun k p ->
    Capsule.Data.iter k (fun r -> r.v <- 123) p)
  with
  | exception Capsule.Rwlock.Frozen -> ()
  | _ -> assert false
;;

exception ReadLockTestException of int myref @@ shared

(* Exceptions raised from [with_read_lock]. *)
let () =
  let ptr =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    Mk (m, Capsule.Data.create (fun () -> { v = 999 }))
  in
  with_read_guarded ptr (fun (type k) (k : k Capsule.Password.Shared.t) p ->
    match Capsule.Data.extract_shared k (fun r -> reraise (ReadLockTestException r)) p with
    | exception Capsule.Encapsulated_shared (id, exn_data) ->
      (match Capsule.Password.Id.equality_witness id (Capsule.Password.Shared.id k) with
       | Some Equal ->
         Capsule.Data.Shared.iter k (function
           | ReadLockTestException { v = 999 } -> ()
           | _ -> assert false)
           exn_data
       | None -> assert false)
    | _ -> assert false)
;;
