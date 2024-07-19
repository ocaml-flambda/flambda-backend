(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 runtime5;
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule

external reraise : exn -> 'a @ portable @@ portable = "%reraise"

type 'a myref = { mutable v : 'a}

(* We need extra annotations to convince the typechecker for examples below. *)
let mk_ref : ('a : value mod portable uncontended) .
  ('a -> 'a myref) @@ portable = fun v -> {v}
let read_ref : ('a : value mod portable uncontended) .
  ('a myref -> 'a @ portable contended) @@ portable = fun r -> r.v
let write_ref : ('a : value mod portable uncontended) .
  'a -> ('a myref -> unit) @ portable = fun v -> fun r -> r.v <- v

type 'a guarded =
  | Mk : 'k Capsule.Mutex.t * ('a, 'k) Capsule.Data.t -> 'a guarded

let with_guarded x (f : 'k . 'k Capsule.Password.t @ local -> ('a, 'k) Capsule.Data.t -> 'b) =
  let (Mk (m, p)) = x in
  Capsule.Mutex.with_lock m (fun k -> f k p)
;;

(* [create]. *)
let ptr =
  let (P m) = Capsule.create_with_mutex () in
  Mk (m, Capsule.Data.create (fun () -> mk_ref 42))
;;

(* [extract]. *)
let () =
  with_guarded ptr (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 42))
;;

let ptr' =
  let (Mk (m, p)) = ptr in
  Mk (m, Capsule.Data.create (fun () -> mk_ref 2))

(* [iter]. *)
let () =
  with_guarded ptr (fun k p ->
    Capsule.Data.iter k (write_ref 15) p)
;;

let () =
  with_guarded ptr (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 15))
;;

let () =
  with_guarded ptr' (fun k p ->
    assert (Capsule.Data.extract k read_ref p = 2))
;;

exception Leak of int myref

(* An exception raised from [iter] is marked as [contended]: *)
let () =
  with_guarded ptr (fun k p ->
    match Capsule.Data.iter k (fun r -> reraise (Leak r)) p with
    | exception Capsule.Data.Contended (Leak r) -> ()
    | _ -> assert false)
;;

(* [map], [both]. *)
let ptr2 =
  let (Mk (m, p)) = ptr in
  let p' =
    Capsule.Mutex.with_lock m (fun k -> Capsule.Data.map k (fun _ -> mk_ref 3) p)
  in
  Mk (m, Capsule.Data.both p p')
;;


(* [expose]. *)
let () =
  let (Mk (m, p)) = ptr2 in
  let k = Capsule.Mutex.destroy m in
  let (r1, r2) = Capsule.Data.expose k p in
  assert (read_ref r1 = 15 && read_ref r2 = 3)
;;

let () =
  match with_guarded ptr (fun _ _ -> ()) with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let () =
  match with_guarded ptr' (fun _ _ -> ()) with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let () =
  match with_guarded ptr2 (fun _ _ -> ()) with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

(* [inject], [project]. *)
let () =
  let ptr = Capsule.Data.inject 100 in
  assert (Capsule.Data.project ptr = 100)
;;

external (+) : int -> int -> int @@ portable = "%addint"

type lost_capsule = |

(* [bind]. *)
let ptr' : (int, lost_capsule) Capsule.Data.t =
  let (P m) = Capsule.create_with_mutex () in
  let ptr = Capsule.Data.inject 100 in
  Capsule.Mutex.with_lock m (fun k ->
    Capsule.Data.bind k (fun x -> Capsule.Data.inject (((+) x) 11)) ptr)
;;

let () =
  assert (Capsule.Data.project ptr' = 111)
;;
