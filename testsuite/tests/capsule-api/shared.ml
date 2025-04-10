(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 runtime5;
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule
module Data = Capsule.Data

external ( = ) : 'a -> 'a -> bool @@ portable = "%eq"
external ( + ) : int -> int -> int @@ portable = "%addint"
external ( * ) : int -> int -> int @@ portable = "%mulint"

external reraise : exn -> 'a @ portable = "%reraise"

type 'a myref = { mutable v : 'a }

let mk_ref : ('a -> 'a myref) @ portable = fun v -> {v}
let read_ref : ('a : value mod portable) .
  ('a myref -> 'a @ portable contended) @ portable = fun r -> r.v
let write_ref : ('a : value mod portable contended) .
  'a -> ('a myref -> unit) @ portable = fun v r -> r.v <- v

type 'a guarded =
  | Mk : 'k Capsule.Rwlock.t * ('a, 'k) Data.Shared.t -> 'a guarded

let with_guarded (Mk (rw, p)) (f : 'k . 'k Capsule.Password.Shared.t @ local -> ('a, 'k) Data.Shared.t -> 'b) =
  Capsule.Rwlock.with_read_lock rw (fun k -> f k p)

(* Create a reference in Data.Shared *)
let ptr =
  let Capsule.Key.P brand = Capsule.create () in
  let rw = Capsule.Rwlock.create brand in
  Mk (rw, Data.Shared.create (fun () -> mk_ref 42))

(* Extract a value. *)
let () =
  with_guarded ptr (fun k p ->
    assert (Data.Shared.extract k read_ref p = 42))

(* Create another pointer. *)
let ptr' =
  let (Mk (rw, _)) = ptr in
  Mk (rw, Data.Shared.create (fun () -> mk_ref 10))

(* Write via iter, then read via extract. *)
let () =
  with_guarded ptr' (fun k p ->
    Data.Shared.iter k (write_ref 25) p)

let () =
  with_guarded ptr' (fun k p ->
    assert (Data.Shared.extract k read_ref p = 25))

(* Test [map]. *)
let ptr2 =
  let (Mk (rw, p)) = ptr in
  let p' =
    Capsule.Rwlock.with_read_lock rw
      (fun k -> Data.Shared.map k (fun _ -> mk_ref 3) p)
  in
  Mk (rw, p')

(* Test [both], [fst], [snd]. *)
let () =
  let (Mk (rw, p)) = ptr' in
  with_guarded (Mk (rw, p)) (fun k ptr_left ->
    let ptr_right = Data.Shared.create (fun () -> mk_ref 999) in
    let both_ptr = Data.Shared.both ptr_left ptr_right in
    let left_part = Data.Shared.fst both_ptr in
    let right_part = Data.Shared.snd both_ptr in
    Data.Shared.iter k (fun l -> assert (l.v = 25)) left_part;
    Data.Shared.iter k (fun r -> assert (r.v = 999)) right_part)

(* Test [inject], [project]. *)
let () =
  let i = Data.Shared.inject 123 in
  assert (Data.Shared.project i = 123);
  let both_ = Data.Shared.both i i in
  assert (Data.Shared.project (Data.Shared.fst both_) = 123);
  assert (Data.Shared.project (Data.Shared.snd both_) = 123)

(* Test [bind]. *)
let bind_test =
  let Capsule.Key.P brand = Capsule.create () in
  let rw = Capsule.Rwlock.create brand in
  Mk (rw, Data.Shared.inject 7)

let () =
  with_guarded bind_test (fun k p ->
    let p2 =
      Data.Shared.bind k (fun x -> Data.Shared.inject (x * 10)) p
    in
    assert (Data.Shared.project p2 = 70))

(* Ensure that the Rwlock is frozen on exceptions. *)
exception E

let poison_test =
  let Capsule.Key.P brand = Capsule.create () in
  let rw = Capsule.Rwlock.create brand in
  Mk (rw, Data.Shared.inject ())

let () =
  match with_guarded poison_test (fun _ _ -> raise E) with
  | exception E -> () (* Original exception *)
  | _ -> assert false

(* Verify Rwlock is now frozen. *)
let () =
  let (Mk (rw, _)) = poison_test in
  match Capsule.Rwlock.with_write_lock rw (fun k -> ()) with
  | exception Capsule.Rwlock.Frozen -> ()
  | _ -> assert false
