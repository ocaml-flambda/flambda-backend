(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule

external reraise : exn -> 'a @ portable @@ portable = "%reraise"

type 'a myref = { mutable v : 'a}

let mk_ref : ('a -> 'a myref) @@ portable = fun v -> {v}

(* We need ['a] to be [portable] to return a [portable] value from the read.
   The return value is marked as [contended] because our callsites require that,
   but the typechecker does not implicitly downcast the result. *)
let read_ref : ('a : value mod portable) .
  ('a myref -> 'a @ portable contended) @@ portable = fun r -> r.v

(* We need ['a] to be [portable] and [contended] to capture it in
   a [portable] closure like this.*)
let write_ref : ('a : value mod portable contended) .
  'a -> ('a myref -> unit) @ portable = fun v r -> r.v <- v

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

(* [wrap] and [unwrap]. *)
let ptr'' =
  let (Mk (m, p)) = ptr in
  let p' =
  Capsule.Mutex.with_lock m (fun k ->
    Capsule.access k (fun c ->
      let x = Capsule.Data.unwrap c p in
      x.v <- 45;
      Capsule.Data.wrap c x))
  in
  Mk (m, p')

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
  with_guarded ptr (fun (type k) (k : k Capsule.Password.t) p ->
    match Capsule.Data.iter k (fun r -> reraise (Leak r)) p with
    | exception Capsule.Encapsulated (name, exn_data) ->
      (match Capsule.Name.equality_witness name (Capsule.Password.name k) with
       | Some Equal ->
         Capsule.Data.iter k (function
           | Leak r -> ()
           | _ -> assert false)
           exn_data
       | None -> assert false)
    | _ -> assert false)
;;

(* An exception raised from [access] is marked as [contended]: *)
let () =
  with_guarded ptr (fun (type k) (k : k Capsule.Password.t) (p : _ Capsule.Data.t) ->
    match Capsule.access k
            (fun c -> reraise (Leak (Capsule.Data.unwrap c p))) with
    | exception Capsule.Encapsulated (name, exn_data) ->
      (match Capsule.Name.equality_witness name (Capsule.Password.name k) with
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
    Capsule.Mutex.with_lock m (fun k -> Capsule.Data.map k (fun _ -> mk_ref 3) p)
  in
  Mk (m, Capsule.Data.both p p')
;;

(* [fst], [snd] *)
let () =
  let a = 1 in
  let b = 2 in
  let tup = Capsule.Data.create (fun () -> (a, b)) in
  let a_cap = Capsule.Data.fst tup in
  let b_cap = Capsule.Data.snd tup in
  assert (a = Capsule.Data.project a_cap);
  assert (b = Capsule.Data.project b_cap)

(* [destroy]. *)
let () =
  let (Mk (m, p)) = ptr2 in
  let c = Capsule.Mutex.destroy m in
  let (r1, r2) = Capsule.Data.unwrap c p in
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

(* [with_password]. *)
exception Exn of string

let () =
  match Capsule.with_password (fun _password -> "ok") with
  | s -> assert (s = "ok")
  | exception _ -> assert false
;;

let () =
  match Capsule.with_password (fun _password -> Exn "ok") with
  | Exn s -> assert (s = "ok")
  | _ -> assert false
;;

let () =
  match Capsule.with_password (fun _password -> reraise (Exn "fail")) with
  | exception (Exn s) -> assert (s = "fail")
  | _ -> assert false
;;

let () =
  match Capsule.with_password (fun (Capsule.Password.P password) ->
    let data = Capsule.Data.create (fun () -> "fail") in
    let msg = Capsule.Data.extract password (fun s : string -> s) data in
    reraise (Exn msg))
  with
  | exception (Exn s) -> assert (s = "fail")
  | _ -> assert false
;;

let () =
  match Capsule.with_password (fun (Capsule.Password.P password) ->
    let data = Capsule.Data.create (fun () -> "fail") in
    let () = Capsule.Data.extract password (fun s -> reraise (Exn s)) data in
    ())
  with
  | exception (Exn s) -> assert (s = "fail")
  | _ -> assert false
;;
