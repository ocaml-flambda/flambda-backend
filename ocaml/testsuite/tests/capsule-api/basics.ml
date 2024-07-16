(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
*)

module Capsule = Stdlib_alpha.Capsule

type 'a myref = { mutable v : 'a}

(* We need extra annotations to convince the typechecker for examples below. *)
let mk_ref : ('a : value mod portable uncontended) .
  ('a -> 'a myref) @@ portable = fun v -> {v}
let read_ref : ('a : value mod portable uncontended) .
  ('a myref -> 'a @ portable contended) @@ portable = fun r -> r.v
let write_ref : ('a : value mod portable uncontended) .
  'a -> ('a myref -> unit) @ portable = fun v -> fun r -> r.v <- v

module Cell = struct
  type 'a t =
    | Mk : 'k Capsule.Mutex.t * ('a myref, 'k) Capsule.Ptr.t -> 'a t

  let create x =
    let (P m) = Capsule.create_with_mutex () in
    let p = Capsule.Ptr.create (fun () -> mk_ref x)  in
    Mk (m, p)

  let read t =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m (fun k ->
      Capsule.Ptr.extract k read_ref p)

  let write t x =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m (fun k ->
      Capsule.Ptr.iter k (write_ref x) p)
end

let () =
  let ptr = Cell.create 42 in
  assert (Cell.read ptr = 42);
  Cell.write ptr 45;
  assert (Cell.read ptr = 45)
