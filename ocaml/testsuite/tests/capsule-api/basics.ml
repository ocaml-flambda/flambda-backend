(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 runtime5;
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule

type 'a myref = { mutable v : 'a}

module Cell = struct
  type 'a t =
    | Mk : 'k Capsule.Mutex.t * ('a myref, 'k) Capsule.Data.t -> 'a t

  let create (type a : value mod portable uncontended) (x : a) : a t =
    let (P m) = Capsule.create_with_mutex () in
    let p = Capsule.Data.create (fun () -> {v = x})  in
    Mk (m, p)

  let read (type a : value mod portable uncontended) (t : a t) : a =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m (fun k ->
      let read' : a myref -> a @ portable contended @@ portable = (fun r -> r.v) in
      Capsule.Data.extract k read' p)

  let write (type a : value mod portable uncontended) (t : a t) (x : a) =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m (fun k ->
      Capsule.Data.iter k (fun r -> r.v <- x) p)
end

let () =
  let ptr = Cell.create 42 in
  assert (Cell.read ptr = 42);
  Cell.write ptr 45;
  assert (Cell.read ptr = 45)
