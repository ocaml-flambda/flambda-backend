(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha -alert -unsafe_multidomain";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

[@@@ocaml.alert "-unsafe_parallelism"]

module Capsule = Stdlib_alpha.Capsule

external ref : 'a -> 'a ref @@ portable = "%makemutable"
external ( ! ) : 'a ref -> 'a @@ portable = "%field0"
external ( := ) : 'a ref -> 'a -> unit @@ portable = "%setfield0"

let () = (* Signal *)
  let (P k) = Capsule.create () in
  let mut = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let wait = Atomic.make true in
  let domain = Domain.spawn (fun () ->
    Capsule.Mutex.with_lock mut (fun password ->
      Atomic.set wait false;
      while Capsule.Data.extract password (fun go : bool -> !go) go do
        Capsule.Condition.wait cond mut password
      done))
  in
  while Atomic.get wait do () done;
  Capsule.Mutex.with_lock mut (fun password ->
    Capsule.Data.iter password (fun go -> go := false) go;
  Capsule.Condition.signal cond);
  Domain.join domain
;;

let () = (* Broadcast *)
  let (P k) = Capsule.create () in
  let mut = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let ready = Atomic.make 0 in
  let domains = List.init 4 (fun _ -> Domain.spawn (fun () ->
    Capsule.Mutex.with_lock mut (fun password ->
      Atomic.incr ready;
      while Capsule.Data.extract password (fun go : bool -> !go) go do
        Capsule.Condition.wait cond mut password
      done)))
  in
  while Atomic.get ready < 4 do () done;
  Capsule.Mutex.with_lock mut (fun password ->
    Capsule.Data.iter password (fun go -> go := false) go;
  Capsule.Condition.broadcast cond);
  List.iter Domain.join domains
;;
