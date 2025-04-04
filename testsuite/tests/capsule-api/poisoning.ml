(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule

let m = 
  let (P k) = Capsule.create () in
  Capsule.Mutex.P (Capsule.Mutex.create k)
;;

(* Normal execution. *)
let () =
  let x = ref 1 in
  let (P m) = m in
  Capsule.Mutex.with_lock m (fun _ ->
    x := 2
  );
  assert (!x = 2)
;;

(* Trying to lock a mutex that is already locked from the same domain. *)
let () =
  let (P m) = m in
  Capsule.Mutex.with_lock m (fun _ ->
    match
      Capsule.Mutex.with_lock m (fun _ -> ())
    with
    | exception Sys_error _ -> ()
    | _ -> assert false
  )
;;

(* Poisoning the mutex by raising an exception. *)
let () =
  let (P m) = m in
  match
    Capsule.Mutex.with_lock m (fun _ ->
      raise Division_by_zero
    )
  with
  | exception Division_by_zero -> ()
  | _ -> assert false
;;

(* Operations on a poisoned mutex raise [Poisoned]. *)
let () =
  let (P m) = m in
  match
    Capsule.Mutex.with_lock m (fun _ ->
      raise Division_by_zero
    )
  with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

(* Reset *)
let m =
  let (P k) = Capsule.create () in
  Capsule.Mutex.P (Capsule.Mutex.create k)
;;

let () =
  let x = ref 1 in
  let (P m) = m in
  Capsule.Mutex.with_lock m (fun _ ->
    x := 2
  );
  assert (!x = 2)
;;

(* Destroying the mutex leaks a key. *)
let () =
  let (P m) = m in
  let _k : _ Capsule.Key.t= Capsule.Mutex.destroy m in
  ()
;;

(* Destroyed mutex is poisoned. *)
let () =
  let (P m) = m in
  match
    Capsule.Mutex.with_lock m (fun _ ->
      raise Division_by_zero
    )
  with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

(* Destroying a poisoned mutex raises [Poisoned]. *)
let () =
  let (P m) = m in
  match Capsule.Mutex.destroy m with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;
