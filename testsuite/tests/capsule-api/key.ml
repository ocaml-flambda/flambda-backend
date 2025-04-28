(* TEST
 include stdlib_alpha;
 flags = "-extension-universe alpha";
 { bytecode; }
 { native; }
*)

module Capsule = Stdlib_alpha.Capsule

external ( = ) : 'a @ local shared -> 'a @ local shared -> bool @@ portable = "%equal"

external ref : 'a -> 'a ref @@ portable = "%makemutable"
external ( ! ) : 'a ref -> 'a @@ portable = "%field0"
external ( := ) : 'a ref -> 'a -> unit @@ portable = "%setfield0"

external raise : exn -> 'a @ portable unique @@ portable = "%reraise"

type 'a aliased = Aliased of 'a @@ aliased [@@unboxed]



let () =
  (* [Capsule.create] creates a new capsule with an unique key. *)
  let packed : Capsule.Key.packed @ unique = Capsule.create () in
  let (P k) = packed in
  (* [Capsule.Key.access] *)
  let Aliased x, k =
    Capsule.Key.access k (fun a ->
      Aliased (Capsule.Data.wrap a (ref "Hello world!")))
  in
  (* [Capsule.Key.with_password] *)
  let (), k =
    Capsule.Key.with_password k (fun p ->
      Capsule.Data.iter p (fun s -> assert (!s = "Hello world!")) x)
  in
  ()
;;

let () =
  let (P k) = Capsule.create () in
  let Aliased x, k =
    Capsule.Key.access k (fun a ->
      Aliased (Capsule.Data.wrap a (ref "My value")))
  in
  (* [Capsule.Mutex.create] takes the key. *)
  let m = Capsule.Mutex.create k in
  let () = Capsule.Mutex.with_lock m (fun p ->
    Capsule.Data.iter p (fun s -> s := "Another value") x)
  in
  (* [Capsule.Mutex.destroy] returns the key. *)
  let k = Capsule.Mutex.destroy m in
  (* [Capsule.Key.destroy] merges the capsule into the current one. *)
  let a = Capsule.Key.destroy k in
  assert (!(Capsule.Data.unwrap a x) = "Another value")
;;

let () =
  let exception E of string in
  let (P k) = Capsule.create () in
  (* On exception, it is re-raised and the key is destroyed.*)
  try
    let (), _k = Capsule.Key.access k (fun _ -> raise (E "Exception!")) in ()
  with E s -> assert (s = "Exception!")
;;

let () =
  let exception F of int in
  let (P k) = Capsule.create () in
  (* Encapsulated exceptions from inside [with_password] are unwrapped. *)
  try
    let (), _k = Capsule.Key.with_password k (fun p ->
      let () = Capsule.access p (fun _ -> raise (F 1)) in ())
    in ()
  with F n -> assert (n = 1)
;;

let () =
  let (P k) = Capsule.create () in
  (* [with_password_local] destroys the key but returns a local value. *)
  let x, p = Capsule.Key.with_password_local k (fun p ->
    exclave_ (Capsule.access_local p (fun a ->
      Capsule.Data.wrap a (ref "Local value")), p))
  in
  (* We can still access the data through the password. *)
  let s =
    Capsule.Data.Local.extract p
      (fun x -> (x.contents : string @ portable)) x
  in
  assert (s = "Local value")
;;

let () =
  let exception E of string in
  let (P k) = Capsule.create () in
  (* On exception, it is re-raised and the key is destroyed. *)
  try
    Capsule.Key.with_password_local k (fun _ -> raise (E "Exception!"))
  with E s -> assert (s = "Exception!")
;;

let () =
  let exception F of int in
  let (P k) = Capsule.create () in
  (* Encapsulated exceptions from inside [with_password_local] are unwrapped. *)
  try
    Capsule.Key.with_password_local k (fun (type k) (p : k Capsule.Password.t) ->
      (Capsule.access_local p (fun _ -> raise (F 2))[@nontail] : unit))
  with F n -> assert (n = 2)
;;

let () =
  let (P k) @ aliased = Capsule.create () in
  (* Read-only access to the capsule. *)
  let x = Capsule.Data.create (fun () -> "Shared value") in
  (* Requires the type to be portable. *)
  Capsule.Key.with_password_shared k (fun p ->
    Capsule.Data.extract_shared p (fun s ->
      assert (s = "Shared value")) x)
;;

let () =
  let exception E of string in
  let (P k) = Capsule.create () in
  (* Exceptiosn are re-raised as is. *)
  try
    Capsule.Key.access_shared k (fun _ -> raise (E "Exception with access_shared"))
  with E s -> assert (s = "Exception with access_shared")
;;

let () =
  let exception E of string in
  let (P k) = Capsule.create () in
  (* Exceptiosn are re-raised as is. *)
  try
    Capsule.Key.with_password_shared k (fun _ ->
      raise (E "Exception with with_password_shared"))
  with E s -> assert (s = "Exception with with_password_shared")
;;

let () =
  let exception E of string in
  let (P k) = Capsule.create () in
  (* Exceptiosn are re-raised as is. *)
  try
    Capsule.Key.with_password_shared_local k (fun _ ->
      raise (E "Exception with with_password_shared_local"))
  with E s -> assert (s = "Exception with with_password_shared_local")
;;

let () =
  let exception F of int in
  let (P k) = Capsule.create () in
  (* Encapsulated exceptions from inside [with_password_shared] are unwrapped. *)
  try
    Capsule.Key.with_password_shared k
      (fun (type k) (p : k Capsule.Password.Shared.t) ->
        (Capsule.access_shared p (fun _ -> raise (F 2)) : unit))
  with F n -> assert (n = 2)
;;

let () =
  let exception F of int in
  let (P k) = Capsule.create () in
  (* Encapsulated exceptions from inside [with_password_shared_local] are unwrapped. *) 
  try
    Capsule.Key.with_password_shared_local k
      (fun (type k) (p : k Capsule.Password.Shared.t) ->
        (Capsule.access_shared_local p (fun _ -> raise (F 2)) : unit))
  with F n -> assert (n = 2)
;;
