(* TEST
 flags = "-dlambda";
 expect;
*)

(* Normal arrays are [genarray]s. *)

let mk_gen (x : 'a) = [| x |]
[%%expect{|
(let
  (mk_gen/283 =
     (function {nlocal = 0} x/285 : genarray (makearray[gen] x/285)))
  (apply (field_imm 1 (global Toploop!)) "mk_gen" mk_gen/283))
val mk_gen : 'a -> 'a array = <fun>
|}]

let get_gen (xs : 'a array) i = xs.(i)
[%%expect{|
(let
  (get_gen/286 =
     (function {nlocal = 0} xs/288[genarray] i/289[int]
       (array.get[gen indexed by int] xs/288 i/289)))
  (apply (field_imm 1 (global Toploop!)) "get_gen" get_gen/286))
val get_gen : 'a array -> int -> 'a = <fun>
|}]

let set_gen (xs : 'a array) x i = xs.(i) <- x
[%%expect{|
(let
  (set_gen/340 =
     (function {nlocal = 0} xs/342[genarray] x/343 i/344[int] : int
       (array.set[gen indexed by int] xs/342 i/344 x/343)))
  (apply (field_imm 1 (global Toploop!)) "set_gen" set_gen/340))
val set_gen : 'a array -> 'a -> int -> unit = <fun>
|}]

(* [non_float] arrays are [addrarray]s. *)

let mk (type t : value mod non_float) (x : t) = [| x |]
[%%expect{|
(let
  (mk/345 = (function {nlocal = 0} x/348 : addrarray (makearray[addr] x/348)))
  (apply (field_imm 1 (global Toploop!)) "mk" mk/345))
val mk : ('t : value mod non_float). 't -> 't array = <fun>
|}]

let get (type t : value mod non_float) (xs : t array) i = xs.(i)
[%%expect{|
(let
  (get/349 =
     (function {nlocal = 0} xs/352[addrarray] i/353[int]
       (array.get[addr indexed by int] xs/352 i/353)))
  (apply (field_imm 1 (global Toploop!)) "get" get/349))
val get : ('t : value mod non_float). 't array -> int -> 't = <fun>
|}]

let set (type t : value mod non_float) (xs : t array) x i = xs.(i) <- x

[%%expect{|
(let
  (set/354 =
     (function {nlocal = 0} xs/357[addrarray] x/358 i/359[int] : int
       (array.set[addr indexed by int] xs/357 i/359 x/358)))
  (apply (field_imm 1 (global Toploop!)) "set" set/354))
val set : ('t : value mod non_float). 't array -> 't -> int -> unit = <fun>
|}]

(* A concrete example. *)

module X : sig
  type t : immutable_data

  val x1 : t
  val x2 : t
end = struct
  type t = { a: string; b: int }

  let x1 = { a = "first"; b = 1 }
  let x2 = { a = "second"; b = 2 }
end

[%%expect{|
(apply (field_imm 1 (global Toploop!)) "X/370"
  (let
    (x1/365 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "first" 1]
     x2/366 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "second" 2])
    (makeblock 0 x1/365 x2/366)))
module X : sig type t : immutable_data val x1 : t val x2 : t end
|}]

let () =
  let xs = Array.make 4 X.x1 in
  xs.(1) <- X.x2;
  xs.(2) <- X.x2;
  assert (xs.(0) = xs.(3));
  assert (xs.(1) = xs.(2));
  assert (not (xs.(0) = xs.(1)))
;;

[%%expect{|
(let
  (X/370 = (apply (field_imm 0 (global Toploop!)) "X/370")
   *match*/375 =[int]
     (let (xs/373 =[addrarray] (caml_make_vect 4 (field_imm 0 X/370)))
       (seq (array.set[addr indexed by int] xs/373 1 (field_imm 1 X/370))
         (array.set[addr indexed by int] xs/373 2 (field_imm 1 X/370))
         (if
           (caml_equal (array.get[addr indexed by int] xs/373 0)
             (array.get[addr indexed by int] xs/373 3))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 5 2])))
         (if
           (caml_equal (array.get[addr indexed by int] xs/373 1)
             (array.get[addr indexed by int] xs/373 2))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 6 2])))
         (if
           (not
             (caml_equal (array.get[addr indexed by int] xs/373 0)
               (array.get[addr indexed by int] xs/373 1)))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 7 2]))))))
  0)
|}]
