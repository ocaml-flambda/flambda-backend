(* TEST
 flags = "-dlambda";
 expect;
*)

(* [non_float] arrays are [addrarray]s. *)

let mk (type t : value mod non_float) (x : t) = [| x |]
[%%expect{|
(let
  (mk/283 = (function {nlocal = 0} x/286 : addrarray (makearray[addr] x/286)))
  (apply (field_imm 1 (global Toploop!)) "mk" mk/283))
val mk : ('t : value mod non_float). 't -> 't array = <fun>
|}]

let get (type t : value mod non_float) (xs : t array) i = xs.(i)
[%%expect{|
(let
  (get/287 =
     (function {nlocal = 0} xs/290[addrarray] i/291[int]
       (array.get[addr indexed by int] xs/290 i/291)))
  (apply (field_imm 1 (global Toploop!)) "get" get/287))
val get : ('t : value mod non_float). 't array -> int -> 't = <fun>
|}]

let set (type t : value mod non_float) (xs : t array) x i = xs.(i) <- x

[%%expect{|
(let
  (set/342 =
     (function {nlocal = 0} xs/345[addrarray] x/346 i/347[int] : int
       (array.set[addr indexed by int] xs/345 i/347 x/346)))
  (apply (field_imm 1 (global Toploop!)) "set" set/342))
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
(apply (field_imm 1 (global Toploop!)) "X/358"
  (let
    (x1/353 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "first" 1]
     x2/354 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "second" 2])
    (makeblock 0 x1/353 x2/354)))
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
  (X/358 = (apply (field_imm 0 (global Toploop!)) "X/358")
   *match*/363 =[int]
     (let (xs/361 =[addrarray] (caml_make_vect 4 (field_imm 0 X/358)))
       (seq (array.set[addr indexed by int] xs/361 1 (field_imm 1 X/358))
         (array.set[addr indexed by int] xs/361 2 (field_imm 1 X/358))
         (if
           (caml_equal (array.get[addr indexed by int] xs/361 0)
             (array.get[addr indexed by int] xs/361 3))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 5 2])))
         (if
           (caml_equal (array.get[addr indexed by int] xs/361 1)
             (array.get[addr indexed by int] xs/361 2))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 6 2])))
         (if
           (not
             (caml_equal (array.get[addr indexed by int] xs/361 0)
               (array.get[addr indexed by int] xs/361 1)))
           0
           (raise (makeblock 0 (getpredef Assert_failure/40!!) [0: "" 7 2]))))))
  0)
|}]
