(* TEST
 flags = "-dlambda -dno-unique-ids";
 expect;
*)

(* Normal arrays are [genarray]s. Due to the float array optimization,
   they must check whether their elements are float or non-float on
   creation, [get] and [set].

   We can see what kind of array we are getting by looking at Lambda. *)

let mk_gen (x : 'a) = [| x |]
[%%expect{|
(let (mk_gen = (function {nlocal = 0} x : genarray (makearray[gen] x)))
  (apply (field_imm 1 (global Toploop!)) "mk_gen" mk_gen))
val mk_gen : 'a -> 'a array = <fun>
|}]

let get_gen (xs : 'a array) i = xs.(i)
[%%expect{|
(let
  (get_gen =
     (function {nlocal = 0} xs[genarray] i[int]
       (array.get[gen indexed by int] xs i)))
  (apply (field_imm 1 (global Toploop!)) "get_gen" get_gen))
val get_gen : 'a array -> int -> 'a = <fun>
|}]

let set_gen (xs : 'a array) x i = xs.(i) <- x
[%%expect{|
(let
  (set_gen =
     (function {nlocal = 0} xs[genarray] x i[int] : int
       (array.set[gen indexed by int] xs i x)))
  (apply (field_imm 1 (global Toploop!)) "set_gen" set_gen))
val set_gen : 'a array -> 'a -> int -> unit = <fun>
|}]

(* [non_float] arrays are [addrarray]s. Operations on [addrarray]s
   skip checks related to floats.

   Here we can see that our operations are postfixed with [addr]. *)

let mk (type t : value mod non_float) (x : t) = [| x |]
[%%expect{|
(let (mk = (function {nlocal = 0} x : addrarray (makearray[addr] x)))
  (apply (field_imm 1 (global Toploop!)) "mk" mk))
val mk : ('t : value mod non_float). 't -> 't array = <fun>
|}]

let get (type t : value mod non_float) (xs : t array) i = xs.(i)
[%%expect{|
(let
  (get =
     (function {nlocal = 0} xs[addrarray] i[int]
       (array.get[addr indexed by int] xs i)))
  (apply (field_imm 1 (global Toploop!)) "get" get))
val get : ('t : value mod non_float). 't array -> int -> 't = <fun>
|}]

let set (type t : value mod non_float) (xs : t array) x i = xs.(i) <- x

[%%expect{|
(let
  (set =
     (function {nlocal = 0} xs[addrarray] x i[int] : int
       (array.set[addr indexed by int] xs i x)))
  (apply (field_imm 1 (global Toploop!)) "set" set))
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
    (x1 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "first" 1]
     x2 =[(consts ()) (non_consts ([0: *, [int]]))] [0: "second" 2])
    (makeblock 0 x1 x2)))
module X : sig type t : immutable_data val x1 : t val x2 : t end
|}]

(* Create an [addrarray] and perform [addr] operations on it. *)

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
  (X = (apply (field_imm 0 (global Toploop!)) "X/370")
   *match* =[int]
     (let (xs =[addrarray] (caml_make_vect 4 (field_imm 0 X)))
       (seq (array.set[addr indexed by int] xs 1 (field_imm 1 X))
         (array.set[addr indexed by int] xs 2 (field_imm 1 X))
         (if
           (caml_equal (array.get[addr indexed by int] xs 0)
             (array.get[addr indexed by int] xs 3))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 5 2])))
         (if
           (caml_equal (array.get[addr indexed by int] xs 1)
             (array.get[addr indexed by int] xs 2))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 6 2])))
         (if
           (not
             (caml_equal (array.get[addr indexed by int] xs 0)
               (array.get[addr indexed by int] xs 1)))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 7 2]))))))
  0)
|}]
