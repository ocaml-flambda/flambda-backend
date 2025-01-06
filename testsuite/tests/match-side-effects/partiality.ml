(* TEST
 flags = "-dlambda";
 stack-allocation;
 expect;
*)

(* The original example of unsoundness in #7421. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = _;     b = _} when (x.b <- None; false) -> 2
  | {a = true;  b = Some y} -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 1) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/285 =
     (function {nlocal = 0} x/287 : int
       (if (field_int 0 x/287)
         (let (*match*/291 =o (field_mut 1 x/287))
           (if *match*/291
             (if (seq (setfield_ptr 1 x/287 0) 0) 2
               (let (*match*/292 =o (field_mut 1 x/287))
                 (field_imm 0 *match*/292)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/285))
val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/296 =
     (function {nlocal = 0} x/297 : int
       (if (field_int 0 x/297)
         (let (*match*/301 =o (field_mut 1 x/297))
           (if *match*/301 (field_imm 0 *match*/301) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/296))
val f : t -> int = <fun>
|}]



(* A variant of the #7421 example. *)
let f r =
  match Some r with
  | Some { contents = None } -> 0
  | _ when (r := None; false) -> 1
  | Some { contents = Some n } -> n
  | None -> 3
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 0) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
(let
  (f/303 =
     (function {nlocal = 0} r/304 : int
       (region
         (let
           (*match*/306 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 (*) r/304))
           (catch
             (if *match*/306
               (let (*match*/308 =o (field_mut 0 (field_imm 0 *match*/306)))
                 (if *match*/308 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/304 0) 0) 1
               (if *match*/306
                 (let
                   (*match*/310 =o (field_mut 0 (field_imm 0 *match*/306)))
                   (field_imm 0 *match*/310))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/303))
val f : int option ref -> int = <fun>
|}]



(* This example has an ill-typed counter-example: the type-checker
   finds it Total, but the pattern-matching compiler cannot see that
   (Some (Some (Bool b))) cannot occur. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | None -> 0
  | Some (Int n) -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/314 =
     (function {nlocal = 0} param/317[(consts (0)) (non_consts ([0: *]))]
       : int (if param/317 (field_imm 0 (field_imm 0 param/317)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/314))
val test : int t option -> int = <fun>
|}]


(* This example has an ill-typed counter-example, inside
   a mutable position.  *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | { contents = None } -> 0
  | { contents = Some (Int n) } -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/322 =
     (function {nlocal = 0} param/324 : int
       (let (*match*/325 =o (field_mut 0 param/324))
         (if *match*/325 (field_imm 0 (field_imm 0 *match*/325)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/322))
val test : int t option ref -> int = <fun>
|}]



(* This example has a ill-typed counter-example,
   and also mutable sub-patterns, but in different places. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test n =
  match Some (ref true, Int 42) with
  | Some ({ contents = true }, Int n) -> n
  | Some ({ contents = false }, Int n) -> -n
  | None -> 3
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/330 =
     (function {nlocal = 0} n/331 : int
       (region
         (let
           (*match*/334 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 ([(consts ())
                                  (non_consts ([0: *,
                                                [(consts ())
                                                 (non_consts ([1: [int]]
                                                 [0: [int]]))]]))])
                (makelocalblock 0 (*,[(consts ()) (non_consts ([1: [int]]
                                      [0: [int]]))])
                  (makelocalmutable 0 (int) 1) [0: 42])))
           (if *match*/334
             (let
               (*match*/335 =a (field_imm 0 *match*/334)
                *match*/337 =o (field_mut 0 (field_imm 0 *match*/335)))
               (if *match*/337 (field_imm 0 (field_imm 1 *match*/335))
                 (~ (field_imm 0 (field_imm 1 *match*/335)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/330))
val test : 'a -> int = <fun>
|}]
