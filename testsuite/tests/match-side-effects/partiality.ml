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
  (f/287 =
     (function {nlocal = 0} x/289 : int
       (if (field_int 0 x/289)
         (let (*match*/293 =o (field_mut 1 x/289))
           (if *match*/293
             (if (seq (setfield_ptr 1 x/289 0) 0) 2
               (let (*match*/294 =o (field_mut 1 x/289))
                 (field_imm 0 *match*/294)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/287))
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
  (f/300 =
     (function {nlocal = 0} x/301 : int
       (if (field_int 0 x/301)
         (let (*match*/305 =o (field_mut 1 x/301))
           (if *match*/305 (field_imm 0 *match*/305) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/300))
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
  (f/307 =
     (function {nlocal = 0} r/308 : int
       (region
         (let
           (*match*/310 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 (*) r/308))
           (catch
             (if *match*/310
               (let (*match*/312 =o (field_mut 0 (field_imm 0 *match*/310)))
                 (if *match*/312 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/308 0) 0) 1
               (if *match*/310
                 (let
                   (*match*/314 =o (field_mut 0 (field_imm 0 *match*/310)))
                   (field_imm 0 *match*/314))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/307))
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
  (test/318 =
     (function {nlocal = 0} param/321[(consts (0)) (non_consts ([0: *]))]
       : int (if param/321 (field_imm 0 (field_imm 0 param/321)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/318))
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
  (test/326 =
     (function {nlocal = 0} param/328 : int
       (let (*match*/329 =o (field_mut 0 param/328))
         (if *match*/329 (field_imm 0 (field_imm 0 *match*/329)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/326))
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
  (test/334 =
     (function {nlocal = 0} n/335 : int
       (region
         (let
           (*match*/338 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 ([(consts ())
                                  (non_consts ([0: *,
                                                [(consts ())
                                                 (non_consts ([1: [int]]
                                                 [0: [int]]))]]))])
                (makelocalblock 0 (*,[(consts ()) (non_consts ([1: [int]]
                                      [0: [int]]))])
                  (makelocalmutable 0 (int) 1) [0: 42])))
           (if *match*/338
             (let
               (*match*/339 =a (field_imm 0 *match*/338)
                *match*/341 =o (field_mut 0 (field_imm 0 *match*/339)))
               (if *match*/341 (field_imm 0 (field_imm 1 *match*/339))
                 (~ (field_imm 0 (field_imm 1 *match*/339)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/334))
val test : 'a -> int = <fun>
|}]
