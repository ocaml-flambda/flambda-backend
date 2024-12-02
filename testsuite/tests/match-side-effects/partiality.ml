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
  (f/284 =
     (function {nlocal = 0} x/286 : int
       (if (field_int 0 x/286)
         (let (*match*/290 =o (field_mut 1 x/286))
           (if *match*/290
             (if (seq (setfield_ptr 1 x/286 0) 0) 2
               (let (*match*/291 =o (field_mut 1 x/286))
                 (field_imm 0 *match*/291)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/284))
val f : t -> int @@ global many = <fun>
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
  (f/295 =
     (function {nlocal = 0} x/296 : int
       (if (field_int 0 x/296)
         (let (*match*/300 =o (field_mut 1 x/296))
           (if *match*/300 (field_imm 0 *match*/300) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/295))
val f : t -> int @@ global many = <fun>
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
  (f/302 =
     (function {nlocal = 0} r/303 : int
       (region
         (let
           (*match*/305 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 (*) r/303))
           (catch
             (if *match*/305
               (let (*match*/307 =o (field_mut 0 (field_imm 0 *match*/305)))
                 (if *match*/307 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/303 0) 0) 1
               (if *match*/305
                 (let
                   (*match*/309 =o (field_mut 0 (field_imm 0 *match*/305)))
                   (field_imm 0 *match*/309))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/302))
val f : int option ref -> int @@ global many = <fun>
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
  (test/313 =
     (function {nlocal = 0} param/316[(consts (0)) (non_consts ([0: *]))]
       : int (if param/316 (field_imm 0 (field_imm 0 param/316)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/313))
val test : int t option -> int @@ global many = <fun>
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
  (test/321 =
     (function {nlocal = 0} param/323 : int
       (let (*match*/324 =o (field_mut 0 param/323))
         (if *match*/324 (field_imm 0 (field_imm 0 *match*/324)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/321))
val test : int t option ref -> int @@ global many = <fun>
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
  (test/329 =
     (function {nlocal = 0} n/330 : int
       (region
         (let
           (*match*/333 =[(consts (0)) (non_consts ([0: *]))]
              (makelocalblock 0 ([(consts ())
                                  (non_consts ([0: *,
                                                [(consts ())
                                                 (non_consts ([1: [int]]
                                                 [0: [int]]))]]))])
                (makelocalblock 0 (*,[(consts ()) (non_consts ([1: [int]]
                                      [0: [int]]))])
                  (makelocalmutable 0 (int) 1) [0: 42])))
           (if *match*/333
             (let
               (*match*/334 =a (field_imm 0 *match*/333)
                *match*/336 =o (field_mut 0 (field_imm 0 *match*/334)))
               (if *match*/336 (field_imm 0 (field_imm 1 *match*/334))
                 (~ (field_imm 0 (field_imm 1 *match*/334)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/329))
val test : 'a -> int @@ global many = <fun>
|}]
