(* TEST
   flags += "-extension-universe alpha ";
   flags += "-dlambda";
   expect;
*)

(* This file tests the lambda code that is generated for projections out of unique values.
   We need to ensure that if an allocation is used uniquely, all projections out of
   this allocation happen before the unique use and are not pushed down beyond that point.
*)

type record = { x : string; y : string @@ many aliased }
[%%expect{|
0
type record = { x : string; y : string @@ many aliased; }
|}]

let aliased_use x = x
[%%expect{|
(let (aliased_use/285 = (function {nlocal = 0} x/287 x/287))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/285))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/288 = (function {nlocal = 0} x/290 x/290))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/288))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/285 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/291 =
     (function {nlocal = 0} r/293[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/294 = (field_imm 1 r/293)
          r/295 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/285 r/293))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/295 y/294))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/291))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/288 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/296 =
     (function {nlocal = 0} r/298[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/299 = (field_mut 1 r/298)
          r/300 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/288 r/298))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/300 y/299))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/296))
val proj_unique : record @ unique -> record * string = <fun>
|}]

(* This output would be unsound if [aliased_use] was able to overwrite [r]
   because the [field_imm 1 r] read happens after calling [aliased_use]. *)
let match_aliased r =
  match r with
  | { y } ->
    let r = aliased_use r in
    (r, y)
[%%expect{|
(let
  (aliased_use/285 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/301 =
     (function {nlocal = 0} r/303[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/305 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/285 r/303))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/305
           (field_imm 1 r/303)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/301))
val match_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_unique r =
  match r with
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use/288 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/307 =
     (function {nlocal = 0} r/309[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/310 =o (field_mut 1 r/309)
          r/311 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/288 r/309))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/311 y/310))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/307))
val match_unique : record @ unique -> record * string = <fun>
|}]

(* Similarly, this would be unsound since Lambda performs a mini ANF pass. *)
let match_mini_anf_aliased r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/285 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/313 =
     (function {nlocal = 0} r/315[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/321 =[int] 1
          r/318 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/285 r/315))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/318
           (field_imm 1 r/315)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/313))
val match_mini_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_mini_anf_unique r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/288 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/323 =
     (function {nlocal = 0} r/325[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/327 =o (field_mut 1 r/325)
          *match*/331 =[int] 1
          r/328 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/288 r/325))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/328 y/327))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/323))
val match_mini_anf_unique : record @ unique -> record * string = <fun>
|}]

let match_anf_aliased r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/285 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/333 =
     (function {nlocal = 0} r/335[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/337 =a (field_imm 1 r/335))
           (if (== y/337 "") (let (*match*/344 =[int] 0) (exit 8 y/337))
             (let (*match*/342 =[int] 1) (exit 8 (field_imm 1 r/335)))))
        with (8 y/336)
         (let
           (r/339 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/285 r/335))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/339
             y/336)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/333))
val match_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] using [field_mut] *)
let match_anf_unique r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/288 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/345 =
     (function {nlocal = 0} r/347[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/349 =o (field_mut 1 r/347))
           (if (== y/349 "") (let (*match*/356 =[int] 0) (exit 14 y/349))
             (let (y/350 =o (field_mut 1 r/347) *match*/354 =[int] 1)
               (exit 14 y/350))))
        with (14 y/348)
         (let
           (r/351 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/288 r/347))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/351
             y/348)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/345))
val match_anf_unique : record @ unique -> record * string = <fun>
|}]

type tree =
  | Leaf
  | Node of { l : tree; x : int; r : tree }
[%%expect{|
0
type tree = Leaf | Node of { l : tree; x : int; r : tree; }
|}]

(* This output would be unsound with overwriting:
   If we naively replaced makeblock with reuseblock,
   then we would first overwrite r to have left child lr.
   But then, the overwrite of l still has to read the left child of r
   (as field_imm 0 *match*/329). But this value has been overwritten and so in fact,
   this code drops the rl and sets lr to be the inner child of both l and r.
*)
let swap_inner (t : tree) =
  match t with
  | Node ({ l = Node ({ r = lr } as l); r = Node ({ l = rl } as r) } as t) ->
    Node { t with l = Node { l with r = rl; }; r = Node { r with l = lr; }}
  | _ -> t
[%%expect{|
(let
  (swap_inner/363 =
     (function {nlocal = 0}
       t/365[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/365
           (let (*match*/374 =a (field_imm 0 t/365))
             (if *match*/374
               (let (*match*/378 =a (field_imm 2 t/365))
                 (if *match*/378
                   (makeblock 0 ([(consts (0))
                                  (non_consts ([0:
                                                [(consts (0))
                                                 (non_consts ([0: *, [int],
                                                               *]))], [int],
                                                [(consts (0))
                                                 (non_consts ([0: *, [int],
                                                               *]))]]))],int,
                     [(consts (0))
                      (non_consts ([0:
                                    [(consts (0))
                                     (non_consts ([0: *, [int], *]))], [int],
                                    [(consts (0))
                                     (non_consts ([0: *, [int], *]))]]))])
                     (makeblock 0 ([(consts (0))
                                    (non_consts ([0:
                                                  [(consts (0))
                                                   (non_consts ([0: *, [int],
                                                                 *]))],
                                                  [int],
                                                  [(consts (0))
                                                   (non_consts ([0: *, [int],
                                                                 *]))]]))],int,
                       [(consts (0))
                        (non_consts ([0:
                                      [(consts (0))
                                       (non_consts ([0: *, [int], *]))],
                                      [int],
                                      [(consts (0))
                                       (non_consts ([0: *, [int], *]))]]))])
                       (field_imm 0 *match*/374) (field_int 1 *match*/374)
                       (field_imm 0 *match*/378))
                     (field_int 1 t/365)
                     (makeblock 0 ([(consts (0))
                                    (non_consts ([0:
                                                  [(consts (0))
                                                   (non_consts ([0: *, [int],
                                                                 *]))],
                                                  [int],
                                                  [(consts (0))
                                                   (non_consts ([0: *, [int],
                                                                 *]))]]))],int,
                       [(consts (0))
                        (non_consts ([0:
                                      [(consts (0))
                                       (non_consts ([0: *, [int], *]))],
                                      [int],
                                      [(consts (0))
                                       (non_consts ([0: *, [int], *]))]]))])
                       (field_imm 2 *match*/374) (field_int 1 *match*/378)
                       (field_imm 2 *match*/378)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/365)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/363))
val swap_inner : tree -> tree = <fun>
|}]

(* CR uniqueness: Update this test once overwriting is fully implemented.
   let swap_inner (t : tree) =
   match t with
   | Node { l = Node { r = lr } as l; r = Node { l = rl } as r } as t ->
   overwrite_ t with
   Node { l = overwrite_ l with Node { r = rl; };
   r = overwrite_ r with Node { l = lr; }}
   | _ -> t
   [%%expect{|

   |}]
*)

(***********************)
(* Barriers for guards *)

let match_guard r =
  match r with
  | { y } when String.equal y "" ->
    let r = aliased_use r in
    (r, y)
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use/288 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/285 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/381 =
     (function {nlocal = 0} r/383[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let (y/384 =o (field_mut 1 r/383))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/384 "")
           (let
             (r/455 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/285 r/383))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/455
               y/384))
           (let
             (y/385 =o (field_mut 1 r/383)
              r/456 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/288 r/383))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/456
               y/385))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/381))
val match_guard : record @ unique -> record * string = <fun>
|}]

let match_guard_unique (unique_ r) =
  match r with
  | { y } when String.equal ((unique_use r).x) "" -> y
  | _ -> ""
[%%expect{|
Line 3, characters 4-9:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
        ^^^^^
Error: This value is read from here, but it is already being used as unique:
Line 3, characters 41-42:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
                                             ^

|}]

(********************************************)
(* Global allocations in overwritten fields *)

type option_record = { x : string option; y : string option }
[%%expect{|
0
type option_record = { x : string option; y : string option; }
|}]

let check_heap_alloc_in_overwrite (unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

let check_heap_alloc_in_overwrite (local_ unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

(*******************************)
(* Overwrite of mutable fields *)

type mutable_record = { mutable x : string; y : string }
[%%expect{|
0
type mutable_record = { mutable x : string; y : string; }
|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]
