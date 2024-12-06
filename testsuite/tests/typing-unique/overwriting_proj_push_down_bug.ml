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
(let (aliased_use/282 = (function {nlocal = 0} x/284 x/284))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/282))
val aliased_use : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/285 = (function {nlocal = 0} x/287 x/287))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/285))
val unique_use : ('a : value_or_null). 'a @ unique -> 'a @@ global many =
  <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/288 =
     (function {nlocal = 0} r/290[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/291 = (field_imm 1 r/290)
          r/292 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/290))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/292 y/291))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/288))
val proj_aliased : record -> record * string @@ global many = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/293 =
     (function {nlocal = 0} r/295[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/296 = (field_mut 1 r/295)
          r/297 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/295))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/297 y/296))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/293))
val proj_unique : record @ unique -> record * string @@ global many = <fun>
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
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/298 =
     (function {nlocal = 0} r/300[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/302 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/300))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/302
           (field_imm 1 r/300)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/298))
val match_aliased : record -> record * string @@ global many = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_unique r =
  match r with
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/304 =
     (function {nlocal = 0} r/306[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/307 =o (field_mut 1 r/306)
          r/308 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/306))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/308 y/307))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/304))
val match_unique : record @ unique -> record * string @@ global many = <fun>
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
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/310 =
     (function {nlocal = 0} r/312[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/318 =[int] 1
          r/315 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/312))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/315
           (field_imm 1 r/312)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/310))
val match_mini_anf_aliased : record -> record * string @@ global many = <fun>
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
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/320 =
     (function {nlocal = 0} r/322[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/324 =o (field_mut 1 r/322)
          *match*/328 =[int] 1
          r/325 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/322))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/325 y/324))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/320))
val match_mini_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
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
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/330 =
     (function {nlocal = 0} r/332[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/334 =a (field_imm 1 r/332))
           (if (== y/334 "") (let (*match*/341 =[int] 0) (exit 8 y/334))
             (let (*match*/339 =[int] 1) (exit 8 (field_imm 1 r/332)))))
        with (8 y/333)
         (let
           (r/336 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/282 r/332))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/336
             y/333)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/330))
val match_anf_aliased : record -> record * string @@ global many = <fun>
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
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/342 =
     (function {nlocal = 0} r/344[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/346 =o (field_mut 1 r/344))
           (if (== y/346 "") (let (*match*/353 =[int] 0) (exit 14 y/346))
             (let (y/347 =o (field_mut 1 r/344) *match*/351 =[int] 1)
               (exit 14 y/347))))
        with (14 y/345)
         (let
           (r/348 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/285 r/344))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/348
             y/345)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/342))
val match_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
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
  (swap_inner/360 =
     (function {nlocal = 0}
       t/362[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/362
           (let (*match*/371 =a (field_imm 0 t/362))
             (if *match*/371
               (let (*match*/375 =a (field_imm 2 t/362))
                 (if *match*/375
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
                       (field_imm 0 *match*/371) (field_int 1 *match*/371)
                       (field_imm 0 *match*/375))
                     (field_int 1 t/362)
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
                       (field_imm 2 *match*/371) (field_int 1 *match*/375)
                       (field_imm 2 *match*/375)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/362)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/360))
val swap_inner : tree -> tree @@ global many = <fun>
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
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/378 =
     (function {nlocal = 0} r/380[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let (y/381 =o (field_mut 1 r/380))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/381 "")
           (let
             (r/452 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/282 r/380))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/452
               y/381))
           (let
             (y/382 =o (field_mut 1 r/380)
              r/453 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/285 r/380))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/453
               y/382))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/378))
val match_guard : record @ unique -> record * string @@ global many = <fun>
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
