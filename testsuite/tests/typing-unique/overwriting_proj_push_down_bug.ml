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
(let (aliased_use/286 = (function {nlocal = 0} x/288 x/288))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/286))
val aliased_use : ('a : value_or_null). 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/289 = (function {nlocal = 0} x/291 x/291))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/289))
val unique_use : ('a : value_or_null). 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/286 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/292 =
     (function {nlocal = 0} r/294[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/295 = (field_imm 1 r/294)
          r/296 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/286 r/294))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/296 y/295))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/292))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/289 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/297 =
     (function {nlocal = 0} r/299[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/300 = (field_mut 1 r/299)
          r/301 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/289 r/299))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/301 y/300))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/297))
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
  (aliased_use/286 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/302 =
     (function {nlocal = 0} r/304[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/306 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/286 r/304))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/306
           (field_imm 1 r/304)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/302))
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
  (unique_use/289 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/308 =
     (function {nlocal = 0} r/310[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/311 =o (field_mut 1 r/310)
          r/312 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/289 r/310))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/312 y/311))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/308))
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
  (aliased_use/286 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/314 =
     (function {nlocal = 0} r/316[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/322 =[int] 1
          r/319 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/286 r/316))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/319
           (field_imm 1 r/316)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/314))
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
  (unique_use/289 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/324 =
     (function {nlocal = 0} r/326[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/328 =o (field_mut 1 r/326)
          *match*/332 =[int] 1
          r/329 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/289 r/326))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/329 y/328))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/324))
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
  (aliased_use/286 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/334 =
     (function {nlocal = 0} r/336[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/338 =a (field_imm 1 r/336))
           (if (== y/338 "") (let (*match*/345 =[int] 0) (exit 8 y/338))
             (let (*match*/343 =[int] 1) (exit 8 (field_imm 1 r/336)))))
        with (8 y/337)
         (let
           (r/340 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/286 r/336))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/340
             y/337)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/334))
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
  (unique_use/289 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/346 =
     (function {nlocal = 0} r/348[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/350 =o (field_mut 1 r/348))
           (if (== y/350 "") (let (*match*/357 =[int] 0) (exit 14 y/350))
             (let (y/351 =o (field_mut 1 r/348) *match*/355 =[int] 1)
               (exit 14 y/351))))
        with (14 y/349)
         (let
           (r/352 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/289 r/348))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/352
             y/349)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/346))
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
  (swap_inner/364 =
     (function {nlocal = 0}
       t/366[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/366
           (let (*match*/375 =a (field_imm 0 t/366))
             (if *match*/375
               (let (*match*/379 =a (field_imm 2 t/366))
                 (if *match*/379
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
                       (field_imm 0 *match*/375) (field_int 1 *match*/375)
                       (field_imm 0 *match*/379))
                     (field_int 1 t/366)
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
                       (field_imm 2 *match*/375) (field_int 1 *match*/379)
                       (field_imm 2 *match*/379)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/366)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/364))
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
  (unique_use/289 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/286 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/382 =
     (function {nlocal = 0} r/384[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let (y/385 =o (field_mut 1 r/384))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/385 "")
           (let
             (r/456 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/286 r/384))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/456
               y/385))
           (let
             (y/386 =o (field_mut 1 r/384)
              r/457 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/289 r/384))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/457
               y/386))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/382))
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
