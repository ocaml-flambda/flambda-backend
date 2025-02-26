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
(let (aliased_use/287 = (function {nlocal = 0} x/289 x/289))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/287))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/290 = (function {nlocal = 0} x/292 x/292))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/290))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/287 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/293 =
     (function {nlocal = 0} r/295[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/296 = (field_imm 1 r/295)
          r/297 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/287 r/295))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/297 y/296))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/293))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/290 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/298 =
     (function {nlocal = 0} r/300[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/301 = (field_mut 1 r/300)
          r/302 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/290 r/300))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/302 y/301))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/298))
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
  (aliased_use/287 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/303 =
     (function {nlocal = 0} r/305[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/307 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/287 r/305))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/307
           (field_imm 1 r/305)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/303))
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
  (unique_use/290 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/309 =
     (function {nlocal = 0} r/311[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/312 =o (field_mut 1 r/311)
          r/313 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/290 r/311))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/313 y/312))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/309))
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
  (aliased_use/287 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/315 =
     (function {nlocal = 0} r/317[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/323 =[int] 1
          r/320 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/287 r/317))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/320
           (field_imm 1 r/317)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/315))
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
  (unique_use/290 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/325 =
     (function {nlocal = 0} r/327[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/329 =o (field_mut 1 r/327)
          *match*/333 =[int] 1
          r/330 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/290 r/327))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/330 y/329))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/325))
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
  (aliased_use/287 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/335 =
     (function {nlocal = 0} r/337[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/339 =a (field_imm 1 r/337))
           (if (== y/339 "") (let (*match*/346 =[int] 0) (exit 8 y/339))
             (let (*match*/344 =[int] 1) (exit 8 (field_imm 1 r/337)))))
        with (8 y/338)
         (let
           (r/341 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/287 r/337))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/341
             y/338)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/335))
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
  (unique_use/290 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/347 =
     (function {nlocal = 0} r/349[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/351 =o (field_mut 1 r/349))
           (if (== y/351 "") (let (*match*/358 =[int] 0) (exit 14 y/351))
             (let (y/352 =o (field_mut 1 r/349) *match*/356 =[int] 1)
               (exit 14 y/352))))
        with (14 y/350)
         (let
           (r/353 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/290 r/349))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/353
             y/350)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/347))
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
  (swap_inner/365 =
     (function {nlocal = 0}
       t/367[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/367
           (let (*match*/376 =a (field_imm 0 t/367))
             (if *match*/376
               (let (*match*/380 =a (field_imm 2 t/367))
                 (if *match*/380
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
                       (field_imm 0 *match*/376) (field_int 1 *match*/376)
                       (field_imm 0 *match*/380))
                     (field_int 1 t/367)
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
                       (field_imm 2 *match*/376) (field_int 1 *match*/380)
                       (field_imm 2 *match*/380)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/367)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/365))
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
  (unique_use/290 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/287 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/383 =
     (function {nlocal = 0} r/385[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let (y/386 =o (field_mut 1 r/385))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/386 "")
           (let
             (r/457 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/287 r/385))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/457
               y/386))
           (let
             (y/387 =o (field_mut 1 r/385)
              r/458 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/290 r/385))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/458
               y/387))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/383))
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
