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
(let (aliased_use/288 = (function {nlocal = 0} x/290 x/290))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/288))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/291 = (function {nlocal = 0} x/293 x/293))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/291))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/288 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/294 =
     (function {nlocal = 0} r/296[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/297 = (field_imm 1 r/296)
          r/298 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/288 r/296))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/298 y/297))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/294))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/291 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/299 =
     (function {nlocal = 0} r/301[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/302 = (field_mut 1 r/301)
          r/303 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/291 r/301))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/303 y/302))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/299))
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
  (aliased_use/288 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/304 =
     (function {nlocal = 0} r/306[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/308 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/288 r/306))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/308
           (field_imm 1 r/306)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/304))
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
  (unique_use/291 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/310 =
     (function {nlocal = 0} r/312[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/313 =o (field_mut 1 r/312)
          r/314 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/291 r/312))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/314 y/313))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/310))
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
  (aliased_use/288 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/316 =
     (function {nlocal = 0} r/318[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/324 =[int] 1
          r/321 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/288 r/318))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/321
           (field_imm 1 r/318)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/316))
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
  (unique_use/291 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/326 =
     (function {nlocal = 0} r/328[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/330 =o (field_mut 1 r/328)
          *match*/334 =[int] 1
          r/331 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/291 r/328))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/331 y/330))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/326))
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
  (aliased_use/288 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/336 =
     (function {nlocal = 0} r/338[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/340 =a (field_imm 1 r/338))
           (if (%eq y/340 "") (let (*match*/347 =[int] 0) (exit 8 y/340))
             (let (*match*/345 =[int] 1) (exit 8 (field_imm 1 r/338)))))
        with (8 y/339)
         (let
           (r/342 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/288 r/338))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/342
             y/339)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/336))
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
  (unique_use/291 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/348 =
     (function {nlocal = 0} r/350[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/352 =o (field_mut 1 r/350))
           (if (%eq y/352 "") (let (*match*/359 =[int] 0) (exit 14 y/352))
             (let (y/353 =o (field_mut 1 r/350) *match*/357 =[int] 1)
               (exit 14 y/353))))
        with (14 y/351)
         (let
           (r/354 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/291 r/350))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/354
             y/351)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/348))
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
  (swap_inner/366 =
     (function {nlocal = 0}
       t/368[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/368
           (let (*match*/377 =a (field_imm 0 t/368))
             (if *match*/377
               (let (*match*/381 =a (field_imm 2 t/368))
                 (if *match*/381
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
                       (field_imm 0 *match*/377) (field_int 1 *match*/377)
                       (field_imm 0 *match*/381))
                     (field_int 1 t/368)
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
                       (field_imm 2 *match*/377) (field_int 1 *match*/381)
                       (field_imm 2 *match*/381)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/368)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/366))
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
  (unique_use/291 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/288 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/384 =
     (function {nlocal = 0} r/386[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let (y/387 =o (field_mut 1 r/386))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/387 "")
           (let
             (r/458 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/288 r/386))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/458
               y/387))
           (let
             (y/388 =o (field_mut 1 r/386)
              r/459 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/291 r/386))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/459
               y/388))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/384))
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
