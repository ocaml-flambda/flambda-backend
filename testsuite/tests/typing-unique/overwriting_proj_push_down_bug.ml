(* TEST
   flags += "-extension unique_alpha ";
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
(let (aliased_use/284 = (function {nlocal = 0} x/286 x/286))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/284))
val aliased_use : 'a -> 'a @@ global many = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/287 = (function {nlocal = 0} x/289 x/289))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/287))
val unique_use : 'a @ unique -> 'a @@ global many = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/284 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/290 =
     (function {nlocal = 0} r/292[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/293 = (field_imm 1 r/292)
          r/294 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/284 r/292))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/294 y/293))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/290))
val proj_aliased : record -> record * string @@ global many = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/287 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/295 =
     (function {nlocal = 0} r/297[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/298 = (field_mut 1 r/297)
          r/299 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/287 r/297))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/299 y/298))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/295))
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
  (aliased_use/284 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/300 =
     (function {nlocal = 0} r/302[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/304 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/284 r/302))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/304
           (field_imm 1 r/302)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/300))
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
  (unique_use/287 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/306 =
     (function {nlocal = 0} r/308[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/309 =o (field_mut 1 r/308)
          r/310 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/287 r/308))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/310 y/309))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/306))
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
  (aliased_use/284 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/312 =
     (function {nlocal = 0} r/314[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/320 =[int] 1
          r/317 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/284 r/314))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/317
           (field_imm 1 r/314)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/312))
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
  (unique_use/287 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/322 =
     (function {nlocal = 0} r/324[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/326 =o (field_mut 1 r/324)
          *match*/330 =[int] 1
          r/327 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/287 r/324))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/327 y/326))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/322))
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
  (aliased_use/284 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/332 =
     (function {nlocal = 0} r/334[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/336 =a (field_imm 1 r/334))
           (if (== y/336 "") (let (*match*/343 =[int] 0) (exit 8 y/336))
             (let (*match*/341 =[int] 1) (exit 8 (field_imm 1 r/334)))))
        with (8 y/335)
         (let
           (r/338 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/284 r/334))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/338
             y/335)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/332))
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
  (unique_use/287 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/344 =
     (function {nlocal = 0} r/346[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/348 =o (field_mut 1 r/346))
           (if (== y/348 "") (let (*match*/355 =[int] 0) (exit 14 y/348))
             (let (y/349 =o (field_mut 1 r/346) *match*/353 =[int] 1)
               (exit 14 y/349))))
        with (14 y/347)
         (let
           (r/350 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/287 r/346))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/350
             y/347)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/344))
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
  (swap_inner/362 =
     (function {nlocal = 0}
       t/364[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/364
           (let (*match*/373 =a (field_imm 0 t/364))
             (if *match*/373
               (let (*match*/377 =a (field_imm 2 t/364))
                 (if *match*/377
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
                       (field_imm 0 *match*/373) (field_int 1 *match*/373)
                       (field_imm 0 *match*/377))
                     (field_int 1 t/364)
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
                       (field_imm 2 *match*/373) (field_int 1 *match*/377)
                       (field_imm 2 *match*/377)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/364)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/362))
val swap_inner : tree -> tree @@ global many = <fun>
|}]

(* TODO: Update this test once overwriting is fully implemented.
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
