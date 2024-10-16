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
(let (aliased_use/280 = (function {nlocal = 0} x/282 x/282))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/280))
val aliased_use : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/283 = (function {nlocal = 0} x/285 x/285))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/283))
val unique_use : ('a : value_or_null). unique_ 'a -> 'a @@ global many =
  <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/286 =
     (function {nlocal = 0} r/288[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/289 = (field_imm 1 r/288)
          r/290 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/288))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/290 y/289))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/286))
val proj_aliased : record -> record * string @@ global many = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/291 =
     (function {nlocal = 0} r/293[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/294 = (field_mut 1 r/293)
          r/295 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/293))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/295 y/294))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/291))
val proj_unique : unique_ record -> record * string @@ global many = <fun>
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
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/296 =
     (function {nlocal = 0} r/298[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (r/300 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/298))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/300
           (field_imm 1 r/298)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/296))
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
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/302 =
     (function {nlocal = 0} r/304[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/305 =o (field_mut 1 r/304)
          r/306 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/304))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/306 y/305))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/302))
val match_unique : unique_ record -> record * string @@ global many = <fun>
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
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/308 =
     (function {nlocal = 0} r/310[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (*match*/316 =[int] 1
          r/313 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/310))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/313
           (field_imm 1 r/310)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/308))
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
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/318 =
     (function {nlocal = 0} r/320[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
         (y/322 =o (field_mut 1 r/320)
          *match*/326 =[int] 1
          r/323 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/320))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/323 y/322))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/318))
val match_mini_anf_unique : unique_ record -> record * string @@ global many =
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
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/328 =
     (function {nlocal = 0} r/330[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/332 =a (field_imm 1 r/330))
           (if (== y/332 "") (let (*match*/339 =[int] 0) (exit 8 y/332))
             (let (*match*/337 =[int] 1) (exit 8 (field_imm 1 r/330)))))
        with (8 y/331)
         (let
           (r/334 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/280 r/330))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/334
             y/331)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/328))
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
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/340 =
     (function {nlocal = 0} r/342[(consts ()) (non_consts ([0: *, *]))]
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
         (let (y/344 =o (field_mut 1 r/342))
           (if (== y/344 "") (let (*match*/351 =[int] 0) (exit 14 y/344))
             (let (y/345 =o (field_mut 1 r/342) *match*/349 =[int] 1)
               (exit 14 y/345))))
        with (14 y/343)
         (let
           (r/346 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/283 r/342))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/346
             y/343)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/340))
val match_anf_unique : unique_ record -> record * string @@ global many =
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
  (swap_inner/358 =
     (function {nlocal = 0}
       t/360[(consts (0))
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
         (if t/360
           (let (*match*/369 =a (field_imm 0 t/360))
             (if *match*/369
               (let (*match*/373 =a (field_imm 2 t/360))
                 (if *match*/373
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
                       (field_imm 0 *match*/369) (field_int 1 *match*/369)
                       (field_imm 0 *match*/373))
                     (field_int 1 t/360)
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
                       (field_imm 2 *match*/369) (field_int 1 *match*/373)
                       (field_imm 2 *match*/373)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/360)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/358))
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
