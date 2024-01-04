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
<<<<<<< HEAD
(let (aliased_use/282 = (function {nlocal = 0} x/284 x/284))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/282))
val aliased_use : ('a : value_or_null). 'a -> 'a = <fun>
||||||| parent of f06c0085dd (Locals x effects)
(let (aliased_use/280 = (function {nlocal = 0} x/282 x/282))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/280))
val aliased_use : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
=======
(let (aliased_use/281 = (function {nlocal = 0} x/283 x/283))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/281))
val aliased_use : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
|}]

let unique_use (unique_ x) = x
[%%expect{|
<<<<<<< HEAD
(let (unique_use/285 = (function {nlocal = 0} x/287 x/287))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/285))
val unique_use : ('a : value_or_null). 'a @ unique -> 'a = <fun>
||||||| parent of f06c0085dd (Locals x effects)
(let (unique_use/283 = (function {nlocal = 0} x/285 x/285))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/283))
val unique_use : ('a : value_or_null). 'a @ unique -> 'a @@ global many =
  <fun>
=======
(let (unique_use/284 = (function {nlocal = 0} x/286 x/286))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/284))
val unique_use : ('a : value_or_null). 'a @ unique -> 'a @@ global many =
  <fun>
>>>>>>> f06c0085dd (Locals x effects)
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
<<<<<<< HEAD
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/288 =
     (function {nlocal = 0} r/290[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/286 =
     (function {nlocal = 0} r/288[(consts ()) (non_consts ([0: *, *]))]
=======
  (aliased_use/281 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/287 =
     (function {nlocal = 0} r/289[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (y/291 = (field_imm 1 r/290)
          r/292 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/290))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/292 y/291))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/288))
val proj_aliased : record -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
         (y/289 = (field_imm 1 r/288)
          r/290 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/288))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/290 y/289))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/286))
val proj_aliased : record -> record * string @@ global many = <fun>
=======
         (y/290 = (field_imm 1 r/289)
          r/291 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/281 r/289))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/291 y/290))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/287))
val proj_aliased : record -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
<<<<<<< HEAD
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/293 =
     (function {nlocal = 0} r/295[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/291 =
     (function {nlocal = 0} r/293[(consts ()) (non_consts ([0: *, *]))]
=======
  (unique_use/284 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/292 =
     (function {nlocal = 0} r/294[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (y/296 = (field_mut 1 r/295)
          r/297 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/295))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/297 y/296))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/293))
val proj_unique : record @ unique -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
         (y/294 = (field_mut 1 r/293)
          r/295 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/293))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/295 y/294))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/291))
val proj_unique : record @ unique -> record * string @@ global many = <fun>
=======
         (y/295 = (field_mut 1 r/294)
          r/296 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/284 r/294))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/296 y/295))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/292))
val proj_unique : record @ unique -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/298 =
     (function {nlocal = 0} r/300[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/296 =
     (function {nlocal = 0} r/298[(consts ()) (non_consts ([0: *, *]))]
=======
  (aliased_use/281 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/297 =
     (function {nlocal = 0} r/299[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (r/302 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/300))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/302
           (field_imm 1 r/300)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/298))
val match_aliased : record -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
         (r/300 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/298))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/300
           (field_imm 1 r/298)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/296))
val match_aliased : record -> record * string @@ global many = <fun>
=======
         (r/301 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/281 r/299))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/301
           (field_imm 1 r/299)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/297))
val match_aliased : record -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_unique r =
  match r with
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
<<<<<<< HEAD
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/304 =
     (function {nlocal = 0} r/306[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/302 =
     (function {nlocal = 0} r/304[(consts ()) (non_consts ([0: *, *]))]
=======
  (unique_use/284 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/303 =
     (function {nlocal = 0} r/305[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (y/307 =o (field_mut 1 r/306)
          r/308 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/306))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/308 y/307))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/304))
val match_unique : record @ unique -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
         (y/305 =o (field_mut 1 r/304)
          r/306 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/304))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/306 y/305))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/302))
val match_unique : record @ unique -> record * string @@ global many = <fun>
=======
         (y/306 =o (field_mut 1 r/305)
          r/307 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/284 r/305))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/307 y/306))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/303))
val match_unique : record @ unique -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/310 =
     (function {nlocal = 0} r/312[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/308 =
     (function {nlocal = 0} r/310[(consts ()) (non_consts ([0: *, *]))]
=======
  (aliased_use/281 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/309 =
     (function {nlocal = 0} r/311[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (*match*/318 =[int] 1
          r/315 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/282 r/312))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/315
           (field_imm 1 r/312)))))
||||||| parent of f06c0085dd (Locals x effects)
         (*match*/316 =[int] 1
          r/313 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/280 r/310))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/313
           (field_imm 1 r/310)))))
=======
         (*match*/317 =[int] 1
          r/314 =[(consts ()) (non_consts ([0: *, *]))]
            (apply aliased_use/281 r/311))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/314
           (field_imm 1 r/311)))))
>>>>>>> f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
<<<<<<< HEAD
    match_mini_anf_aliased/310))
val match_mini_anf_aliased : record -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
    match_mini_anf_aliased/308))
val match_mini_anf_aliased : record -> record * string @@ global many = <fun>
=======
    match_mini_anf_aliased/309))
val match_mini_anf_aliased : record -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/320 =
     (function {nlocal = 0} r/322[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/318 =
     (function {nlocal = 0} r/320[(consts ()) (non_consts ([0: *, *]))]
=======
  (unique_use/284 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/319 =
     (function {nlocal = 0} r/321[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (let
<<<<<<< HEAD
         (y/324 =o (field_mut 1 r/322)
          *match*/328 =[int] 1
          r/325 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/285 r/322))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/325 y/324))))
||||||| parent of f06c0085dd (Locals x effects)
         (y/322 =o (field_mut 1 r/320)
          *match*/326 =[int] 1
          r/323 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/283 r/320))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/323 y/322))))
=======
         (y/323 =o (field_mut 1 r/321)
          *match*/327 =[int] 1
          r/324 =[(consts ()) (non_consts ([0: *, *]))]
            (apply unique_use/284 r/321))
         (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/324 y/323))))
>>>>>>> f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
<<<<<<< HEAD
    match_mini_anf_unique/320))
val match_mini_anf_unique : record @ unique -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
    match_mini_anf_unique/318))
val match_mini_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
=======
    match_mini_anf_unique/319))
val match_mini_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/330 =
     (function {nlocal = 0} r/332[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/328 =
     (function {nlocal = 0} r/330[(consts ()) (non_consts ([0: *, *]))]
=======
  (aliased_use/281 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/329 =
     (function {nlocal = 0} r/331[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
<<<<<<< HEAD
         (let (y/334 =a (field_imm 1 r/332))
           (if (== y/334 "") (let (*match*/341 =[int] 0) (exit 8 y/334))
             (let (*match*/339 =[int] 1) (exit 8 (field_imm 1 r/332)))))
        with (8 y/333)
||||||| parent of f06c0085dd (Locals x effects)
         (let (y/332 =a (field_imm 1 r/330))
           (if (== y/332 "") (let (*match*/339 =[int] 0) (exit 8 y/332))
             (let (*match*/337 =[int] 1) (exit 8 (field_imm 1 r/330)))))
        with (8 y/331)
=======
         (let (y/333 =a (field_imm 1 r/331))
           (if (== y/333 "") (let (*match*/340 =[int] 0) (exit 8 y/333))
             (let (*match*/338 =[int] 1) (exit 8 (field_imm 1 r/331)))))
        with (8 y/332)
>>>>>>> f06c0085dd (Locals x effects)
         (let
<<<<<<< HEAD
           (r/336 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/282 r/332))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/336
             y/333)))))
||||||| parent of f06c0085dd (Locals x effects)
           (r/334 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/280 r/330))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/334
             y/331)))))
=======
           (r/335 =[(consts ()) (non_consts ([0: *, *]))]
              (apply aliased_use/281 r/331))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/335
             y/332)))))
>>>>>>> f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
<<<<<<< HEAD
    match_anf_aliased/330))
val match_anf_aliased : record -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
    match_anf_aliased/328))
val match_anf_aliased : record -> record * string @@ global many = <fun>
=======
    match_anf_aliased/329))
val match_anf_aliased : record -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/342 =
     (function {nlocal = 0} r/344[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/340 =
     (function {nlocal = 0} r/342[(consts ()) (non_consts ([0: *, *]))]
=======
  (unique_use/284 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/341 =
     (function {nlocal = 0} r/343[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
       (catch
<<<<<<< HEAD
         (let (y/346 =o (field_mut 1 r/344))
           (if (== y/346 "") (let (*match*/353 =[int] 0) (exit 14 y/346))
             (let (y/347 =o (field_mut 1 r/344) *match*/351 =[int] 1)
               (exit 14 y/347))))
        with (14 y/345)
||||||| parent of f06c0085dd (Locals x effects)
         (let (y/344 =o (field_mut 1 r/342))
           (if (== y/344 "") (let (*match*/351 =[int] 0) (exit 14 y/344))
             (let (y/345 =o (field_mut 1 r/342) *match*/349 =[int] 1)
               (exit 14 y/345))))
        with (14 y/343)
=======
         (let (y/345 =o (field_mut 1 r/343))
           (if (== y/345 "") (let (*match*/352 =[int] 0) (exit 14 y/345))
             (let (y/346 =o (field_mut 1 r/343) *match*/350 =[int] 1)
               (exit 14 y/346))))
        with (14 y/344)
>>>>>>> f06c0085dd (Locals x effects)
         (let
<<<<<<< HEAD
           (r/348 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/285 r/344))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/348
             y/345)))))
||||||| parent of f06c0085dd (Locals x effects)
           (r/346 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/283 r/342))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/346
             y/343)))))
=======
           (r/347 =[(consts ()) (non_consts ([0: *, *]))]
              (apply unique_use/284 r/343))
           (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/347
             y/344)))))
>>>>>>> f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
<<<<<<< HEAD
    match_anf_unique/342))
val match_anf_unique : record @ unique -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
    match_anf_unique/340))
val match_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
=======
    match_anf_unique/341))
val match_anf_unique : record @ unique -> record * string @@ global many =
  <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (swap_inner/360 =
||||||| parent of f06c0085dd (Locals x effects)
  (swap_inner/358 =
=======
  (swap_inner/359 =
>>>>>>> f06c0085dd (Locals x effects)
     (function {nlocal = 0}
<<<<<<< HEAD
       t/362[(consts (0))
||||||| parent of f06c0085dd (Locals x effects)
       t/360[(consts (0))
=======
       t/361[(consts (0))
>>>>>>> f06c0085dd (Locals x effects)
             (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                           [int],
                           [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       [(consts (0))
        (non_consts ([0: [(consts (0)) (non_consts ([0: *, [int], *]))],
                      [int], [(consts (0)) (non_consts ([0: *, [int], *]))]]))]
       (catch
<<<<<<< HEAD
         (if t/362
           (let (*match*/371 =a (field_imm 0 t/362))
             (if *match*/371
               (let (*match*/375 =a (field_imm 2 t/362))
                 (if *match*/375
||||||| parent of f06c0085dd (Locals x effects)
         (if t/360
           (let (*match*/369 =a (field_imm 0 t/360))
             (if *match*/369
               (let (*match*/373 =a (field_imm 2 t/360))
                 (if *match*/373
=======
         (if t/361
           (let (*match*/370 =a (field_imm 0 t/361))
             (if *match*/370
               (let (*match*/374 =a (field_imm 2 t/361))
                 (if *match*/374
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
                       (field_imm 0 *match*/371) (field_int 1 *match*/371)
                       (field_imm 0 *match*/375))
                     (field_int 1 t/362)
||||||| parent of f06c0085dd (Locals x effects)
                       (field_imm 0 *match*/369) (field_int 1 *match*/369)
                       (field_imm 0 *match*/373))
                     (field_int 1 t/360)
=======
                       (field_imm 0 *match*/370) (field_int 1 *match*/370)
                       (field_imm 0 *match*/374))
                     (field_int 1 t/361)
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
                       (field_imm 2 *match*/371) (field_int 1 *match*/375)
                       (field_imm 2 *match*/375)))
||||||| parent of f06c0085dd (Locals x effects)
                       (field_imm 2 *match*/369) (field_int 1 *match*/373)
                       (field_imm 2 *match*/373)))
=======
                       (field_imm 2 *match*/370) (field_int 1 *match*/374)
                       (field_imm 2 *match*/374)))
>>>>>>> f06c0085dd (Locals x effects)
                   (exit 19)))
               (exit 19)))
           (exit 19))
<<<<<<< HEAD
        with (19) t/362)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/360))
val swap_inner : tree -> tree = <fun>
||||||| parent of f06c0085dd (Locals x effects)
        with (19) t/360)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/358))
val swap_inner : tree -> tree @@ global many = <fun>
=======
        with (19) t/361)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/359))
val swap_inner : tree -> tree @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
<<<<<<< HEAD
  (unique_use/285 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/282 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/378 =
     (function {nlocal = 0} r/380[(consts ()) (non_consts ([0: *, *]))]
||||||| parent of f06c0085dd (Locals x effects)
  (unique_use/283 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/280 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/376 =
     (function {nlocal = 0} r/378[(consts ()) (non_consts ([0: *, *]))]
=======
  (unique_use/284 = (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/281 = (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/377 =
     (function {nlocal = 0} r/379[(consts ()) (non_consts ([0: *, *]))]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ())
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))], *]))]
<<<<<<< HEAD
       (let (y/381 =o (field_mut 1 r/380))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/381 "")
||||||| parent of f06c0085dd (Locals x effects)
       (let (y/379 =o (field_mut 1 r/378))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/379 "")
=======
       (let (y/380 =o (field_mut 1 r/379))
         (if (apply (field_imm 8 (global Stdlib__String!)) y/380 "")
>>>>>>> f06c0085dd (Locals x effects)
           (let
<<<<<<< HEAD
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
val match_guard : record @ unique -> record * string = <fun>
||||||| parent of f06c0085dd (Locals x effects)
             (r/450 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/280 r/378))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/450
               y/379))
           (let
             (y/380 =o (field_mut 1 r/378)
              r/451 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/283 r/378))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/451
               y/380))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/376))
val match_guard : record @ unique -> record * string @@ global many = <fun>
=======
             (r/451 =[(consts ()) (non_consts ([0: *, *]))]
                (apply aliased_use/281 r/379))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/451
               y/380))
           (let
             (y/381 =o (field_mut 1 r/379)
              r/452 =[(consts ()) (non_consts ([0: *, *]))]
                (apply unique_use/284 r/379))
             (makeblock 0 ([(consts ()) (non_consts ([0: *, *]))],*) r/452
               y/381))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/377))
val match_guard : record @ unique -> record * string @@ global many = <fun>
>>>>>>> f06c0085dd (Locals x effects)
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
