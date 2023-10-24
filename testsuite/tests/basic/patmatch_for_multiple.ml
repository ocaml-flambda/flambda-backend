(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
<<<<<<< HEAD
(let (*match*/274 =[int] 3 *match*/275 =[int] 2 *match*/276 =[int] 1)
||||||| merged common ancestors
(let (*match*/273 = 3 *match*/274 = 2 *match*/275 = 1)
=======
(let (*match*/276 = 3 *match*/277 = 2 *match*/278 = 1)
>>>>>>> ocaml/5.1
  (catch
    (catch
<<<<<<< HEAD
      (catch (if (!= *match*/275 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/274 1) (exit 2) (exit 1)))
||||||| merged common ancestors
      (catch (if (!= *match*/274 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/273 1) (exit 2) (exit 1)))
=======
      (catch (if (!= *match*/277 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/276 1) (exit 2) (exit 1)))
>>>>>>> ocaml/5.1
     with (2) 0)
   with (1) 1))
<<<<<<< HEAD
(let (*match*/274 =[int] 3 *match*/275 =[int] 2 *match*/276 =[int] 1)
  (catch (if (!= *match*/275 3) (if (!= *match*/274 1) 0 (exit 1)) (exit 1))
||||||| merged common ancestors
(let (*match*/273 = 3 *match*/274 = 2 *match*/275 = 1)
  (catch (if (!= *match*/274 3) (if (!= *match*/273 1) 0 (exit 1)) (exit 1))
=======
(let (*match*/276 = 3 *match*/277 = 2 *match*/278 = 1)
  (catch (if (!= *match*/277 3) (if (!= *match*/276 1) 0 (exit 1)) (exit 1))
>>>>>>> ocaml/5.1
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
<<<<<<< HEAD
(let (*match*/279 =[int] 3 *match*/280 =[int] 2 *match*/281 =[int] 1)
||||||| merged common ancestors
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
=======
(let (*match*/281 = 3 *match*/282 = 2 *match*/283 = 1)
>>>>>>> ocaml/5.1
  (catch
    (catch
      (catch
<<<<<<< HEAD
        (if (!= *match*/280 3) (exit 6)
          (let
            (x/283 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/279 *match*/280 *match*/281))
            (exit 4 x/283)))
||||||| merged common ancestors
        (if (!= *match*/279 3) (exit 6)
          (let (x/282 =a (makeblock 0 *match*/278 *match*/279 *match*/280))
            (exit 4 x/282)))
=======
        (if (!= *match*/282 3) (exit 6)
          (let (x/285 =a (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/285)))
>>>>>>> ocaml/5.1
       with (6)
<<<<<<< HEAD
        (if (!= *match*/279 1) (exit 5)
          (let
            (x/282 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/279 *match*/280 *match*/281))
            (exit 4 x/282))))
||||||| merged common ancestors
        (if (!= *match*/278 1) (exit 5)
          (let (x/281 =a (makeblock 0 *match*/278 *match*/279 *match*/280))
            (exit 4 x/281))))
=======
        (if (!= *match*/281 1) (exit 5)
          (let (x/284 =a (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/284))))
>>>>>>> ocaml/5.1
     with (5) 0)
<<<<<<< HEAD
   with (4 x/277[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/277) 1)))
(let (*match*/279 =[int] 3 *match*/280 =[int] 2 *match*/281 =[int] 1)
||||||| merged common ancestors
   with (4 x/276) (seq (ignore x/276) 1)))
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
=======
   with (4 x/279) (seq (ignore x/279) 1)))
(let (*match*/281 = 3 *match*/282 = 2 *match*/283 = 1)
>>>>>>> ocaml/5.1
  (catch
<<<<<<< HEAD
    (if (!= *match*/280 3)
      (if (!= *match*/279 1) 0
        (exit 4 (makeblock 0 *match*/279 *match*/280 *match*/281)))
      (exit 4 (makeblock 0 *match*/279 *match*/280 *match*/281)))
   with (4 x/277[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/277) 1)))
||||||| merged common ancestors
    (if (!= *match*/279 3)
      (if (!= *match*/278 1) 0
        (exit 4 (makeblock 0 *match*/278 *match*/279 *match*/280)))
      (exit 4 (makeblock 0 *match*/278 *match*/279 *match*/280)))
   with (4 x/276) (seq (ignore x/276) 1)))
=======
    (if (!= *match*/282 3)
      (if (!= *match*/281 1) 0
        (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
      (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
   with (4 x/279) (seq (ignore x/279) 1)))
>>>>>>> ocaml/5.1
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/284[int] b/285 : int 0)
(function {nlocal = 0} a/284[int] b/285 : int 0)
||||||| merged common ancestors
(function a/283[int] b/284 : int 0)
(function a/283[int] b/284 : int 0)
=======
(function a/286[int] b/287 : int 0)
(function a/286[int] b/287 : int 0)
>>>>>>> ocaml/5.1
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/288[int] b/289
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/290 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/288 b/289))
                                              p/290))
(function {nlocal = 0} a/288[int] b/289
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/288 b/289))
||||||| merged common ancestors
(function a/287[int] b/288 (let (p/289 =a (makeblock 0 a/287 b/288)) p/289))
(function a/287[int] b/288 (makeblock 0 a/287 b/288))
=======
(function a/290[int] b/291 (let (p/292 =a (makeblock 0 a/290 b/291)) p/292))
(function a/290[int] b/291 (makeblock 0 a/290 b/291))
>>>>>>> ocaml/5.1
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/292[int] b/293
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/294 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/292 b/293))
                                              p/294))
(function {nlocal = 0} a/292[int] b/293
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/292 b/293))
||||||| merged common ancestors
(function a/291[int] b/292 (let (p/293 =a (makeblock 0 a/291 b/292)) p/293))
(function a/291[int] b/292 (makeblock 0 a/291 b/292))
=======
(function a/294[int] b/295 (let (p/296 =a (makeblock 0 a/294 b/295)) p/296))
(function a/294[int] b/295 (makeblock 0 a/294 b/295))
>>>>>>> ocaml/5.1
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/298[int] b/299
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/300 =a[int] a/298
     p/301 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/298 b/299))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/300
      p/301)))
(function {nlocal = 0} a/298[int] b/299
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/298
    (makeblock 0 a/298 b/299)))
||||||| merged common ancestors
(function a/297[int] b/298
  (let (x/299 =a[int] a/297 p/300 =a (makeblock 0 a/297 b/298))
    (makeblock 0 (int,*) x/299 p/300)))
(function a/297[int] b/298
  (makeblock 0 (int,*) a/297 (makeblock 0 a/297 b/298)))
=======
(function a/300[int] b/301
  (let (x/302 =a[int] a/300 p/303 =a (makeblock 0 a/300 b/301))
    (makeblock 0 (int,*) x/302 p/303)))
(function a/300[int] b/301
  (makeblock 0 (int,*) a/300 (makeblock 0 a/300 b/301)))
>>>>>>> ocaml/5.1
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/304[int] b/305
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/306 =a[int] a/304
     p/307 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/304 b/305))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/306
      p/307)))
(function {nlocal = 0} a/304[int] b/305
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/304
    (makeblock 0 a/304 b/305)))
||||||| merged common ancestors
(function a/303[int] b/304
  (let (x/305 =a[int] a/303 p/306 =a (makeblock 0 a/303 b/304))
    (makeblock 0 (int,*) x/305 p/306)))
(function a/303[int] b/304
  (makeblock 0 (int,*) a/303 (makeblock 0 a/303 b/304)))
=======
(function a/306[int] b/307
  (let (x/308 =a[int] a/306 p/309 =a (makeblock 0 a/306 b/307))
    (makeblock 0 (int,*) x/308 p/309)))
(function a/306[int] b/307
  (makeblock 0 (int,*) a/306 (makeblock 0 a/306 b/307)))
>>>>>>> ocaml/5.1
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/314[int] b/315[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/314
    (let
      (x/316 =a[int] a/314
       p/317 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/314 b/315))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/316
        p/317))
    (let
      (x/318 =a[(consts ()) (non_consts ([0: ]))] b/315
       p/319 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/314 b/315))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/318
        p/319))))
(function {nlocal = 0} a/314[int] b/315[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/314
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/314
      (makeblock 0 a/314 b/315))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/315
      (makeblock 0 a/314 b/315))))
||||||| merged common ancestors
(function a/313[int] b/314[int]
  (if a/313
    (let (x/315 =a[int] a/313 p/316 =a (makeblock 0 a/313 b/314))
      (makeblock 0 (int,*) x/315 p/316))
    (let (x/317 =a b/314 p/318 =a (makeblock 0 a/313 b/314))
      (makeblock 0 (int,*) x/317 p/318))))
(function a/313[int] b/314[int]
  (if a/313 (makeblock 0 (int,*) a/313 (makeblock 0 a/313 b/314))
    (makeblock 0 (int,*) b/314 (makeblock 0 a/313 b/314))))
=======
(function a/316[int] b/317[int]
  (if a/316
    (let (x/318 =a[int] a/316 p/319 =a (makeblock 0 a/316 b/317))
      (makeblock 0 (int,*) x/318 p/319))
    (let (x/320 =a b/317 p/321 =a (makeblock 0 a/316 b/317))
      (makeblock 0 (int,*) x/320 p/321))))
(function a/316[int] b/317[int]
  (if a/316 (makeblock 0 (int,*) a/316 (makeblock 0 a/316 b/317))
    (makeblock 0 (int,*) b/317 (makeblock 0 a/316 b/317))))
>>>>>>> ocaml/5.1
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/320[int] b/321[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
||||||| merged common ancestors
(function a/319[int] b/320[int]
=======
(function a/322[int] b/323[int]
>>>>>>> ocaml/5.1
  (catch
<<<<<<< HEAD
    (if a/320
      (let
        (x/328 =a[int] a/320
         p/329 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/320 b/321))
        (exit 10 x/328 p/329))
      (let
        (x/326 =a[(consts ()) (non_consts ([0: ]))] b/321
         p/327 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/320 b/321))
        (exit 10 x/326 p/327)))
   with (10 x/322[int] p/323[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/322
      p/323)))
(function {nlocal = 0} a/320[int] b/321[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
||||||| merged common ancestors
    (if a/319
      (let (x/327 =a[int] a/319 p/328 =a (makeblock 0 a/319 b/320))
        (exit 10 x/327 p/328))
      (let (x/325 =a b/320 p/326 =a (makeblock 0 a/319 b/320))
        (exit 10 x/325 p/326)))
   with (10 x/321[int] p/322) (makeblock 0 (int,*) x/321 p/322)))
(function a/319[int] b/320[int]
=======
    (if a/322
      (let (x/330 =a[int] a/322 p/331 =a (makeblock 0 a/322 b/323))
        (exit 10 x/330 p/331))
      (let (x/328 =a b/323 p/329 =a (makeblock 0 a/322 b/323))
        (exit 10 x/328 p/329)))
   with (10 x/324[int] p/325) (makeblock 0 (int,*) x/324 p/325)))
(function a/322[int] b/323[int]
>>>>>>> ocaml/5.1
  (catch
<<<<<<< HEAD
    (if a/320 (exit 10 a/320 (makeblock 0 a/320 b/321))
      (exit 10 b/321 (makeblock 0 a/320 b/321)))
   with (10 x/322[int] p/323[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/322
      p/323)))
||||||| merged common ancestors
    (if a/319 (exit 10 a/319 (makeblock 0 a/319 b/320))
      (exit 10 b/320 (makeblock 0 a/319 b/320)))
   with (10 x/321[int] p/322) (makeblock 0 (int,*) x/321 p/322)))
=======
    (if a/322 (exit 10 a/322 (makeblock 0 a/322 b/323))
      (exit 10 b/323 (makeblock 0 a/322 b/323)))
   with (10 x/324[int] p/325) (makeblock 0 (int,*) x/324 p/325)))
>>>>>>> ocaml/5.1
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/330[int] b/331[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/330
    (let
      (x/332 =a[int] a/330
       _p/333 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/330 b/331))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/332
        [0: 1 1]))
    (let
      (x/334 =a[int] a/330
       p/335 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/330 b/331))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/334
        p/335))))
(function {nlocal = 0} a/330[int] b/331[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/330
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/330
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/330
      (makeblock 0 a/330 b/331))))
||||||| merged common ancestors
(function a/329[int] b/330[int]
  (if a/329
    (let (x/331 =a[int] a/329 _p/332 =a (makeblock 0 a/329 b/330))
      (makeblock 0 (int,*) x/331 [0: 1 1]))
    (let (x/333 =a[int] a/329 p/334 =a (makeblock 0 a/329 b/330))
      (makeblock 0 (int,*) x/333 p/334))))
(function a/329[int] b/330[int]
  (if a/329 (makeblock 0 (int,*) a/329 [0: 1 1])
    (makeblock 0 (int,*) a/329 (makeblock 0 a/329 b/330))))
=======
(function a/332[int] b/333[int]
  (if a/332
    (let (x/334 =a[int] a/332 _p/335 =a (makeblock 0 a/332 b/333))
      (makeblock 0 (int,*) x/334 [0: 1 1]))
    (let (x/336 =a[int] a/332 p/337 =a (makeblock 0 a/332 b/333))
      (makeblock 0 (int,*) x/336 p/337))))
(function a/332[int] b/333[int]
  (if a/332 (makeblock 0 (int,*) a/332 [0: 1 1])
    (makeblock 0 (int,*) a/332 (makeblock 0 a/332 b/333))))
>>>>>>> ocaml/5.1
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/336[int] b/337
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/338 =a[int] a/336
     p/339 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/336 b/337))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/338
      p/339)))
(function {nlocal = 0} a/336[int] b/337
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/336
    (makeblock 0 a/336 b/337)))
||||||| merged common ancestors
(function a/335[int] b/336
  (let (x/337 =a[int] a/335 p/338 =a (makeblock 0 a/335 b/336))
    (makeblock 0 (int,*) x/337 p/338)))
(function a/335[int] b/336
  (makeblock 0 (int,*) a/335 (makeblock 0 a/335 b/336)))
=======
(function a/338[int] b/339
  (let (x/340 =a[int] a/338 p/341 =a (makeblock 0 a/338 b/339))
    (makeblock 0 (int,*) x/340 p/341)))
(function a/338[int] b/339
  (makeblock 0 (int,*) a/338 (makeblock 0 a/338 b/339)))
>>>>>>> ocaml/5.1
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/349[int]
  b/350[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/349
                                                                    (if b/350
                                                                    (let
                                                                    (p/351 =a
                                                                    (field 0
                                                                    b/350))
                                                                    p/351)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/352 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/349
                                                                    b/350))
                                                                    p/352)))
(function {nlocal = 0} a/349[int]
  b/350[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/349
                                                                    (if b/350
                                                                    (field 0
                                                                    b/350)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/349
                                                                    b/350)))
||||||| merged common ancestors
(function a/348[int] b/349
  (catch
    (if a/348 (if b/349 (let (p/350 =a (field 0 b/349)) p/350) (exit 12))
      (exit 12))
   with (12) (let (p/351 =a (makeblock 0 a/348 b/349)) p/351)))
(function a/348[int] b/349
  (catch (if a/348 (if b/349 (field 0 b/349) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/348 b/349)))
=======
(function a/351[int] b/352
  (catch
    (if a/351 (if b/352 (let (p/353 =a (field_imm 0 b/352)) p/353) (exit 12))
      (exit 12))
   with (12) (let (p/354 =a (makeblock 0 a/351 b/352)) p/354)))
(function a/351[int] b/352
  (catch (if a/351 (if b/352 (field_imm 0 b/352) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/351 b/352)))
>>>>>>> ocaml/5.1
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/353[int]
  b/354[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/353
                                                                    (if b/354
                                                                    (let
                                                                    (p/358 =a
                                                                    (field 0
                                                                    b/354))
                                                                    (exit 13
                                                                    p/358))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/357 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/353
                                                                    b/354))
                                                                    (exit 13
                                                                    p/357)))
                                                                    with (13 p/355
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/355))
(function {nlocal = 0} a/353[int]
  b/354[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/353
                                                                    (if b/354
                                                                    (exit 13
                                                                    (field 0
                                                                    b/354))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/353
                                                                    b/354)))
                                                                    with (13 p/355
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/355))
||||||| merged common ancestors
(function a/352[int] b/353
  (catch
    (catch
      (if a/352
        (if b/353 (let (p/357 =a (field 0 b/353)) (exit 13 p/357)) (exit 14))
        (exit 14))
     with (14) (let (p/356 =a (makeblock 0 a/352 b/353)) (exit 13 p/356)))
   with (13 p/354) p/354))
(function a/352[int] b/353
  (catch
    (catch
      (if a/352 (if b/353 (exit 13 (field 0 b/353)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/352 b/353)))
   with (13 p/354) p/354))
=======
(function a/355[int] b/356
  (catch
    (catch
      (if a/355
        (if b/356 (let (p/360 =a (field_imm 0 b/356)) (exit 13 p/360))
          (exit 14))
        (exit 14))
     with (14) (let (p/359 =a (makeblock 0 a/355 b/356)) (exit 13 p/359)))
   with (13 p/357) p/357))
(function a/355[int] b/356
  (catch
    (catch
      (if a/355 (if b/356 (exit 13 (field_imm 0 b/356)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/355 b/356)))
   with (13 p/357) p/357))
>>>>>>> ocaml/5.1
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
