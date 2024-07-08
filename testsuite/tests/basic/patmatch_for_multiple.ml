(* TEST
 flags = "-drawlambda -dlambda";
 expect;
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
(let (*match*/276 =[int] 3 *match*/277 =[int] 2 *match*/278 =[int] 1)
||||||| 121bedcfd2
(let (*match*/276 = 3 *match*/277 = 2 *match*/278 = 1)
=======
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
>>>>>>> 5.2.0
  (catch
    (catch
      (catch (if (!= *match*/278 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/277 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
<<<<<<< HEAD
(let (*match*/276 =[int] 3 *match*/277 =[int] 2 *match*/278 =[int] 1)
  (catch (if (!= *match*/277 3) (if (!= *match*/276 1) 0 (exit 1)) (exit 1))
||||||| 121bedcfd2
(let (*match*/276 = 3 *match*/277 = 2 *match*/278 = 1)
  (catch (if (!= *match*/277 3) (if (!= *match*/276 1) 0 (exit 1)) (exit 1))
=======
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
  (catch (if (!= *match*/278 3) (if (!= *match*/277 1) 0 (exit 1)) (exit 1))
>>>>>>> 5.2.0
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
(let (*match*/281 =[int] 3 *match*/282 =[int] 2 *match*/283 =[int] 1)
||||||| 121bedcfd2
(let (*match*/281 = 3 *match*/282 = 2 *match*/283 = 1)
=======
(let (*match*/282 = 3 *match*/283 = 2 *match*/284 = 1)
>>>>>>> 5.2.0
  (catch
    (catch
      (catch
<<<<<<< HEAD
        (if (!= *match*/282 3) (exit 6)
          (let
            (x/285 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/285)))
||||||| 121bedcfd2
        (if (!= *match*/282 3) (exit 6)
          (let (x/285 =a (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/285)))
=======
        (if (!= *match*/283 3) (exit 6)
          (let (x/286 =a (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/286)))
>>>>>>> 5.2.0
       with (6)
<<<<<<< HEAD
        (if (!= *match*/281 1) (exit 5)
          (let
            (x/284 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/284))))
||||||| 121bedcfd2
        (if (!= *match*/281 1) (exit 5)
          (let (x/284 =a (makeblock 0 *match*/281 *match*/282 *match*/283))
            (exit 4 x/284))))
=======
        (if (!= *match*/282 1) (exit 5)
          (let (x/285 =a (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/285))))
>>>>>>> 5.2.0
     with (5) 0)
<<<<<<< HEAD
   with (4 x/279[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/279) 1)))
(let (*match*/281 =[int] 3 *match*/282 =[int] 2 *match*/283 =[int] 1)
||||||| 121bedcfd2
   with (4 x/279) (seq (ignore x/279) 1)))
(let (*match*/281 = 3 *match*/282 = 2 *match*/283 = 1)
=======
   with (4 x/280) (seq (ignore x/280) 1)))
(let (*match*/282 = 3 *match*/283 = 2 *match*/284 = 1)
>>>>>>> 5.2.0
  (catch
<<<<<<< HEAD
    (if (!= *match*/282 3)
      (if (!= *match*/281 1) 0
        (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
      (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
   with (4 x/279[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/279) 1)))
||||||| 121bedcfd2
    (if (!= *match*/282 3)
      (if (!= *match*/281 1) 0
        (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
      (exit 4 (makeblock 0 *match*/281 *match*/282 *match*/283)))
   with (4 x/279) (seq (ignore x/279) 1)))
=======
    (if (!= *match*/283 3)
      (if (!= *match*/282 1) 0
        (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
      (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
   with (4 x/280) (seq (ignore x/280) 1)))
>>>>>>> 5.2.0
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/286[int] b/287 : int 0)
(function {nlocal = 0} a/286[int] b/287 : int 0)
||||||| 121bedcfd2
(function a/286[int] b/287 : int 0)
(function a/286[int] b/287 : int 0)
=======
(function a/287[int] b/288 : int 0)
(function a/287[int] b/288 : int 0)
>>>>>>> 5.2.0
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
(function {nlocal = 0} a/290[int] b/291
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/292 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/290 b/291))
                                              p/292))
(function {nlocal = 0} a/290[int] b/291
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/290 b/291))
||||||| 121bedcfd2
(function a/290[int] b/291 (let (p/292 =a (makeblock 0 a/290 b/291)) p/292))
(function a/290[int] b/291 (makeblock 0 a/290 b/291))
=======
(function a/291[int] b/292 (let (p/293 =a (makeblock 0 a/291 b/292)) p/293))
(function a/291[int] b/292 (makeblock 0 a/291 b/292))
>>>>>>> 5.2.0
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/294[int] b/295
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/296 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/294 b/295))
                                              p/296))
(function {nlocal = 0} a/294[int] b/295
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/294 b/295))
||||||| 121bedcfd2
(function a/294[int] b/295 (let (p/296 =a (makeblock 0 a/294 b/295)) p/296))
(function a/294[int] b/295 (makeblock 0 a/294 b/295))
=======
(function a/295[int] b/296 (let (p/297 =a (makeblock 0 a/295 b/296)) p/297))
(function a/295[int] b/296 (makeblock 0 a/295 b/296))
>>>>>>> 5.2.0
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/300[int] b/301
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/302 =a[int] a/300
     p/303 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/300 b/301))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/302
      p/303)))
(function {nlocal = 0} a/300[int] b/301
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/300
    (makeblock 0 a/300 b/301)))
||||||| 121bedcfd2
(function a/300[int] b/301
  (let (x/302 =a[int] a/300 p/303 =a (makeblock 0 a/300 b/301))
    (makeblock 0 (int,*) x/302 p/303)))
(function a/300[int] b/301
  (makeblock 0 (int,*) a/300 (makeblock 0 a/300 b/301)))
=======
(function a/301[int] b/302
  (let (x/303 =a[int] a/301 p/304 =a (makeblock 0 a/301 b/302))
    (makeblock 0 (int,*) x/303 p/304)))
(function a/301[int] b/302
  (makeblock 0 (int,*) a/301 (makeblock 0 a/301 b/302)))
>>>>>>> 5.2.0
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/306[int] b/307
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/308 =a[int] a/306
     p/309 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/306 b/307))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/308
      p/309)))
(function {nlocal = 0} a/306[int] b/307
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/306
    (makeblock 0 a/306 b/307)))
||||||| 121bedcfd2
(function a/306[int] b/307
  (let (x/308 =a[int] a/306 p/309 =a (makeblock 0 a/306 b/307))
    (makeblock 0 (int,*) x/308 p/309)))
(function a/306[int] b/307
  (makeblock 0 (int,*) a/306 (makeblock 0 a/306 b/307)))
=======
(function a/307[int] b/308
  (let (x/309 =a[int] a/307 p/310 =a (makeblock 0 a/307 b/308))
    (makeblock 0 (int,*) x/309 p/310)))
(function a/307[int] b/308
  (makeblock 0 (int,*) a/307 (makeblock 0 a/307 b/308)))
>>>>>>> 5.2.0
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/316[int] b/317[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/316
    (let
      (x/318 =a[int] a/316
       p/319 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/316 b/317))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/318
        p/319))
    (let
      (x/320 =a[(consts ()) (non_consts ([0: ]))] b/317
       p/321 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/316 b/317))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/320
        p/321))))
(function {nlocal = 0} a/316[int] b/317[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/316
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/316
      (makeblock 0 a/316 b/317))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/317
      (makeblock 0 a/316 b/317))))
||||||| 121bedcfd2
(function a/316[int] b/317[int]
  (if a/316
    (let (x/318 =a[int] a/316 p/319 =a (makeblock 0 a/316 b/317))
      (makeblock 0 (int,*) x/318 p/319))
    (let (x/320 =a b/317 p/321 =a (makeblock 0 a/316 b/317))
      (makeblock 0 (int,*) x/320 p/321))))
(function a/316[int] b/317[int]
  (if a/316 (makeblock 0 (int,*) a/316 (makeblock 0 a/316 b/317))
    (makeblock 0 (int,*) b/317 (makeblock 0 a/316 b/317))))
=======
(function a/317[int] b/318[int]
  (if a/317
    (let (x/319 =a[int] a/317 p/320 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/319 p/320))
    (let (x/321 =a b/318 p/322 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/321 p/322))))
(function a/317[int] b/318[int]
  (if a/317 (makeblock 0 (int,*) a/317 (makeblock 0 a/317 b/318))
    (makeblock 0 (int,*) b/318 (makeblock 0 a/317 b/318))))
>>>>>>> 5.2.0
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/322[int] b/323[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
||||||| 121bedcfd2
(function a/322[int] b/323[int]
=======
(function a/323[int] b/324[int]
>>>>>>> 5.2.0
  (catch
<<<<<<< HEAD
    (if a/322
      (let
        (x/330 =a[int] a/322
         p/331 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/322 b/323))
        (exit 10 x/330 p/331))
      (let
        (x/328 =a[(consts ()) (non_consts ([0: ]))] b/323
         p/329 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/322 b/323))
        (exit 10 x/328 p/329)))
   with (10 x/324[int] p/325[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/324
      p/325)))
(function {nlocal = 0} a/322[int] b/323[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
||||||| 121bedcfd2
    (if a/322
      (let (x/330 =a[int] a/322 p/331 =a (makeblock 0 a/322 b/323))
        (exit 10 x/330 p/331))
      (let (x/328 =a b/323 p/329 =a (makeblock 0 a/322 b/323))
        (exit 10 x/328 p/329)))
   with (10 x/324[int] p/325) (makeblock 0 (int,*) x/324 p/325)))
(function a/322[int] b/323[int]
=======
    (if a/323
      (let (x/331 =a[int] a/323 p/332 =a (makeblock 0 a/323 b/324))
        (exit 10 x/331 p/332))
      (let (x/329 =a b/324 p/330 =a (makeblock 0 a/323 b/324))
        (exit 10 x/329 p/330)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
(function a/323[int] b/324[int]
>>>>>>> 5.2.0
  (catch
<<<<<<< HEAD
    (if a/322 (exit 10 a/322 (makeblock 0 a/322 b/323))
      (exit 10 b/323 (makeblock 0 a/322 b/323)))
   with (10 x/324[int] p/325[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/324
      p/325)))
||||||| 121bedcfd2
    (if a/322 (exit 10 a/322 (makeblock 0 a/322 b/323))
      (exit 10 b/323 (makeblock 0 a/322 b/323)))
   with (10 x/324[int] p/325) (makeblock 0 (int,*) x/324 p/325)))
=======
    (if a/323 (exit 10 a/323 (makeblock 0 a/323 b/324))
      (exit 10 b/324 (makeblock 0 a/323 b/324)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
>>>>>>> 5.2.0
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
(function {nlocal = 0} a/332[int] b/333[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/332
    (let
      (x/334 =a[int] a/332
       _p/335 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/332 b/333))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/334
        [0: 1 1]))
    (let
      (x/336 =a[int] a/332
       p/337 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/332 b/333))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/336
        p/337))))
(function {nlocal = 0} a/332[int] b/333[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/332
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/332
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/332
      (makeblock 0 a/332 b/333))))
||||||| 121bedcfd2
(function a/332[int] b/333[int]
  (if a/332
    (let (x/334 =a[int] a/332 _p/335 =a (makeblock 0 a/332 b/333))
      (makeblock 0 (int,*) x/334 [0: 1 1]))
    (let (x/336 =a[int] a/332 p/337 =a (makeblock 0 a/332 b/333))
      (makeblock 0 (int,*) x/336 p/337))))
(function a/332[int] b/333[int]
  (if a/332 (makeblock 0 (int,*) a/332 [0: 1 1])
    (makeblock 0 (int,*) a/332 (makeblock 0 a/332 b/333))))
=======
(function a/333[int] b/334[int]
  (if a/333
    (let (x/335 =a[int] a/333 _p/336 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/335 [0: 1 1]))
    (let (x/337 =a[int] a/333 p/338 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/337 p/338))))
(function a/333[int] b/334[int]
  (if a/333 (makeblock 0 (int,*) a/333 [0: 1 1])
    (makeblock 0 (int,*) a/333 (makeblock 0 a/333 b/334))))
>>>>>>> 5.2.0
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/338[int] b/339
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/340 =a[int] a/338
     p/341 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/338 b/339))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/340
      p/341)))
(function {nlocal = 0} a/338[int] b/339
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/338
    (makeblock 0 a/338 b/339)))
||||||| 121bedcfd2
(function a/338[int] b/339
  (let (x/340 =a[int] a/338 p/341 =a (makeblock 0 a/338 b/339))
    (makeblock 0 (int,*) x/340 p/341)))
(function a/338[int] b/339
  (makeblock 0 (int,*) a/338 (makeblock 0 a/338 b/339)))
=======
(function a/339[int] b/340
  (let (x/341 =a[int] a/339 p/342 =a (makeblock 0 a/339 b/340))
    (makeblock 0 (int,*) x/341 p/342)))
(function a/339[int] b/340
  (makeblock 0 (int,*) a/339 (makeblock 0 a/339 b/340)))
>>>>>>> 5.2.0
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
(function {nlocal = 0} a/351[int]
  b/352[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/351
                                                                    (if b/352
                                                                    (let
                                                                    (p/353 =a
                                                                    (field_imm 0
                                                                    b/352))
                                                                    p/353)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/354 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/351
                                                                    b/352))
                                                                    p/354)))
(function {nlocal = 0} a/351[int]
  b/352[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/351
                                                                    (if b/352
                                                                    (field_imm 0
                                                                    b/352)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/351
                                                                    b/352)))
||||||| 121bedcfd2
(function a/351[int] b/352
  (catch
    (if a/351 (if b/352 (let (p/353 =a (field_imm 0 b/352)) p/353) (exit 12))
      (exit 12))
   with (12) (let (p/354 =a (makeblock 0 a/351 b/352)) p/354)))
(function a/351[int] b/352
  (catch (if a/351 (if b/352 (field_imm 0 b/352) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/351 b/352)))
=======
(function a/352[int] b/353
  (catch
    (if a/352 (if b/353 (let (p/354 =a (field_imm 0 b/353)) p/354) (exit 12))
      (exit 12))
   with (12) (let (p/355 =a (makeblock 0 a/352 b/353)) p/355)))
(function a/352[int] b/353
  (catch (if a/352 (if b/353 (field_imm 0 b/353) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/352 b/353)))
>>>>>>> 5.2.0
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/355[int]
  b/356[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/355
                                                                    (if b/356
                                                                    (let
                                                                    (p/360 =a
                                                                    (field_imm 0
                                                                    b/356))
                                                                    (exit 13
                                                                    p/360))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/359 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/355
                                                                    b/356))
                                                                    (exit 13
                                                                    p/359)))
                                                                    with (13 p/357
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/357))
(function {nlocal = 0} a/355[int]
  b/356[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/355
                                                                    (if b/356
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/356))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/355
                                                                    b/356)))
                                                                    with (13 p/357
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/357))
||||||| 121bedcfd2
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
=======
(function a/356[int] b/357
  (catch
    (catch
      (if a/356
        (if b/357 (let (p/361 =a (field_imm 0 b/357)) (exit 13 p/361))
          (exit 14))
        (exit 14))
     with (14) (let (p/360 =a (makeblock 0 a/356 b/357)) (exit 13 p/360)))
   with (13 p/358) p/358))
(function a/356[int] b/357
  (catch
    (catch
      (if a/356 (if b/357 (exit 13 (field_imm 0 b/357)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/356 b/357)))
   with (13 p/358) p/358))
>>>>>>> 5.2.0
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
