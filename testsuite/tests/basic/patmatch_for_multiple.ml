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
(let (*match*/279 =[int] 3 *match*/280 =[int] 2 *match*/281 =[int] 1)
||||||| parent of f06c0085dd (Locals x effects)
(let (*match*/277 =[int] 3 *match*/278 =[int] 2 *match*/279 =[int] 1)
=======
(let (*match*/278 =[int] 3 *match*/279 =[int] 2 *match*/280 =[int] 1)
>>>>>>> f06c0085dd (Locals x effects)
  (catch
    (catch
<<<<<<< HEAD
      (catch (if (!= *match*/280 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/279 1) (exit 2) (exit 1)))
||||||| parent of f06c0085dd (Locals x effects)
      (catch (if (!= *match*/278 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/277 1) (exit 2) (exit 1)))
=======
      (catch (if (!= *match*/279 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/278 1) (exit 2) (exit 1)))
>>>>>>> f06c0085dd (Locals x effects)
     with (2) 0)
   with (1) 1))
<<<<<<< HEAD
(let (*match*/279 =[int] 3 *match*/280 =[int] 2 *match*/281 =[int] 1)
  (catch (if (!= *match*/280 3) (if (!= *match*/279 1) 0 (exit 1)) (exit 1))
||||||| parent of f06c0085dd (Locals x effects)
(let (*match*/277 =[int] 3 *match*/278 =[int] 2 *match*/279 =[int] 1)
  (catch (if (!= *match*/278 3) (if (!= *match*/277 1) 0 (exit 1)) (exit 1))
=======
(let (*match*/278 =[int] 3 *match*/279 =[int] 2 *match*/280 =[int] 1)
  (catch (if (!= *match*/279 3) (if (!= *match*/278 1) 0 (exit 1)) (exit 1))
>>>>>>> f06c0085dd (Locals x effects)
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
(let (*match*/284 =[int] 3 *match*/285 =[int] 2 *match*/286 =[int] 1)
||||||| parent of f06c0085dd (Locals x effects)
(let (*match*/282 =[int] 3 *match*/283 =[int] 2 *match*/284 =[int] 1)
=======
(let (*match*/283 =[int] 3 *match*/284 =[int] 2 *match*/285 =[int] 1)
>>>>>>> f06c0085dd (Locals x effects)
  (catch
    (catch
      (catch
<<<<<<< HEAD
        (if (!= *match*/285 3) (exit 6)
||||||| parent of f06c0085dd (Locals x effects)
        (if (!= *match*/283 3) (exit 6)
=======
        (if (!= *match*/284 3) (exit 6)
          (let
            (x/287 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/283 *match*/284 *match*/285))
            (exit 4 x/287)))
       with (6)
        (if (!= *match*/283 1) (exit 5)
>>>>>>> f06c0085dd (Locals x effects)
          (let
<<<<<<< HEAD
            (x/288 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/284 *match*/285 *match*/286))
            (exit 4 x/288)))
       with (6)
        (if (!= *match*/284 1) (exit 5)
          (let
            (x/287 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/284 *match*/285 *match*/286))
            (exit 4 x/287))))
||||||| parent of f06c0085dd (Locals x effects)
            (x/286 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/286)))
       with (6)
        (if (!= *match*/282 1) (exit 5)
          (let
            (x/285 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/285))))
=======
            (x/286 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/283 *match*/284 *match*/285))
            (exit 4 x/286))))
>>>>>>> f06c0085dd (Locals x effects)
     with (5) 0)
<<<<<<< HEAD
   with (4 x/282[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/282) 1)))
(let (*match*/284 =[int] 3 *match*/285 =[int] 2 *match*/286 =[int] 1)
||||||| parent of f06c0085dd (Locals x effects)
   with (4 x/280[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/280) 1)))
(let (*match*/282 =[int] 3 *match*/283 =[int] 2 *match*/284 =[int] 1)
=======
   with (4 x/281[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/281) 1)))
(let (*match*/283 =[int] 3 *match*/284 =[int] 2 *match*/285 =[int] 1)
>>>>>>> f06c0085dd (Locals x effects)
  (catch
<<<<<<< HEAD
    (if (!= *match*/285 3)
      (if (!= *match*/284 1) 0
        (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
      (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
   with (4 x/282[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/282) 1)))
||||||| parent of f06c0085dd (Locals x effects)
    (if (!= *match*/283 3)
      (if (!= *match*/282 1) 0
        (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
      (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
   with (4 x/280[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/280) 1)))
=======
    (if (!= *match*/284 3)
      (if (!= *match*/283 1) 0
        (exit 4 (makeblock 0 *match*/283 *match*/284 *match*/285)))
      (exit 4 (makeblock 0 *match*/283 *match*/284 *match*/285)))
   with (4 x/281[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/281) 1)))
>>>>>>> f06c0085dd (Locals x effects)
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/289[int] b/290 : int 0)
(function {nlocal = 0} a/289[int] b/290 : int 0)
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/287[int] b/288 : int 0)
(function {nlocal = 0} a/287[int] b/288 : int 0)
=======
(function {nlocal = 0} a/288[int] b/289 : int 0)
(function {nlocal = 0} a/288[int] b/289 : int 0)
>>>>>>> f06c0085dd (Locals x effects)
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
(function {nlocal = 0} a/293[int] b/294
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/291[int] b/292
=======
(function {nlocal = 0} a/292[int] b/293
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ()) (non_consts ([0: [int], *]))](let
<<<<<<< HEAD
                                              (p/295 =a[(consts ())
||||||| parent of f06c0085dd (Locals x effects)
                                              (p/293 =a[(consts ())
=======
                                              (p/294 =a[(consts ())
>>>>>>> f06c0085dd (Locals x effects)
                                                        (non_consts (
                                                        [0: [int], *]))]
<<<<<<< HEAD
                                                 (makeblock 0 a/293 b/294))
                                              p/295))
(function {nlocal = 0} a/293[int] b/294
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/293 b/294))
||||||| parent of f06c0085dd (Locals x effects)
                                                 (makeblock 0 a/291 b/292))
                                              p/293))
(function {nlocal = 0} a/291[int] b/292
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/291 b/292))
=======
                                                 (makeblock 0 a/292 b/293))
                                              p/294))
(function {nlocal = 0} a/292[int] b/293
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/292 b/293))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/297[int] b/298
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/295[int] b/296
=======
(function {nlocal = 0} a/296[int] b/297
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ()) (non_consts ([0: [int], *]))](let
<<<<<<< HEAD
                                              (p/299 =a[(consts ())
||||||| parent of f06c0085dd (Locals x effects)
                                              (p/297 =a[(consts ())
=======
                                              (p/298 =a[(consts ())
>>>>>>> f06c0085dd (Locals x effects)
                                                        (non_consts (
                                                        [0: [int], *]))]
<<<<<<< HEAD
                                                 (makeblock 0 a/297 b/298))
                                              p/299))
(function {nlocal = 0} a/297[int] b/298
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/297 b/298))
||||||| parent of f06c0085dd (Locals x effects)
                                                 (makeblock 0 a/295 b/296))
                                              p/297))
(function {nlocal = 0} a/295[int] b/296
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/295 b/296))
=======
                                                 (makeblock 0 a/296 b/297))
                                              p/298))
(function {nlocal = 0} a/296[int] b/297
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/296 b/297))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/303[int] b/304
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/301[int] b/302
=======
(function {nlocal = 0} a/302[int] b/303
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
<<<<<<< HEAD
    (x/305 =a[int] a/303
     p/306 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/303 b/304))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/305
      p/306)))
(function {nlocal = 0} a/303[int] b/304
||||||| parent of f06c0085dd (Locals x effects)
    (x/303 =a[int] a/301
     p/304 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/301 b/302))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/303
      p/304)))
(function {nlocal = 0} a/301[int] b/302
=======
    (x/304 =a[int] a/302
     p/305 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/302 b/303))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/304
      p/305)))
(function {nlocal = 0} a/302[int] b/303
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
<<<<<<< HEAD
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/303
    (makeblock 0 a/303 b/304)))
||||||| parent of f06c0085dd (Locals x effects)
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/301
    (makeblock 0 a/301 b/302)))
=======
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/302
    (makeblock 0 a/302 b/303)))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/309[int] b/310
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/307[int] b/308
=======
(function {nlocal = 0} a/308[int] b/309
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
<<<<<<< HEAD
    (x/311 =a[int] a/309
     p/312 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/309 b/310))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/311
      p/312)))
(function {nlocal = 0} a/309[int] b/310
||||||| parent of f06c0085dd (Locals x effects)
    (x/309 =a[int] a/307
     p/310 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/307 b/308))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/309
      p/310)))
(function {nlocal = 0} a/307[int] b/308
=======
    (x/310 =a[int] a/308
     p/311 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/308 b/309))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/310
      p/311)))
(function {nlocal = 0} a/308[int] b/309
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
<<<<<<< HEAD
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/309
    (makeblock 0 a/309 b/310)))
||||||| parent of f06c0085dd (Locals x effects)
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/307
    (makeblock 0 a/307 b/308)))
=======
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/308
    (makeblock 0 a/308 b/309)))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/319[int] b/320[int]
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/317[int] b/318[int]
=======
(function {nlocal = 0} a/318[int] b/319[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
<<<<<<< HEAD
  (if a/319
||||||| parent of f06c0085dd (Locals x effects)
  (if a/317
=======
  (if a/318
>>>>>>> f06c0085dd (Locals x effects)
    (let
<<<<<<< HEAD
      (x/321 =a[int] a/319
       p/322 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/319 b/320))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/321
        p/322))
    (let
      (x/323 =a[(consts ()) (non_consts ([0: ]))] b/320
       p/324 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/319 b/320))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/323
        p/324))))
(function {nlocal = 0} a/319[int] b/320[int]
||||||| parent of f06c0085dd (Locals x effects)
      (x/319 =a[int] a/317
       p/320 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/317 b/318))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/319
        p/320))
    (let
      (x/321 =a[(consts ()) (non_consts ([0: ]))] b/318
       p/322 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/317 b/318))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/321
        p/322))))
(function {nlocal = 0} a/317[int] b/318[int]
=======
      (x/320 =a[int] a/318
       p/321 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/318 b/319))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/320
        p/321))
    (let
      (x/322 =a[(consts ()) (non_consts ([0: ]))] b/319
       p/323 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/318 b/319))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/322
        p/323))))
(function {nlocal = 0} a/318[int] b/319[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
<<<<<<< HEAD
  (if a/319
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/319
      (makeblock 0 a/319 b/320))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/320
      (makeblock 0 a/319 b/320))))
||||||| parent of f06c0085dd (Locals x effects)
  (if a/317
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/317
      (makeblock 0 a/317 b/318))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/318
      (makeblock 0 a/317 b/318))))
=======
  (if a/318
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/318
      (makeblock 0 a/318 b/319))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/319
      (makeblock 0 a/318 b/319))))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/325[int] b/326[int]
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/323[int] b/324[int]
=======
(function {nlocal = 0} a/324[int] b/325[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
<<<<<<< HEAD
    (if a/325
||||||| parent of f06c0085dd (Locals x effects)
    (if a/323
=======
    (if a/324
>>>>>>> f06c0085dd (Locals x effects)
      (let
<<<<<<< HEAD
        (x/333 =a[int] a/325
         p/334 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/325 b/326))
        (exit 10 x/333 p/334))
      (let
        (x/331 =a[(consts ()) (non_consts ([0: ]))] b/326
         p/332 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/325 b/326))
        (exit 10 x/331 p/332)))
   with (10 x/327[int] p/328[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/327
      p/328)))
(function {nlocal = 0} a/325[int] b/326[int]
||||||| parent of f06c0085dd (Locals x effects)
        (x/331 =a[int] a/323
         p/332 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/323 b/324))
        (exit 10 x/331 p/332))
      (let
        (x/329 =a[(consts ()) (non_consts ([0: ]))] b/324
         p/330 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/323 b/324))
        (exit 10 x/329 p/330)))
   with (10 x/325[int] p/326[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/325
      p/326)))
(function {nlocal = 0} a/323[int] b/324[int]
=======
        (x/332 =a[int] a/324
         p/333 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/324 b/325))
        (exit 10 x/332 p/333))
      (let
        (x/330 =a[(consts ()) (non_consts ([0: ]))] b/325
         p/331 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/324 b/325))
        (exit 10 x/330 p/331)))
   with (10 x/326[int] p/327[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/326
      p/327)))
(function {nlocal = 0} a/324[int] b/325[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
<<<<<<< HEAD
    (if a/325 (exit 10 a/325 (makeblock 0 a/325 b/326))
      (exit 10 b/326 (makeblock 0 a/325 b/326)))
   with (10 x/327[int] p/328[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/327
      p/328)))
||||||| parent of f06c0085dd (Locals x effects)
    (if a/323 (exit 10 a/323 (makeblock 0 a/323 b/324))
      (exit 10 b/324 (makeblock 0 a/323 b/324)))
   with (10 x/325[int] p/326[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/325
      p/326)))
=======
    (if a/324 (exit 10 a/324 (makeblock 0 a/324 b/325))
      (exit 10 b/325 (makeblock 0 a/324 b/325)))
   with (10 x/326[int] p/327[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/326
      p/327)))
>>>>>>> f06c0085dd (Locals x effects)
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
(function {nlocal = 0} a/335[int] b/336[int]
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/333[int] b/334[int]
=======
(function {nlocal = 0} a/334[int] b/335[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
<<<<<<< HEAD
  (if a/335
||||||| parent of f06c0085dd (Locals x effects)
  (if a/333
=======
  (if a/334
>>>>>>> f06c0085dd (Locals x effects)
    (let
<<<<<<< HEAD
      (x/337 =a[int] a/335
       _p/338 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/335 b/336))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/337
||||||| parent of f06c0085dd (Locals x effects)
      (x/335 =a[int] a/333
       _p/336 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/333 b/334))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/335
=======
      (x/336 =a[int] a/334
       _p/337 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/334 b/335))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/336
>>>>>>> f06c0085dd (Locals x effects)
        [0: 1 1]))
    (let
<<<<<<< HEAD
      (x/339 =a[int] a/335
       p/340 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/335 b/336))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/339
        p/340))))
(function {nlocal = 0} a/335[int] b/336[int]
||||||| parent of f06c0085dd (Locals x effects)
      (x/337 =a[int] a/333
       p/338 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/333 b/334))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/337
        p/338))))
(function {nlocal = 0} a/333[int] b/334[int]
=======
      (x/338 =a[int] a/334
       p/339 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/334 b/335))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/338
        p/339))))
(function {nlocal = 0} a/334[int] b/335[int]
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
<<<<<<< HEAD
  (if a/335
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/335
||||||| parent of f06c0085dd (Locals x effects)
  (if a/333
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/333
=======
  (if a/334
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/334
>>>>>>> f06c0085dd (Locals x effects)
      [0: 1 1])
<<<<<<< HEAD
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/335
      (makeblock 0 a/335 b/336))))
||||||| parent of f06c0085dd (Locals x effects)
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/333
      (makeblock 0 a/333 b/334))))
=======
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/334
      (makeblock 0 a/334 b/335))))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/341[int] b/342
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/339[int] b/340
=======
(function {nlocal = 0} a/340[int] b/341
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
<<<<<<< HEAD
    (x/343 =a[int] a/341
     p/344 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/341 b/342))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/343
      p/344)))
(function {nlocal = 0} a/341[int] b/342
||||||| parent of f06c0085dd (Locals x effects)
    (x/341 =a[int] a/339
     p/342 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/339 b/340))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/341
      p/342)))
(function {nlocal = 0} a/339[int] b/340
=======
    (x/342 =a[int] a/340
     p/343 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/340 b/341))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/342
      p/343)))
(function {nlocal = 0} a/340[int] b/341
>>>>>>> f06c0085dd (Locals x effects)
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
<<<<<<< HEAD
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/341
    (makeblock 0 a/341 b/342)))
||||||| parent of f06c0085dd (Locals x effects)
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/339
    (makeblock 0 a/339 b/340)))
=======
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/340
    (makeblock 0 a/340 b/341)))
>>>>>>> f06c0085dd (Locals x effects)
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
(function {nlocal = 0} a/354[int]
  b/355[(consts (0))
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/352[int]
  b/353[(consts (0))
=======
(function {nlocal = 0} a/353[int]
  b/354[(consts (0))
>>>>>>> f06c0085dd (Locals x effects)
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
<<<<<<< HEAD
                                                                    (if a/354
                                                                    (if b/355
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (if a/352
                                                                    (if b/353
=======
                                                                    (if a/353
                                                                    (if b/354
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (let
<<<<<<< HEAD
                                                                    (p/356 =a
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (p/354 =a
=======
                                                                    (p/355 =a
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (field_imm 0
<<<<<<< HEAD
                                                                    b/355))
                                                                    p/356)
||||||| parent of f06c0085dd (Locals x effects)
                                                                    b/353))
                                                                    p/354)
=======
                                                                    b/354))
                                                                    p/355)
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
<<<<<<< HEAD
                                                                    (p/357 =a
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (p/355 =a
=======
                                                                    (p/356 =a
>>>>>>> f06c0085dd (Locals x effects)
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
<<<<<<< HEAD
                                                                    a/354
                                                                    b/355))
                                                                    p/357)))
(function {nlocal = 0} a/354[int]
  b/355[(consts (0))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    a/352
                                                                    b/353))
                                                                    p/355)))
(function {nlocal = 0} a/352[int]
  b/353[(consts (0))
=======
                                                                    a/353
                                                                    b/354))
                                                                    p/356)))
(function {nlocal = 0} a/353[int]
  b/354[(consts (0))
>>>>>>> f06c0085dd (Locals x effects)
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
<<<<<<< HEAD
                                                                    (if a/354
                                                                    (if b/355
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (if a/352
                                                                    (if b/353
=======
                                                                    (if a/353
                                                                    (if b/354
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (field_imm 0
<<<<<<< HEAD
                                                                    b/355)
||||||| parent of f06c0085dd (Locals x effects)
                                                                    b/353)
=======
                                                                    b/354)
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
<<<<<<< HEAD
                                                                    a/354
                                                                    b/355)))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    a/352
                                                                    b/353)))
=======
                                                                    a/353
                                                                    b/354)))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/358[int]
  b/359[(consts (0))
||||||| parent of f06c0085dd (Locals x effects)
(function {nlocal = 0} a/356[int]
  b/357[(consts (0))
=======
(function {nlocal = 0} a/357[int]
  b/358[(consts (0))
>>>>>>> f06c0085dd (Locals x effects)
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
<<<<<<< HEAD
                                                                    (if a/358
                                                                    (if b/359
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (if a/356
                                                                    (if b/357
=======
                                                                    (if a/357
                                                                    (if b/358
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (let
<<<<<<< HEAD
                                                                    (p/363 =a
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (p/361 =a
=======
                                                                    (p/362 =a
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (field_imm 0
<<<<<<< HEAD
                                                                    b/359))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    b/357))
=======
                                                                    b/358))
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 13
<<<<<<< HEAD
                                                                    p/363))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    p/361))
=======
                                                                    p/362))
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
<<<<<<< HEAD
                                                                    (p/362 =a
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (p/360 =a
=======
                                                                    (p/361 =a
>>>>>>> f06c0085dd (Locals x effects)
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
<<<<<<< HEAD
                                                                    a/358
                                                                    b/359))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    a/356
                                                                    b/357))
=======
                                                                    a/357
                                                                    b/358))
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 13
<<<<<<< HEAD
                                                                    p/362)))
                                                                    with (13 p/360
||||||| parent of f06c0085dd (Locals x effects)
                                                                    p/360)))
                                                                    with (13 p/358
=======
                                                                    p/361)))
                                                                    with (13 p/359
>>>>>>> f06c0085dd (Locals x effects)
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
<<<<<<< HEAD
                                                                    p/360))
(function {nlocal = 0} a/358[int]
  b/359[(consts (0))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    p/358))
(function {nlocal = 0} a/356[int]
  b/357[(consts (0))
=======
                                                                    p/359))
(function {nlocal = 0} a/357[int]
  b/358[(consts (0))
>>>>>>> f06c0085dd (Locals x effects)
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
<<<<<<< HEAD
                                                                    (if a/358
                                                                    (if b/359
||||||| parent of f06c0085dd (Locals x effects)
                                                                    (if a/356
                                                                    (if b/357
=======
                                                                    (if a/357
                                                                    (if b/358
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 13
                                                                    (field_imm 0
<<<<<<< HEAD
                                                                    b/359))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    b/357))
=======
                                                                    b/358))
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
<<<<<<< HEAD
                                                                    a/358
                                                                    b/359)))
                                                                    with (13 p/360
||||||| parent of f06c0085dd (Locals x effects)
                                                                    a/356
                                                                    b/357)))
                                                                    with (13 p/358
=======
                                                                    a/357
                                                                    b/358)))
                                                                    with (13 p/359
>>>>>>> f06c0085dd (Locals x effects)
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
<<<<<<< HEAD
                                                                    p/360))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    p/358))
=======
                                                                    p/359))
>>>>>>> f06c0085dd (Locals x effects)
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
