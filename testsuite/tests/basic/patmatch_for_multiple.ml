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
(let (*match*/282 =[int] 3 *match*/283 =[int] 2 *match*/284 =[int] 1)
  (catch
    (catch
      (catch (if (%noteq *match*/280 3) (exit 3) (exit 1)) with (3)
        (if (%noteq *match*/279 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/279 =[int] 3 *match*/280 =[int] 2 *match*/281 =[int] 1)
  (catch
    (if (%noteq *match*/280 3) (if (%noteq *match*/279 1) 0 (exit 1))
      (exit 1))
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
(let (*match*/287 =[int] 3 *match*/288 =[int] 2 *match*/289 =[int] 1)
  (catch
    (catch
      (catch
        (if (%noteq *match*/285 3) (exit 6)
          (let
            (x/291 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/287 *match*/288 *match*/289))
            (exit 4 x/291)))
       with (6)
        (if (%noteq *match*/284 1) (exit 5)
          (let
            (x/290 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/287 *match*/288 *match*/289))
            (exit 4 x/290))))
     with (5) 0)
   with (4 x/285[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/285) 1)))
(let (*match*/287 =[int] 3 *match*/288 =[int] 2 *match*/289 =[int] 1)
  (catch
    (if (%noteq *match*/285 3)
      (if (%noteq *match*/284 1) 0
        (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
      (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
   with (4 x/282[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/282) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/292[int] b/293 : int 0)
(function {nlocal = 0} a/292[int] b/293 : int 0)
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
(function {nlocal = 0} a/296[int] b/297
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/298 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/296 b/297))
                                              p/298))
(function {nlocal = 0} a/296[int] b/297
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/296 b/297))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/300[int] b/301
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/302 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/300 b/301))
                                              p/302))
(function {nlocal = 0} a/300[int] b/301
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/300 b/301))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
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
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/312[int] b/313
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/314 =a[int] a/312
     p/315 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/312 b/313))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/314
      p/315)))
(function {nlocal = 0} a/312[int] b/313
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/312
    (makeblock 0 a/312 b/313)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/322[int] b/323[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/322
    (let
      (x/324 =a[int] a/322
       p/325 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/322 b/323))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/324
        p/325))
    (let
      (x/326 =a[(consts ()) (non_consts ([0: ]))] b/323
       p/327 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/322 b/323))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/326
        p/327))))
(function {nlocal = 0} a/322[int] b/323[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/322
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/322
      (makeblock 0 a/322 b/323))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/323
      (makeblock 0 a/322 b/323))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/328[int] b/329[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/328
      (let
        (x/336 =a[int] a/328
         p/337 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/328 b/329))
        (exit 10 x/336 p/337))
      (let
        (x/334 =a[(consts ()) (non_consts ([0: ]))] b/329
         p/335 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/328 b/329))
        (exit 10 x/334 p/335)))
   with (10 x/330[int] p/331[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/330
      p/331)))
(function {nlocal = 0} a/328[int] b/329[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/328 (exit 10 a/328 (makeblock 0 a/328 b/329))
      (exit 10 b/329 (makeblock 0 a/328 b/329)))
   with (10 x/330[int] p/331[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/330
      p/331)))
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
(function {nlocal = 0} a/338[int] b/339[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/338
    (let
      (x/340 =a[int] a/338
       _p/341 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/338 b/339))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/340
        [0: 1 1]))
    (let
      (x/342 =a[int] a/338
       p/343 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/338 b/339))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/342
        p/343))))
(function {nlocal = 0} a/338[int] b/339[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/338
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/338
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/338
      (makeblock 0 a/338 b/339))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/344[int] b/345
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/346 =a[int] a/344
     p/347 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/344 b/345))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/346
      p/347)))
(function {nlocal = 0} a/344[int] b/345
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/344
    (makeblock 0 a/344 b/345)))
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
(function {nlocal = 0} a/357[int]
  b/358[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/357
                                                                    (if b/358
                                                                    (let
                                                                    (p/359 =a
                                                                    (field_imm 0
                                                                    b/358))
                                                                    p/359)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/360 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/357
                                                                    b/358))
                                                                    p/360)))
(function {nlocal = 0} a/357[int]
  b/358[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/357
                                                                    (if b/358
                                                                    (field_imm 0
                                                                    b/358)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/357
                                                                    b/358)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/361[int]
  b/362[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/361
                                                                    (if b/362
                                                                    (let
                                                                    (p/366 =a
                                                                    (field_imm 0
                                                                    b/362))
                                                                    (exit 13
                                                                    p/366))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/365 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/361
                                                                    b/362))
                                                                    (exit 13
                                                                    p/365)))
                                                                    with (13 p/363
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/363))
(function {nlocal = 0} a/361[int]
  b/362[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/361
                                                                    (if b/362
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/362))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/361
                                                                    b/362)))
                                                                    with (13 p/363
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/363))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
