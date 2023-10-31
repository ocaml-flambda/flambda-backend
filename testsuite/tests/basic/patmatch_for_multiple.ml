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
(let (*match*/271 =[int] 3 *match*/272 =[int] 2 *match*/273 =[int] 1)
  (catch
    (catch
      (catch (if (!= *match*/272 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/271 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/271 =[int] 3 *match*/272 =[int] 2 *match*/273 =[int] 1)
  (catch (if (!= *match*/272 3) (if (!= *match*/271 1) 0 (exit 1)) (exit 1))
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
(let (*match*/276 =[int] 3 *match*/277 =[int] 2 *match*/278 =[int] 1)
  (catch
    (catch
      (catch
        (if (!= *match*/277 3) (exit 6)
          (let
            (x/280 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/276 *match*/277 *match*/278))
            (exit 4 x/280)))
       with (6)
        (if (!= *match*/276 1) (exit 5)
          (let
            (x/279 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/276 *match*/277 *match*/278))
            (exit 4 x/279))))
     with (5) 0)
   with (4 x/274[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/274) 1)))
(let (*match*/276 =[int] 3 *match*/277 =[int] 2 *match*/278 =[int] 1)
  (catch
    (if (!= *match*/277 3)
      (if (!= *match*/276 1) 0
        (exit 4 (makeblock 0 *match*/276 *match*/277 *match*/278)))
      (exit 4 (makeblock 0 *match*/276 *match*/277 *match*/278)))
   with (4 x/274[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/274) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/281[int] b/282 : int 0)
(function {nlocal = 0} a/281[int] b/282 : int 0)
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
(function {nlocal = 0} a/285[int] b/286
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/287 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/285 b/286))
                                              p/287))
(function {nlocal = 0} a/285[int] b/286
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/285 b/286))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/289[int] b/290
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/291 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/289 b/290))
                                              p/291))
(function {nlocal = 0} a/289[int] b/290
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/289 b/290))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/295[int] b/296
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/297 =a[int] a/295
     p/298 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/295 b/296))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/297
      p/298)))
(function {nlocal = 0} a/295[int] b/296
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/295
    (makeblock 0 a/295 b/296)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/301[int] b/302
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/303 =a[int] a/301
     p/304 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/301 b/302))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/303
      p/304)))
(function {nlocal = 0} a/301[int] b/302
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/301
    (makeblock 0 a/301 b/302)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/311[int] b/312[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/311
    (let
      (x/313 =a[int] a/311
       p/314 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/311 b/312))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/313
        p/314))
    (let
      (x/315 =a[(consts ()) (non_consts ([0: ]))] b/312
       p/316 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/311 b/312))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/315
        p/316))))
(function {nlocal = 0} a/311[int] b/312[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/311
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/311
      (makeblock 0 a/311 b/312))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/312
      (makeblock 0 a/311 b/312))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/317[int] b/318[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/317
      (let
        (x/325 =a[int] a/317
         p/326 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/317 b/318))
        (exit 10 x/325 p/326))
      (let
        (x/323 =a[(consts ()) (non_consts ([0: ]))] b/318
         p/324 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/317 b/318))
        (exit 10 x/323 p/324)))
   with (10 x/319[int] p/320[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/319
      p/320)))
(function {nlocal = 0} a/317[int] b/318[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/317 (exit 10 a/317 (makeblock 0 a/317 b/318))
      (exit 10 b/318 (makeblock 0 a/317 b/318)))
   with (10 x/319[int] p/320[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/319
      p/320)))
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
(function {nlocal = 0} a/327[int] b/328[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/327
    (let
      (x/329 =a[int] a/327
       _p/330 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/327 b/328))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/329
        [0: 1 1]))
    (let
      (x/331 =a[int] a/327
       p/332 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/327 b/328))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/331
        p/332))))
(function {nlocal = 0} a/327[int] b/328[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/327
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/327
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/327
      (makeblock 0 a/327 b/328))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/333[int] b/334
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/335 =a[int] a/333
     p/336 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/333 b/334))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/335
      p/336)))
(function {nlocal = 0} a/333[int] b/334
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/333
    (makeblock 0 a/333 b/334)))
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
(function {nlocal = 0} a/346[int]
  b/347[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/346
                                                                    (if b/347
                                                                    (let
                                                                    (p/348 =a
                                                                    (field_imm 0
                                                                    b/347))
                                                                    p/348)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/349 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/346
                                                                    b/347))
                                                                    p/349)))
(function {nlocal = 0} a/346[int]
  b/347[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/346
                                                                    (if b/347
                                                                    (field_imm 0
                                                                    b/347)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/346
                                                                    b/347)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/350[int]
  b/351[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/350
                                                                    (if b/351
                                                                    (let
                                                                    (p/355 =a
                                                                    (field_imm 0
                                                                    b/351))
                                                                    (exit 13
                                                                    p/355))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
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
                                                                    a/350
                                                                    b/351))
                                                                    (exit 13
                                                                    p/354)))
                                                                    with (13 p/352
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/352))
(function {nlocal = 0} a/350[int]
  b/351[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/350
                                                                    (if b/351
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/351))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/350
                                                                    b/351)))
                                                                    with (13 p/352
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/352))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
