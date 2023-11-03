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
(let (*match*/272 =[int] 3 *match*/273 =[int] 2 *match*/274 =[int] 1)
  (catch
    (catch
      (catch (if (!= *match*/273 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/272 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/272 =[int] 3 *match*/273 =[int] 2 *match*/274 =[int] 1)
  (catch (if (!= *match*/273 3) (if (!= *match*/272 1) 0 (exit 1)) (exit 1))
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
(let (*match*/277 =[int] 3 *match*/278 =[int] 2 *match*/279 =[int] 1)
  (catch
    (catch
      (catch
        (if (!= *match*/278 3) (exit 6)
          (let
            (x/281 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/277 *match*/278 *match*/279))
            (exit 4 x/281)))
       with (6)
        (if (!= *match*/277 1) (exit 5)
          (let
            (x/280 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/277 *match*/278 *match*/279))
            (exit 4 x/280))))
     with (5) 0)
   with (4 x/275[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/275) 1)))
(let (*match*/277 =[int] 3 *match*/278 =[int] 2 *match*/279 =[int] 1)
  (catch
    (if (!= *match*/278 3)
      (if (!= *match*/277 1) 0
        (exit 4 (makeblock 0 *match*/277 *match*/278 *match*/279)))
      (exit 4 (makeblock 0 *match*/277 *match*/278 *match*/279)))
   with (4 x/275[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/275) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/282[int] b/283 : int 0)
(function {nlocal = 0} a/282[int] b/283 : int 0)
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
(function {nlocal = 0} a/286[int] b/287
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/288 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/286 b/287))
                                              p/288))
(function {nlocal = 0} a/286[int] b/287
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/286 b/287))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/290[int] b/291
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/292 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/290 b/291))
                                              p/292))
(function {nlocal = 0} a/290[int] b/291
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/290 b/291))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/296[int] b/297
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/298 =a[int] a/296
     p/299 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/296 b/297))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/298
      p/299)))
(function {nlocal = 0} a/296[int] b/297
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/296
    (makeblock 0 a/296 b/297)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/302[int] b/303
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/304 =a[int] a/302
     p/305 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/302 b/303))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/304
      p/305)))
(function {nlocal = 0} a/302[int] b/303
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/302
    (makeblock 0 a/302 b/303)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/312[int] b/313[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/312
    (let
      (x/314 =a[int] a/312
       p/315 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/312 b/313))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/314
        p/315))
    (let
      (x/316 =a[(consts ()) (non_consts ([0: ]))] b/313
       p/317 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/312 b/313))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/316
        p/317))))
(function {nlocal = 0} a/312[int] b/313[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/312
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/312
      (makeblock 0 a/312 b/313))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/313
      (makeblock 0 a/312 b/313))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/318[int] b/319[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/318
      (let
        (x/326 =a[int] a/318
         p/327 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/318 b/319))
        (exit 10 x/326 p/327))
      (let
        (x/324 =a[(consts ()) (non_consts ([0: ]))] b/319
         p/325 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/318 b/319))
        (exit 10 x/324 p/325)))
   with (10 x/320[int] p/321[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/320
      p/321)))
(function {nlocal = 0} a/318[int] b/319[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/318 (exit 10 a/318 (makeblock 0 a/318 b/319))
      (exit 10 b/319 (makeblock 0 a/318 b/319)))
   with (10 x/320[int] p/321[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/320
      p/321)))
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
(function {nlocal = 0} a/328[int] b/329[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/328
    (let
      (x/330 =a[int] a/328
       _p/331 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/328 b/329))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/330
        [0: 1 1]))
    (let
      (x/332 =a[int] a/328
       p/333 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/328 b/329))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/332
        p/333))))
(function {nlocal = 0} a/328[int] b/329[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/328
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/328
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/328
      (makeblock 0 a/328 b/329))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/334[int] b/335
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/336 =a[int] a/334
     p/337 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/334 b/335))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/336
      p/337)))
(function {nlocal = 0} a/334[int] b/335
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/334
    (makeblock 0 a/334 b/335)))
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
(function {nlocal = 0} a/347[int]
  b/348[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/347
                                                                    (if b/348
                                                                    (let
                                                                    (p/349 =a
                                                                    (field_imm 0
                                                                    b/348))
                                                                    p/349)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/350 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/347
                                                                    b/348))
                                                                    p/350)))
(function {nlocal = 0} a/347[int]
  b/348[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/347
                                                                    (if b/348
                                                                    (field_imm 0
                                                                    b/348)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/347
                                                                    b/348)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/351[int]
  b/352[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/351
                                                                    (if b/352
                                                                    (let
                                                                    (p/356 =a
                                                                    (field_imm 0
                                                                    b/352))
                                                                    (exit 13
                                                                    p/356))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/355 =a
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
                                                                    (exit 13
                                                                    p/355)))
                                                                    with (13 p/353
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/353))
(function {nlocal = 0} a/351[int]
  b/352[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/351
                                                                    (if b/352
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/352))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/351
                                                                    b/352)))
                                                                    with (13 p/353
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/353))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
