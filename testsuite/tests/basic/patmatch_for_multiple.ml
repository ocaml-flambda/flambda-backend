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
(let (*match*/283 =[int] 3 *match*/284 =[int] 2 *match*/285 =[int] 1)
  (catch
    (catch
      (catch (if (%noteq *match*/284 3) (exit 3) (exit 1)) with (3)
        (if (%noteq *match*/283 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/283 =[int] 3 *match*/284 =[int] 2 *match*/285 =[int] 1)
  (catch
    (if (%noteq *match*/284 3) (if (%noteq *match*/283 1) 0 (exit 1))
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
(let (*match*/288 =[int] 3 *match*/289 =[int] 2 *match*/290 =[int] 1)
  (catch
    (catch
      (catch
        (if (%noteq *match*/289 3) (exit 6)
          (let
            (x/292 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/288 *match*/289 *match*/290))
            (exit 4 x/292)))
       with (6)
        (if (%noteq *match*/288 1) (exit 5)
          (let
            (x/291 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/288 *match*/289 *match*/290))
            (exit 4 x/291))))
     with (5) 0)
   with (4 x/286[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/286) 1)))
(let (*match*/288 =[int] 3 *match*/289 =[int] 2 *match*/290 =[int] 1)
  (catch
    (if (%noteq *match*/289 3)
      (if (%noteq *match*/288 1) 0
        (exit 4 (makeblock 0 *match*/288 *match*/289 *match*/290)))
      (exit 4 (makeblock 0 *match*/288 *match*/289 *match*/290)))
   with (4 x/286[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/286) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/293[int] b/294 : int 0)
(function {nlocal = 0} a/293[int] b/294 : int 0)
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
(function {nlocal = 0} a/297[int] b/298
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/299 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/297 b/298))
                                              p/299))
(function {nlocal = 0} a/297[int] b/298
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/297 b/298))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/301[int] b/302
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/303 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/301 b/302))
                                              p/303))
(function {nlocal = 0} a/301[int] b/302
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/301 b/302))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/307[int] b/308
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/309 =a[int] a/307
     p/310 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/307 b/308))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/309
      p/310)))
(function {nlocal = 0} a/307[int] b/308
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/307
    (makeblock 0 a/307 b/308)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/313[int] b/314
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/315 =a[int] a/313
     p/316 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/313 b/314))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/315
      p/316)))
(function {nlocal = 0} a/313[int] b/314
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/313
    (makeblock 0 a/313 b/314)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/323[int] b/324[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/323
    (let
      (x/325 =a[int] a/323
       p/326 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/323 b/324))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/325
        p/326))
    (let
      (x/327 =a[(consts ()) (non_consts ([0: ]))] b/324
       p/328 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/323 b/324))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/327
        p/328))))
(function {nlocal = 0} a/323[int] b/324[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/323
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/323
      (makeblock 0 a/323 b/324))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/324
      (makeblock 0 a/323 b/324))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/329[int] b/330[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/329
      (let
        (x/337 =a[int] a/329
         p/338 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/329 b/330))
        (exit 10 x/337 p/338))
      (let
        (x/335 =a[(consts ()) (non_consts ([0: ]))] b/330
         p/336 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/329 b/330))
        (exit 10 x/335 p/336)))
   with (10 x/331[int] p/332[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/331
      p/332)))
(function {nlocal = 0} a/329[int] b/330[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/329 (exit 10 a/329 (makeblock 0 a/329 b/330))
      (exit 10 b/330 (makeblock 0 a/329 b/330)))
   with (10 x/331[int] p/332[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/331
      p/332)))
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
(function {nlocal = 0} a/339[int] b/340[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/339
    (let
      (x/341 =a[int] a/339
       _p/342 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/339 b/340))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/341
        [0: 1 1]))
    (let
      (x/343 =a[int] a/339
       p/344 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/339 b/340))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/343
        p/344))))
(function {nlocal = 0} a/339[int] b/340[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/339
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/339
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/339
      (makeblock 0 a/339 b/340))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/345[int] b/346
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/347 =a[int] a/345
     p/348 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/345 b/346))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/347
      p/348)))
(function {nlocal = 0} a/345[int] b/346
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/345
    (makeblock 0 a/345 b/346)))
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
(function {nlocal = 0} a/358[int]
  b/359[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/358
                                                                    (if b/359
                                                                    (let
                                                                    (p/360 =a
                                                                    (field_imm 0
                                                                    b/359))
                                                                    p/360)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/361 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/358
                                                                    b/359))
                                                                    p/361)))
(function {nlocal = 0} a/358[int]
  b/359[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/358
                                                                    (if b/359
                                                                    (field_imm 0
                                                                    b/359)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/358
                                                                    b/359)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/362[int]
  b/363[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/362
                                                                    (if b/363
                                                                    (let
                                                                    (p/367 =a
                                                                    (field_imm 0
                                                                    b/363))
                                                                    (exit 13
                                                                    p/367))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/366 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/362
                                                                    b/363))
                                                                    (exit 13
                                                                    p/366)))
                                                                    with (13 p/364
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/364))
(function {nlocal = 0} a/362[int]
  b/363[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/362
                                                                    (if b/363
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/363))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/362
                                                                    b/363)))
                                                                    with (13 p/364
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/364))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
