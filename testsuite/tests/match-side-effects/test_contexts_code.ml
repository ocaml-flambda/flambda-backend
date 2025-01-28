(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda";
 stack-allocation;
 expect;
*)

#use "contexts_1.ml";;
(* Notice that (field_mut 1 input) occurs twice, it
   is evaluated once in the 'false' branch and once in the 'true'
   branch. The compiler assumes that its static knowledge about the
   first read (it cannot be a [Right] as we already matched against it
   and failed) also applies to the second read, which is unsound.
*)
[%%expect {|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
<<<<<<< HEAD
  (example_1/297 =
     (function {nlocal = 0} param/321[int]
||||||| parent of f06c0085dd (Locals x effects)
  (example_1/295 =
     (function {nlocal = 0} param/319[int]
=======
  (example_1/296 =
     (function {nlocal = 0} param/320[int]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ()) (non_consts ([1: *] [0: *]))](region
                                                   (let
<<<<<<< HEAD
                                                     (input/299 =
||||||| parent of f06c0085dd (Locals x effects)
                                                     (input/297 =
=======
                                                     (input/298 =
>>>>>>> f06c0085dd (Locals x effects)
                                                        (makelocalmutable 0 (int,
                                                          [(consts ())
                                                           (non_consts (
                                                           [1: *] [0: *]))])
                                                          1 [0: 1]))
                                                     (if
                                                       (field_int 0
<<<<<<< HEAD
                                                         input/299)
||||||| parent of f06c0085dd (Locals x effects)
                                                         input/297)
=======
                                                         input/298)
>>>>>>> f06c0085dd (Locals x effects)
                                                       (let
<<<<<<< HEAD
                                                         (*match*/324 =o
||||||| parent of f06c0085dd (Locals x effects)
                                                         (*match*/322 =o
=======
                                                         (*match*/323 =o
>>>>>>> f06c0085dd (Locals x effects)
                                                            (field_mut 1
<<<<<<< HEAD
                                                              input/299))
                                                         (switch* *match*/324
||||||| parent of f06c0085dd (Locals x effects)
                                                              input/297))
                                                         (switch* *match*/322
=======
                                                              input/298))
                                                         (switch* *match*/323
>>>>>>> f06c0085dd (Locals x effects)
                                                          case tag 0:
                                                           (if
                                                             (seq
                                                               (setfield_ptr(maybe-stack) 1
<<<<<<< HEAD
                                                                 input/299
||||||| parent of f06c0085dd (Locals x effects)
                                                                 input/297
=======
                                                                 input/298
>>>>>>> f06c0085dd (Locals x effects)
                                                                 [1: 3])
                                                               0)
                                                             [1: 3]
                                                             (let
<<<<<<< HEAD
                                                               (*match*/326 =o
||||||| parent of f06c0085dd (Locals x effects)
                                                               (*match*/324 =o
=======
                                                               (*match*/325 =o
>>>>>>> f06c0085dd (Locals x effects)
                                                                  (field_mut 1
<<<<<<< HEAD
                                                                    input/299))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    input/297))
=======
                                                                    input/298))
>>>>>>> f06c0085dd (Locals x effects)
                                                               (makeblock 0 (int)
                                                                 (field_imm 0
<<<<<<< HEAD
                                                                   *match*/326))))
||||||| parent of f06c0085dd (Locals x effects)
                                                                   *match*/324))))
=======
                                                                   *match*/325))))
>>>>>>> f06c0085dd (Locals x effects)
                                                          case tag 1: [1: 2]))
                                                       [1: 1])))))
<<<<<<< HEAD
  (apply (field_imm 1 (global Toploop!)) "example_1" example_1/297))
||||||| parent of f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "example_1" example_1/295))
=======
  (apply (field_imm 1 (global Toploop!)) "example_1" example_1/296))
>>>>>>> f06c0085dd (Locals x effects)
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_2.ml";;
[%%expect {|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
<<<<<<< HEAD
  (example_2/333 =
     (function {nlocal = 0} param/337[int]
||||||| parent of f06c0085dd (Locals x effects)
  (example_2/331 =
     (function {nlocal = 0} param/335[int]
=======
  (example_2/332 =
     (function {nlocal = 0} param/336[int]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ()) (non_consts ([1: *] [0: *]))](region
                                                   (let
<<<<<<< HEAD
                                                     (input/335 =[(consts ())
||||||| parent of f06c0085dd (Locals x effects)
                                                     (input/333 =[(consts ())
=======
                                                     (input/334 =[(consts ())
>>>>>>> f06c0085dd (Locals x effects)
                                                                  (non_consts (
                                                                  [0: [int],
                                                                   *]))]
                                                        (makelocalblock 0 (int,*)
                                                          1
                                                          (makelocalmutable 0 (
                                                            [(consts ())
                                                             (non_consts (
                                                             [1: *] [0: *]))])
                                                            [0: 1])))
                                                     (if
                                                       (field_int 0
<<<<<<< HEAD
                                                         input/335)
||||||| parent of f06c0085dd (Locals x effects)
                                                         input/333)
=======
                                                         input/334)
>>>>>>> f06c0085dd (Locals x effects)
                                                       (let
<<<<<<< HEAD
                                                         (*match*/341 =o
||||||| parent of f06c0085dd (Locals x effects)
                                                         (*match*/339 =o
=======
                                                         (*match*/340 =o
>>>>>>> f06c0085dd (Locals x effects)
                                                            (field_mut 0
                                                              (field_imm 1
<<<<<<< HEAD
                                                                input/335)))
                                                         (switch* *match*/341
||||||| parent of f06c0085dd (Locals x effects)
                                                                input/333)))
                                                         (switch* *match*/339
=======
                                                                input/334)))
                                                         (switch* *match*/340
>>>>>>> f06c0085dd (Locals x effects)
                                                          case tag 0:
                                                           (if
                                                             (seq
                                                               (setfield_ptr(maybe-stack) 0
                                                                 (field_imm 1
<<<<<<< HEAD
                                                                   input/335)
||||||| parent of f06c0085dd (Locals x effects)
                                                                   input/333)
=======
                                                                   input/334)
>>>>>>> f06c0085dd (Locals x effects)
                                                                 [1: 3])
                                                               0)
                                                             [1: 3]
                                                             (let
<<<<<<< HEAD
                                                               (*match*/344 =o
||||||| parent of f06c0085dd (Locals x effects)
                                                               (*match*/342 =o
=======
                                                               (*match*/343 =o
>>>>>>> f06c0085dd (Locals x effects)
                                                                  (field_mut 0
                                                                    (field_imm 1
<<<<<<< HEAD
                                                                    input/335)))
||||||| parent of f06c0085dd (Locals x effects)
                                                                    input/333)))
=======
                                                                    input/334)))
>>>>>>> f06c0085dd (Locals x effects)
                                                               (makeblock 0 (int)
                                                                 (field_imm 0
<<<<<<< HEAD
                                                                   *match*/344))))
||||||| parent of f06c0085dd (Locals x effects)
                                                                   *match*/342))))
=======
                                                                   *match*/343))))
>>>>>>> f06c0085dd (Locals x effects)
                                                          case tag 1: [1: 2]))
                                                       [1: 1])))))
<<<<<<< HEAD
  (apply (field_imm 1 (global Toploop!)) "example_2" example_2/333))
||||||| parent of f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "example_2" example_2/331))
=======
  (apply (field_imm 1 (global Toploop!)) "example_2" example_2/332))
>>>>>>> f06c0085dd (Locals x effects)
val example_2 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_3.ml";;
[%%expect {|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
<<<<<<< HEAD
  (example_3/350 =
     (function {nlocal = 0} param/354[int]
||||||| parent of f06c0085dd (Locals x effects)
  (example_3/348 =
     (function {nlocal = 0} param/352[int]
=======
  (example_3/349 =
     (function {nlocal = 0} param/353[int]
>>>>>>> f06c0085dd (Locals x effects)
       [(consts ()) (non_consts ([1: *] [0: *]))](region
                                                   (let
<<<<<<< HEAD
                                                     (input/352 =mut[(consts ())
||||||| parent of f06c0085dd (Locals x effects)
                                                     (input/350 =mut[(consts ())
=======
                                                     (input/351 =mut[(consts ())
>>>>>>> f06c0085dd (Locals x effects)
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [1: *]
                                                                    [0: *]))]]))]
                                                        [0: 1 [0: 1]]
<<<<<<< HEAD
                                                      *match*/355 =o
                                                        *input/352)
||||||| parent of f06c0085dd (Locals x effects)
                                                      *match*/353 =o
                                                        *input/350)
=======
                                                      *match*/354 =o
                                                        *input/351)
>>>>>>> f06c0085dd (Locals x effects)
                                                     (if
                                                       (field_imm 0
<<<<<<< HEAD
                                                         *match*/355)
||||||| parent of f06c0085dd (Locals x effects)
                                                         *match*/353)
=======
                                                         *match*/354)
>>>>>>> f06c0085dd (Locals x effects)
                                                       (switch* (field_imm 1
<<<<<<< HEAD
                                                                  *match*/355)
||||||| parent of f06c0085dd (Locals x effects)
                                                                  *match*/353)
=======
                                                                  *match*/354)
>>>>>>> f06c0085dd (Locals x effects)
                                                        case tag 0:
                                                         (if
                                                           (seq
                                                             (assign
<<<<<<< HEAD
                                                               input/352
||||||| parent of f06c0085dd (Locals x effects)
                                                               input/350
=======
                                                               input/351
>>>>>>> f06c0085dd (Locals x effects)
                                                               [0: 1 [1: 3]])
                                                             0)
                                                           [1: 3]
                                                           (makeblock 0 (int)
                                                             (field_imm 0
                                                               (field_imm 1
<<<<<<< HEAD
                                                                 *match*/355))))
||||||| parent of f06c0085dd (Locals x effects)
                                                                 *match*/353))))
=======
                                                                 *match*/354))))
>>>>>>> f06c0085dd (Locals x effects)
                                                        case tag 1: [1: 2])
                                                       [1: 1])))))
<<<<<<< HEAD
  (apply (field_imm 1 (global Toploop!)) "example_3" example_3/350))
||||||| parent of f06c0085dd (Locals x effects)
  (apply (field_imm 1 (global Toploop!)) "example_3" example_3/348))
=======
  (apply (field_imm 1 (global Toploop!)) "example_3" example_3/349))
>>>>>>> f06c0085dd (Locals x effects)
val example_3 : unit -> (bool, int) Result.t = <fun>
|}]
